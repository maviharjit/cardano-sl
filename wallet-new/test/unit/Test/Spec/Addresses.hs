{-# LANGUAGE TypeApplications #-}
module Test.Spec.Addresses (spec, withFixture, Fixture(..)) where

import           Universum

import           Control.Monad.Except (runExceptT)
import           Data.Acid (update)
import qualified Data.ByteString as B
-- import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Formatting (build, sformat)
import           Servant.Server

import           Test.Hspec (Spec, describe, shouldBe, shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (arbitrary, choose, withMaxSuccess)
import           Test.QuickCheck.Monadic (PropertyM, monadicIO, pick)

import           Pos.Core (Address, addrRoot)
import           Pos.Crypto (EncryptedSecretKey, safeDeterministicKeyGen)

import           Cardano.Wallet.API.Request (RequestParams (..))
import           Cardano.Wallet.API.Request.Pagination (Page (..),
                     PaginationParams (..), PerPage (..))
import           Cardano.Wallet.API.Response (WalletResponse (wrData))
import           Cardano.Wallet.API.V1.Handlers.Addresses as Handlers
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel.Addresses as Kernel
import           Cardano.Wallet.Kernel.DB.AcidState
import           Cardano.Wallet.Kernel.DB.HdWallet (AssuranceLevel (..),
                     HasSpendingPassword (..), HdAccountId (..),
                     HdAccountIx (..), HdRootId (..), WalletName (..),
                     eskToHdRootId)
import           Cardano.Wallet.Kernel.DB.HdWallet.Create (initHdRoot)
import           Cardano.Wallet.Kernel.DB.HdWallet.Derivation
                     (HardeningMode (..), deriveIndex)
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Cardano.Wallet.Kernel.Internal (PassiveWallet, wallets)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import qualified Cardano.Wallet.Kernel.Read as Kernel
import           Cardano.Wallet.Kernel.Types (AccountId (..), WalletId (..))
import           Cardano.Wallet.WalletLayer (PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer
import qualified Cardano.Wallet.WalletLayer.Kernel.Accounts as Accounts
import qualified Cardano.Wallet.WalletLayer.Kernel.Addresses as Addresses
import qualified Cardano.Wallet.WalletLayer.Kernel.Conv as Kernel.Conv
import qualified Cardano.Wallet.WalletLayer.Kernel.Wallets as Wallets

import qualified Test.Spec.Fixture as Fixture
import qualified Test.Spec.Wallets as Wallets

import           Util.Buildable (ShowThroughBuild (..))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

data Fixture = Fixture {
      fixtureHdRootId  :: HdRootId
    , fixtureESK       :: EncryptedSecretKey
    , fixtureAccountId :: AccountId
    , fixturePw        :: PassiveWallet
    }

data AddressFixture = AddressFixture {
    addressFixtureAddress :: V1.WalletAddress
    }

-- | Prepare some fixtures using the 'PropertyM' context to prepare the data,
-- and execute the 'acid-state' update once the 'PassiveWallet' gets into
-- scope (after the bracket initialisation).
prepareFixtures :: Fixture.GenPassiveWalletFixture Fixture
prepareFixtures = do
    let (_, esk) = safeDeterministicKeyGen (B.pack $ replicate 32 0x42) mempty
    let newRootId = eskToHdRootId esk
    newRoot <- initHdRoot <$> pure newRootId
                          <*> pure (WalletName "A wallet")
                          <*> pure NoSpendingPassword
                          <*> pure AssuranceLevelNormal
                          <*> (InDb <$> pick arbitrary)
    newAccountId <- HdAccountId newRootId <$> deriveIndex (pick . choose) HdAccountIx HardDerivation
    let accounts = M.singleton newAccountId mempty
    return $ \pw -> do
        void $ liftIO $ update (pw ^. wallets) (CreateHdWallet newRoot accounts)
        return $ Fixture {
                           fixtureHdRootId = newRootId
                         , fixtureAccountId = AccountIdHdRnd newAccountId
                         , fixtureESK = esk
                         , fixturePw  = pw
                         }

prepareAddressFixture
  :: Int  -- ^ Number of 'AddressFixture's to create.
  -> Fixture.GenPassiveWalletFixture [AddressFixture]
prepareAddressFixture n = do
    spendingPassword <- Fixture.genSpendingPassword
    newWalletRq <- WalletLayer.CreateWallet <$> Wallets.genNewWalletRq spendingPassword
    return $ \pw -> do
        Right v1Wallet <- Wallets.createWallet pw newWalletRq
        -- Get all the available accounts
        db <- Kernel.getWalletSnapshot pw
        let Right accs = Accounts.getAccounts (V1.walId v1Wallet) db
        let (acc : _) = IxSet.toList accs
        let newAddressRq = V1.NewAddress spendingPassword (V1.accIndex acc) (V1.walId v1Wallet)
        res <- replicateM n (Addresses.createAddress pw newAddressRq)
        case sequence res of
             Left e     -> error (show e)
             Right addr -> return (map AddressFixture addr)

prepareAddressesFixture
    :: Int  -- ^ Number of Accounts to create.
    -> Int  -- ^ Number of 'Address per account to create.
    -> Fixture.GenPassiveWalletFixture (M.Map V1.AccountIndex [V1.WalletAddress])
prepareAddressesFixture acn adn = do
    spendingPassword <- Fixture.genSpendingPassword
    newWalletRq <- WalletLayer.CreateWallet <$> Wallets.genNewWalletRq spendingPassword
    return $ \pw -> do
        let newAcc (n :: Int) = (V1.NewAccount spendingPassword ("My Account " <> show n))
        Right v1Wallet <- Wallets.createWallet pw newWalletRq
        forM_ [1..acn] $ \n ->
            Accounts.createAccount pw (V1.walId v1Wallet) (WalletLayer.CreateHdAccountRandomIndex $ newAcc n)
        -- Get all the available accounts
        db <- Kernel.getWalletSnapshot pw
        let Right accs = Accounts.getAccounts (V1.walId v1Wallet) db
        let accounts = IxSet.toList accs
        length accounts `shouldBe` (acn + 1)
        let insertAddresses :: V1.Account -> IO (V1.AccountIndex, [V1.WalletAddress])
            insertAddresses acc = do
                let accId = V1.accIndex acc
                let newAddressRq = V1.NewAddress spendingPassword accId (V1.walId v1Wallet)
                res <- replicateM adn (Addresses.createAddress pw newAddressRq)
                case sequence res of
                    Left e     -> error (show e)
                    Right addr -> return (accId, addr)
        res <- mapM insertAddresses accounts
        return $ M.fromList res


withFixture :: (  Keystore.Keystore
               -> PassiveWalletLayer IO
               -> PassiveWallet
               -> Fixture
               -> IO a
               )
            -> PropertyM IO a
withFixture = Fixture.withPassiveWalletFixture prepareFixtures

withAddressFixtures
  :: Int -- Number of fixture addresses to create
  -> (  Keystore.Keystore
     -> PassiveWalletLayer IO
     -> PassiveWallet
     -> [AddressFixture]
     -> IO a
     )
  -> PropertyM IO a
withAddressFixtures n =
  Fixture.withPassiveWalletFixture $ do
      prepareAddressFixture n

withAddressesFixtures :: Int -> Int ->
       (  Keystore.Keystore
    -> PassiveWalletLayer IO
    -> PassiveWallet
    -> M.Map V1.AccountIndex [V1.WalletAddress]
    -> IO a
    )
    -> PropertyM IO a
withAddressesFixtures n m =
    Fixture.withPassiveWalletFixture $ do
        prepareAddressesFixture n m

spec :: Spec
spec = describe "Addresses" $ do
    describe "CreateAddress" $ do
        describe "Address listing with multiple Accounts (Servant)" $ do


            prop "page 1, 2 pages" $ withMaxSuccess 20 $ do
                monadicIO $
                    withAddressesFixtures 3 8 $ \_ layer _ _ -> do
                        let (expectedTotal :: Int) = (3 + 1)*(8 + 1) -1
                        let pp = PaginationParams (Page 1) (PerPage 40)
                        let pp1 = PaginationParams (Page 1) (PerPage (quot expectedTotal 3 + 1))
                        let pp2 = PaginationParams (Page 2) (PerPage (quot expectedTotal 3 + 1))
                        let pp3 = PaginationParams (Page 3) (PerPage (quot expectedTotal 3 + 1))
                        res <- runExceptT $ runHandler' $ do
                            Handlers.listAddresses layer (RequestParams pp)
                        res1 <- runExceptT $ runHandler' $ do
                            Handlers.listAddresses layer (RequestParams pp1)
                        res2 <- runExceptT $ runHandler' $ do
                            Handlers.listAddresses layer (RequestParams pp2)
                        res3 <- runExceptT $ runHandler' $ do
                            Handlers.listAddresses layer (RequestParams pp3)
                        case (res, res1, res2, res3) of
                            (Right wr, Right wr1, Right wr2, Right wr3) -> do
                                length (wrData wr) `shouldBe` expectedTotal
                                length (wrData wr1) + length (wrData wr2) + length (wrData wr3) `shouldBe` expectedTotal
                                (addrRoot . V1.unV1 . V1.addrId <$> wrData wr1 <> wrData wr2 <> wrData wr3)
                                    `shouldBe` (addrRoot . V1.unV1 . V1.addrId <$> wrData wr) -- (addrRoot . V1.unV1 . V1.addrId <$> concat (M.elems mp))
--                                (addrRoot . V1.unV1 . V1.addrId <$> wrData wr1 <> wrData wr2) `shouldSatisfy`
--                                    (L.isSubsequenceOf (addrRoot . V1.unV1 . V1.addrId <$> concat (M.elems mp)))
                            _        -> fail ("Got " ++ show res)

            prop "page 0, per page 0" $ withMaxSuccess 20 $ do
                monadicIO $
                    withAddressesFixtures 4 4 $ \_ layer _ _ -> do
                        let pp = PaginationParams (Page 0) (PerPage 0)
                        res <- runExceptT $ runHandler' $ do
                            Handlers.listAddresses layer (RequestParams pp)
                        case res of
                            Right wr | null (wrData wr) -> pure ()
                            _ -> fail ("Got " ++ show res)

            prop "page 1, per page 40" $ withMaxSuccess 20 $ do
                monadicIO $
                    withAddressesFixtures 3 4 $ \_ layer _ _ -> do
                        let pp = PaginationParams (Page 1) (PerPage 40)
                        res <- runExceptT $ runHandler' $ do
                            Handlers.listAddresses layer (RequestParams pp)
                        case res of
                            Right wr -> do
                                length (wrData wr) `shouldBe` ((3 + 1)*(4 + 1) - 1)
                            _        -> fail ("Got " ++ show res)


        describe "Address creation (wallet layer)" $ do
            prop "works as expected in the happy path scenario" $ withMaxSuccess 200 $
                monadicIO $ do
                    withFixture $ \keystore layer _ Fixture{..} -> do
                        Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                        let (HdRootId hdRoot) = fixtureHdRootId
                            (AccountIdHdRnd myAccountId) = fixtureAccountId
                            wId = sformat build (view fromDb hdRoot)
                            accIdx = Kernel.Conv.toAccountId myAccountId
                        res <- WalletLayer.createAddress layer (V1.NewAddress Nothing accIdx (V1.WalletId wId))
                        (bimap STB STB res) `shouldSatisfy` isRight

        describe "Address creation (kernel)" $ do
            prop "works as expected in the happy path scenario" $ withMaxSuccess 200 $
                monadicIO $ do
                    withFixture $ \keystore _ _ Fixture{..} -> do
                        Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                        res <- Kernel.createAddress mempty fixtureAccountId fixturePw
                        (bimap STB STB res) `shouldSatisfy` isRight

            prop "fails if the account has no associated key in the keystore" $ do
                monadicIO $ do
                    withFixture $ \_ _ _ Fixture{..} -> do
                        res <- Kernel.createAddress mempty fixtureAccountId fixturePw
                        case res of
                            (Left (Kernel.CreateAddressKeystoreNotFound acc)) | acc == fixtureAccountId -> return ()
                            x -> fail (show (bimap STB STB x))

            prop "fails if the parent account doesn't exist" $ do
                monadicIO $ do
                    withFixture $ \keystore _ _ Fixture{..} -> do
                        Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                        let (AccountIdHdRnd hdAccountId) = fixtureAccountId
                        void $ update (fixturePw ^. wallets) (DeleteHdAccount hdAccountId)
                        res <- Kernel.createAddress mempty fixtureAccountId fixturePw
                        case res of
                            Left (Kernel.CreateAddressUnknownHdAccount _) -> return ()
                            x -> fail (show (bimap STB STB x))

        describe "Address creation (Servant)" $ do
            prop "works as expected in the happy path scenario" $ do
                monadicIO $
                    withFixture $ \keystore layer _ Fixture{..} -> do
                        Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                        let (HdRootId hdRoot) = fixtureHdRootId
                            (AccountIdHdRnd myAccountId) = fixtureAccountId
                            wId = sformat build (view fromDb hdRoot)
                            accIdx = Kernel.Conv.toAccountId myAccountId
                            req = V1.NewAddress Nothing accIdx (V1.WalletId wId)
                        res <- runExceptT . runHandler' $ Handlers.newAddress layer req
                        (bimap identity STB res) `shouldSatisfy` isRight

        describe "Address creation (wallet layer & kernel consistency)" $ do
            prop "layer & kernel agrees on the result" $ do
                monadicIO $ do
                    res1 <- withFixture $ \keystore _ _ Fixture{..} -> do
                        Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                        Kernel.createAddress mempty fixtureAccountId fixturePw
                    res2 <- withFixture $ \keystore layer _ Fixture{..} -> do
                        Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                        let (HdRootId hdRoot) = fixtureHdRootId
                            (AccountIdHdRnd myAccountId) = fixtureAccountId
                            wId = sformat build (view fromDb hdRoot)
                            accIdx = Kernel.Conv.toAccountId myAccountId
                        WalletLayer.createAddress layer (V1.NewAddress Nothing accIdx (V1.WalletId wId))
                    case res2 of
                         Left (WalletLayer.CreateAddressError err) ->
                             return $ (bimap STB STB res1) `shouldBe` (bimap STB STB (Left err))
                         Left (WalletLayer.CreateAddressAddressDecodingFailed _) ->
                             fail "Layer & Kernel mismatch: impossible error, CreateAddressAddressDecodingFailed"
                         Right _ -> do
                             -- If we get and 'Address', let's check that this is the case also for
                             -- the kernel run. Unfortunately we cannot compare the two addresses for equality
                             -- because the random index will be generated with a seed which changes every time
                             -- as we uses random, IO-based generation deep down the guts.
                             return $ (bimap STB STB res1) `shouldSatisfy` isRight

        describe "Address listing (Servant)" $ do
            prop "0 addresses, page 0, per page 0" $ do
                monadicIO $
                    withAddressFixtures 0 $ \_ layer _ _ -> do
                        let pp = PaginationParams (Page 0) (PerPage 0)
                        res <- runExceptT $ runHandler' $ do
                           Handlers.listAddresses layer (RequestParams pp)
                        case res of
                           Right wr | null (wrData wr) -> pure ()
                           _ -> fail ("Got " ++ show res)

            prop "1 addresses, page 0, per page 0" $ do
                monadicIO $
                    withAddressFixtures 1 $ \_ layer _ _ -> do
                        let pp = PaginationParams (Page 0) (PerPage 0)
                        res <- runExceptT $ runHandler' $ do
                           Handlers.listAddresses layer (RequestParams pp)
                        case res of
                           Right wr | null (wrData wr) -> pure ()
                           _ -> fail ("Got " ++ show res)

            prop "3 addresses, page 0, per page 0" $ do
                monadicIO $
                    withAddressFixtures 3 $ \_ layer _ _ -> do
                        let pp = PaginationParams (Page 0) (PerPage 0)
                        res <- runExceptT $ runHandler' $ do
                           Handlers.listAddresses layer (RequestParams pp)
                        case res of
                           Right wr | null (wrData wr) -> pure ()
                           _ -> fail ("Got " ++ show res)

            prop "3 addresses, page 1, per page 0" $ do
                monadicIO $
                    withAddressFixtures 3 $ \_ layer _ _ -> do
                        let pp = PaginationParams (Page 1) (PerPage 0)
                        res <- runExceptT $ runHandler' $ do
                           Handlers.listAddresses layer (RequestParams pp)
                        case res of
                           Right wr | null (wrData wr) -> pure ()
                           _ -> fail ("Got " ++ show res)

            prop "3 addresses, page 1, per page 1" $ do
                monadicIO $
                    withAddressFixtures 3 $ \_ layer _ [wa0, _, _] -> do
                        let pp = PaginationParams (Page 1) (PerPage 1)
                        res <- runExceptT $ runHandler' $ do
                           Handlers.listAddresses layer (RequestParams pp)
                        case res of
                           Right wr | [wa0'] <- wrData wr
                                    , wa0' == addressFixtureAddress wa0
                                    -> pure ()
                           _ -> fail ("Got " ++ show res)

            prop "3 addresses, page 1, per page 2" $ do
                monadicIO $
                    withAddressFixtures 3 $ \_ layer _ [wa0, wa1, _wa2] -> do
                        let pp = PaginationParams (Page 1) (PerPage 2)
                        res <- runExceptT $ runHandler' $ do
                           Handlers.listAddresses layer (RequestParams pp)
                        case res of
                           Right wr | [wa0', wa1'] <- wrData wr
                                    , wa0' == addressFixtureAddress wa0
                                    , wa1' == addressFixtureAddress wa1
                                    -> pure ()
                           _ -> fail ("Got " ++ show res)

            prop "3 addresses, page 1, per page 3" $ do
                monadicIO $
                    withAddressFixtures 3 $ \_ layer _ [wa0, wa1, wa2] -> do
                        let pp = PaginationParams (Page 1) (PerPage 3)
                        res <- runExceptT $ runHandler' $ do
                           Handlers.listAddresses layer (RequestParams pp)
                        case res of
                           Right wr | [wa0', wa1', wa2'] <- wrData wr
                                    , wa0' == addressFixtureAddress wa0
                                    , wa1' == addressFixtureAddress wa1
                                    , wa2' == addressFixtureAddress wa2
                                    -> pure ()
                           _ -> fail ("Got " ++ show res)

            prop "3 addresses, page 2, per page 2" $ do
                monadicIO $
                    withAddressFixtures 3 $ \_ layer _ [_wa0, _wa1, wa2] -> do
                        let pp = PaginationParams (Page 2) (PerPage 2)
                        res <- runExceptT $ runHandler' $ do
                           Handlers.listAddresses layer (RequestParams pp)
                        case res of
                           Right wr | [wa2'] <- wrData wr
                                    , wa2' == addressFixtureAddress wa2
                                    -> pure ()
                           _ -> fail ("Got " ++ show res)

            prop "4 addresses, page 2, per page 2" $ do
                monadicIO $
                    withAddressFixtures 4 $ \_ layer _ [_wa0, _wa1, wa2, wa3] -> do
                        let pp = PaginationParams (Page 2) (PerPage 2)
                        res <- runExceptT $ runHandler' $ do
                           Handlers.listAddresses layer (RequestParams pp)
                        case res of
                           Right wr | [wa2', wa3'] <- wrData wr
                                    , wa2' == addressFixtureAddress wa2
                                    , wa3' == addressFixtureAddress wa3
                                    -> pure ()
                           _ -> fail ("Got " ++ show res)

    describe "ValidateAddress" $ do
        describe "Address validation (wallet layer)" $ do

            prop "works as expected in the happy path scenario (valid address, ours)" $ withMaxSuccess 25 $
                monadicIO $ do
                    withAddressFixtures 1 $ \_ layer _ [af] -> do
                        res <- WalletLayer.validateAddress layer
                            (sformat build (V1.unV1 $ V1.addrId $ addressFixtureAddress af))
                        bimap STB STB res `shouldSatisfy` isRight

            prop "rejects a malformed address" $ withMaxSuccess 1 $
                monadicIO $ do
                    withAddressFixtures 1 $ \_ layer _ _ -> do
                        res <- WalletLayer.validateAddress layer "foobar"
                        case res of
                             Left (WalletLayer.ValidateAddressDecodingFailed "foobar") -> return ()
                             Left err -> fail $ "Got different error than expected: " <> show err
                             Right _ -> fail "I was expecting a failure, but it didn't happen."

            prop "returns not used/not change for an address which is not ours" $ withMaxSuccess 1 $ do
                monadicIO $ do
                    (randomAddr :: Address) <- pick arbitrary
                    let expected :: V1.WalletAddress
                        expected = V1.WalletAddress {
                            addrId            = V1.V1 randomAddr
                          , addrUsed          = False
                          , addrChangeAddress = False
                          }
                    withAddressFixtures 1 $ \_ layer _ _ -> do
                        res <- WalletLayer.validateAddress layer (sformat build randomAddr)
                        bimap STB STB res `shouldBe` bimap STB STB (Right expected)
