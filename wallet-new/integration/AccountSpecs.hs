{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module AccountSpecs (accountSpecs) where

import           Universum

import           Cardano.Wallet.API.Indices (accessIx)
import           Cardano.Wallet.Client.Http
import           Control.Concurrent (threadDelay)
import           Control.Lens
import           Pos.Core.Common (mkCoin)
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (arbitrary, shuffle, withMaxSuccess)
import           Test.QuickCheck.Monadic (PropertyM, monadicIO, pick, run)

import           Util

import qualified Pos.Core as Core
import qualified Prelude


accountSpecs :: WalletRef -> WalletClient IO -> Spec
accountSpecs wRef wc =

    describe "Accounts" $ do

        prop "can retrieve only an account's balance" $ withMaxSuccess 10 $
            monadicIO $ do
            let zero = V1 (mkCoin 0)
            (Wallet{..}, Account{..}) <- run $ randomAccount wc
            eresp <- run $ getAccountBalance wc walId accIndex

            partialAccount <- run $ wrData <$> eresp `shouldPrism` _Right
            liftIO $ partialAccount `shouldBe` AccountBalance zero

        prop "can retrieve only an account's addresses" $ withMaxSuccess 10 $
            monadicIO $ do
            pair@(Wallet{..}, Account{..}) <- run $ randomAccount wc
            addresses <- run $ createAddresses wc 10 pair
            let addr = Prelude.head addresses
            let tests =
                    [ PaginationTest (Just 1) (Just 5) NoFilters NoSorts
                        (expectNAddresses 5)
                    , PaginationTest (Just 1) (Just 5) (filterByAddress addr) NoSorts
                        (expectExactlyAddresses [addr])
                    , PaginationTest (Just 2) (Just 5) (filterByAddress addr) NoSorts
                        (expectExactlyAddresses [])
                    ]

            forM_ tests $ \PaginationTest{..} -> do
                eresp <- run $ getAccountAddresses wc walId accIndex page perPage filters
                liftIO $ expectations . acaAddresses . wrData =<< eresp `shouldPrism` _Right

        prop ("can retrieve initial and updated balances of several accounts from getAccountBalances"
              <> "that are equivalent to what is obtained from getAccounts") $ withMaxSuccess 1 $
            monadicIO $ do
            genesis <- run $ genesisWallet wc
            (fromAcct, _) <- run $ firstAccountAndId wc genesis

            wallet <- run $ sampleWallet wRef wc
            -- We create 4 accounts, plus one is created automatically
            -- by the 'sampleWallet', for a total of 5.
            randomNewAccount <- forM [1..4] $ \(_i :: Int) ->
                pick arbitrary :: PropertyM IO NewAccount
            forM_ randomNewAccount $ \(rAcc :: NewAccount) ->
                run $ postAccount wc (walId wallet) rAcc

            accResp' <- run $ getAccounts wc (walId wallet)
            accs <- run $ wrData <$> accResp' `shouldPrism` _Right

            balancesPartialResp' <- forM (map accIndex accs) $ \(accIndex :: AccountIndex) ->
                run $ getAccountBalance wc (walId wallet) accIndex

            balancesPartial <- run $ mapM (\resp -> wrData <$> resp `shouldPrism` _Right) balancesPartialResp'

            liftIO $ map (AccountBalance . accAmount) accs `shouldBe` balancesPartial

            -- Now transfering money to 5 accounts from genesis wallet and checking balances once again
            let payment amount toAddr = Payment
                    { pmtSource =  PaymentSource
                        { psWalletId = walId genesis
                        , psAccountIndex = accIndex fromAcct
                        }
                    , pmtDestinations = pure PaymentDistribution
                        { pdAddress = addrId toAddr
                        , pdAmount = V1 (Core.mkCoin amount)
                        }
                    , pmtGroupingPolicy = Nothing
                    , pmtSpendingPassword = Nothing
                    }
            amounts <- pick $ shuffle [1..5]
            let addrAndAmount = zip (map (\(addr : _) -> addr) $ map accAddresses accs) amounts
            forM_  addrAndAmount $ \(addr, amount) ->
                run $ postTransaction wc (payment amount addr)

            liftIO $ threadDelay 90000000

            accUpdatedResp' <- run $ getAccounts wc (walId wallet)
            accsUpdated <- run $ wrData <$> accUpdatedResp' `shouldPrism` _Right

            balancesPartialUpdatedResp' <- run $ forM (map accIndex accsUpdated) $
                \(accIndex :: AccountIndex) -> getAccountBalance wc (walId wallet) accIndex

            balancesPartialUpdated <-
                run $ mapM (\resp -> wrData <$> resp `shouldPrism` _Right) balancesPartialUpdatedResp'

            liftIO $ map (AccountBalance . accAmount) accsUpdated `shouldBe` balancesPartialUpdated

  where
    filterByAddress :: WalletAddress -> FilterOperations '[V1 Address] WalletAddress
    filterByAddress addr =
        FilterOp (FilterByIndex $ accessIx @_ @(V1 Core.Address) addr) NoFilters

    expectNAddresses :: Int -> [WalletAddress] -> IO ()
    expectNAddresses n addrs =
        length addrs `shouldBe` n

    expectExactlyAddresses :: [WalletAddress] -> [WalletAddress] -> IO ()
    expectExactlyAddresses as bs =
        sort as `shouldBe` sort bs
