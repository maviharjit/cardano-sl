{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module AddressSpecs (addressSpecs) where

import           Universum

import           Cardano.Wallet.Client.Http
import           Control.Lens
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (withMaxSuccess)
import           Test.QuickCheck.Monadic (monadicIO, run)


import           Util


addressSpecs :: WalletRef -> WalletClient IO -> Spec
addressSpecs wRef wc = do
    describe "Addresses" $ do

        prop "Creating an address makes it available" $ withMaxSuccess 10 $
            monadicIO $ do
            -- create a wallet
            Wallet{..} <- run $ sampleWallet wRef wc

            -- create an account
            accResp <- run $ postAccount wc walId (NewAccount Nothing "hello")
            acc@Account{..} <- run $ wrData <$> accResp `shouldPrism` _Right

            -- accounts should exist
            accResp' <- run $ getAccounts wc walId
            accs <- run $ wrData <$> accResp' `shouldPrism` _Right
            liftIO $ accs `shouldContain` [acc]

            -- create an address
            addResp <- run $ postAddress wc (NewAddress Nothing accIndex walId)
            addr <- run $ wrData <$> addResp `shouldPrism` _Right

            -- verify that address is in the API
            idxResp <- run $ getAddressIndex wc
            addrs <- run $ wrData <$> idxResp `shouldPrism` _Right

            liftIO $ map addrId addrs `shouldContain` [addrId addr]


        prop "Index returns real data" $ withMaxSuccess 10 $
            monadicIO $ do
            addrsResp <- run $ getAddressIndex wc
            addrs <- run $ wrData <$> addrsResp `shouldPrism` _Right

            addrsResp' <- run $ getAddressIndex wc
            addrs' <- run $ wrData <$> addrsResp' `shouldPrism` _Right

            liftIO $ addrs `shouldBe` addrs'
