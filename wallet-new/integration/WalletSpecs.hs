{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module WalletSpecs (walletSpecs) where

import           Universum

import           Cardano.Wallet.Client.Http
import           Control.Lens
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (withMaxSuccess)
import           Test.QuickCheck.Monadic (monadicIO, run)

import           Util


walletSpecs :: WalletRef -> WalletClient IO -> Spec
walletSpecs _ wc =

    describe "Wallets" $ do

        prop "Creating a wallet makes it available." $ withMaxSuccess 10 $
            monadicIO $ do
            newWallet <- run $ randomWallet CreateWallet
            Wallet{..} <- run $ createWalletCheck wc newWallet

            eresp <- run $ getWallet wc walId
            liftIO $ void $ eresp `shouldPrism` _Right

        prop "Updating a wallet persists the update" $ withMaxSuccess 10 $
            monadicIO $ do
            newWallet <- run $ randomWallet CreateWallet
            wallet <- run $ createWalletCheck wc newWallet
            let newName = "Foobar Bazquux"
                newAssurance = NormalAssurance
            eupdatedWallet <- run $ updateWallet wc (walId wallet) WalletUpdate
                { uwalName = newName
                , uwalAssuranceLevel = newAssurance
                }
            Wallet{..} <- run $ wrData <$> eupdatedWallet `shouldPrism` _Right
            liftIO $ walName `shouldBe` newName
            liftIO $ walAssuranceLevel `shouldBe` newAssurance

        prop "CreateWallet with the same mnemonics rises WalletAlreadyExists error" $ withMaxSuccess 10 $
            monadicIO $ do
            testWalletAlreadyExists CreateWallet

        prop "RestoreWallet with the same mnemonics throws WalletAlreadyExists" $ withMaxSuccess 10 $
            monadicIO $ do
            testWalletAlreadyExists RestoreWallet

        prop "Can accept Unicode characters" $ withMaxSuccess 10 $
            monadicIO $ do
            newWallet <- run $ randomWallet CreateWallet
            wallet <- run $ createWalletCheck wc newWallet

            eresp <- run $ updateWallet wc (walId wallet) WalletUpdate
                { uwalName = "patate漢patate字patat"
                , uwalAssuranceLevel = NormalAssurance
                }

            liftIO $ eresp `shouldPrism_` _Right

        prop "creating wallet gives rise to an empty Utxo histogram" $ withMaxSuccess 10 $
            monadicIO $ do
            newWallet <- run $ randomWallet CreateWallet
            wallet <- run $ createWalletCheck wc newWallet

            eresp <- run $ getUtxoStatistics wc (walId wallet)
            utxoStatistics <- run $ fmap wrData eresp `shouldPrism` _Right
            let utxoStatisticsExpected = computeUtxoStatistics log10 []
            liftIO $ utxoStatistics `shouldBe` utxoStatisticsExpected

  where
    testWalletAlreadyExists action = do
            newWallet1 <- run $ randomWallet action
            preWallet2 <- run $ randomWallet action
            let newWallet2 =
                    preWallet2
                        { newwalBackupPhrase = newwalBackupPhrase newWallet1
                        }
            -- First wallet creation/restoration should succeed
            result <- run $ postWallet wc newWallet1
            wallet <- run $ fmap wrData (result `shouldPrism` _Right)
            -- Second wallet creation/restoration should rise WalletAlreadyExists
            eresp <- run $ postWallet wc newWallet2
            clientError <- run $ eresp `shouldPrism` _Left
            liftIO $ clientError
                `shouldBe`
                    ClientWalletError (WalletAlreadyExists (walId wallet))
