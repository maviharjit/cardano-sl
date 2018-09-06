{-# LANGUAGE LambdaCase #-}

-- | React to BListener events
module Cardano.Wallet.Kernel.BListener (
    -- * Respond to block chain events
    applyBlock
  , switchToFork
    -- * Testing
  , observableRollbackUseInTestsOnly
  ) where

import           Universum hiding (State)

import           Control.Concurrent.MVar (modifyMVar_)
import           Data.Acid.Advanced (update')
import qualified Data.Map.Strict as Map

import           Pos.Core.Txp (TxId)
import           Pos.Crypto (EncryptedSecretKey)

import           Cardano.Wallet.Kernel.DB.AcidState (ApplyBlock (..),
                     ObservableRollbackUseInTestsOnly (..), SwitchToFork (..),
                     SwitchToForkError (..), SwitchToForkInternalError (..))
import           Cardano.Wallet.Kernel.DB.BlockContext
import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock, rbContext)
import           Cardano.Wallet.Kernel.DB.Spec.Pending (Pending)
import           Cardano.Wallet.Kernel.DB.Spec.Update (ApplyBlockFailed)
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.Internal
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as Node
import           Cardano.Wallet.Kernel.PrefilterTx (PrefilteredBlock (..),
                     prefilterBlock)
import           Cardano.Wallet.Kernel.Read (getWalletCredentials)
import qualified Cardano.Wallet.Kernel.Submission as Submission
import           Cardano.Wallet.Kernel.Types (WalletId (..))

{-------------------------------------------------------------------------------
  Passive Wallet API implementation
-------------------------------------------------------------------------------}

-- | Prefilter the block for each account.
--
-- TODO: Improve performance (CBR-379)
prefilterBlock' :: PassiveWallet
                -> ResolvedBlock
                -> IO ((BlockContext, Map HdAccountId PrefilteredBlock), [TxMeta])
prefilterBlock' pw b = do
    aux <$> getWalletCredentials pw
  where
    aux :: [(WalletId, EncryptedSecretKey)]
        -> ((BlockContext, Map HdAccountId PrefilteredBlock), [TxMeta])
    aux ws =
      let (conMap, conMeta) = mconcat $ map (uncurry (prefilterBlock b)) ws
      in ((b ^. rbContext, conMap), conMeta)

-- | Notify all the wallets in the PassiveWallet of a new block
applyBlock :: PassiveWallet
           -> ResolvedBlock
           -> IO (Either ApplyBlockFailed ())
applyBlock pw@PassiveWallet{..} b = do
    k <- Node.getSecurityParameter _walletNode
    ((ctxt, blocksByAccount), metas) <- prefilterBlock' pw b
    -- apply block to all Accounts in all Wallets
    mConfirmed <- update' _wallets $ ApplyBlock k ctxt blocksByAccount
    case mConfirmed of
      Left  err       -> return $ Left err
      Right confirmed -> do
        modifyMVar_ _walletSubmission $ return . Submission.remPending confirmed
        mapM_ (putTxMeta _walletMeta) metas
        return $ Right ()

-- | Switch to a new fork
--
-- NOTE: The Ouroboros protocol says that this is only valid if the number of
-- resolved blocks exceeds the length of blocks to roll back.
switchToFork :: PassiveWallet
             -> Int             -- ^ Number of blocks to roll back
             -> [ResolvedBlock] -- ^ Blocks in the new fork
             -> IO (Either SwitchToForkError ())
switchToFork pw@PassiveWallet{..} n bs = do
    k <- Node.getSecurityParameter _walletNode
    blocksAndMeta <- mapM (prefilterBlock' pw) bs
    let (blockssByAccount, metas) = unzip blocksAndMeta

    trySwitchingToFork k blockssByAccount >>= \case
        Left  err     -> return $ Left err
        Right changes -> do
            mapM_ (putTxMeta _walletMeta) $ concat metas
            modifyMVar_ _walletSubmission $
                return . Submission.addPendings (fst <$> changes)
            modifyMVar_ _walletSubmission $
                return . Submission.remPending (snd <$> changes)
            return $ Right ()
  where

    trySwitchingToFork :: Node.SecurityParameter
                       -> [(BlockContext, Map HdAccountId PrefilteredBlock)]
                       -> IO (Either SwitchToForkError (Map HdAccountId (Pending, Set TxId)))
    trySwitchingToFork k blockssByAccount = do
        -- Find any new restorations that we didn't know about.
        restorations <- Map.elems <$> currentRestorations pw
        -- Stop the restorations and get the re-started account states that should be used.
        newAccts <- Map.unions <$> mapM prepareForRestoration restorations
        -- Switch to the fork, retrying if another restoration begins in the meantime.
        update' _wallets (SwitchToFork k n blockssByAccount newAccts) >>= \case
            Left RollbackDuringRestoration      -> trySwitchingToFork k blockssByAccount
              -- ^ Some more accounts started restoring, try again.
            Left (ApplyBlockFailedInternal err) -> return $ Left (ApplyBlockFailed err)
            Left NotEnoughBlocksInternal        -> return $ Left NotEnoughBlocks
            Right changes                       -> do
                -- Restart the restorations, and return the changes.
                mapM_ restartRestoration restorations
                return $ Right changes

-- | Observable rollback
--
-- Only used for tests. See 'switchToFork'.
-- TODO(kde): Do we want tests to deal with metadata?
observableRollbackUseInTestsOnly :: PassiveWallet
                                 -> IO (Either SwitchToForkInternalError ())
observableRollbackUseInTestsOnly PassiveWallet{..} = do
    res <- update' _wallets $ ObservableRollbackUseInTestsOnly
    case res of
      Left err           -> return $ Left err
      Right reintroduced -> do modifyMVar_ _walletSubmission $
                                 return . Submission.addPendings reintroduced
                               return $ Right ()
