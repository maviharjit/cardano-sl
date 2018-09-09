module Cardano.Wallet.Server.Plugins.SoftwareUpdates (
    softwareUpdatesWatchdog
    ) where

import           Universum

import           Data.Acid (AcidState)
import           Data.Acid.Advanced (update')

import           Pos.Chain.Update (cpsSoftwareVersion)
import           Pos.Util.Wlog (logInfo)

import           Cardano.Wallet.Kernel.DB.AcidState (AddUpdate (..), DB)
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..))
import qualified Cardano.Wallet.Kernel.Mode as Kernel
import           Cardano.Wallet.Kernel.NodeStateAdaptor (NodeStateAdaptor,
                     waitForUpdate, withNodeState)


softwareUpdatesWatchdog
    :: AcidState DB
    -> NodeStateAdaptor IO
    -> Kernel.WalletMode ()
softwareUpdatesWatchdog dbRef adaptor = forever go
  where
    go :: Kernel.WalletMode ()
    go = do
        cps <- liftIO $ withNodeState adaptor $ \_lock -> waitForUpdate
        logInfo "A new software update was found..."
        liftIO $ update' dbRef $ AddUpdate (InDb $ cpsSoftwareVersion cps)
        go
