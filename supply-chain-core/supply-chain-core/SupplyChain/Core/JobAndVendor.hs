-- | Description: /job/ + /vendor/

module SupplyChain.Core.JobAndVendor
  (
    {- * Types -} Job (..), Vendor (..),
    {- * Alteration -} alterJob, alterVendor,
    {- * Conversion -} loop, once,
  )
  where

import Data.Functor ((<&>))
import SupplyChain.Core.Effect (Effect)
import SupplyChain.Core.Job (Job)
import SupplyChain.Core.Job qualified as Job
import SupplyChain.Core.Referral (Referral (Referral))
import SupplyChain.Core.Referral qualified as Referral
import SupplyChain.Core.Unit (Unit (Unit))
import SupplyChain.Core.Vendor (Vendor (Vendor, handle))
import SupplyChain.Core.VendorAndReferral (alterVendor)

alterJob :: (forall x. Effect up action x -> Job up' action' x)
    -> Job up action product -> Job up' action' product
alterJob = Job.alter

loop :: Job up action product -> Vendor up (Unit product) action
loop j = go
  where
    go = Vendor{ handle = \Unit -> j <&> \product -> Referral product go }

once :: Vendor up (Unit product) action -> Job up action product
once v = handle v Unit <&> Referral.product
