module Crypto.KeyStore.Types where

import Codec.Serialise (Serialise)
import Crypto.JOSE (Crv, KeyMaterialGenParam, OKPCrv)
import Crypto.JOSE qualified as JOSE

data Crv' = P_256 | P_384 | P_521 | Secp256k1
  deriving stock (Show, Eq, Generic)

instance Serialise Crv'

crvToJose :: Crv' -> Crv
crvToJose P_256 = JOSE.P_256
crvToJose P_384 = JOSE.P_384
crvToJose P_521 = JOSE.P_521
crvToJose Secp256k1 = JOSE.Secp256k1

crvFromJose :: Crv -> Crv'
crvFromJose JOSE.P_256 = P_256
crvFromJose JOSE.P_384 = P_384
crvFromJose JOSE.P_521 = P_521
crvFromJose JOSE.Secp256k1 = Secp256k1

data OKPCrv' = Ed25519 | Ed448 | X25519 | X448
  deriving stock (Show, Eq, Generic)

instance Serialise OKPCrv'

okpCrvToJose :: OKPCrv' -> OKPCrv
okpCrvToJose Ed25519 = JOSE.Ed25519
okpCrvToJose Ed448 = JOSE.Ed448
okpCrvToJose X25519 = JOSE.X25519
okpCrvToJose X448 = JOSE.X448

okpCrvFromJose :: OKPCrv -> OKPCrv'
okpCrvFromJose JOSE.Ed25519 = Ed25519
okpCrvFromJose JOSE.Ed448 = Ed448
okpCrvFromJose JOSE.X25519 = X25519
okpCrvFromJose JOSE.X448 = X448

data KeyMaterialGenParam'
  = ECGenParam Crv'
  | RSAGenParam Int
  | OctGenParam Int
  | OKPGenParam OKPCrv'
  deriving stock (Show, Eq, Generic)

instance Serialise KeyMaterialGenParam'

genParamToJose :: KeyMaterialGenParam' -> KeyMaterialGenParam
genParamToJose (ECGenParam crv) = JOSE.ECGenParam $ crvToJose crv
genParamToJose (RSAGenParam int) = JOSE.RSAGenParam int
genParamToJose (OctGenParam int) = JOSE.OctGenParam int
genParamToJose (OKPGenParam crv) = JOSE.OKPGenParam $ okpCrvToJose crv

genParamFromJose :: KeyMaterialGenParam -> KeyMaterialGenParam'
genParamFromJose (JOSE.ECGenParam crv) = ECGenParam $ crvFromJose crv
genParamFromJose (JOSE.RSAGenParam int) = RSAGenParam int
genParamFromJose (JOSE.OctGenParam int) = OctGenParam int
genParamFromJose (JOSE.OKPGenParam crv) = OKPGenParam $ okpCrvFromJose crv
