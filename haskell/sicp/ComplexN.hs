module ComplexN where

{-
問題 2.78


問題 2.79


問題 2.80


-}

type CNum = Double

class Complex complex where
  realPart, imagPart, magnitude, angle :: complex -> CNum
  --realPart, imagPart, magnitude, angle :: complex -> complex

data RealImag = RealImag { real :: CNum
                         , imag :: CNum
                         }
                deriving (Show, Eq)

data MagAng = MagAng { mag :: CNum
                     , ang :: CNum
                     }
              deriving (Show, Eq)

r2m :: RealImag -> MagAng
r2m r = MagAng { mag = sqrt (real r ** 2 + imag r ** 2), ang = atan (imag r / real r) }

m2r :: MagAng -> RealImag
m2r m = RealImag { real = mag m * cos (ang m), imag = mag m * sin (ang m) }


riPlus :: (Complex c0, Complex c1) => c0 -> c1 -> RealImag
riPlus a b = RealImag (realPart a + realPart b) (imagPart a + imagPart b)

maMul :: (Complex c0, Complex c1) => c0 -> c1 -> MagAng
maMul  a b = MagAng (magnitude a * magnitude b) (angle a + angle b)

class ZNum n where
  zeroP :: n -> Bool


instance Num RealImag where
  a + b = riPlus a b
  a * b = m2r $ maMul a b
  abs x = RealImag (magnitude x) 0
  signum x = RealImag (realPart x / mag') (imagPart x / mag')
    where mag' = magnitude x
  fromInteger x = RealImag (fromInteger x :: CNum) 0

instance ZNum RealImag where
  zeroP x = magnitude x == 0


instance Complex RealImag where
  realPart = real
  imagPart = imag
  magnitude c = sqrt (real c ** 2 + imag c ** 2)
  angle c = atan (imag c / real c)


instance Num MagAng where
  a + b = r2m $ riPlus a b
  a * b = maMul a b
  abs x = r2m $ RealImag (magnitude x) 0
  signum x = r2m $ RealImag (realPart x /mag') (imagPart x / mag')
    where mag' = magnitude $ abs x
  fromInteger x = r2m $ RealImag (fromInteger x :: CNum) 0
  
instance ZNum MagAng where
  zeroP x = magnitude x == 0
  
  
instance Complex MagAng where
  realPart c = mag c * cos (ang c)
  imagPart c = mag c * sin (ang c)
  magnitude = mag
  angle = ang

makeFromRealImag :: CNum -> CNum -> RealImag
makeFromRealImag =  RealImag

makeFromMagAng :: CNum -> CNum -> MagAng
makeFromMagAng =  MagAng

--let v = makeFromRealImag 1.0 1.0 in makeFromMagAng (magnitude v) (angle v)
--let v = makeFromRealImag (-1.0) 0.0 in makeFromMagAng (magnitude v) (angle v)
