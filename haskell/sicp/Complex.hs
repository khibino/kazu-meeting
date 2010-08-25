module Complex where

{-
問題 2.75

-}

type CNum = Double

class Complex complex where
  realPart, imagPart, magnitude, angle :: complex -> CNum

data RealImag = RealImag { real :: CNum
                         , imag :: CNum
                         }
                deriving Show

--instance Num n => Complex (RealImag n)

instance Complex RealImag where
  realPart = real
  imagPart = imag
  magnitude c = sqrt (real c ** 2 + imag c ** 2)
  angle c = atan (imag c / real c)



data MagAng = MagAng { mag :: CNum
                     , ang :: CNum
                     }
              deriving Show

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
