import Data.Complex
import Data.List
import Graphics.Gloss
import System.Environment
import Graphics.Gloss.Data.Color (Color)
import Graphics.Gloss.Data.Display (Display(InWindow))
import Graphics.Gloss.Interface.Pure.Animate (animate)
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Raster.Field (animateField, rgbI)
type C=Complex Float
type Width=Int
type Height=Int
type X=Float
type Y=Float
type Time=Float

newton::(C->C)->(C->C)->C->C
newton f f' z=z-(f z)/(f' z)

i::C
i=0:+1

complexToPoint::C->Point
complexToPoint c=(realPart c,imagPart c)

pointToComplex::Point->C
pointToComplex (a,b)=(a*2):+(b*2)

--to get fig 18.4a
fig184a::C->C
fig184a  z=z^2+1

fig184a'::C->C
fig184a' z=2*z

a=newton fig184a fig184a'

testa= length . take 100 . takeWhile (\z->magnitude z<4) . iterate a

eps=1e-6
testa' c = length (take 100 (takeWhile (>eps) skillnader))
  where 
    skillnader = map (\(x1,x2)-> magnitude (x1-x2)) grannpar
    grannpar = zip as (tail as)
    as = iterate a c
      

frametesta::Time->Point->Color
frametesta time p= rgbI c c c
  where c=40*(testa' (pointToComplex p))

----------------------------------------------------------------------
disc::C->Bool
disc a = (magnitude a)<=1 

func::Point->Int
func a=if disc (pointToComplex a) then 255
         else 0

myframe::Time->Point->Color
myframe time p= rgbI c c c
  where c=func p
          --cycleColorF (time*(fromIntegral (func p)))
-------------------------------------------------------------------------

main=test

test :: IO ()
test = do
  animateField
    (InWindow "fractal" windowSize (50, 10))
    (pixelSize, pixelSize)
    frametesta
  where
    windowSize = (500, 500)
    pixelSize = 1
