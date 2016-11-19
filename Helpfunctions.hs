import Data.Complex
import Data.List
type C=Complex Double
type X=Int
type Y=Int

--f::C->Int
data Crop = Crop {lowerLeft::C, higherRight::C, width::X, height::Y}

type AllPoints = [[C]]

--Takes the lists of floating points (Real part and Imaginary part) and permutates them with help pf helpfunction cross to the matrix of all points.
--doAll::[Float]->[Float]->AllPoints

--List crossing to matrix.
--cross::[a]->[b]->[[(a,b)]]

--Converts a complex point with the help of the width and height to all the lists of floating points.
--complexToFloat::C->X->Y->[Float]->[Float]

--Do bitmap
--doBitmap::(C->Int)->Crop->Bitmap
--type Bitmap=[[Int]]

--Working on cross
--Testing the unit circle
unitcircle=zip (map realPart [1,i,-1,-i]) (map imagPart [1,i,-1,-i])

i::C
i=0:+1

--We know here how to unite two lists, with zip, but how do we do it when we want all the permutations?
--I introduce an example that come from cross [0.0,0.5,1.0] [0.0,0.5,1.0]
example::[[C]]
example=[[(0.0:+0.0),(0.0:+0.5),(0.0:+1.0)],[(0.5:+0.0),(0.5:+0.5),(0.5:+1.0)],[(1.0:+0.0),(1.0:+0.5),(1.0:+1.0)]]

cross::[a]->[b]->[[(a,b)]]
cross xs ys = [ [ (x, y) | x<-xs ] | y<-ys]
