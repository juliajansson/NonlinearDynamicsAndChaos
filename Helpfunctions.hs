import Data.Complex
import Data.List
type C=Complex Float
type Width=Float
type Height=Float
type X=Float
type Y=Float

--f::C->Int
data Crop = Crop {lowerLeft::C, higherRight::C, width::Width, height::Height}

type AllPoints = [[C]]

--Takes the lists of floating points (Real part and Imaginary part) and permutates them with help of helpfunction cross to the matrix of all points.
--doAll::[X]->[Y]->AllPoints

--List crossing to matrix.
--cross::[a]->[b]->[[(a,b)]]

--Converts a complex point with the help of the width and height to all the lists of floating points.
--complexToFloat::C->C->Width->Height->[X]->[Y]

--Do bitmap
--doBitmap::(C->Int)->Crop->Bitmap
--type Bitmap=[[Int]]

-------------------------------------------------------------------------------
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

--We want to go between complex float and tuple
complexToTuple::C->(Float,Float)
complexToTuple c=(realPart c,imagPart c)

tupleToComplex::(Float,Float)->C
tupleToComplex (a,b)=a:+b

test=map (map tupleToComplex )(cross [0.0,0.5,1.0] [0.0,0.5,1.0])==transpose example

--So the combination of tupleToComplex and cross gives us the complex points, but that's actually probably completely unneccesary, nevermind I'll see later 

-----------------------------------------------------------------------------
--Working on complexToFloat
--Converts a complex point with the help of the width and height to all the lists of floating points.
--complexToFloat::C->C->Width->Height->[X]->[Y]

--I realise that it is better to split it up in two separate functions, x and y, which will be very similar.
--complexToFloatx::C->C->Width->[X]
--complexToFloaty::C->C->Height->[Y]

--lowerLeft is defined c=(r1:+i1) and higherRight=(r2,i2) then the side x will be r2-r1 and the side y will be i2-i1, then the distance with which it changes for each point can be found by dividing r2-r1 with the width-1 etc. Why -1 I'm not sore right now but it is that way

--I reconsider and realise I want a help function that can give me the real side from the two lowerLeft and higherRight points

complexToFloatx::C->C->Width->[X]
complexToFloatx a b x=[realPart a+n*(realSide a b)/(x-1)|n<-[0..(x-1)]]

realSide::C->C->Float
realSide a b=abs ((realPart a) - (realPart b))

--Then complexToFloaty can be defined similarly

complexToFloaty::C->C->Height->[Y]
complexToFloaty a b y=[realPart a+n*(realSide a b)/(y-1)|n<-[0..(y-1)]]

test2=cross (complexToFloatx (0:+0) (1:+1) 3) (complexToFloaty (0:+0) (1:+1) 3)

--Yay it worked :)
