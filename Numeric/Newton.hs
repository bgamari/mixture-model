module Numeric.Newton ( findRoot
                      , numericalDeriv
                      ) where
                      
type Objective a = a -> a
type Derivative a = a -> a

numericalDeriv :: (RealFrac a) => a -> Objective a -> a -> a -> a
numericalDeriv _ _ h _ | h < 1e-14 = error "numericalDeriv: Derivative underflow"
numericalDeriv tol f h x =
    let deriv h = (f (x+h) - f x) / h
        dfSmall = deriv (h/2)
        dfBig   = deriv h
    in if abs (dfSmall -dfBig) / dfSmall > tol
              then numericalDeriv tol f (h/2) x
              else dfSmall

newton :: (RealFrac a) => Objective a -> Derivative a -> a -> [a]
newton f fDeriv x0 =
    let x = x0 - f x0 / fDeriv x0
    in x : newton f fDeriv x

findRoot' :: (RealFrac a) => Int -> a -> Objective a -> Derivative a -> a -> a
findRoot' maxIter tol f fDeriv x0 =
    case dropWhile (\x->abs (f x) > tol) $ take maxIter $ newton f fDeriv x0 of
      []  -> error "findRoot': Failed to find root"
      x:_ -> x
    
findRoot :: (RealFrac a) => a -> Objective a -> Derivative a -> a -> a
findRoot = findRoot' 1000