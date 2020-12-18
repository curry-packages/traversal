---------------------------------------------------------------------------
--- Library to support lightweight generic traversals
--- through tree-structured data. See
--- [here](http://www-ps.informatik.uni-kiel.de/~sebf/projects/traversal.html)
--- for a description of the library.
---
--- @author Sebastian Fischer
--- @version December 2020
---------------------------------------------------------------------------

module Data.Traversal (

  Traversable, noChildren,

  children, replaceChildren, mapChildren,

  family, childFamilies, mapFamily, mapChildFamilies,

  evalFamily, evalChildFamilies, fold, foldChildren,

  replaceChildrenM, mapChildrenM, mapFamilyM, mapChildFamiliesM,

  evalFamilyM, evalChildFamiliesM

  ) where

--- A datatype is `Traversable` if it defines a function
--- that can decompose a value into a list of children of the same type
--- and recombine new children to a new value of the original type. 
---
type Traversable a b = a -> ([b], [b] -> a)

--- Traversal function for constructors without children.
---
noChildren :: Traversable _ _
noChildren x = ([], const x)

--- Yields the children of a value.
---
children :: Traversable a b -> a -> [b]
children tr = fst . tr

--- Replaces the children of a value.
--- 
replaceChildren :: Traversable a b -> a -> [b] -> a
replaceChildren tr = snd . tr

--- Applies the given function to each child of a value.
---
mapChildren :: Traversable a b -> (b -> b) -> a -> a
mapChildren tr f x = replaceChildren tr x (map f (children tr x))

--- Computes a list of the given value, its children, those children, etc.
---
family :: Traversable a a -> a -> [a]
family tr x = familyFL tr x []

--- Computes a list of family members of the children of a value.
--- The value and its children can have different types.
---
childFamilies :: Traversable a b -> Traversable b b -> a -> [b]
childFamilies tra trb x = childFamiliesFL tra trb x [] 

--- Applies the given function to each member of the family of a value.
--- Proceeds bottom-up.
---
mapFamily :: Traversable a a -> (a -> a) -> a -> a
mapFamily tr f = f . mapChildFamilies tr tr f

--- Applies the given function to each member of the families of the children
--- of a value. The value and its children can have different types.
--- Proceeds bottom-up.
---
mapChildFamilies :: Traversable a b -> Traversable b b -> (b -> b) -> a -> a
mapChildFamilies tra trb = mapChildren tra . mapFamily trb

--- Applies the given function to each member of the family of a value 
--- as long as possible. On each member of the family of the result the given
--- function will yield `Nothing`.
--- Proceeds bottom-up.
---
evalFamily :: Traversable a a -> (a -> Maybe a) -> a -> a
evalFamily tr f = mapFamily tr g
 where g x = maybe x (mapFamily tr g) (f x)

--- Applies the given function to each member of the families of the children
--- of a value as long as possible.
--- Similar to 'evalFamily'.
---
evalChildFamilies :: Traversable a b -> Traversable b b
                  -> (b -> Maybe b) -> a -> a
evalChildFamilies tra trb = mapChildren tra . evalFamily trb

--- Implements a traversal similar to a fold with possible default cases.
---
fold :: Traversable a a -> (a -> [r] -> r) -> a -> r
fold tr f = foldChildren tr tr f f

--- Fold the children and combine the results.
---
foldChildren :: Traversable a b -> Traversable b b
             -> (a -> [rb] -> ra) -> (b -> [rb] -> rb) -> a -> ra
foldChildren tra trb f g a = f a (map (fold trb g) (children tra a))

--- Monadic version of replaceChildren
---
replaceChildrenM :: Monad m => Traversable a b -> a -> m [b] -> m a
replaceChildrenM tr = fmap . replaceChildren tr

--- Monadic version of mapChildren
---
mapChildrenM :: Monad m => Traversable a b -> (b -> m b) -> a -> m a
mapChildrenM tr f a = replaceChildrenM tr a (mapM f (children tr a))

--- Monadic version of mapFamily
---
mapFamilyM :: Monad m => Traversable a a -> (a -> m a) -> a -> m a
mapFamilyM tr f a = mapChildFamiliesM tr tr f a >>= f

--- Monadic version of mapChildFamilies
---
mapChildFamiliesM :: Monad m => Traversable a b -> Traversable b b
                  -> (b -> m b) -> a -> m a
mapChildFamiliesM tra trb = mapChildrenM tra . mapFamilyM trb

--- Monadic version of evalFamily
---
evalFamilyM :: Monad m => Traversable a a -> (a -> m (Maybe a)) -> a -> m a
evalFamilyM tr f = mapFamilyM tr g
 where g a = f a >>= maybe (return a) (mapFamilyM tr g)

--- Monadic version of evalChildFamilies
---
evalChildFamiliesM :: Monad m => Traversable a b -> Traversable b b
                  -> (b -> m (Maybe b)) -> a -> m a
evalChildFamiliesM tra trb = mapChildrenM tra . evalFamilyM trb


-- implementation of 'family' with functional lists for efficiency reasons

type FunList a = [a] -> [a]

concatFL :: [FunList a] -> FunList a
concatFL [] ys = ys
concatFL (x:xs) ys = x (concatFL xs ys)

familyFL :: Traversable a a -> a -> FunList a
familyFL tr x xs = x : childFamiliesFL tr tr x xs

childFamiliesFL :: Traversable a b -> Traversable b b -> a -> FunList b
childFamiliesFL tra trb x xs = concatFL (map (familyFL trb) (children tra x)) xs

