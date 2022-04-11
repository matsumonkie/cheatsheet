{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Optics where

import GHC.Generics (Generic)
import Optics.Generic
import Optics.Label
import Optics.Operators ((^.), (^..), (^?), (.~))
import Data.Maybe.Optics (_Just, (%?))
import qualified Optics.Core.Extras as O
import Optics.Optic ((%))
import Optics.Lens (Lens')
import qualified Optics.AffineFold as O
--import Optics.Prism (Prism')
import qualified Optics.Lens as O
--import qualified Optics.Prism as O
import qualified Optics.Getter as O
import qualified Optics.Traversal as O
import qualified Optics.Fold as O
import Optics.Fold (Fold)

import Data.Function ((&))

import Data.Char (toUpper, isUpper)

-- * Manually writting optics

data User = User
  { _firstname :: String
  , _lastname :: String
  , _siblings :: [User]
  } deriving (Show)

{- non polymorphic lens: type Lens' s a = Optic' A_Lens NoIx s a
   `s` is main the structure where `a` is the value we focus on
-}

{- | Writing full lens signature

>>> (User "john" "doe" []) ^. firstname
"john"

-}
firstname :: Lens' User String
firstname =
  O.lens getFirstname setFirstname
  where
    getFirstname :: User -> String
    getFirstname = _firstname

    setFirstname :: User -> String -> User
    setFirstname user newFirstname = user { _firstname = newFirstname }

-- Simpler lens
lastname :: Lens' User String
lastname = O.lens _lastname (\user newLastname -> user { _lastname = newLastname })


{- | type Fold s a = Optic' A_Fold NoIx s a

A Fold allows to focus a structure `s` on multiple `a`
But Fold is a read only optic.

(^..) = toListOff

>>> (User "john" "doe" [ User "jane" "doe" [], User "john" "smith" []]) ^.. siblings
[User {_firstname = "jane", _lastname = "doe", _siblings = []},User {_firstname = "john", _lastname = "smith", _siblings = []}]

-}
siblings :: Fold User User
siblings =
  O.folding _siblings

{- | Note that lens are fold, so better write a lens instead so you can have the `set` ability

>>> (User "john" "doe" [ User "jane" "doe" [], User "john" "smith" []]) ^. siblings2
[User {_firstname = "jane", _lastname = "doe", _siblings = []},User {_firstname = "john", _lastname = "smith", _siblings = []}]

-}
siblings2 :: Lens' User [User]
siblings2 =
  O.lens _siblings (\user newSiblings -> user { _siblings = newSiblings })


{- | we can easily build custom Getter with the `to` function, `to :: (s -> a) -> Getter s a` function
     Getter are Fold but not Lens, they help chaining optics

>>> (User "john" "doe" []) ^. firstname % O.to (fmap toUpper)
"JOHN"

Optics can compose each other easily so: Optics A B % Optics B C => Optics A C

>>> (User "john" "doe" [ User "jane" "doe" [], User "john" "smith" []]) ^.. siblings % firstname
["jane","john"]

but Optics A [B] % Optics B C doesn't compose
(User "john" "doe" [ User "jane" "doe" [], User "john" "smith" []]) ^. siblings2 % firstname
would return: Couldn't match type ‘User’ with ‘[User]’

So to solve this, we need to transform Optics A [B] to Optics A B
this is where folded and traversed comes into play:
folded :: Foldable f => Fold (f a) a
traversed :: Traversable t => Traversal (t a) (t b) a b

siblings  :: Lens' User [User]
folded    :: Fold [User] User
firstname :: Lens' User String

let's compose:

  siblings2 % O.folded % firstname :: Fold' User String

and because only ^.. apply to Fold and Traversal (and because we want to fetch the list of firstname) we can do:

>>> (User "john" "doe" [ User "jane" "doe" [], User "john" "smith" []]) ^.. siblings2 % O.folded % firstname
["jane","john"]

Note that traversed works as well but allows to

>>> (User "john" "doe" [ User "jane" "doe" [], User "john" "smith" []]) ^.. siblings2 % O.traversed % firstname
["jane","john"]

-}

{- |

>>> User2 "john" ^. #name
"john"

-}
data User2 = User2
  { name :: String
  } deriving (Generic, Show)

-- folding :: Foldable f => (s -> f a) -> Fold s a

-- polymorphic lens: Lens s t a b = Optic A_Lens NoIx s t a b


data A = A
  { b :: B
  } deriving (Generic, Show)

data B = B
  { c :: C
  } deriving (Generic, Show)

data C = C
  { d :: Maybe D
  } deriving (Generic, Show)

data D = D
  { e :: Maybe E
  } deriving (Generic, Show)

data E = E
  { fs :: [F]
  , gs :: [G]
  } deriving (Generic, Show)

data F = F
  { h :: String
  , i :: String
  } deriving (Generic, Show)

data G = G
  { j :: String
  , k :: String
  } deriving (Generic, Show)


{- |

$setup
>>> let a = A { b = B { c = C { d = Just D { e = Just E { fs = [F { h = "hey", i = "ho" }], gs = [G { j = "foo", k = "bar" }]}}}}}

>>> a ^.. (#b % #c % #d % _Just % #e % _Just % #fs % O.folded % #h)
["hey"]


(%?) is a shortcut for `% _Just %`
>>> a ^.. (#b % #c % #d %? #e %? #fs % O.folded % #i)
["ho"]

>>> a ^.. (#b % #c % #d %? #e %? #fs % O.folded)
[F {h = "hey", i = "ho"}]

>>> a ^? #b % #c % #d %? #e %? #fs
Just [F {h = "hey", i = "ho"}]

>>> let b = a & #b % #c % #d %? #e .~ Nothing
>>> b ^? #b % #c % #d %? #e % _Just
Nothing


`is` or `has` (has :: Is k A_Fold => Optic' k is s a -> s -> Bool) are interesting if you need to check
if a Maybe is _Just or _Nothing or a Either is a _Left or _Right
>>> O.is _Just (b ^? #b % #c % #d %? #e)
True

>>> O.has _Just (b ^? #b % #c % #d %? #e)
True


elemOf :: (Is k A_Fold, Eq a) => Optic' k is s a -> a -> s -> Bool
>>> let b = a & #b % #c % #d %? #e %? #fs .~ [F {h = "a", i ="b"},F {h = "c", i ="d"},F {h = "e", i ="f"}]
>>> O.elemOf (#b % #c % #d %? #e %? #fs % O.folded % #h) "a" b
True

anyOf :: Is k A_Fold => Optic' k is s a -> (a -> Bool) -> s -> Bool
or
allOf :: Is k A_Fold => Optic' k is s a -> (a -> Bool) -> s -> Bool
>>> let b = a & #b % #c % #d %? #e %? #fs .~ [F {h = "a", i ="b"},F {h = "c", i ="d"},F {h = "e", i ="f"}]
>>> O.allOf (#b % #c % #d %? #e %? #fs % O.folded % #h) (\s -> length s == 1) b
True

Find the first element matching a predicate
findOf :: Is k A_Fold => Optic' k is s a -> (a -> Bool) -> s -> Maybe a
>>> let b = a & #b % #c % #d %? #e %? #fs .~ [F {h = "a", i ="b"},F {h = "c", i ="d"},F {h = "e", i ="f"}]
>>> O.findOf (#b % #c % #d %? #e %? #fs % O.folded % #h) (\s -> s == "c") b
Just "c"

Filter a specific value
filtered :: (a -> Bool) -> AffineFold a a
>>> [1,2,3,4,5] ^.. O.folded % O.filtered (\e -> e > 2)
[3,4,5]
-}
