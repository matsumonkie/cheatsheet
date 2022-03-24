{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Optics where

import GHC.Generics (Generic)
import Optics.Generic
import Optics.Label
import Optics.Operators ((^.), (^..))
import Optics.Optic ((%))
import Optics.Lens (Lens')
--import Optics.Prism (Prism')
import qualified Optics.Lens as O
--import qualified Optics.Prism as O
import qualified Optics.Getter as O
import qualified Optics.Traversal as O
import qualified Optics.Fold as O
import Optics.Fold (Fold)

import Data.Function ((&))

import Data.Char (toUpper)

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
