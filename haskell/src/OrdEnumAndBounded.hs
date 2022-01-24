module OrdEnumAndBounded where


{- |

Ord: compare ordering, needs `Eq` typeclass

>>> Monday <= Tuesday
True

Down allows to reverse the ordering
>>> :m + Data.Ord
>>> Down Wednesday <= Down Friday
False

-}
data Days = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving stock (Show, Eq, Ord)


{- |

Enum: allows the conversion of constructor to an Int and vice versa
minimal implementation:
  toEnum :: Int -> a
  fromEnum :: a -> Int

>>> fromEnum Duck
0

>>> toEnum @Animal 1
Elephant

`toEnum 10` will raise:
*** Exception: toEnum{Vehicle}: tag (10) is outside of enumeration's range (0,3)
-}
data Animal
  = Duck
  | Elephant
  | Dolphin
  deriving stock (Show, Enum)


{- |
Bounded: set boundaries to a data type
minimal implementation
  minBound :: a
  maxBound :: a

>>> minBound @Vehicle
Car

>>> maxBound @Vehicle
Train

-}
data Vehicle
  = Car
  | Bike
  | Bus
  | Train
  deriving stock (Show, Bounded)


{- Note, if you have a data type that has the Enum and Bounded constraints,
   you can list all its values with `Data.List.Extra.enumerate :: (Enum a, Bounded a) => [a]`
   from the `extra` package
-}
