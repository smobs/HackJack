module Tests.CardTestInstances
where
import Cards.PlayingCards
import Test.QuickCheck
instance Arbitrary Card where
    arbitrary = do
        suit  <- arbitraryBoundedEnum
        cv <- arbitraryBoundedEnum
        return (Card suit cv)