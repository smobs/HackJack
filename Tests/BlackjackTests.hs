module BlackjackTests
where

import Cards.Blackjack
import Test.QuickCheck
import Tests.CardTestInstances

prop_deck_has_one_of_each_card c = length (filter (c ==) blackjackDeck) == 1

prop_twelve_card_hand_must_bust = forAll (handGenerator 52) (\h -> length h >= 12 ==> computeHandValue h == Bust)

prop_two_card_hand_cant_bust = forAll (handGenerator 2) (\h -> computeHandValue h /= Bust)

main = do quickCheck prop_deck_has_one_of_each_card
          quickCheck prop_twelve_card_hand_must_bust
          quickCheck prop_two_card_hand_cant_bust
          
handGenerator :: Int -> Gen Hand
handGenerator maxsize = do i <- choose(0, maxsize)
                           return (handGenerator' blackjackDeck i)

handGenerator' :: DeckOfCards -> Int -> Hand
handGenerator' [] _ = []
handGenerator' _ 0 = []
handGenerator' (c:d) i = c : (handGenerator' d (i - 1))