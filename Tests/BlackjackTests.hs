module BlackjackTests
where

import Cards.Blackjack
import Test.QuickCheck
import Tests.CardTestInstances

prop_deck_has_one_of_each_card c = length (filter (c ==) blackjackDeck) == 1

prop_twelve_card_hand_must_bust hand = length hand >= 12 ==> computeHandValue hand == Bust

main = do quickCheck prop_deck_has_one_of_each_card
          quickCheck prop_twelve_card_hand_must_bust

