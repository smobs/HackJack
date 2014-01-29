module BlackjackTests
where

import Cards.Blackjack
import Test.QuickCheck
import Tests.CardTestInstances
prop_deck_has_one_of_each_card c = length (filter (c ==) blackjackDeck) == 1

main = quickCheck prop_deck_has_one_of_each_card

