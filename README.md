Pitch
=====

Pitch card game


#### Program Architechture

Main thread runs the game loop, manages the game state and makes calls to the players in turn:

  mkBid will give the player the scores, the player's hand, and the previous bids
    
    mkBid :: [Int] -> [Cards] -> [Bids] -> (Int, Suit)
 
  mkDiscard will give the player the scores, the player's hand, trump, and the bids
  
    mkDiscard :: [Int] -> [Cards] -> Suit -> [Bids] -> [Card]
  
  mkPlay will give the player the scores, the player's hand, trump, the current cards played, the bids, and the previous tricks
  
    mkPlay :: [Int] -> [Cards] -> Suit -> [Play] -> [Bid] -> [Trick] -> Card
    
The main loop will validate the results of these calls and then repeat the calls if necessary 

The `PlayerLogic` typeclass will encapsulate the functions above. There will be three instances of the typeclass:

* HumanPlayer, which will prompt the user for input anytime it needs to make a descision
* ComputerPlayer, which will algorithmically solve for a solution given the game state
* NetworkPlayer, which will communicate over a network to determine a result

Each NetworkPlayer will run it's own thread with a minimal webserver that the client can hit. 
