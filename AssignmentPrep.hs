module AssignmentPrep where
    import Data.Maybe (fromMaybe)
    -- Types describing various elements of the game of dominoes
    type Domino = (Int, Int)
    type Hand = [Domino]
    type Board = [Domino]
    data End = Left | Right deriving (Eq, Show)

    -- Constants defining the sides of the board
    leftSide = AssignmentPrep.Left
    rightSide = AssignmentPrep.Right

    {-
     getEndDomino: given an End and a Board returns one side 
     of a Domino (an Int) at the specified end of the board. The
     side returned is the one that a domino can be played at
    -}
    getEndDominoSide :: End -> Board -> Int
    getEndDominoSide e b | e == leftSide    = fst (head b)
                         | e == rightSide   = snd (last b)
    
    {-
     flipDomino: given a Domino flips it. (Int1, Int2) turns into
     (Int2, Int1)
    -}
    flipDomino :: Domino -> Domino
    flipDomino (firstSide, secondSide) = (secondSide, firstSide)

    {-
     dominoIsDouble: returns True if a given Domino is a double
    -}
    dominoIsDouble :: Domino -> Bool
    dominoIsDouble d | fst d == snd d    = True
                     | otherwise         = False
    
    {-
     calculatePipTotal: takes a Board and returns an Int of sum of pips
     which are used to calculate the score of the board
    -}
    calculatePipTotal :: Board -> Int
    calculatePipTotal b | dominoIsDouble (head b) && dominoIsDouble (last b)           = leftSidePips*2 + rightSidePips*2
                        | dominoIsDouble (head b)                                      = leftSidePips*2 + rightSidePips 
                        | dominoIsDouble (last b)                                      = leftSidePips + rightSidePips*2
                        | otherwise                                                    = leftSidePips + rightSidePips
        where
            leftSidePips = getEndDominoSide leftSide b
            rightSidePips = getEndDominoSide rightSide b

    {-
     canPlay: predicate returning True if a given domino can be
     played at a given end of a given board
    -}
    canPlay :: Domino -> End -> Board -> Bool
    canPlay d e b | getEndDominoSide e b == fst d    = True
                  | getEndDominoSide e b == snd d    = True
                  | otherwise                        = False
    
    {-
     blocked: predicate given a Hand and a Board returning True
     if there is no domino in the hand which can be played on the
     given board (i.e. the player is ‘knocking’)
    -}
    blocked :: Hand -> Board -> Bool
    blocked [] b = True 
    blocked (h:hs) b | fst h == getEndDominoSide leftSide b   = False
                     | fst h == getEndDominoSide rightSide b  = False
                     | snd h == getEndDominoSide leftSide b   = False
                     | snd h == getEndDominoSide rightSide b  = False
                     | otherwise                              = blocked hs b
    
    {-
     played: predicate returning True if a given Domino has already
     been played on a given Board
    -}
    played :: Domino -> Board -> Bool
    played d [] = False
    played d (b:bs) | fst d == fst b && snd d == snd b    = True
                    | fst d == snd b && snd d == fst b    = True
                    | otherwise                           = played d bs
    
    {-
     possPlays: given a Hand and a Board, return all the Dominoes which
     may be played at the left End and all the Domines which may be played
     at the right End. Return type should be a pair
    -}
    possPlays :: Hand -> Board -> [(End, Domino)]
    possPlays h b = [(boardEnd, domino) | 
                      boardEnd <- [leftSide, rightSide],
                      domino <- h, canPlay domino boardEnd b]

    {-
     playDom: given a Domino, a Board and an End, play the domino at the
     given end if it will go, returning the new Board. The return type
     should be a Maybe
    -}
    playDom :: Domino -> Board -> End -> Maybe Board
    playDom d b e | canPlay d e b && e == leftSide && 
                    snd d == getEndDominoSide e b             = Just ([d] ++ b)
                  | canPlay d e b && e == leftSide &&
                    snd d /= getEndDominoSide e b             = Just ([flipDomino d] ++ b)
                  | canPlay d e b && e == rightSide && 
                    fst d == getEndDominoSide e b             = Just (b ++ [d])
                  | canPlay d e b && e == rightSide &&
                    fst d /= getEndDominoSide e b             = Just (b ++ [flipDomino d])
                  | otherwise                                 = Nothing
    
    {-
     scoreBoard: takes a Board and returns its 5s-and-3s score
    -}
    scoreBoard :: Board -> Int
    scoreBoard b | pipsBoard == 3 || pipsBoard == 5        = 1
                 | pipsBoard == 6 || pipsBoard == 10       = 2
                 | pipsBoard == 9                          = 3
                 | pipsBoard == 12 || pipsBoard == 20      = 4
                 | pipsBoard == 15                         = 8
                 | pipsBoard == 18                         = 6
                 | otherwise                               = 0
        where 
            pipsBoard = calculatePipTotal b
    
    {-
     scoreN: given a Board and an Int n, returns a list of (End, Domino)
     which can be played to reach a score of n
    -}
    scoreN :: Board -> Int -> [(End, Domino)]
    scoreN b n = [(boardEnd, domino) |
                   boardEnd <- [leftSide, rightSide],
                   domino <- notPlayedPossibleDom b boardEnd, 
                   scoreBoard (fromMaybe [(0,0)] (playDom domino b boardEnd)) == n]    -- Possible error with Maybe. Solution: return a Board and not a Maybe Board in playDom
    
    {-
     notPlayedPossibleDom: Given a Board and an End, returns all Dominoes
     that are not already played and can be played at that End
    -}
    notPlayedPossibleDom :: Board -> End -> [Domino]
    notPlayedPossibleDom b e = [(l, r) | l <- [0..6],
                                         r <- [l..6], 
                                         (not(played (l,r) b)) && (canPlay (l,r) e b)]    
    