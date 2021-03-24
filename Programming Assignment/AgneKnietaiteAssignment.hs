{-
  COM2108 Functional Programming
  Individual Programming Assignment

  Agne Knietaite

  Module which builds upon the functions provided in DomsMatch.hs and
  develops 5 different domino players.
-}
module AgneKnietaiteAssignment where
    -- IMPORTS
    import DomsMatch
    import qualified Data.Maybe
    import Debug.Trace
    import Data.List

    -- CONSTANTS
    winScore = 61           -- score to win the game
    secondReachScore = 59   -- best score to aim for if not winning score
    biggestMoveScore = 8    -- best score that can be achieved with one move
    halfGame = 30           -- score which marks half of the game

    -- TYPE DECLARATIONS
    {- Type tactic is introduced to allow easy switch between different tactics used by 
        different player types. -}
    type Tactic = DomsPlayer
    {- Variation of tactic: if tactic is not successful Nothing is returned, hence 
        the player can respond accordingly -}
    type TacticUnsure = Hand -> DominoBoard -> Player -> Scores -> Maybe (Domino, End)

    -- PLAYERS
    {-  highestScoringPlayer.
        Player which always plays the highest scoring domino no matter what the situation of the game is. -}
    highestScoringPlayer :: DomsPlayer
    highestScoringPlayer = playHighestScoring


    {-  reachesForGoalPlayer.
        - Beginning of the game:    Tries to place the popular choice (5,4) if the board is empty to cause
                                    the opponent to score less with their next turn.
        - Middle of the game:       Plays the highest scoring dominoes.
        - End of the game:          Looks for a domino which reaches the winScore (61). If that is not possible,
                                    tries to reach secondBestScore (59). If that also fails, plays the highest scoring domino. -}
    reachesForGoalPlayer :: DomsPlayer
    reachesForGoalPlayer hand board player scores
        -- checks if board is empty and (5,4)/(4,5) is in hand
        | board == InitBoard && Data.Maybe.isJust firstMove         = Data.Maybe.fromJust firstMove
        -- checks if winning domino is in hand. Doesn't check for winning domino until it's possible to reach the end
        | myScore >= winScore-biggestMoveScore &&
            Data.Maybe.isJust exactScore                            = Data.Maybe.fromJust exactScore
        -- checks if second best score can be reached. Avoids the check if it's impossible to reach it
        | myScore >= secondReachScore-biggestMoveScore &&
            Data.Maybe.isJust secondBestScore                       = Data.Maybe.fromJust secondBestScore
        -- if middle of the game or everything fails, plays highest scoring domino
        | otherwise                                                 = playHighestScoring hand board player scores
        where
        myScore = playerScore player scores
        -- dominoes for different occasions
        firstMove = playFirstMove hand board player scores 
        exactScore = playExactScore hand board player scores
        secondBestScore = playSecondBest hand board player scores
    

    {- avoidsKnockingPlayer. 
        - 1st half of the game:     Tries to get rid of doubles if they are present in hand to avoid knocking later.
                                    Although, only plays a double if it's not dangerous- there are dominoes in hand to
                                    be placed after this domino is placed. If no doubles present/can be placed, acts as in 2nd half.
        - 2nd half of the game:     Plays the most frequent domino, if its score isn't equal to 0. If such domino doesn't exist
                                    looks for a highest scoring safe domino which is not equal to 0. If that also fails, plays
                                    the most frequent domino, disregarding that its score is 0 because it's the safest.                                       
        - End of the game:          Looks for a domino which reaches the winScore (61). If that fails, acts as 2nd half. -}
    avoidsKnockingPlayer :: DomsPlayer
    avoidsKnockingPlayer hand board player scores
        -- checks if winning domino is in hand. Doesn't check for winning domino until it's possible to reach the end
        | myScore >= winScore-biggestMoveScore &&
            Data.Maybe.isJust exactScore                      = Data.Maybe.fromJust exactScore
        -- checks if it's 1st half of the game and if there is a safe double to play
        | myScore <= halfGame &&
            Data.Maybe.isJust safeDouble                            = Data.Maybe.fromJust safeDouble
        -- checks if the most frequent domino isn't equal to 0. If not, plays it
        | frequentScore /= 0                                        = frequent
        -- checks if a safe domino, whose score isn't 0, exists
        | Data.Maybe.isJust safe &&
            safeScore /= 0                                          = Data.Maybe.fromJust safe
        -- if everything else fails, plays highest scoring most frequent domino
        | otherwise                                                 = frequent
        where
        myScore = playerScore player scores
        -- dominoes for different occasions
        safeDouble = playSafeDouble hand board player scores
        safe = playSafe hand board player scores
        frequent = playFrequent hand board player scores
        exactScore = playExactScore hand board player scores
        -- scores of dominoes if they would be placed
        frequentScore = scoreDomino frequent board player
        safeScore = scoreDomino (Data.Maybe.fromJust safe) board player
    

    {- saboteurPlayer.
        During the game:            Tries to play a domino which makes/could make the opponent to knock. If this
                                    is not successful or such domino is equal to 0, plays the highest scoring domino. 
        Ending of the game:         Looks for a domino which reaches the winScore (61). If that fails, acts as 2nd half. -}
    saboteurPlayer :: DomsPlayer
    saboteurPlayer hand board player scores
        -- checks if winning domino is in hand. Doesn't check for winning domino until it's possible to reach the end
        | myScore >= winScore-biggestMoveScore &&
            Data.Maybe.isJust exactScore                            = Data.Maybe.fromJust exactScore
        -- plays worst possible domino for the opponent if such exists and isn't equal to 0
        | board /= InitBoard &&
            Data.Maybe.isJust opponentKnocking &&
            opponentKnockingScore /= 0                              = Data.Maybe.fromJust opponentKnocking
        -- if all fails, plays highest scoring domino
        | otherwise                                                 = highestScoring
        where
        myScore = playerScore player scores
        -- dominoes for different occasions
        opponentKnocking = playOpponentKnocking hand board player scores
        highestScoring = playHighestScoring hand board player scores
        -- scores of dominoes if they would be placed
        opponentKnockingScore = scoreDomino (Data.Maybe.fromJust opponentKnocking) board player
        exactScore = playExactScore hand board player scores


    {- beginnerPlayer.
        Beginning of the game:      Tries  to place the popular choice (5,4) if the board is empty to cause
                                    the opponent to score less with their next turn.
        Middle of the game:         - Gets the most frequent domino in hand and plays it, if its score is equal to
                                      the score of highest scoring domino.
                                    - Looks for a highest scoring domino, which is safe. If such domino exists,
                                      plays it only if its score is equal to a highest scoring domino score.
                                    - If all fails, plays the highest scoring domino in hand.
        Player near winning:        Looks for a domino which reaches the winScore (61). If that is not possible,
                                    tries to reach secondBestScore (59). 
        Opponent near winning:      Plays a domino which is worst for opponent if such domino exists and playing that domino
                                    doesn't result in getting 0 score. If opponent is extremelly near winning a domino
                                    which has the highest chances of blocking the opponent regardless of its score. Avoids
                                    both if opponent has knocked before to avoid stitching. 
        Both near winning:          If player is extremelly near winning but doesn't have a domino which would win the game
                                    and the opponent is near winning as well, tries to stitch the game- play such domino which
                                    is dangerous for them as well as the opponent. -}
    beginnerPlayer :: DomsPlayer
    beginnerPlayer hand board player scores
         -- checks if board is empty and (5,4)/(4,5) is in hand
        | board == InitBoard && Data.Maybe.isJust firstMove         = Data.Maybe.fromJust firstMove
        -- checks if winning domino is in hand. Doesn't check for winning domino until it's possible to reach the end
        | myScore >= winScore-biggestMoveScore &&
            Data.Maybe.isJust exactScore                            = Data.Maybe.fromJust exactScore
        -- checks if second best score can be reached. Avoids the check if it's impossible to reach it
        | myScore >= secondReachScore-biggestMoveScore &&
            Data.Maybe.isJust secondBestScore                       = Data.Maybe.fromJust secondBestScore
        -- if both players are extremelly near winning, and player doesn't have the domino to win, they try to stitch the game
        |   board /= InitBoard &&
            myScore >= winScore-(biggestMoveScore `div` 2) &&
            opponentScore >= winScore-(biggestMoveScore `div` 2) &&
            Data.Maybe.isJust stitching                             = Data.Maybe.fromJust stitching
        -- checks if opponent is near the end and tries to make them knock. Doesn't play the domino, if the score would be 0
        | board /= InitBoard &&
            opponentScore >= secondReachScore-biggestMoveScore &&
            Data.Maybe.isJust opponentKnocking &&
            opponentKnockingScore /= 0 &&
            not(knockedLastTurn history player)                     = Data.Maybe.fromJust opponentKnocking
        -- checks if opponent is extremelly near winning and tries to make them knock if they haven't already knocked on the last turn
        | board /= InitBoard &&
            opponentScore >= winScore-(biggestMoveScore `div` 2) &&
            not(knockedLastTurn history player) &&
            Data.Maybe.isJust opponentKnocking                      = Data.Maybe.fromJust opponentKnocking
        -- checks if most frequent domino is frequent and plays it, if its score is equal to highest scoring domino
        | frequentHowFrequent*2 >= length hand &&
            frequentScore == highestScoringScore                    = frequent
        -- checks if safe domino exists and plays it if its score is equal to highest scoring domino
        | Data.Maybe.isJust safe &&
            safeScore == highestScoringScore                        = Data.Maybe.fromJust safe
        -- if all fails, plays highest scoring domino
        | otherwise                                                 = highestScoring
        where
        -- scores of players
        myScore = playerScore player scores
        opponentScore = playerScore (opponent player) scores
        -- dominoes for different occasions
        firstMove = playFirstMove hand board player scores 
        exactScore = playExactScore hand board player scores
        secondBestScore = playSecondBest hand board player scores
        opponentKnocking = playOpponentKnocking hand board player scores
        frequent = playFrequent hand board player scores
        highestScoring = playHighestScoring hand board player scores
        safe = playSafe hand board player scores
        stitching = playStitching hand board player scores
        -- scores that would be achieved by placing certain dominoes
        opponentKnockingScore = scoreDomino (Data.Maybe.fromJust opponentKnocking) board player
        frequentScore = scoreDomino frequent board player
        highestScoringScore = scoreDomino highestScoring board player
        safeScore = scoreDomino (Data.Maybe.fromJust safe) board player
        -- other needed expressions
        frequentHowFrequent = scoreDominoFrequency hand (fst frequent)
        (Board _ _ history) = board
        


    -- TACTICS
    {- Tactic which picks a highest scoring domino -}
    playHighestScoring :: Tactic
    playHighestScoring hand board player _ = (extractFirst bestDomino, extractSecond bestDomino)
        where
        -- takes the first highest scoring domino, which is last in the sorted list
        bestDomino = last (sortedScoringDominoes hand board player)
    
    {- Tactic which tries to pick a domino which brings the score to the winning score (61).
        If more than one domino is available, picks the first one. -}
    playExactScore :: TacticUnsure
    playExactScore hand board player scores 
        | null availableDominoes        = Nothing
        | otherwise                     = Just (head availableDominoes)
        where
        -- need to score this much to win
        targetScore = winScore - playerScore player scores
        -- dominoes which, if placed, would score needed score
        possibleDominoes = scoreN board targetScore
        -- dominoes which if played would win the game
        availableDominoes = possDom hand board player `intersect` 
                            (possibleDominoes ++ map swapSides possibleDominoes) -- Adds flipped dominoes to the list as well

    {- Tactic which tries to pick a domino which brings the score to second-best score (59).
        If more than one domino is available, picks the first one. -}
    playSecondBest :: TacticUnsure
    playSecondBest hand board player scores
        | null availableDominoes        = Nothing
        | otherwise                     = Just (head availableDominoes)
        where
        -- need to score this much to reach second best
        targetScore = secondReachScore - playerScore player scores
        -- dominoes which, if placed, would score needed score
        possibleDominoes = scoreN board targetScore
        -- dominoes which if played would win the game
        availableDominoes = possDom hand board player `intersect`
                            (possibleDominoes ++ map swapSides possibleDominoes) -- Adds flipped dominoes to the list as well

    {- Tactic which is used when a board is empty. Checks the player's dominoes and
        plays (5,4)/(4,5) if possible. -}
    playFirstMove :: TacticUnsure
    playFirstMove hand board player _
        | dominoLeft `elem` availableDominoes                   = Just dominoLeft
        | swapSides dominoLeft `elem` availableDominoes         = Just (swapSides dominoLeft)
        | otherwise                                             = Nothing
        where
        availableDominoes = possDom hand board player
        dominoLeft = ((5,4), L) -- No need to check for R, because domino is first
    
    {- Tactic which tries to plays a highest scoring safe domino. Safe domino is the one,
        which if placed, has a successor in hand which could be placed next to it. -}
    playSafe :: TacticUnsure
    playSafe hand board player _
        | Data.Maybe.isJust safeDomino                  = safeDomino
        | otherwise                                     = Nothing
        where
        safeDomino = chooseSafeDomino hand board (sortedScoringDominoes hand board player)

    {- Tactic which plays the most frequent domino. If this domino was placed, it would
        have the most possible successors in hand.
        The hand is first sorted by highest scoring dominoes, so if there are multiple dominoes
        with the same frequency, the higher scoring one will be picked. This is possible,
        because sortOn is a stable sort. -}
    playFrequent :: Tactic
    playFrequent hand board player _ = (extractFirst bestDomino, extractSecond bestDomino)
        where
        sortedScoredHand = sortedScoringHand (sortedScoringDominoes hand board player)
        bestDomino = last (sortOn extractThird (scoreHandFrequency sortedScoredHand hand))
    
    {- Tactic which tries to play a highest scoring safe double if such exists in the hand -}
    playSafeDouble :: TacticUnsure
    playSafeDouble hand board player _
        | Data.Maybe.isJust doubleDomino            = doubleDomino
        | otherwise                                 = Nothing
        where doubleDomino = findSafeDouble (sortedScoringDominoes hand board player) hand board

    {- Plays a highest scoring domino which has the most chances to make the opponent knock-}
    playOpponentKnocking :: TacticUnsure
    playOpponentKnocking hand board@(Board _ _ history) player _
        | Data.Maybe.isJust knocksDomino            = knocksDomino
        | otherwise                                 = Nothing
        where
        knocksDomino = chooseOpponentKnockingDomino hand history board player
    
    {- Plays a domino which has has the probability of stitching the game. Such domino is dangerous to play
        for the player and potentially can make the opponent knock. -}
    playStitching :: TacticUnsure
    playStitching hand board@(Board _ _ history) player scores
        | null knocksOn                                         = Nothing
        | null dangerousDominoes                                = Nothing
        | otherwise                                             = Just (last dangerousDominoes)
        where
        -- gets playable dominoes, sorted by highest score
        playableDominoes = sortedScoringHand (sortedScoringDominoes hand board player)
        -- filters history to not include opponent moves and sorts it according to moveNumber
        filteredHistory = sortFilterHistory history player
        -- gets pips on which opponent knocks on
        knocksOn = opponentKnocksOn filteredHistory history
        -- gets all dominoes which would make the opponent knock
        opponentKnockingDominoes = sortedScoringHand(scoreOpponentKnockingHand playableDominoes board player knocksOn)
        -- gets all dominoes which are dangerous to play
        dangerousForPlayerDominoes = dangerousHand playableDominoes hand board
        -- finds dominoes which are both dangerous for the player and the opponent
        dangerousDominoes = opponentKnockingDominoes `intersect` dangerousForPlayerDominoes
    


    -- HELPER FUNCTIONS
    -- BASIC HELPER FUNCTIONS
    {- Gets first element of a triple tuple-}
    extractFirst :: (a, b, c) -> a
    extractFirst (a,_,_) = a

    {- Gets second element of a triple tuple-}
    extractSecond :: (a, b, c) -> b
    extractSecond (_,b,_) = b

    {- Gets third element of a triple tuple-}
    extractThird :: (a, b, c) -> c
    extractThird (_,_,c) = c

    {- Takes a domino with an end to place it at and flips it-}
    swapSides :: (Domino, End) -> (Domino, End)
    swapSides ((first, second), end) = ((second, first), end)

    {- Given self, returns opponent player-}
    opponent :: Player -> Player
    opponent player
        | player == P1          = P2
        | player == P2          = P1
        | otherwise             = error "Player is neither P1 or P2"

    {- Returns a score of a corresponding player -}
    playerScore :: Player -> Scores -> Int
    playerScore player scores
        | player == P1      = fst scores
        | player == P2      = snd scores
        | otherwise         = error "Player is neither P1 or P2"
    
    {- Function which takes a board and returns history -}
    getHistory :: DominoBoard -> History
    getHistory InitBoard = []
    getHistory (Board d1 d2 history) = history

    {- Given a hand, board and a player returns list of dominoes which can
        be played. -}
    possDom :: Hand -> DominoBoard -> Player -> [(Domino, End)]
    possDom dominoes board player = [(d, e) | 
        e <- [L, R], 
        d <- dominoes, Data.Maybe.isJust(playDom player d board e)]

    {- Given most recent story in the current turn, determines if the player's opponent 
        has knocked (skipped) the last turn. -}
    knockedLastTurn :: History -> Player -> Bool
    knockedLastTurn history player = player == lastPlayer
        where
        sortedHistory = sortOn extractThird history
        lastPlayer = extractSecond (last sortedHistory)



    -- SCORING/EVALUATING A DOMINO IN DIFFERENT WAYS
    {- Takes a domino and returns the score it would achieve if placed on a given board.
        Returns -1 if domino can't be placed at a given end of the board-}
    scoreDomino :: (Domino,End) -> DominoBoard -> Player -> Int
    scoreDomino (domino, end) board player 
        | Data.Maybe.isJust newBoard            = scoreBoard (Data.Maybe.fromJust newBoard)
        | otherwise                             = -1
        where
        newBoard = playDom player domino board end

    {- Scores a domino based on its pips frequency in the hand without said domino.
        e.g. in hand [(1,5),(4,1),(2,4),(5,5)]
                domino (5,5) would score 1
                domino (4,1) would score 2-}
    scoreDominoFrequency :: Hand -> Domino -> Int
    scoreDominoFrequency hand (first, second) 
        -- if domino is a double
        | first == second           = firstSideScore - 2
        | otherwise                 = firstSideScore + secondSideScore - 2
        where
        firstSideScore = quantityOfKind hand first
        secondSideScore = quantityOfKind hand second
    
    {-  Determines whether Domino to be placed at the specified End is going to
        be dangerous or not. Domino is dangerous if when placed the two pip
        sums on the left and the right of the board are not equal to any pips on hand. -}
    dangerous :: (Domino,End) -> Hand -> DominoBoard -> Bool
    -- if board is empty
    dangerous ((first, second), end) hand InitBoard
        | first == second              = (quantityOfKind newHand first)*2 == 0  -- if domino is a double
        | otherwise                    = quantityOfKind newHand first + quantityOfKind newHand second == 0
        where
        newHand = delete (first, second) hand
    -- if board is not empty
    dangerous ((first, second), end) hand (Board d1 d2 _)
        | onRight                      = quantityOfKind newHand leftSide + quantityOfKind newHand outwardSide == 0
        | onLeft                       = quantityOfKind newHand rightSide + quantityOfKind newHand outwardSide == 0
        where
        leftSide = fst d1
        rightSide = snd d2
        newHand = delete (first, second) hand
        -- side of the placed domino which faces the outside and will be used for placing other dominoes
        outwardSide 
            | end == L && first == fst d1       = second
            | end == L && second == fst d1      = first
            | end == R && first == snd d2       = second
            | end == R && second == snd d2      = first
            | otherwise                         = error "Domino can't be played"
        -- board side which domino is played at
        onRight = end == R  
        onLeft = end == L



    -- SCORING A HAND IN DIFFERENT WAYS
    {- Takes a hand and a board and returns all playable
        dominoes with the end they should be played at and their score. -}
    scoreHand :: Hand -> DominoBoard -> Player -> [(Domino, End, Int)]
    scoreHand dominoes board player = [(d, e, i) |
        e <- [L, R],
        d <- dominoes, Data.Maybe.isJust(playDom player d board e),
        i <- [scoreBoard(Data.Maybe.fromJust(playDom player d board e))]]
    
    {- Returns a sorted list of dominoes with the end to play at and 
        acquired points if played at that end-}
    sortedScoringDominoes :: Hand -> DominoBoard -> Player -> [(Domino, End, Int)]
    sortedScoringDominoes hand board player = sortOn extractThird (scoreHand hand board player)

    {- Takes a scored hand and returns only the Dominoes and End to be placed at-}
    sortedScoringHand :: [(Domino, End, Int)] -> [(Domino, End)]
    sortedScoringHand [] = []
    sortedScoringHand (current:rest) = [(extractFirst current, extractSecond current)] ++ sortedScoringHand rest
    
    {- Scores frequency of pips of each domino in a list of playable dominoes
        with an end to be played at. Returns the given list of dominoes with
        a frequency score. -}
    scoreHandFrequency :: [(Domino, End)] -> Hand -> [(Domino, End, Int)]
    scoreHandFrequency [] _ = []
    scoreHandFrequency (current:rest) hand = 
        [(fst current, snd current, scoreDominoFrequency hand (fst current))] ++ scoreHandFrequency rest hand
    
    {- Given a hand and a number of pips returns how many sides
        there are with that number of pips.
        e.g. quantityOfKind [(2,1),(5,3)] 2 = 1 -}
    quantityOfKind :: Hand -> Int -> Int
    quantityOfKind [] pips = 0
    quantityOfKind (domino:rest) pips
        | fst domino == snd domino && fst domino == pips        = quantityOfKind rest pips +2
        | fst domino == pips                                    = quantityOfKind rest pips +1
        | snd domino == pips                                    = quantityOfKind rest pips +1
        | otherwise                                             = quantityOfKind rest pips

    {- Function which takes available to play dominoes with their end and returns only those, that
        are dangerous to play -}
    dangerousHand :: [(Domino, End)] -> Hand -> DominoBoard -> [(Domino, End)]
    dangerousHand [] _ _ = []
    dangerousHand (current:rest) hand board
        | dangerous current hand board      = [current] ++ dangerousHand rest hand board
        | otherwise                         = dangerousHand rest hand board



    -- CORE FUNCTIONS OF SOME TACTICS
     {- Takes a list of sorted highest scoring dominoes and plays the highest scoring safe domino, if
        it exists. If not, returns Nothing-}
    chooseSafeDomino :: Hand -> DominoBoard -> [(Domino, End, Int)] -> Maybe (Domino, End)
    -- if no safe dominoes present, return Nothing 
    chooseSafeDomino _ _ []                                 = Nothing
    chooseSafeDomino hand board possibleDominoes
        | not (dangerous currentDomino hand board)          = Just currentDomino
        | otherwise                                         = chooseSafeDomino hand board (init possibleDominoes)
        where
        -- gets actual Domino and the End to be played at
        currentDomino = (extractFirst (last possibleDominoes), extractSecond (last possibleDominoes))

    {- Takes a list of sorted highest scoring dominoes and, if possible, returns the highest scoring
        safe double -}
    findSafeDouble :: [(Domino, End, Int)] -> Hand -> DominoBoard -> Maybe (Domino, End) 
    -- if no doubles present, return Nothing
    findSafeDouble [] _ _                                          = Nothing
    findSafeDouble possibleDominoes hand board
        | fst(fst currentDomino) == snd (fst currentDomino) &&
            not (dangerous currentDomino hand board)               = Just currentDomino
        | otherwise                                                = findSafeDouble (init possibleDominoes) hand board
        where
        -- gets actual Domino and End to be played at
        currentDomino = (extractFirst (last possibleDominoes), extractSecond (last possibleDominoes))



    -- FUNCTIONS DEALING WITH HISTORY AND KNOCKING
    {- Places given domino on a board and returns the ends of that new board.
        Assumes that domino can be placed at the specified end. -}
    findOutwardSides :: (Domino, End) -> DominoBoard -> Player -> (Int, Int)
    findOutwardSides (domino, end) board player = (fst d1, snd d2)
        where
        Board d1 d2 history = Data.Maybe.fromJust (playDom player domino board end)
    {- Takes a domino and a list of pips the opponent is knocking on and returns domino's
        knocking score if placed on that board. Knocking score is based on the ends of the
        board that will be present if that domino is placed on board.
        e.g. If a domino has a knocking score of 2 it is guaranteed that the opponent will knock. -}
    scoreOpponentKnockingDomino :: (Domino, End) -> DominoBoard -> Player -> [Int] -> Int
    scoreOpponentKnockingDomino _ _ _ [] = 0
    scoreOpponentKnockingDomino dominoWithEnd board player (current:rest)
        | leftSide == current && rightSide == current               
            = 2 + scoreOpponentKnockingDomino dominoWithEnd board player rest
        | leftSide == current || rightSide == current
            = 1 + scoreOpponentKnockingDomino dominoWithEnd board player rest
        | otherwise     = scoreOpponentKnockingDomino dominoWithEnd board player rest
        where
        outwardSides = findOutwardSides dominoWithEnd board player
        leftSide = fst outwardSides
        rightSide = snd outwardSides

    {- Takes a list of dominoes which are available to play and returns them with an 'opponent
        knocking score'. This score is higher the more chances the opponent will knock on this 
        domino.-}
    scoreOpponentKnockingHand :: [(Domino, End)] -> DominoBoard -> Player -> [Int] -> [(Domino, End, Int)]
    scoreOpponentKnockingHand [] _ _ _ = []
    scoreOpponentKnockingHand (current:rest) board player knockingOn = 
        [(fst current, snd current, (scoreOpponentKnockingDomino current board player knockingOn))] ++
        scoreOpponentKnockingHand rest board player knockingOn

    {- Takes history, sorts it by MoveNum and removes opponent's moves -}
    sortFilterHistory :: History -> Player -> History
    sortFilterHistory history player = 
        sortOn extractThird                                 -- sorts based on move number
        (filter (\p -> extractSecond p == player) history)  -- removes opponent moves

    {- Takes history, which has to be sorted by the MoveNum and filtered to not have opponent
        moves and returns a list of pips the opponent is knocking on. -}
    opponentKnocksOn :: History -> History -> [Int]
    opponentKnocksOn [] _ = []
    opponentKnocksOn (first:[]) _ = []
    opponentKnocksOn (first:second:rest) fullHistory
        -- if opponent skipped a move, returns to the board at that time and adds board ends to the list of knocking
        | difference == 1           = historyBoardEnds fullHistory (extractThird second) ++ opponentKnocksOn (second:rest) fullHistory
        -- if current two moves differ by more than 1 means that opponent has made a move and didn't knock
        | otherwise                 = opponentKnocksOn (second:rest) fullHistory
        where
        difference = (extractThird second) - (extractThird first)
    
    {- Function which takes History and an integer from which the history has to be removed
        and returns outwards ends of the board corresponding to that moment in history -}
    historyBoardEnds :: History -> Int -> [Int]
    historyBoardEnds history moment = [leftSide] ++ [rightSide]
        where
        -- filters history: moves above and equal to 'moment' are removed
        filteredHistory = filter (\p -> extractThird p < moment) history
        -- takes the ends of the board which was present at that moment in history
        leftSide = fst (extractFirst (head filteredHistory))
        rightSide = snd (extractFirst (last filteredHistory))
    
    {- Returns a domino, which if played would have the most chances of making the opponent knock.
        Returns Nothing if finding such domino is impossible. -}
    chooseOpponentKnockingDomino :: Hand -> History -> DominoBoard -> Player -> Maybe (Domino, End)
    chooseOpponentKnockingDomino hand history board player
        -- opponent hasn't knocked on anything
        | null knocksOn             = Nothing
        | otherwise                 = Just (extractFirst bestDomino, extractSecond bestDomino)
        where
        -- gets playable dominoes, sorted by highest score
        playableDominoes = sortedScoringHand (sortedScoringDominoes hand board player)
        -- filters history to not include opponent moves and sorts it according to moveNumber
        filteredHistory = sortFilterHistory history player
        -- gets pips on which opponent knocks on
        knocksOn = opponentKnocksOn filteredHistory history
        -- scores playable dominoes according to the ability to block opponent
        scoredHand = scoreOpponentKnockingHand playableDominoes board player knocksOn
        -- takes the most likely to block domino
        bestDomino = last (sortOn extractThird scoredHand)