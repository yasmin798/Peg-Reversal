type Position = (Int,Int)
data Color = W | B deriving (Eq, Show)
data Peg = Peg Position Color deriving (Eq, Show)
data Move = M Position deriving (Eq, Show)
type Board = [Peg]
data State = S Move Board deriving (Eq, Show)

createBoard :: Position -> Board
createBoard pos@(x, y)
    | not (isValidPosition pos) = error "The position is not valid."
    | otherwise = [Peg p (if p == pos then W else B) | p <- allowedPositions]
    where
        isValidPosition (x, y) = (x, y) `elem` allowedPositions
        allowedPositions = [(-3,-1), (-3,0), (-3,1), (-2,-1), (-2,0), (-2,1), (-1,-3), (-1,-2), (-1,-1), (-1,0), (-1,1), (-1,2), (-1,3), (0,-3), (0,-2), (0,-1), (0,0), (0,1), (0,2), (0,3), (1,-3), (1,-2), (1,-1), (1,0), (1,1), (1,2), (1,3), (2,-1), (2,0), (2,1), (3,-1), (3,0), (3,1)]



isValidMove :: Move -> Board -> Bool
isValidMove (M pos) board = isValidPosition pos && isBlack pos board && hasWhiteNeighbor pos board
    where
        isValidPosition (x, y) = abs x <= 3 && abs y <= 3
        isBlack p b = any (\(Peg p' c) -> p' == p && c == B) b
        hasWhiteNeighbor (x, y) b = any (\(Peg (x', y') c) -> c == W && abs (x' - x) <= 1 && abs (y' - y) <= 1) b

isGoal :: Board -> Bool
isGoal board = all isWhite board
    where
        isWhite (Peg _ color) = color == W

		
showPossibleNextStates :: Board -> [State]
showPossibleNextStates board
    | isGoal board = error "No Possible States Exist."
    | otherwise = validCheck allMoves
    where
        allMoves = [(M pos) | Peg pos B <- board]
        validCheck [] = []
        validCheck (h:t)
            | isValidMove h board = S h (executeMove h board) : validCheck t
            | otherwise = validCheck t

executeMove :: Move -> Board -> Board
executeMove (M pos) board = [if p == pos then Peg p W else Peg p c | Peg p c <- board]

