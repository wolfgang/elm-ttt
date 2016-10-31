module AI exposing (makeRandomMoveCmd)
import ListExt
import Random
import Msg exposing(Msg(RandomMove))
import Board

makeRandomMoveCmd model = 
    Random.generate RandomMove (mapRandomIndexToCellCoord (Board.getEmptyCells model))

mapRandomIndexToCellCoord cellCoords = 
    Random.map 
        (\index -> ListExt.nth index cellCoords (-1, -1)) 
        (Random.int 0 (List.length cellCoords - 1))
