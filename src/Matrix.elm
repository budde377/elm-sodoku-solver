module Matrix exposing (Matrix, MatrixRow, Position, set, mapToArray, mapToList, slice, init, map, reduce, indexedMap, get, toArray, toList)

import Array exposing (Array)

type alias Position = (Int, Int)

type alias MatrixRow a = Array a

type alias Matrix a = Array (MatrixRow a)

set : a -> Position -> Matrix a -> Matrix a
set value (row, column) matrix =
    case Array.get row matrix of
        Nothing -> matrix
        Just r -> Array.set row (Array.set column value r) matrix

get : Position -> Matrix a -> Maybe a
get (row, column) matrix =
    case Array.get row matrix of
        Nothing -> Nothing
        Just r -> Array.get column r


mapToArray : (Position -> a -> b) -> Matrix a -> Array b
mapToArray map matrix = toArray (indexedMap map matrix)

toArray : Matrix a -> Array a
toArray matrix = Array.foldl
    Array.append
    Array.empty
    matrix

toList : Matrix a -> List a
toList matrix = Array.toList (toArray matrix)

mapToList : (Position-> a -> b) -> Matrix a -> List b
mapToList map matrix = toList (indexedMap map matrix)

map : (a -> b) -> Matrix a -> Matrix b
map mapper matrix =
    Array.map (\ row -> Array.map mapper row) matrix

indexedMap : (Position -> a -> b) -> Matrix a -> Matrix b
indexedMap mapper matrix =
    Array.indexedMap (\i row -> Array.indexedMap (\j val -> (mapper (i, j) val)) row) matrix

reduce : (a -> a -> a) -> Matrix a -> List (Matrix a) -> Matrix a
reduce reducer matrix matrices =
    indexedMap (\ pos val ->
        let
            getter = (get pos)
            listA = List.filterMap getter matrices
            newA = List.foldl reducer val listA
        in
            newA) matrix


slice : Int -> Int -> Int -> Int -> Matrix a -> Matrix a
slice startRow endRow startColumn endColumn matrix =
    Array.map (\ row -> Array.slice startColumn endColumn row) (Array.slice startRow endRow matrix)

init : Int -> Int -> (Position  -> a) -> Matrix a
init rows columns map =
    Array.initialize rows (\ r -> Array.initialize columns (\ c -> map (r, c)))

