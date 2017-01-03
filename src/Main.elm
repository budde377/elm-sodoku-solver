import Html exposing (Html, text, h1, div, table, tr, td, input, ul, li, button)
import Html.Attributes exposing (type_, value, style, placeholder)
import Html.Events exposing (onInput, onClick)
import Style
import Array exposing (Array)
import Matrix
import Debug
import Set

main = Html.program {
    init = init,
    update = update,
    subscriptions = subscriptions,
    view = view
    }


optValueToMaybe : OptValue -> Maybe Int
optValueToMaybe v = case v of
    None -> Nothing
    Soft a -> Just a
    Hard a -> Just a


type OptValue = Soft Int | Hard Int | None

type alias ValueMatrixRow = Matrix.MatrixRow OptValue

type alias ValueMatrix = Matrix.Matrix OptValue

type alias SuggestionType = Set.Set Int

type alias SuggestionMatrixRow = Matrix.MatrixRow SuggestionType

type alias SuggestionMatrix = Matrix.Matrix SuggestionType

type alias Position = (Int, Int)

type ConflictType = Row | Column | Part

type alias ConflictDetails = {from : Position, to : Position, conflictType : ConflictType}

type Error = InvalidInput Position
    | Conflict ConflictDetails
    | NonSolvable Position

type alias Model =
    { values: ValueMatrix
    , syntaxErrors: List Error
    , suggestions: SuggestionMatrix
    }

type Msg =
    NoOp | Update { pos: Position, value: OptValue} | Solve | Clear

initialValueMatrix : ValueMatrix
initialValueMatrix = Matrix.init 9 9 (\ _ -> None)

initialSuggestions : SuggestionMatrix
initialSuggestions = matrixSuggestions initialValueMatrix

initialSyntaxErrors : List Error
initialSyntaxErrors = matrixSyntaxErrors initialValueMatrix initialSuggestions


init : (Model, Cmd Msg)
init =
    (Model initialValueMatrix initialSyntaxErrors initialSuggestions, Cmd.none)


subValueMatrixConflictErrors : ConflictType -> Int -> Int -> ValueMatrix -> List Error
subValueMatrixConflictErrors tpe offsetRow offsetColumn matrix =
    let matrixList = Matrix.mapToList (\ (r, c) v -> {pos = (r + offsetRow, c + offsetColumn), value = v}) matrix
        list = List.filterMap (\ v -> case optValueToMaybe v.value of
            Nothing -> Nothing
            Just val -> Just {pos = v.pos, value = val}) matrixList
        sortedList = List.sortBy .value list
        checker = (\ l ->
            case l of
                [] -> []
                [_] -> []
                a::b::ls -> (if a.value == b.value then [Conflict {from = a.pos, to = b.pos, conflictType = tpe}] else []) ++ checker (b::ls))
    in
        checker sortedList


rowMatrix : Int -> Matrix.Matrix a -> Matrix.Matrix a
rowMatrix rowNumber matrix = Matrix.slice rowNumber (rowNumber + 1) 0 9 matrix

colMatrix : Int -> Matrix.Matrix a -> Matrix.Matrix a
colMatrix colNumber matrix = Matrix.slice 0 9 colNumber (colNumber + 1) matrix

partMatrix : Position -> Matrix.Matrix a -> Matrix.Matrix a
partMatrix (row, col) matrix =
    let
        normalizedRow = (row // 3) * 3
        normalizedCol = (col // 3) * 3
    in Matrix.slice normalizedRow (normalizedRow + 3) normalizedCol (normalizedCol + 3) matrix

matrixConflictErrors : ValueMatrix -> List Error
matrixConflictErrors matrix = List.foldl
    (++)
    []
    (List.map
        (\ i ->
            (subValueMatrixConflictErrors Column 0 i (rowMatrix i matrix))
            ++ (subValueMatrixConflictErrors Row i 0 (colMatrix i matrix))
            ++ (
                let
                    r = (i % 3) * 3
                    c = (i // 3) * 3
                in
                    subValueMatrixConflictErrors Part r c (partMatrix (r, c) matrix)))
        (List.range 0 8))


matrixInvalidInputErrors : ValueMatrix -> List Error
matrixInvalidInputErrors matrix = List.filterMap
    identity
    (Matrix.mapToList
        (\ (r, c) v ->
            case optValueToMaybe v of
                Just v -> if v > 9 || v < 1 then Just (InvalidInput (r, c)) else Nothing
                Nothing -> Nothing)
        matrix)

matrixUnsolvableErrors : ValueMatrix -> SuggestionMatrix -> List Error
matrixUnsolvableErrors valueMatrix matrix =
    let
        isNone = (\ v -> case v of
            Just None -> True
            _ -> False)
        suggestions = Matrix.toList (Matrix.indexedMap (\ pos value -> {pos = pos, value = value}) matrix)
        emptySuggestions = List.filter (\ {value, pos} -> Set.size value == 0 && (isNone (Matrix.get pos valueMatrix))) suggestions
    in
        List.map (\ {pos} -> NonSolvable pos ) emptySuggestions


matrixSyntaxErrors : ValueMatrix -> SuggestionMatrix -> List Error
matrixSyntaxErrors matrix suggestionMatrix = (matrixInvalidInputErrors matrix) ++ (matrixConflictErrors matrix) ++ (matrixUnsolvableErrors matrix suggestionMatrix)

fullSet : Set.Set Int
fullSet = Set.fromList [1,2,3,4,5,6,7,8,9]

matrixSuggestions : ValueMatrix -> SuggestionMatrix
matrixSuggestions values =
    let
        matrices = Matrix.mapToList (\(row, col) val -> Matrix.init 9 9 (\ (r, c) ->
            case optValueToMaybe val of
                Nothing -> Set.empty
                Just v -> (if row == r || col == c || (row // 3 == r // 3 && col // 3 == c // 3)
                    then (if row == r && col == c then fullSet else (Set.singleton v))
                    else Set.empty))) values
        takenMatrix = Matrix.reduce Set.union (Matrix.init 9 9 (\ _ -> Set.empty)) matrices
        invertedMatrix = Matrix.map (Set.diff fullSet) takenMatrix
        filteredMatrix = Matrix.indexedMap
            (\ (row, col) value ->
                let
                    filteredInvertedMatrix = Matrix.set Set.empty (row, col) invertedMatrix
                    rowSet =  Array.foldl Set.union Set.empty (Matrix.toArray (rowMatrix row filteredInvertedMatrix))
                    colSet = Array.foldl Set.union Set.empty (Matrix.toArray (colMatrix col filteredInvertedMatrix))
                    partSet = Array.foldl Set.union Set.empty (Matrix.toArray  (partMatrix (row, col) filteredInvertedMatrix))
                in Set.foldl (\ v set -> if Set.member v rowSet && Set.member v colSet && Set.member v partSet then set else Set.singleton v) value value)
            invertedMatrix
    in
        filteredMatrix

setValue : Position -> OptValue -> Model -> Model
setValue pos value model =
    let
        newValueMatrix = (Matrix.set value pos model.values)
        newSuggestionsMatrix = matrixSuggestions newValueMatrix
        newSyntaxErrors = matrixSyntaxErrors newValueMatrix newSuggestionsMatrix
    in {model | values = newValueMatrix, syntaxErrors = newSyntaxErrors, suggestions = newSuggestionsMatrix}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)
        Update {pos, value} -> (setValue pos value model, Cmd.none)
        Solve -> (solve model, Cmd.none)
        Clear -> (clear model, Cmd.none)

clear : Model -> Model
clear model =
    let
        filter = (\ {value} -> case value of
            Soft a -> True
            _ -> False)
        subjects = List.filter filter (Matrix.toList (Matrix.indexedMap (\ pos val -> {pos = pos, value = val}) model.values))
    in
        List.foldl (\ {pos} model -> setValue pos None model) model subjects

solve : Model -> Model
solve model =
    case findStep model of
        Nothing -> guessSolve model
        Just {pos, value} -> solve (setValue pos value model)

guessSolve : Model -> Model
guessSolve model  =
    case (model.syntaxErrors, List.filter (\ {set} -> Set.size set /= 0) (Matrix.toList (Matrix.indexedMap (\ pos set -> {pos = pos, set = set}) model.suggestions))) of
        ([], {pos, set}::_) ->
            let
                f = (\ l -> case l of
                    i::is ->
                        let
                            nextModel = solve (setValue pos (Soft i) model)
                        in
                            case nextModel.syntaxErrors of
                            [] -> nextModel
                            _ -> f is
                    _ -> {model | syntaxErrors = [NonSolvable pos]})
            in f (Set.toList set)
        _ -> model

findStep : Model -> Maybe {pos : Position, value : OptValue}
findStep model =
    case List.filter (\ {set} -> Set.size set == 1) (Matrix.toList (Matrix.indexedMap (\ pos set -> {pos = pos, set = set}) model.suggestions)) of
        [] -> Nothing
        {set, pos}::_ ->
            case Set.toList set of
                [v] -> Just {pos = pos, value = (Soft v)}
                _ -> Nothing


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

parseValue : OptValue -> String -> OptValue
parseValue origValue str =
    if str == "" then
        None
    else
        case String.toInt str of
            Err _ -> origValue
            Ok v -> Hard v

solverCell : Position -> Model -> Html Msg
solverCell (row, col) model =
    let
        val = case Matrix.get (row, col) model.values of
            Nothing -> None
            Just a -> a
        suggestions = case Matrix.get (row, col) model.suggestions of
            Nothing -> ""
            Just set -> String.join ", " (Set.toList (Set.map toString set))
        softHardStyle = case val of
            None -> []
            Hard _ -> Style.hardCell
            Soft _ -> Style.softCell
        s = (if xor ((col // 3) % 2 == 0) ((row // 3) % 2 == 0) then Style.secondGrid else []) ++ softHardStyle
    in
        td []
            [input
                [ type_ "text"
                , style (Style.cell ++ s)
                , placeholder suggestions
                , value (case val of
                    None -> ""
                    Hard i -> (toString i)
                    Soft i -> (toString i))
                , (onInput  (\s -> Update {value = (parseValue val s), pos = (row, col)}))
                ]
                []]

solverRow : Int -> ValueMatrixRow -> Model -> Html Msg
solverRow rowNumber row model = tr
    []
    (Array.toList
        (Array.indexedMap
            (\ c value -> solverCell (rowNumber, c) model)
            row
        ))


xor : Bool -> Bool -> Bool
xor b1 b2 = (b1 && b2) || not (b1 || b2)

solverGrid: Model -> Html Msg
solverGrid model = table
    [style Style.table]
    (Array.toList
        (Array.indexedMap
            (\ r row -> solverRow r row model)
            model.values))


errorToString : Error -> String
errorToString error =
    case error of
        InvalidInput (row, column) -> "Invalid input at row " ++ (toString row) ++ " and column " ++ (toString column)
        NonSolvable (row, column) -> "It is non solveable because of (" ++ (toString row) ++", " ++ (toString column) ++ ")"
        Conflict {from, to, conflictType} ->
            let
                t = case conflictType of
                    Row -> "Row"
                    Column -> "Column"
                    Part -> "Part"
            in
                t ++ " conflict from (" ++ (toString (Tuple.first from)) ++", " ++ (toString (Tuple.second from)) ++ ") to (" ++ (toString (Tuple.first to)) ++", " ++ (toString (Tuple.second to)) ++ ")"

errorToLi : Error -> Html Msg
errorToLi err = li [] [text (errorToString err)]


view : Model -> Html Msg
view model =
    div
        []
        [ h1 [] [text "Soduko solver"]
        , div [] [solverGrid model]
        , ul [] (List.map errorToLi model.syntaxErrors)
        , button [onClick Solve] [text "Solve"]
        , button [onClick Clear] [text "Clear"]
        ]

