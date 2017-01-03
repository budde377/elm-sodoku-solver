module Style exposing (cell, table, secondGrid, hardCell, softCell, Styles, Style)

cell : Styles
cell = [
    ("width", "4em"),
    ("height", "4em"),
    ("text-align", "center"),
    ("border", "0.1em solid #a59797")
    ]

secondGrid : Styles
secondGrid = [
    ("background-color", "#d8d8d8")
    ]

table : Styles
table = [
    ("margin", "auto")
    ]

softCell : Styles
softCell =
    [ ("background-color", "#25c325")
    ]

hardCell : Styles
hardCell = []

type alias Styles = List Style
type alias Style = (String, String)
