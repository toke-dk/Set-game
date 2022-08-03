module SetGame exposing (..)

import Browser
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Random
import Random.List



-- CARD (Denne sektion definerer Card typen)


type Shape
    = Diamond
    | Squiggle
    | Oval


type Color
    = Green
    | Purple
    | Red


type Shading
    = Open
    | Striped
    | Solid


type Number
    = One
    | Two
    | Three


type alias Card =
    { shape : Shape
    , color : Color
    , shading : Shading
    , number : Number
    }



-- CONSTANTS (Her finder du "konstanter" eller eksempeldata, som du kan bruge til at tjekke dine funktioner med)

numberToInt: Number -> Int
numberToInt number = 
    case number of
        One -> 1
        Two -> 2
        Three -> 3   

exampleCard : Card 
exampleCard =
    { shape = Squiggle
    , color = Red
    , shading = Solid
    , number = Two
    }


exampleRow : List Card
exampleRow =
    List.repeat 3 exampleCard


exampleTable : List Card
exampleTable =
    List.repeat 12 exampleCard


myTable : List Card
myTable =
    [ { shape = Oval, color = Green, shading = Solid, number = Three }
    , { shape = Squiggle, color = Purple, shading = Striped, number = One }
    , { shape = Diamond, color = Purple, shading = Striped, number = Two }
    , { shape = Diamond, color = Red, shading = Striped, number = One }
    , { shape = Squiggle, color = Purple, shading = Solid, number = One }
    , { shape = Diamond, color = Red, shading = Open, number = Two }
    , { shape = Oval, color = Purple, shading = Open, number = Two }
    , { shape = Oval, color = Red, shading = Solid, number = Three }
    , { shape = Diamond, color = Green, shading = Solid, number = Three }
    , { shape = Squiggle, color = Green, shading = Solid, number = Three }
    , { shape = Diamond, color = Red, shading = Striped, number = Two }
    , { shape = Oval, color = Green, shading = Open, number = Two }
    ]



-- MODEL


type alias Model =
    { table : List Card
    }


fullDeck : List Card
fullDeck =
    {- TODO -}
    [ exampleCard, exampleCard ]


randomDeck : Int -> List Card
randomDeck number =
    {- TODO -}
    [ exampleCard ]


init : Model
init =
    { table = myTable
    }



-- UPDATE


type Msg
    = ReplaceMe


isSet : Card -> Card -> Card -> Bool
isSet x y z =
    {- TODO -}
    False


update : Msg -> Model -> Model
update msg model =
    case msg of
        ReplaceMe ->
            model



-- VIEW

viewRow: List Card -> Html Msg
viewRow cards =
    Html.div [Attributes.class "row"]
        (List.map (viewCard []) cards) 

-- [(viewCard [] exampleCard), (viewCard [] exampleCard), (viewCard [] exampleCard)]

viewCard : List Card -> Card -> Html Msg
viewCard _ card =
    Html.div [Attributes.class "card"] 
        (List.repeat (numberToInt card.number) (Html.div
            [Attributes.class "symbol", 
            (shapeToClass card.shape), 
            (shadingToClass card.shading),
            (colorToClass card.color)] [])) 

colorToClass : Color -> Attribute Msg
colorToClass color =
    case color of 
        Green -> Attributes.class "green"
        Red -> Attributes.class "red"
        Purple -> Attributes.class "purple"

shapeToClass : Shape -> Attribute Msg
shapeToClass shape =
    case shape of 
        Diamond -> Attributes.class "diamond"
        Oval -> Attributes.class "oval"
        Squiggle -> Attributes.class "squiggle"


shadingToClass : Shading -> Attribute Msg
shadingToClass shading =
    case shading of 
        Open -> Attributes.class "open"
        Striped -> Attributes.class "striped"
        Solid -> Attributes.class "solid"

buildRows: List Card -> List (List Card)
buildRows cards =
    case cards of 
        x :: y :: z :: rest ->
            [x,y,z] :: (buildRows rest)
        rest ->
            []

viewTable : List Card -> List Card -> Html Msg
viewTable _ cards =
    Html.div [Attributes.class "table"] (List.map viewRow (buildRows cards))

view : Model -> Html Msg
view model =
    Html.div []
        [ Html.header []
            [ Html.h3 []
                [ Html.text "Mit eget SET-spil"
                ]
            ]
        , Html.main_ []
            [ Html.div [][
                Html.div [][viewTable [] model.table]
            ] ]
        ]



-- MAIN - Koden herunder behøves ikke modifikation.


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = \model -> Html.div [] [ stylesheet, view model ]
        }


stylesheet : Html msg
stylesheet =
    Html.div []
        [ Html.node "link"
            [ Attributes.rel "stylesheet"
            , Attributes.href "/assets/simple.min.css"
            ]
            []
        , Html.node "link"
            [ Attributes.rel "stylesheet"
            , Attributes.href "/assets/set.css"
            ]
            []
        ]
