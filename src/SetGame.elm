module SetGame exposing (..)

import Browser
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Random
import Random.List



-- CARD (Denne sektion definerer Card typen)
--test

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
    { replaceMe : ()
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
    { replaceMe = ()
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

colortoClass : Color -> Attribute Msg
colortoClass color =
    case color of
        Green ->
            Attributes.class "green"
        Purple ->
            Attributes.class "purple"
        Red ->
            Attributes.class "red"

shapetoClass : Shape -> Attribute Msg
shapetoClass shape =
    case shape of
        Squiggle ->
            Attributes.class "squiggle"
        Diamond ->
            Attributes.class "diamond"
        Oval ->
            Attributes.class "oval"

shadingtoClass : Shading -> Attribute Msg
shadingtoClass shading =
    case shading of
        Open ->
            Attributes.class "solid"
        Striped ->
            Attributes.class "striped"
        Solid ->
            Attributes.class "solid"

numbertoInt : Number -> Int
numbertoInt number = 
    case number of
        One -> 1
        Two -> 2
        Three -> 3

viewCard : List Card -> Card -> Html Msg
viewCard _ card =
    Html.div [Attributes.class "card"] 
        (List.repeat (numbertoInt card.number) (Html.div 
        [Attributes.class "symbol",
        (shadingtoClass card.shading), 
        (colortoClass card.color), 
        (shapetoClass card.shape)] []))

viewRow : List Card -> Html Msg
viewRow cards =
    Html.div [Attributes.class "row"] 
        (List.map (viewCard []) cards)


viewTable : List Card -> List Card -> Html Msg
viewTable _ cards =
    Html.div [] [ Html.div [] [ Html.text "todo" ] ]


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.header []
            [ Html.h3 []
                [ Html.text "Mit eget SET-spil"
                ]
            ]
        , Html.main_ []
            [(viewRow (List.repeat 3 exampleCard))]
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
