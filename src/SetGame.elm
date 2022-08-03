module SetGame exposing (..)

import Browser
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Random
import Random.List
import List exposing (length, filter)
import Html exposing (a)
import Random.Extra exposing (bool)



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
    { table : List Card,
    selection : List Card,
    message : String
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
    { table = myTable,
    selection = [],
    message = ""
    }



-- UPDATE


type Msg
    = Select Card

removeHelp : a -> a -> Bool
removeHelp x y = 
    x /= y

remove : a -> List a -> List a
remove a listA =
    (filter (removeHelp a) listA)

smartIsSet : a -> a -> a -> Bool
smartIsSet x y z =
    ((x==y && y==z) || (x /= y && y /= z && z /= x))

isSet : Card -> Card -> Card -> Bool
isSet x y z =
    (smartIsSet x.color y.color z.color) 
    && (smartIsSet x.shape y.shape z.shape) 
    && (smartIsSet x.number y.number z.number) 
    && (smartIsSet x.shading y.shading z.shading)



update : Msg -> Model -> Model
update msg model =
    case msg of
        Select card ->
            case (List.member card model.selection) of
                False ->
                    case (length model.selection == 2) of
                        False -> --0 eller 1 kort er valgt
                            {model | selection = card :: model.selection}
                        True -> --2 kort er valgt
                            case model.selection of
                                x :: y :: rest ->
                                    case (isSet x y card) of
                                        True -> --fjerner kort ved set
                                            {model | table = (remove x (remove y (remove card model.table)))}
                                        False -> -- fjerne selection når der ikke er set
                                            {model | selection = []}
                                rest ->
                                    model
                True ->
                    {model | selection = (remove card model.selection)}


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
            Attributes.class "open"
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

getCardAttribute : List Card -> Card -> Attribute Msg
getCardAttribute selection card =
    case (List.member card selection) of
        False -> Attributes.class "card"
        True -> Attributes.class "card selected"


viewCard : List Card -> Card -> Html Msg
viewCard selection card =
    Html.div [(getCardAttribute selection card), Events.onClick (Select card)
        ] 
        (List.repeat (numbertoInt card.number) (Html.div 
        [Attributes.class "symbol",
        (shadingtoClass card.shading), 
        (colortoClass card.color), 
        (shapetoClass card.shape)
        ] []))

viewRow : List Card -> List Card -> Html Msg
viewRow selection cards =
    Html.div [Attributes.class "row"] 
        (List.map (viewCard selection) cards)

buildRows: List Card -> List (List Card)
buildRows cards =
    case cards of
        x :: y :: z :: rest ->
            [x, y, z] :: (buildRows rest)
        rest ->
            []


viewTable : List Card -> List Card -> Html Msg
viewTable selection cards =
    Html.div [Attributes.class "table"] 
        (List.map (viewRow selection) (buildRows cards)) 


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.header []
            [ Html.h3 []
                [ Html.text "Mit eget SET-spil"
                ],Html.text model.message

            ]
        , Html.main_ []
            [(viewTable model.selection model.table)
            ]
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
