module SetGame exposing (..)

import Browser
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Random
import Random.List
import Html exposing (select)
import Html.Attributes exposing (selected)
import List exposing (length)
import Html exposing (a)
import List exposing (filter)



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
    { table : List Card
    , selection : List Card
    , besked : String
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
    , selection = []
    , besked = ""
    }



-- UPDATE


type Msg
    = Select Card
    | Reset

removeHelp : a -> a -> Bool
removeHelp a b =
    (a /= b)

remove : a -> List a -> List a
remove c cards =
    (List.filter (removeHelp c) cards)


smartIsSet : a -> a -> a -> Bool
smartIsSet x y z =
   ((x == y && y == z) || (x /= y && y /= z && x /= z))

isSet : Card -> Card -> Card -> Bool
isSet x y z =
    ((smartIsSet x.color y.color z.color)
    && (smartIsSet x.number y.number z.number)
    && (smartIsSet x.shading y.shading z.shading)
    && (smartIsSet x.shape y.shape z.shape))


update : Msg -> Model -> Model
update msg model =
    case msg of
        Select c->
            case (List.member c model.selection) of
                False ->
                    case ((length model.selection) == 2) of
                        False -> 
                            {model | selection = c :: model.selection}
                        True -> 
                            case model.selection of
                                x :: y :: rest ->
                                    case (isSet x y c) of
                                        True ->
                                            {model | besked = "Det er et set"
                                            , table = (remove c (remove y (remove x model.table)))
                                            , selection = []}
                                        False ->
                                            {model | besked = "Det er ikke et set", selection = []}
                                rest ->
                                    model
                True ->
                    {model | selection = (remove c model.selection)}
        Reset ->
            {model | selection = []}


-- VIEW

colorToClass : Color -> Attribute Msg
colorToClass color =
    case color of
        Green ->
            Attributes.class "green"
        Purple ->
            Attributes.class "purple"
        Red ->
            Attributes.class "red"

shapeToClass : Shape -> Attribute Msg
shapeToClass shape =
    case shape of
        Diamond ->
            Attributes.class "diamond"
        Oval ->
            Attributes.class "oval"
        Squiggle ->
            Attributes.class "squiggle"

shadingToClass : Shading -> Attribute Msg
shadingToClass shading =
    case shading of
        Open ->
            Attributes.class "open"
        Striped ->
            Attributes.class "striped"
        Solid ->
            Attributes.class "solid"

numberToInt : Number -> Int
numberToInt number =
    case number of
        One -> 1
        Two -> 2
        Three -> 3

cardIsSelected : List Card -> Card -> Attribute Msg
cardIsSelected selected card =
    case (List.member card selected) of
        True ->
            Attributes.class "card selected"
        False ->
            Attributes.class "card"

buildRows : List Card -> List (List Card)
buildRows cards =
    case cards of
        x :: y :: z :: rest ->
            [x,y,z] :: (buildRows rest)
        rest ->
            []

viewCard : List Card -> Card -> Html Msg
viewCard selected card =
    Html.div [(cardIsSelected selected card), Events.onClick (Select card)] 
        (List.repeat (numberToInt card.number) (Html.div 
        [ Attributes.class "symbol"
        , (shadingToClass card.shading)
        , (colorToClass card.color)
        , (shapeToClass card.shape)][]))

viewRow : List Card -> List Card -> Html Msg
viewRow selected cards =
    Html.div [Attributes.class "row"] 
        (List.map (viewCard selected) cards)

viewTable : List Card -> List Card -> Html Msg
viewTable selected cards =
    Html.div [Attributes.class "table"] (List.map (viewRow selected) (buildRows cards))


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.header []
            [ Html.h1 []
                [ Html.text "Mit eget SET-spil"
                ]
            , Html.h3 []
                [ Html.text model.besked
                , Html.button [Events.onClick Reset]
                        [Html.text "Reset"]]
            ]
        , Html.main_ []
            [(viewTable model.selection model.table)]
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
