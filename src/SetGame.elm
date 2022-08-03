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
import List exposing (concatMap)
import Random exposing (initialSeed)



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
genCardColor : Card -> List Card
genCardColor card =
    [ {card | color = Red}
    , {card | color = Purple}
    , {card | color = Green}]

genCardShape : Card -> List Card
genCardShape card =
    [ {card | shape = Oval}
    , {card | shape = Squiggle}
    , {card | shape = Diamond}]

genCardShading : Card -> List Card
genCardShading card =
    [ {card | shading = Open}
    , {card | shading = Striped}
    , {card | shading = Solid}]

genCardNumber : Card -> List Card
genCardNumber card =
    [ {card | number = One}
    , {card | number = Two}
    , {card | number = Three}]

type alias Model =
    { table : List Card
    , selection : List Card
    , besked : String
    , cardPile : List Card
    }

fullDeck : List Card
fullDeck =
    (concatMap genCardShape
        (concatMap genCardShading
        (concatMap genCardNumber 
    (genCardColor exampleCard))))


randomDeck : Int -> List Card
randomDeck number =
    let
        (deck, seed1) = (Random.step (Random.List.shuffle fullDeck) (Random.initialSeed number))
    in
        deck

init : Model
init =
    { table = List.take 12 (randomDeck 42)
    , selection = []
    , besked = ""
    , cardPile = List.drop 12 (randomDeck 42)
    }



-- UPDATE


type Msg
    = Select Card
    | Reset
    | MoreCards

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
                                            {model | table = List.append (remove c (remove y (remove x model.table))) (List.take 3 model.cardPile)
                                            , cardPile = List.drop 3 model.cardPile
                                            , selection = []}
                                        False ->
                                            {model | selection = []}
                                rest ->
                                    model
                True ->
                    {model | selection = (remove c model.selection)}
        Reset ->
            {model | selection = []}
        MoreCards ->
            { model | table = List.append model.table (List.take 3 model.cardPile)
            , cardPile = List.drop 3 model.cardPile}


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
                        [Html.text "Reset"]
                , Html.button [Events.onClick MoreCards]
                        [Html.text "More Cards"]
                ]
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
