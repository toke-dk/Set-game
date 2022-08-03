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
import List exposing (length, filter)
import Html exposing (a)
import Random.Extra exposing (bool)
import List exposing (concatMap)
import List exposing (take)
import List exposing (drop)
import List exposing (head)



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

genCardColor : Card -> List Card
genCardColor card =
    [{card | color = Red}
    , {card | color = Green}
    , {card | color = Purple}]

genCardShape : Card -> List Card
genCardShape card =
    [{card | shape = Squiggle}
    , {card | shape = Oval}
    , {card | shape = Diamond}]

genCardShading : Card -> List Card
genCardShading card =
    [{card | shading = Solid}
    , {card | shading = Open}
    , {card | shading = Striped}]

genCardNumber : Card -> List Card
genCardNumber card =
    [{card | number = One}
    , {card | number = Two}
    , {card | number = Three}]


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
    message : String,
    cardPile : List Card
    }


fullDeck : List Card
fullDeck =
    (concatMap genCardNumber
    (concatMap genCardShape 
    (concatMap genCardShading 
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
    , message = ""
    , cardPile = List.drop 12 (randomDeck 42)
    }



-- UPDATE


type Msg
    = Select Card
    | MoreCards

removeHelp : a -> a -> Bool
removeHelp a b =
    (a /= b)

remove : a -> List a -> List a
remove c cards =
    (List.filter (removeHelp c) cards)

listremove : List a -> List a -> List a
listremove r cards =
    case r of
        a :: b :: c :: rest ->
            (remove a (remove b (remove c cards)))
        rest ->
            []

replace : a -> a -> List a -> List a
replace new old cards =
    case (head cards) of
        Just c ->
            case (c == old) of
                True ->
                    (new :: (List.drop 1 cards))
                False ->
                    (c :: (replace new old (List.drop 1 cards)))
        Nothing ->
            []



listReplace : List a -> List a -> List a -> List a
listReplace new old cards =
    case new of
        x :: rest0 ->
            case (List.member x old) of
                False ->
                    case old of
                        a :: rest1 ->
                            case (List.member a new) of
                                True ->
                                    listReplace rest0 (remove a new) (remove a cards)
                                False ->
                                    (replace x a (listReplace rest0 rest1 (remove x cards)))
                        rest1 ->
                            cards
                True ->
                    listReplace rest0 (remove x old) (remove x cards)
        rest0 -> 
            cards


listRemove : List a -> List a -> List a
listRemove r cards =
    case r of
        x :: y :: z :: rest ->
            remove x (remove y (remove z cards))
        rest ->
            []

smartIsSet : a -> a -> a -> Bool
smartIsSet x y z =
    ((x==y && y==z) || (x /= y && y /= z && z /= x))

isSet : Card -> Card -> Card -> Bool
isSet x y z =
    ((smartIsSet x.color y.color z.color)
    && (smartIsSet x.number y.number z.number)
    && (smartIsSet x.shading y.shading z.shading)
    && (smartIsSet x.shape y.shape z.shape))
moreThanTwelve : List Card -> Model -> Model
moreThanTwelve old model =
    case length model.table <= 14 of
        False -> 
            {model | table = (listReplace (drop ((length model.table) - 3) model.table) old model.table)}
        True ->
            {model | 
            table =  listReplace (take 3 model.cardPile) (old) (model.table),
            cardPile = drop 3 model.cardPile,
            selection = []}


listIsSet : List Card -> Bool
listIsSet cards =
    case cards of
        x :: y :: z :: rest ->
            isSet x y z
        rest ->
            False

checkIfSet : List Card -> List Card -> Bool
checkIfSet cards set =
    case (length set == 3) of -- har set vi er på længden 3
        True -> listIsSet set -- returner om det et set
        False -> -- der er ik 3 kort i vores set
            case (head cards) of --første kort fra cards
                Just hc -> --
                    (checkIfSet (drop 1 cards) (hc :: set) || checkIfSet (drop 1 cards ) (set)) -- bruger rekursion med et set md hhv. hc og uden hc
                Nothing -> -- Cards er tom
                    False


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
                                            moreThanTwelve [x,y,card] {model | selection = [] }

                                        False -> -- fjerne selection når der ikke er set
                                            {model | selection = []}
                                rest ->
                                    model
                True ->
                    {model | selection = (remove card model.selection)}
        MoreCards ->
            case (checkIfSet model.table []) of
                True ->
                    model
                False ->
                    {model | 
                    table =  List.append model.table (take 3 model.cardPile), 
                    cardPile = drop 3 model.cardPile
                    }            


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

getCardAttribute : List Card -> Card -> Attribute Msg
getCardAttribute selection card =
    case (List.member card selection) of
        False -> Attributes.class "card"
        True -> Attributes.class "card selected"


viewCard : List Card -> Card -> Html Msg
viewCard selection card =
    Html.div [(getCardAttribute selection card), Events.onClick (Select card)
        ] 
        (List.repeat (numberToInt card.number) (Html.div 
        [Attributes.class "symbol",
        (shadingToClass card.shading), 
        (colorToClass card.color), 
        (shapeToClass card.shape)
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
            [ Html.h1 []
                [ Html.text "Mit eget SET-spil"
                ],Html.text model.message,
                Html.button [Events.onClick MoreCards] [Html.text "Flere kort"]

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