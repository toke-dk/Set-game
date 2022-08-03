module SetGame exposing (..)

import Browser
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Random
import Random.List
import List exposing (length)



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


type alias Model = { 
    table : List Card,
    selection : List Card,
    besked : String,
    cardPile : List Card,
    isReady : Bool,
    playersTurn : Int,
    amountOfPlayers : Int
    }
    

randomDeck : Int -> List Card
randomDeck number = 
    let 
        -- Random.step shuffles a Generator with a seed, and gives the 
        -- seed and the shuffled generator
        (deck, seed1) = Random.step (Random.List.shuffle fullDeck) (Random.initialSeed number)
    in
        deck


init : Model
init =
    { table = List.take 12 (randomDeck 42),
     selection = [],
     besked = "",
     cardPile = List.drop 12 (randomDeck 42),
     isReady = False,
     playersTurn = 1,
     amountOfPlayers = 1
    }



-- UPDATE


type Msg
    = Selected Card
    | ResetSelection (List Card)
    | Set
    | MoreCards
    | ChangeReadyState Bool
    | AddAPlayer

removeHelp : a -> a -> Bool
removeHelp a b = (a /= b)

remove : a -> List(a) -> List(a)
remove card cards = (List.filter (removeHelp card) cards)

smartIsSet : a -> a -> a -> Bool
smartIsSet x y z = ((x == y && y == z) || (x /= y && y /= z && x /= z)) 

isSet : Card -> Card -> Card -> Bool
isSet x y z =
    (smartIsSet x.color y.color z.color) 
    && (smartIsSet x.shading y.shading z.shading)
    && (smartIsSet x.shape y.shape z.shape)
    && (smartIsSet x.number y.number z.number)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Selected card -> 
            case (List.member card model.selection) of
                False ->
                    case ((length model.selection) == 2) of
                        True -> case model.selection of
                            x :: y :: rest ->
                                case (isSet x y card) of 
                                    True ->
                                        moreThanTwelve {model | besked = "Korrekt!"
                                        , selection = []
                                        , table = (remove y (remove x (remove card model.table)))}
                                    False ->
                                        {model | besked = "Fejl!", selection = []}
                            rest ->
                                model
                        False -> {model | selection = card :: model.selection}
                True -> {model | selection = (remove card model.selection)}
        ResetSelection cards -> {model | selection = []}
        Set -> model
        MoreCards -> { model | cardPile = (List.drop 3 model.cardPile), table = (List.append model.table (List.take 3 model.cardPile)) }
        ChangeReadyState state -> {model | isReady = state}
        AddAPlayer -> {model | amountOfPlayers = model.amountOfPlayers + 1}
        -- change


-- VIEW

moreThanTwelve : Model -> Model
moreThanTwelve model = 
    case ((List.length model.table) < 11) of
        False -> model
        True -> 
            { model | table = (List.append (model.table) (List.take 3 model.cardPile))
            ,cardPile = (List.drop 3 model.cardPile)}

createColorList : Card -> List Card
createColorList card = 
    [{ card | color = Purple },{ card | color = Green }, { card | color =  Red }]

createShapeList : Card -> List Card
createShapeList card = 
    [{ card | shape = Oval },{ card | shape = Diamond }, { card | shape =  Squiggle }]

createNumberList : Card -> List Card
createNumberList card = 
    [{ card | number = One },{ card | number = Two }, { card | number =  Three }]

createShadingList : Card -> List Card
createShadingList card = 
    [{ card | shading = Solid },{ card | shading = Open }, { card | shading =  Striped }]


fullDeck : List Card
fullDeck = 
    List.concatMap createShapeList (
    (List.concatMap createShadingList
    (List.concatMap createNumberList 
    (List.concatMap createColorList [exampleCard]))))

viewRow: List Card -> List Card -> Html Msg
viewRow selected cards =
    Html.div [Attributes.class "row"]
        (List.map (viewCard selected) cards) 

-- [(viewCard [] exampleCard), (viewCard [] exampleCard), (viewCard [] exampleCard)]

viewCard : List Card -> Card -> Html Msg
viewCard selected card =
    Html.div [getCardAtrribute selected card, Events.onClick (Selected card) ] 
        (List.repeat (numberToInt card.number) (Html.div
            [Attributes.class "symbol", 
            (shapeToClass card.shape), 
            (shadingToClass card.shading),
            (colorToClass card.color)
            ] [])) 

getCardAtrribute :  List(Card) -> Card -> Attribute Msg
getCardAtrribute selected card =  
    case (List.member card selected) of
        True -> Attributes.class "card selected"
        False -> Attributes.class "card"

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
viewTable selected cards =
    Html.div [Attributes.class "table"] (List.map (viewRow selected) (buildRows cards))

view : Model -> Html Msg
view model =
    case (model.isReady) of 
            False ->
                Html.div []
                    [ Html.header []
                        [ Html.h3 []
                            [ Html.text "Vælg spillere"
                            ]
                        ,   (Html.text ("Spillere i alt: '" ++ (String.fromInt model.amountOfPlayers) ++ "' "))
                        ],
                        Html.div [] [

                            Html.button [Events.onClick AddAPlayer][Html.text "Tilføj spiller"],
                            Html.button [Events.onClick (ChangeReadyState True)][Html.text "Klar til at spille!"]

                        ]
                    ]
            True ->
                Html.div []
                    [ Html.header []
                        [ Html.h3 []
                            [ Html.text "Mit eget SET-spil"
                            ]
                        , (Html.text model.besked)
                        , (Html.button [Events.onClick MoreCards][Html.text "+3 kort"])
                        ],
                        Html.main_ []
                        [ Html.div [][viewTable model.selection model.table] ]
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
