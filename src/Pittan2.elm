module Pittan2 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Html.Events.Extra.Pointer as P
import Dictionary exposing (..)
import Dict exposing (Dict)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated
import Simple.Animation.Property as Prop

main = Browser.element { init = init
                       , update = update
                       , view = view
                       , subscriptions = subscriptions
                       }

type alias Model = { conf: Conf
                   , candidates: List String
                   , board: Board
                   , startedAt: {x: Float, y: Float}
                   , nowAt: {x: Float, y: Float}
                   , moving: Maybe Int
                   , cursor: Int
                   , newWordsAt: List (List Cell)
                   , foundWords: List String
                   }


type alias Conf = List Piece

type alias Piece = { id: Int
                   , x: Int
                   , y: Int
                   , c: String
                   , used: Bool
                   }

type alias Cell = { x: Int
                  , y: Int
                  }
type alias Board = List Cell

type Msg = PDown Int {x: Float, y:Float}
    | PUp {x:Float, y:Float}
    | PMove {x:Float, y:Float}
    | GenPiece Piece {x:Float, y:Float}
    | CursorPlus
    | CursorMinus

unit = 60

init : () -> (Model, Cmd Msg)
init _ =
    ( Model initConf
          aKaraN
          shikaku
          {x=0, y=0} {x=0, y=0} Nothing 0
          []
          []
    , Cmd.none
    )

initConf : List Piece    
initConf = [Piece 0 3 3 "あ" False
           ,Piece 1 3 4 "い" False]

aKaraN : List String
aKaraN = [ "あ", "い", "う", "え", "お"
         , "か", "き", "く", "け", "こ"
         , "さ", "し", "す", "せ", "そ"
         , "た", "ち", "つ", "て", "と"
         , "な", "に", "ぬ", "ね", "の"
         , "は", "ひ", "ふ", "へ", "ほ"
         , "ま", "み", "む", "め", "も"
         , "や", "ゆ", "よ"
         , "ら", "り", "る", "れ", "ろ"
         , "わ"
         , "ん"
         , "が", "ぎ", "ぐ", "げ", "ご"
         , "ざ", "じ", "ず", "ぜ", "ぞ"
         , "だ", "ぢ", "づ", "で", "ど"
         , "ば", "び", "ぶ", "べ", "ぼ"
         , "ぱ", "ぴ", "ぷ", "ぺ", "ぽ"
         , "ぁ", "ぃ", "ぅ", "ぇ", "ぉ"
         , "っ"
         , "ゃ", "ゅ", "ょ"
         ]

shikaku : Board
shikaku  =
    List.concat <|
        List.map (\x ->
                      List.map (\y -> Cell x y) <| List.range 3 7
                 )
            <| List.range 3 7
    

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PDown id pos ->
            ( { model | startedAt = pos
              , nowAt = pos
              , moving = Just id}
            , Cmd.none)
        PMove pos ->
              ( {model | nowAt = pos}
              , Cmd.none)
        PUp pos ->
            let
                x = ((floor  (pos.x / (toFloat unit))) )
                y = ((floor  (pos.y / (toFloat unit))))
                onCell = List.member (Cell x y) model.board
                newConf = if onCell then
                              List.map (\p -> case model.moving of
                                                  Just id -> 
                                                      if id == p.id then
                                                          {p | x=x, y=y}
                                                      else
                                                          p
                                                  _  -> p
                                       ) model.conf
                          else
                              List.filter (\p -> p.id /= (Maybe.withDefault (-1) <| model.moving)) model.conf
                makeWord = valid newConf (Cell x y)
                newWords = fromDictionary newConf (Cell x y)
            in
                ( {model | startedAt = {x=0, y=0}
                  , nowAt = {x=0, y=0}
                  , moving = Nothing
                  , conf = if List.length makeWord > 0 then
                               newConf
                           else
                              List.filter (\p -> p.id /= (Maybe.withDefault (-1) <| model.moving)) model.conf
                  , newWordsAt = makeWord
                  , foundWords = model.foundWords ++ newWords
                  }
                , Cmd.none
                )
        GenPiece p pos ->
            ({ model | conf = p::model.conf
             , moving = Just (p.id)
             , startedAt = pos
             , nowAt = pos
             }
            , Cmd.none
            )
        CursorPlus ->
            ({ model | cursor = if model.cursor < 0 then
                                    model.cursor + 5
                                else
                                    model.cursor
             }
            , Cmd.none
            )
        CursorMinus ->
            ({ model | cursor= if model.cursor > -(List.length model.candidates) then
                                    model.cursor - 5
                                else
                                    model.cursor
             }
            , Cmd.none
            )
            

valid: Conf -> Cell ->  List (List Cell)
valid conf cell  =
    let
        row = Debug.log "row" <| List.sortBy .x <| List.filter (\p -> p.y == cell.y) conf
        xMin = Debug.log "min" <| Basics.min cell.x <| Maybe.withDefault cell.x <| List.minimum <| List.map .x  row
        xMax = Debug.log "max" <| Basics.max cell.x <| Maybe.withDefault cell.x <| List.maximum <| List.map .x  row
        xLow = Debug.log "left" <| List.filter (\i -> (cell.x - i + 1) ==
                                List.length (List.filter (\p -> i <= p.x && p.x <= cell.x) row)
                           ) <| List.range xMin cell.x
        xHigh = Debug.log "right" <| List.filter (\i -> (i - cell.x + 1) ==
                                List.length (List.filter (\p -> cell.x <= p.x && p.x <= i) row)
                           ) <| List.range cell.x xMax
        hRanges = Debug.log "range" <| List.concat <| List.map (\start -> List.map (\end ->  List.range start end) xHigh) xLow
        hWordAt : List Int -> Bool
        hWordAt range =
            member  (String.concat <| List.map .c <| List.concat <|
                List.map (\x -> List.filter (\p -> p.x == x) row) range)
        hWords = Debug.log "horizontal" <|
                 List.foldl (\range cells ->
                                 if hWordAt range then
                                     cells++[List.map (\x -> Cell x (cell.y)) range]
                                 else
                                     cells
                            )
                     [] hRanges

        col = List.sortBy .y <| List.filter (\p -> p.x == cell.x ) conf
        yMin = Basics.min cell.y <| Maybe.withDefault cell.y <| List.minimum <| List.map .y col
        yMax = Basics.max cell.y <| Maybe.withDefault cell.y <| List.maximum <| List.map .y col
        yLow = Debug.log "above" <| List.filter (\j -> (cell.y - j + 1) ==
                                                     List.length (List.filter (\p -> j <= p.y && p.y <= cell.y) col)
                                                ) <| List.range yMin cell.y
        yHigh = Debug.log "below" <| List.filter (\j -> (j - cell.y + 1) ==
                                                      List.length (List.filter (\p -> cell.y <= p.y && p.y <= j) col)
                                                 ) <| List.range cell.y yMax
        vRanges = Debug.log "range" <| List.concat <| List.map (\start -> List.map (\end ->  List.range start end) yHigh) yLow
        vWordAt : List Int -> Bool
        vWordAt range =
            member (String.concat <| List.map .c <| List.concat <|
                             List.map (\y -> List.filter (\p -> p.y == y) col) range)
        vWords = Debug.log "vertical" <|
                 List.foldl (\range cells -> if vWordAt range then
                                                 cells++[List.map (\y -> Cell (cell.x) y) range]
                                             else
                                                 cells
                            )
                     [] vRanges
    in
        (hWords++vWords)

fromDictionary: Conf -> Cell ->  List String
fromDictionary conf cell  =
    let
        row = Debug.log "row" <| List.sortBy .x <| List.filter (\p -> p.y == cell.y) conf
        xMin = Debug.log "min" <| Basics.min cell.x <| Maybe.withDefault cell.x <| List.minimum <| List.map .x  row
        xMax = Debug.log "max" <| Basics.max cell.x <| Maybe.withDefault cell.x <| List.maximum <| List.map .x  row
        xLow = Debug.log "left" <| List.filter (\i -> (cell.x - i + 1) ==
                                List.length (List.filter (\p -> i <= p.x && p.x <= cell.x) row)
                           ) <| List.range xMin cell.x
        xHigh = Debug.log "right" <| List.filter (\i -> (i - cell.x + 1) ==
                                List.length (List.filter (\p -> cell.x <= p.x && p.x <= i) row)
                           ) <| List.range cell.x xMax
        hRanges = Debug.log "range" <| List.concat <| List.map (\start -> List.map (\end ->  List.range start end) xHigh) xLow
        hWordAt : List Int -> List String
        hWordAt range =
            find  (String.concat <| List.map .c <| List.concat <|
                List.map (\x -> List.filter (\p -> p.x == x) row) range)
        hWords = Debug.log "horizontal" <|
                 List.foldl (\range cells -> cells++(hWordAt range))
                     [] hRanges

        col = List.sortBy .y <| List.filter (\p -> p.x == cell.x ) conf
        yMin = Basics.min cell.y <| Maybe.withDefault cell.y <| List.minimum <| List.map .y col
        yMax = Basics.max cell.y <| Maybe.withDefault cell.y <| List.maximum <| List.map .y col
        yLow = Debug.log "above" <| List.filter (\j -> (cell.y - j + 1) ==
                                                     List.length (List.filter (\p -> j <= p.y && p.y <= cell.y) col)
                                                ) <| List.range yMin cell.y
        yHigh = Debug.log "below" <| List.filter (\j -> (j - cell.y + 1) ==
                                                      List.length (List.filter (\p -> cell.y <= p.y && p.y <= j) col)
                                                 ) <| List.range cell.y yMax
        vRanges = Debug.log "range" <| List.concat <| List.map (\start -> List.map (\end ->  List.range start end) yHigh) yLow
        vWordAt : List Int -> List String
        vWordAt range =
            find (String.concat <| List.map .c <| List.concat <|
                             List.map (\y -> List.filter (\p -> p.y == y) col) range)
        vWords = Debug.log "vertical" <|
                 List.foldl (\range cells -> cells++(vWordAt range))
                     [] vRanges
    in
        (hWords++vWords)
            
pieceView : Piece -> Model -> Svg Msg
pieceView piece model =
    let
        dx = case model.moving of
                  Nothing -> 0
                  Just id ->
                    if id == piece.id then
                      model.nowAt.x - model.startedAt.x
                    else
                      0
        dy = case model.moving of
                  Nothing -> 0
                  Just id ->
                    if id == piece.id then
                      model.nowAt.y - model.startedAt.y
                    else
                      0
        dstring = "translate(" ++
                  (String.fromFloat ((toFloat (piece.x*unit)) + dx)) ++
                  ", " ++
                  (String.fromFloat ((toFloat (piece.y*unit)) + dy)) ++ ")"
    in
        Svg.g [ transform dstring
              , P.onDown (\event -> PDown piece.id
                              { x=Tuple.first event.pointer.offsetPos
                              , y=Tuple.second event.pointer.offsetPos
                              }
                         )
              ]
            [ rect [ width (String.fromInt unit)
                   , height (String.fromInt unit)
                   , fill "gray"
                   , fillOpacity "0.3"
                   , stroke "black"
                   ]
                  []
            , text_ [ x (String.fromInt (unit//4))
                    , y (String.fromInt (2*unit//3))
                    , fontSize (String.fromInt (unit//2))
                    , stroke "black"
                    ]
                  [text piece.c]
            ]

cellView : Cell -> Svg Msg
cellView cell =
    rect [x (String.fromInt (unit*cell.x))
         ,y (String.fromInt (unit*cell.y))
         ,width (String.fromInt unit)
         ,height (String.fromInt unit)
         ,stroke "black"
         ,fill "none"
         ][]

boardView : Model -> Svg Msg
boardView model =
    g[] <| (List.map cellView model.board) ++
        [animatedCover (coverAnimation model.newWordsAt)
             []
             [rect
                  [ width (String.fromInt unit)
                  , height (String.fromInt unit)
                  , fill "yellow"
                  , fillOpacity "0.3"
                  , stroke "red"
                  , strokeWidth "5px"
                  ]
                  []
             ]

        ]



candView : Model -> Svg Msg
candView model =
    let
        letterView : Int -> String -> Svg Msg
        letterView i c =
            g [ transform ("translate (0" ++
                                           "," ++ (String.fromInt ((i+model.cursor)*unit)) ++ ")")
              , Svg.Attributes.clipPath "url(#candClip)"
              , P.onDown (\event -> (GenPiece (Piece (List.length model.conf) 1 (i+model.cursor+1) c True))
                              { x=Tuple.first event.pointer.offsetPos
                              , y=Tuple.second event.pointer.offsetPos
                              }
                         )
              ]
            [rect [ width (String.fromInt unit)
                  , height (String.fromInt unit)
                  , fill "gray"
                  , fillOpacity "0.3"
                  , stroke "black"
                  ,Svg.Attributes.clipPath "url(#candClip)"
                  ]
                 []
            , text_ [ x (String.fromInt (unit//4))
                    , y (String.fromInt (2*unit//3))
                    , fontSize (String.fromInt (unit//2))
                    , stroke "black"
                    , Svg.Attributes.clipPath "url(#candClip)"                                     
                    ]
                 [text c]
            ]
    in
        g[transform ("translate(60," ++ (String.fromInt unit) ++ ")")]
            [g [ Svg.Attributes.clipPath "url(#candClip)"
               ]
                 (List.indexedMap letterView model.candidates)
            ,Svg.path [d ("M 0 -5 l " ++
                              (String.fromInt (unit)) ++ " 0 l " ++
                              (String.fromInt (-unit//2)) ++ " " ++
                              (String.fromInt (-unit//2)) ++ " z"
                         )
                      , fill "black"
                      , onClick CursorPlus
                      ]
                 []
            ,Svg.path [d ("M 0 "++ (String.fromInt (8*unit+5)) ++" l " ++
                              (String.fromInt (unit)) ++ " 0 l " ++
                              (String.fromInt (-unit//2)) ++ " " ++
                              (String.fromInt (unit//2)) ++ " z"
                         )
                      , fill "black"
                      , onClick CursorMinus
                      ]
                 []
                
            ]
                 

                     
view : Model -> Html Msg
view model =
    Html.div []
        [ svg [ width "600"
              , height "600"
              , P.onMove (\event -> PMove
                              { x=Tuple.first event.pointer.offsetPos
                              , y=Tuple.second event.pointer.offsetPos
                              }
                         )
              , P.onUp (\event -> PUp
                                { x=Tuple.first event.pointer.offsetPos
                                , y=Tuple.second event.pointer.offsetPos
                                }
                          )
              ]
              (
               [boardView model] ++
                   [Svg.clipPath [id "candClip"]
                        [ rect [x "0"
                               ,y "0"
                               ,width (String.fromInt unit)
                               ,height (String.fromInt (8*unit))
                               ][]
                        , Svg.path [d "M 0 0 l 100 0 l 0 480 l -100 0 Z"][]
                        ]
                   ] ++                       
                   [candView model] ++
                   (List.map (\p -> pieceView p model) model.conf)
              )
        , Html.ul [] (List.map (\w -> Html.li [][Html.text w]) model.foundWords)
        ]

fade : Animation
fade =
    Animation.steps
        { startAt = [Prop.scale 0
                    ,Prop.opacity 0]
        , options = []
        }
    [Animation.step 1000 [Prop.scale 1
                        ,Prop.opacity 1]
    ,Animation.step 1000 [Prop.scale 0
                        ,Prop.opacity 0]
    ]
coverAnimation : List (List Cell) -> Animation
coverAnimation ranges =
    let
        xMin : List Cell -> Int
        xMin range =
            (Maybe.withDefault 0 <| List.minimum <| List.map .x range)
        yMin : List Cell -> Int
        yMin range =
            (Maybe.withDefault 0 <| List.minimum <| List.map .y range)
        xMax : List Cell -> Int
        xMax range =
            (Maybe.withDefault 0 <| List.maximum <| List.map .x range)
        yMax : List Cell -> Int
        yMax range =
            (Maybe.withDefault 0 <| List.maximum <| List.map .y range)
        oneStep range =
            Animation.step 500
            [Prop.x (toFloat (unit*(xMin range)))
            ,Prop.y (toFloat (unit*(yMin range)))
            ,Prop.scaleXY
                 (toFloat ((xMax range) - (xMin range) + 1))
                 (toFloat ((yMax range) - (yMin range) + 1))
            ]
    in
        Animation.steps
            { startAt = [Prop.scale 0
                        ,Prop.opacity 0]
            , options = []
            }<|
            (List.map oneStep ranges) ++ [Animation.step 500 [Prop.scale 0]]
    
animatedSvg =
    Simple.Animation.Animated.svg
        { class = Svg.Attributes.class
        }
        
animatedCover : Animation -> List (Svg.Attribute Msg) -> List (Svg Msg) -> Svg Msg
animatedCover =
    animatedSvg Svg.g

        
        
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
