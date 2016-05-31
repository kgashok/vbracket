module Bracket exposing (..) -- where 

import Html exposing (..)
import Html.Events exposing (on, onInput, targetValue, onClick)
import Html.Attributes exposing (..)
-- import Signal exposing (Address)
-- import StartApp.Simple as StartApp
import Html.App as Html

import String exposing (..)


import SStack as Stack exposing (..)
--import BingoUtils as Utils 
import Version exposing (version, gitRepo)

import Mouse
import Keyboard

import BracketModel exposing (Model, BPair, validate, isValid, validateString, initialModel)



-- UPDATE (aka CONTROL)

-- type Action
type Msg 
  = NoOp
  | UpdateExpression String
  | Mark Int
  | MouseMsg Mouse.Position
  | KeyMsg Keyboard.KeyCode


-- update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)
  
    UpdateExpression contents ->
      ({ model | expression = contents }, Cmd.none)
    Mark id ->
      let
        updateEntry e =
          if e.id == id then { e | isEnabled = (not e.isEnabled) } else e
      in
        ({ model | bmap = List.map updateEntry model.bmap }, Cmd.none)

    MouseMsg position ->
        let 
          {x, y} = position
          onHeader = (x < 800) && (y <60)
        in  
          case (onHeader) of 
            True -> 
              ({ model | showBracket = (not model.showBracket), 
                         showStack   = (not model.showStack)}, Cmd.none)
            False -> 
              (model, Cmd.none)

        
    KeyMsg code ->
      case code of 
        2 ->  -- Ctrl-b
          ({model | showBracket = (not model.showBracket) }, Cmd.none)
        17 -> -- Ctrl-q
          ({model | showStack = (not model.showStack)}, Cmd.none)
        _ ->  
          (model, Cmd.none)




-- VIEW 


stackItem : (Int, Char) -> Html Msg
stackItem (index, token) = 
  li []
    --[ classList [ ("highlight", entry.isEnabled) ],
    --  onClick address (Mark entry.id)
    --]
    [ span [ class "index" ] [ text (toString index) ],
      span [ class "token" ] [ text (String.fromChar token) ]
      -- span [][text "   - closer"] 
      -- button [ class "delete", onClick address (Delete entry.id) ] []
    ]


entryItem : BPair -> Html Msg
entryItem entry =
  li   
    [ classList [ ("highlight", entry.isEnabled) ],
      onClick (Mark entry.id)
    ]
    [ span [ class "phrase" ] [ text (String.fromChar entry.opener) ],
      span [ class "points" ] [ text (String.fromChar entry.closer) ]
      -- span [][text "   - closer"] 
      -- button [ class "delete", onClick (Delete entry.id) ] []
    ]
      
entryForm : Model -> Html Msg
entryForm model =
  let
    res = validateString model
  in
    div [ ] -- id "first" ]
      [ input
          [ type' "text",
            placeholder "{( () )}",
            value model.expression,
            name "phrase",
            autofocus True,
            onInput UpdateExpression,
            -- Utils.onInput UpdateExpression,
            strStyle
          ]
          [ ],
        --button [ class "change" ] [ text "Change" ],
        h2
          [ revStyle]
          [ text (model.expression ++ isValid res) ],
        stackHeader model.showStack res.stack,
        stackList model.showStack res.stack 
      ]

isStackEmpty : SStack -> String
isStackEmpty s = 
  if String.length s == 0 then 
    "Empty"
  else
    ""


getIndexedCharacters : String -> List (Int, Char)
getIndexedCharacters =
  List.indexedMap (,) << String.toList

stackList : Bool -> SStack -> Html Msg
stackList display stack = 
  let
    entryItems = 
      String.reverse stack 
        |> getIndexedCharacters 
        |> List.reverse
    items = 
      if display then 
        List.map stackItem (entryItems ++ [(-1, '-')] )
      else 
        []

  in
    div [ ] 
    [
      ul [ ] items,
      footer
        [ ] [a [href (gitRepo ++ "/issues/new"), 
                target "_blank", 
                rel "noopener noreferrer"] 
            [text version] ]
    ]

entryList : Bool -> List BPair -> Html Msg
entryList display entries =
  let
    entryItems = 
      if display then List.map entryItem entries else []
  in
    --ul [ bracStyle] items
     ul [ ] entryItems

-- view : Address Action -> Model -> Html
view : Model -> Html Msg
view model =
  
  div 
    [ id "container" ]
    [ pageHeader,

      div [id "wrapper"] 
      [ div [id "first"] 
          [ entryForm model ],

        div [id "second"]
          [ bracketHeader model.showBracket,
            entryList model.showBracket model.bmap,
            pageFooter ]
      ]

    ]





-- WIRE IT ALL TOGETHER!

init : (Model, Cmd Msg)
init =
  (initialModel , Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Mouse.clicks MouseMsg
    , Keyboard.presses KeyMsg
    ]

main : Program Never
main =
  Html.program
    { init = init, update = update, view = view, subscriptions = subscriptions}
  -- Html.beginnerProgram
    -- { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


title : String -> Int -> Html Msg
title message times =
  message ++ " "
    |> toUpper
    |> repeat times
    |> trimRight
    |> text


pageHeader : Html Msg
pageHeader =
  h1 [ ] [ title "Validator" 1 ]


bracketHeader : Bool -> Html Msg
bracketHeader display =
  if display then   
    h2 [ ] [ title "Bracket Map" 1 ]
  else 
    h2 [ ] [ ] 

stackHeader : Bool -> SStack -> Html Msg         
stackHeader display stack =
  if display then 
    h3 [ ] [text ( "Stack " ++ (isStackEmpty stack) )]
  else 
    h3 [ ] [ ]

pageFooter : Html Msg
pageFooter =
  div [] 
    [ footer [ ] 
      [ a [ href "http://j.mp/reactiveNotFad", target "_blank", rel "noopener noreferrer" ] 
      [ text "Reactive is not a fad" ] ],    
      footer [ ] 
      [ a [ href "http://j.mp/relevantKG", target "_blank", rel "noopener noreferrer" ] 
      [ text "Stay relevant via KG" ] ],    
      footer [ ] 
      [ a [ href "http://edu.kgisl.com", target "_blank", rel "noopener noreferrer" ] 
      [ text "The Campus Inside" ] ]
    ]


strStyle : Attribute x
strStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
    
revStyle : Attribute x
revStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    , ("color", "red")
    ]

bracStyle : Attribute x
bracStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2")
    , ("text-align", "center")
    , ("color", "#f60")
    ]
