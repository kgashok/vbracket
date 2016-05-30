module BracketModel exposing (..) -- where 

import SStack as Stack exposing (..)
import Dict exposing (fromList, get)
-- import List.Extra as Listx exposing (find)


-- MODEL

type alias BPair = {
  opener: Char, 
  closer: Char,      -- corresponding closer
  isEnabled: Bool,   -- is the bracket pair enabled?
  id: Int
}

type alias BMap = List BPair

type alias Model =
  {
    expression: String, -- what the user inputs
    stack: SStack,      -- integral ADT required for validation 
    bmap: BMap,         -- containing list of bracket pairs
    isBalanced: Bool,   -- intermediary result
    isValid: Bool,      -- the ultimate outcome! 
    showBracket: Bool,
    showStack: Bool
  }



---- Validator Related 

updateE : String -> Model -> Model
updateE e rec = { rec|expression = e }

updateS : SStack -> Model -> Model
updateS s rec = { rec|stack = s }




isOpenr: Char -> List BPair -> Bool 
isOpenr o bmap = 
  bmap 
    |> List.filter .isEnabled
    |> List.map .opener
    |> List.member o
 

isClosr: Char -> List BPair -> Bool 
isClosr c bmap = 
  bmap 
    |> List.filter .isEnabled
    |> List.map .closer
    |> List.member c 


matchEnabledOpenr: Char -> BPair -> Maybe Char
matchEnabledOpenr o bp = 
  case (bp.isEnabled, bp.opener == o) of 
    (True, True) -> Just bp.closer 
    _ -> Nothing 


-- Four variants of the Closure function 
getClosr: Char -> List BPair -> Maybe Char 
getClosr o bm =  
  List.head (List.filterMap (matchEnabledOpenr o) bm )


getClosr2 : Char -> List BPair -> Maybe Char 
getClosr2 o bmap = 
  let 
    getPair {opener, closer, isEnabled} = 
      case isEnabled of 
        True -> 
          (opener, closer)
        False ->
          ('\0', '\0')
  in 
    bmap 
      |> List.map getPair  
      |> Dict.fromList 
      |> Dict.get o


matchEnabledOpenrX : Char -> BPair -> Bool
matchEnabledOpenrX o bp = 
  bp.isEnabled && bp.opener == o 


{- Not able to compile with transition to elm 0.17 
getClosr3 : Char -> List BPair -> Maybe Char 
getClosr3 opener bmap = 
  bmap 
    |> Listx.find (matchEnabledOpenrX opener)
    |> Maybe.map .closer 
-}
  
getClosr4 : Char -> List BPair -> Maybe Char 
getClosr4 o bmap = 
  bmap 
    |> List.filter (matchEnabledOpenrX o)
    |> List.map .opener
    |> List.head 
    


validate: Model -> Model 
validate model = 
  let 
    {expression, stack, bmap} = model 
  in 
    case (pop expression) of 
      Nothing -> 
        {model| isBalanced = Stack.isEmpty stack }

      Just (tok, restExpr) -> 
        case (getClosr tok bmap) of 
          Just (closer) -> 
            model
              |> updateE restExpr |> updateS (pushC closer stack) 
              |> validate 
          Nothing -> 
            if (isClosr tok bmap) == True then
              case (pop stack) of 
                Just (ts, restOfStack) -> 
                  if ts == tok  then
                    model 
                      |> updateE restExpr |> updateS restOfStack 
                      |> validate
                  else 
                    { model| isValid = False} 
                Nothing ->
                  { model| isBalanced = False} 
            else
              validate {model |expression = restExpr}


      
validateString: Model -> Model
validateString model  = 
  let 
    res = validate model  
    --_= Debug.watch "Result " (res.isValid, res.stack, res.expression)
  in 
    res

isValid : Model -> String 
isValid bm = 
  let 
    {expression, stack, isValid, isBalanced} = bm
  in 
    case (isBalanced, isValid) of 
      (True, True)   -> " is valid"
      (False, _)     -> " is imbalanced"
      (_, False)     -> " is invalid"



-- Constructor function for creating new pairs 
newPair : Char -> Char -> Bool -> Int -> BPair
newPair op cl en id =
  { opener = op,
    closer = cl,
    isEnabled = en,
    id = id
  }


initialModel : Model
initialModel =
  { stack      = Stack.empty,
    isBalanced = True, 
    isValid    = True,
    
    expression = "", 
    
    bmap = 
      [
        newPair '(' ')' True 1,
        newPair '{' '}' True 2, 
        newPair '<' '>' True 3
      ], 
    showStack = True,
    showBracket = True
  }