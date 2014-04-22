module PrettyJSON (renderJValue) where 

import Prettify (Doc, (<>), double, series, string, text)
import SimpleJSON (JValue(..))

renderJValue :: JValue -> Doc
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
renderJValue (JArray arr) = series '[' ']' renderJValue arr
renderJValue (JObject obj) = series '{' '}' field obj
    where field (name, val) = string name
                              <> text ": "
                              <> renderJValue val
