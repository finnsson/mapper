module Web.Mapper.MapperSerializer (serializeToXml, serializeToJson) where

import Web.Mapper.Mapper
import Text.XML.Light

import Text.JSON
import Text.JSON.Types
import Text.JSON.Pretty

-- General

serializeToX :: (a -> String) -> (String -> String -> a) -> (String -> [a] -> a) -> MapperOutput -> String
serializeToX disp xKV xArray (MapperOutputError msg) = disp $ xKV "error" msg
serializeToX disp xKV xArray (MapperOutput xs) = disp $ xFold xArray "view" (serializeColumnX xKV xArray) xs
serializeToX disp xKV xArray (MapperOutputMeta m) = disp $ serializeMetaToX xKV xArray m

serializeMetaToX :: (String -> String -> a) -> (String -> [a] -> a) -> MetaInfo -> a
serializeMetaToX xKV xArray mi =serializeTypeX xKV xArray mi
-- xArray "meta" $ return $  
--
serializeColumnX xKV xArray = xFold xArray "row" $ uncurry xKV

serializeTypeX :: (String -> String -> a) -> (String -> [a] -> a) -> MetaInfo -> a
serializeTypeX xKV xArray (MetaInfo tis fis) = xArray "meta" $ (map serializeTypeInfo tis) ++ (map serializeFuncInfo fis) 
  where
    -- serializeTypeInfo :: TypeInfo -> a
    serializeTypeInfo (TableInfo name ns cis pts) = xArray "table" $
      [xKV "name" name] ++ [xKV "namespace" ns] ++ 
      [xArray "columns" $ map serializeColumnInfo cis] ++
      [xArray "priviliges" $ map serializePrivilige pts]
    serializeTypeInfo (PrimInfo name ns) = xArray "prim" $ [xKV "name" name] ++ [xKV "namespace" ns]

    serializeColumnInfo (ColumnInfo name ti) = xArray name $ [serializeTypeInfo ti]
    serializePrivilige (pt,b) = xKV (show pt) (show b)

    serializeFuncInfo (ProcInfo returnType name ns args com) =
      xArray "proc" $ 
      [xKV "name" name] ++
      [xKV "namespace" ns] ++
      (maybe [] (\a -> [xKV "comment" a]) com) ++
      [xArray "return" [ serializeTypeInfo returnType ]] ++
      [xArray "arguments" (map (\ti -> xArray (fst ti) [serializeTypeInfo $ snd ti]) args)]

      
xFold :: (String -> [a] -> a) -> String -> (b -> a) -> [b] -> a
xFold xArray name f xs = xArray name $ map f xs

-- Versions

serializeToXml = serializeToX showElement xmlKV xmlArray
serializeToJson = serializeToX (show . pp_value) jKV jArray

-- XML

xmlKV k v = element' k [Text $ CData CDataText v Nothing] 

xmlArray k v = element' k (map Elem v)

element' k v = Element (QName k Nothing Nothing) [] v Nothing

-- JSON

jKV k v = jObject [(k,jString v)]
  where
    jString = JSString . JSONString

jArray n o = jObject [(n,JSArray o)]

jObject = JSObject . JSONObject
