module Web.Mapper.MapperSerializer where

import Web.Mapper.Mapper
import Text.XML.Light

serializeToXml :: MapperOutput -> String
serializeToXml (MapperOutputError msg) = showElement $ element "error" msg
serializeToXml (MapperOutput xs) = showElement $ elementFold "view" serializeColumn xs

serializeColumn  = elementFold "row" $ uncurry element 

elementFold name f  = element' name . map (Elem . f) 

element k v = element' k [Text $ CData CDataText v Nothing] 

element' k v = Element (QName k Nothing Nothing) [] v Nothing
