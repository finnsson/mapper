module Web.Hack.MapperSerializer where

import Web.Hack.Mapper
import Text.XML.Light

serializeToXml :: MapperOutput -> String
serializeToXml (MapperOutputError msg) = showElement $ element "error" msg
serializeToXml (MapperOutput xs) = showElement $ elementFold "view" serializeColumn xs

serializeColumn xs = elementFold "row" serializeKV xs

serializeKV (k,v) = element k v

elementFold name f xs = element' name $ map (Elem . f) xs

element k v = Element (QName k Nothing Nothing) [] [Text $ CData CDataText v Nothing] Nothing

element' k v = Element (QName k Nothing Nothing) [] v Nothing
