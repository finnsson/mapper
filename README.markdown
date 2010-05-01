= README =

Web-Mapper is a web service layer consisting or three important parts:

== MapperInputter ==

MapperInputter is a type class that takes a Hack.Env and returns a MapperInput.
Example of MapperInputter is RestfulParser.
Is the MapperInput is OK its DataInput is passed on to a MapperOutputter.

== MapperOutputter ==

MapperOutputter is a type class that takes a DataInput and returns a MapperOutput.
Example of MapperOutputter is RuntimeDbMapper found in the package Web-Mapper-Db.
 
== serializeToXml and serializeToJson ==

Finally you got the functions serializeToXml and serializeToJson that can serialize a MapperOutput.
