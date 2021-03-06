module Web.Mapper.Test.All where 

import Test.Framework (defaultMain)

-- Modules to test
import Web.Mapper.Test.MapperRestfulTest (mapperRestfulTests)
import Web.Mapper.Test.MapperSerializerTest (mapperSerializerTest)

main = defaultMain [mapperRestfulTests, mapperSerializerTest]
