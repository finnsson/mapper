module Web.Mapper.Test.All where 

import Test.Framework (defaultMain)

-- Modules to test
import Web.Mapper.Test.RuntimeDbMapperTest (runtimeDbMapperTests)
import Web.Mapper.Test.MapperRestfulTest (mapperRestfulTests)

main = defaultMain [runtimeDbMapperTests, mapperRestfulTests]
