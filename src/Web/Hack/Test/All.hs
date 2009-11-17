module Web.Hack.Test.All where 

import Test.Framework (defaultMain)

-- Modules to test
import Web.Hack.Test.RuntimeDbMapperTest (runtimeDbMapperTests)
import Web.Hack.Test.MapperRestfulTest (mapperRestfulTests)

main = defaultMain [runtimeDbMapperTests, mapperRestfulTests]
