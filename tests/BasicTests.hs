
-- Do not add a module declaration or it will fail to compile

import qualified Controller as C
import Control.Exception            ( bracket_         )
import Control.Monad                ( join             )
import System.Directory             ( copyFile
                                    , listDirectory
                                    , removeFile       )
import Test.Hspec                   ( Spec (..)
                                    , around_
                                    , describe
                                    , hspec
                                    , it
                                    , shouldBe         )

main :: IO ()
main = hspec $ around_ manageIOTests $ do
    describe "hplcToCsv converts to csv" $
        mockConversion

mockConversion :: Spec
mockConversion = it "basic conversion test" action
    where action = do C.routeArg "tests/testing/Beckman1.dat.asc"
                      result   <- readFile "tests/testing/Beckman1.dat.csv"
                      expected <- readFile "tests/files/Beckman1.dat.csv"
                      result `shouldBe` expected


manageIOTests :: IO () -> IO ()
manageIOTests = bracket_ setupFiles tearDown
    where srcDir     = "tests/files/"
          testDir    = "tests/testing/"
          tearDown   = join $ mapM_ ( removeFile . (testDir ++ ) )
                       <$> listDirectory testDir
          setupFiles = copyFile ( srcDir  ++ "Beckman1.dat.asc" )
                                ( testDir ++ "Beckman1.dat.asc" )
