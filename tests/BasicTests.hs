
-- Do not add a module declaration or it will fail to compile

import qualified Controller as C
import qualified Model      as M
import Control.Exception            ( bracket_                  )
import Control.Monad                ( when                      )
import System.Directory             ( copyFile
                                    , createDirectory
                                    , doesDirectoryExist
                                    , removeDirectoryRecursive
                                    , listDirectory             )
import Test.Hspec                   ( Spec (..)
                                    , around_
                                    , describe
                                    , hspec
                                    , it
                                    , shouldBe                  )

main :: IO ()
main = hspec $ around_ manageIOTests $ do
    describe "hplcToCsv converts Beckman output to csv" $
        mockConversion "Beckman1.dat.asc" "Beckman1.dat.csv"

srcDirectory, testDirectory :: FilePath
srcDirectory  = "tests/files/"
testDirectory = "tests/testing/"

mockConversion :: FilePath -> FilePath -> Spec
mockConversion source target = it "basic conversion test" action
    where action = do fp <- setupSrcFile source
                      C.convert fp
                      result   <- readFile . M.changeName $ fp
                      expected <- readFile ( srcDirectory ++ target )
                      result `shouldBe` expected

setupSrcFile :: FilePath -> IO FilePath
setupSrcFile name = do
    let fromPath = srcDirectory  ++ name
        toPath   = testDirectory ++ name
    copyFile fromPath toPath
    pure toPath

manageIOTests :: IO () -> IO ()
manageIOTests = bracket_ setup tearDown
    where setup    = tearDown >> createDirectory testDirectory
          tearDown = do exists <- doesDirectoryExist testDirectory
                        when exists $ removeDirectoryRecursive testDirectory
