files=`git diff --name-only | grep -E '.hs$'`
echo $files
# cp ./src/*.hs ../futhark/src/Futhark/LSP
