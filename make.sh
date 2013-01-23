cp -r _site/.git /tmp
ghc --make hakyll.hs
./hakyll rebuild > /dev/null
mv /tmp/.git/ _site/ 
