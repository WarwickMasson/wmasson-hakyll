cp -r _site/.git /tmp
ghc --make hakyll.hs
./hakyll rebuild
mv /tmp/.git/ _site/ 
