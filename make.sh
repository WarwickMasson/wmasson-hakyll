cp -r _site/.git /tmp
ghc --make site.hs
./site rebuild
mv /tmp/.git/ _site/ 
