#! /bin/sh

mkdir gutenberg_temp
cd gutenberg_temp
filename=$(basename $1)
wget -p -k -nd $1
for file in *.gif
do
    base="${file%.*}"
    convert $file $base.png
done
sed -i 's/\.gif/\.png/' $filename
iconv $filename -f iso-8859-1 -t utf-8 -o temp$filename
pandoc --latex-engine=xelatex temp$filename -o ../$2.pdf
cd ..
rm -rf gutenberg_temp
