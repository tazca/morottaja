#!/bin/sh
rm -f pages/*.jpg
rm -f pages/*.png
./morottaja
cd pages
cat ../pnrTemplate | xargs -i convert page_{}.jpg page_{}_overlay.png -flatten -background white -alpha remove -alpha off unalphad_page_{}.png &> /dev/null
# rm -f *.jpg
img2pdf --output ../aamulehti.pdf unalphad_page_*
# rm -f *.png
