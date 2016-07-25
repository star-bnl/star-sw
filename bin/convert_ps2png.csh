set b = `basename $1 .ps`
convert -density 300 -geometry 100% ${b}.ps ${b}.png
