#!/bin/csh
#
mkdir tmp
#
foreach f (*.html) 
	echo '######################## ' $f
	sed -e \
	's/img src="/img src="http:\/\/iago.lbl.gov\/images\/idldoc\//'\
	$f > tmp/$f
end
#
tar cvf .orig.tar *.html
cd tmp
mv *.* ../
cd ../
rmdir tmp
#
