#! /usr/local/bin/tcsh -f
set tag = $1
#if (! $tag) set tag = "y2015a";

set clobber

echo "Generate in new library"
cat <<EOF > a.C
{
gROOT->LoadMacro("testGeom.C");
testGeom(0,"sdt20150216");
AgPosition::SetReal();
construct("$tag");
gGeoManager->Export("${tag}_1.C");
}
EOF
root -q -b a.C > a.log
sed '/_[a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9]/d' ${tag}_1.C | sed 's/-0.000000/0.000000/' | sed 's/360.000000/0.000000/' > ${tag}_1

grep 'Could not find configuration' a.log


echo "Move new library out of the way"
mv .$STAR_HOST_SYS/ $STAR_HOST_SYS
#starver dev
ln -s DEV/.$STAR_HOST_SYS .

echo "Generate in old library"
cat <<EOF > b.C
{
gROOT->LoadMacro("testGeom.C");
testGeom("$tag");
gGeoManager->Export("${tag}_2.C");
}
EOF
root -q -b b.C > b.log
sed '/_[a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9]/d' ${tag}_2.C | sed 's/-0.000000/0.000000/' | sed 's/360.000000/0.000000/' > ${tag}_2
rm .$STAR_HOST_SYS

# Restore
mv $STAR_HOST_SYS .$STAR_HOST_SYS

diff ${tag}_1 ${tag}_2 | grep -v Medium

