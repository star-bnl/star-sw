#! /usr/local/bin/tcsh -f
set tag = $1
#if (! $tag) set tag = "y2015a";

set clobber

cat <<EOF > /tmp/test_1.C
{
 gROOT->LoadMacro("StarVMC/Geometry/macros/testGeom.C");
 testGeom("$tag");
 gGeoManager->Export("${tag}_1.C");
}
EOF
root4star -q -b /tmp/test_1.C > /tmp/a.log
sed '/_[a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9]/d' ${tag}_1.C | sed 's/-0.000000/0.000000/' | sed 's/360.000000/0.000000/' > ${tag}_1

mv .${STAR_HOST_SYS} ${STAR_HOST_SYS}

cat <<EOF > /tmp/test_2.C
{
 gROOT->LoadMacro("StarVMC/Geometry/macros/testGeom.C");
 testGeom("$tag");
 gGeoManager->Export("${tag}_2.C");
}
EOF
root4star -q -b /tmp/test_2.C > /tmp/a.log
sed '/_[a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9]/d' ${tag}_2.C | sed 's/-0.000000/0.000000/' | sed 's/360.000000/0.000000/' > ${tag}_2

mv ${STAR_HOST_SYS} .${STAR_HOST_SYS}

gvimdiff ${tag}_1 ${tag}_2 


#echo "Generate in old library"
#cat <<EOF > b.C
#{
#gROOT->LoadMacro("StarVMC/Geometry/macros/testGeom.C");
#testGeom("$tag");
#gGeoManager->Export("${tag}_2.C");
#}
#EOF
#root4star -q -b b.C > b.log
#sed '/_[a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9]/d' ${tag}_2.C | sed 's/-0.000000/0.000000/' | sed 's/360.000000/0.000000/' > ${tag}_2
#rm .$STAR_HOST_SYS
#
## Restore
#mv $STAR_HOST_SYS .$STAR_HOST_SYS
#


