#! /bin/tcsh -f
#sed -e 's/\^\[\[33\[0m//g' -e 's/\\033\[31m//g' -e 's/\\033\[32m"//g' -e 's/\\033\[33m//g' -e 's/\\033\[34m//g' -e 's/\\033\[35m//g' ${1} > ${1}.clean
set b = `basename ${1}`;
grep -v StageID ${1} | \
sed \
-e 's/\\[0m//g' \
-e 's/\\[31m//g' \
-e 's/\\[32m//g' \
-e 's/\\[33m//g' \
-e 's/\\[34m//g' \
-e 's/\\[35m//g' \
 > ${b}.clean



