#!/bin/bash

header=$1

classname=$(basename $header)
classname=${classname%.*}

tobject=$(cat $header | sed -n -e "/ClassDef/p")
echo $tobject

# inherited=$(cat $header | sed -n -e "/^class $classname : public/p")
# if [ -n "$inherited" ]; then
#     sed -e  "/^};$/i\ \ ClassDef($classname, 1)\\
# " $header > ${header}_
# mv -f ${header}_ $header
# else
# sed -e "/#define/a\\
# \\
# #include <TObject.h>" \
# -e "/^class $classname {/s/{/: public TObject {/" \
# -e  "/^};$/i\ \ ClassDef($classname, 1)\\
# " $header > ${header}_
# mv -f ${header}_ $header
# fi
# 
# 
# source=${header%.*}.cxx
# 
# if [ -e $source ]; then
#     sed -e "3a\\
# \\
# ClassImp($classname)" $source > ${source}_
#     mv -f ${source}_ $source
# else
#     cat > $source << EOF
# // \$Id: makeTObject.sh,v 1.1 2008/08/03 22:04:26 tai Exp $
# // Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
# #include "$(basename $header)"
# 
# ClassImp($classname)
#  
# EOF
# fi
