#!/bin/bash

header=$1

source=${header/.h/.cxx}

className=$(basename $header)
className=${className%.*}

if [ -e $header ]; then
    echo "$header exists!!"
    exit
fi

if [ -e $source ]; then
    echo "$source exists!!"
    exit
fi

define=$(echo $(basename $header) | tr [a-z] [A-Z] | sed -e "s/\./_/g")

cat > $header <<EOF
// -*- mode: c++;-*-
// \$Id: createNewClass.sh,v 1.1 2008/08/10 23:04:48 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef $define
#define $define

class $className {

public:
  $className() { }
  virtual ~$className() { }

private:

};

#endif // $define
EOF

cat > $source <<EOF
// \$Id: createNewClass.sh,v 1.1 2008/08/10 23:04:48 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "$(basename $header)"
EOF
