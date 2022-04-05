#!/bin/bash

header=$1

source=${header/.hh/.C}

makefile=test/Makefile.am


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
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef $define
#define $define

#include <cppunit/extensions/HelperMacros.h>

class $className : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( $className );
  CPPUNIT_TEST( testOne );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testOne();

private:

};

#endif // $define
EOF

cat > $source <<EOF
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "$(basename $header)"

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( $className );

void $className::setUp() 
{

}

void $className::tearDown() 
{

}

void $className::testOne()
{

}
EOF

sed -e "/libStJetMakerTest_la_SOURCES/,/[^\]$/ {
/[^\]$/a\	$(basename $source) $(basename $header)
s,\([^\]\)$,\1 \\\,
}" $makefile > ${makefile}_
mv -f ${makefile}_ $makefile
