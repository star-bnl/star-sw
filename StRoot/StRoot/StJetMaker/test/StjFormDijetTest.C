// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include <StjJetList.h>

#include <StjFormDijet.h>

#include <StjDijetList.h>

#include "StjFormDijetTest.hh"

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StjFormDijetTest );

void StjFormDijetTest::setUp() 
{

}

void StjFormDijetTest::tearDown() 
{

}

void StjFormDijetTest::testOneJet()
{
  StjJetList jetlist;

  StjJet p1;
  p1.pt = 3.0;
  p1.eta = 0.5;
  jetlist.push_back(p1);

  StjFormDijet formDijet;

  StjDijetList dijetList = formDijet(jetlist);

  CPPUNIT_ASSERT_EQUAL( (size_t)0, dijetList.size());
}

void StjFormDijetTest::testOne()
{
  StjJetList jetlist;

  StjJet p1;
  p1.pt = 3.0;
  p1.eta = 0.5;
  jetlist.push_back(p1);

  StjJet p2;
  p2.pt = 1.0;
  p2.eta = 1.0;
  jetlist.push_back(p2);

  StjJet p3;
  p3.pt = 4.8;
  p3.eta = 0.7;
  jetlist.push_back(p3);

  StjFormDijet formDijet;

  StjDijetList dijetList = formDijet(jetlist);

  CPPUNIT_ASSERT_EQUAL( (size_t)1, dijetList.size());
  CPPUNIT_ASSERT_EQUAL( p3, dijetList[0].jet3);
  CPPUNIT_ASSERT_EQUAL( p1, dijetList[0].jet4);

}
