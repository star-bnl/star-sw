// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include <StjTrackListToStMuTrackFourVecList.h>
#include <StMuTrackFourVec.h>

#include "StjTrackListToStMuTrackFourVecListTest.hh"

using namespace std;
using namespace StSpinJet;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( TrackListToStMuTrackFourVecListTest );

void TrackListToStMuTrackFourVecListTest::setUp() 
{

}

void TrackListToStMuTrackFourVecListTest::tearDown() 
{

}

void TrackListToStMuTrackFourVecListTest::testOne() 
{
  TrackListToStMuTrackFourVecList ttof;
  TrackList tlist;

  Track track;
  track.pt  = 0.7650294;
  track.eta = 1.0071259;
  track.phi = 1.1558674;
  tlist.push_back(track);

  FourList flist = ttof(tlist);
  CPPUNIT_ASSERT_EQUAL( (size_t)1, flist.size());
  StMuTrackFourVec *p4 = (StMuTrackFourVec*)flist[0];
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.7650294 , p4->pt()   , 1e-5);
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0071259 , p4->eta()  , 1e-5);
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.1558674 , p4->phi()  , 1e-5);
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.1395700 , p4->mass() , 1e-5);
 
}

