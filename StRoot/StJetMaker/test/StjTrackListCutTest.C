// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include <StjTrackList.h>

#include <StjTrackListCut.h>

#include <StjTrackCutDca.h>
#include <StjTrackCutDcaPtDependent.h>
#include <StjTrackCutEta.h>
#include <StjTrackCutPossibleHitRatio.h>
#include <StjTrackCutNHits.h>
#include <StjTrackCutFlag.h>

#include <iostream>
#include <set>
#include <cmath>

#include "StjTrackListCutTest.hh"

using namespace std;
using namespace StSpinJet;
using namespace StJetTrackCut;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StjTrackListCutTest );

void StjTrackListCutTest::setUp()
{

}

void StjTrackListCutTest::tearDown()
{

}


void StjTrackListCutTest::testFlag()
{
  StjTrackListCut *cut = new StjTrackListCut();
  cut->addCut(new StjTrackCutNHits());

  StjTrackList listIn;

  StjTrack track1;
  track1.flag = 30;
  listIn.push_back(track1);

  StjTrack track2;
  track2.flag = -3;
  listIn.push_back(track2);

  StjTrackList listExpected;
  listExpected.push_back(track1);

  // excercise
  StjTrackList listActual = (*cut)(listIn);
  
  CPPUNIT_ASSERT_EQUAL( listExpected, listActual );

  delete cut;
}

void StjTrackListCutTest::testNHits()
{
  StjTrackListCut *cut = new StjTrackListCut();
  cut->addCut(new StjTrackCutNHits());

  StjTrackList listIn;

  StjTrack track1;
  track1.nHits = 13;
  listIn.push_back(track1);

  StjTrack track2;
  track2.nHits = 11;
  listIn.push_back(track2);

  StjTrack track3;
  track3.nHits = 12;
  listIn.push_back(track3);

  StjTrackList listExpected;
  listExpected.push_back(track1);

  // excercise
  StjTrackList listActual = (*cut)(listIn);
  
  CPPUNIT_ASSERT_EQUAL( listExpected, listActual );

  delete cut;
}

void StjTrackListCutTest::test2005()
{
  StjTrackListCut *cut = new StjTrackListCut();
  cut->addCut(new StjTrackCutDca());
  cut->addCut(new StjTrackCutEta());
  //  cut->addCut(new StjTrackCutPossibleHitRatio());

  StjTrackList listIn;

  StjTrack track1;
  track1.Tdca = 2.9;
  track1.eta  = 0.2;
  listIn.push_back(track1);

  StjTrack track2;
  track2.Tdca = 3.1;
  track2.eta  = 0.2;
  listIn.push_back(track2);

  StjTrack track3;
  track3.Tdca = 2.9;
  track3.eta  = 2.1;
  listIn.push_back(track3);

  StjTrack track4;
  track4.Tdca = 2.9;
  track4.eta  = -2.1;
  listIn.push_back(track4);

  StjTrackList listExpected;
  listExpected.push_back(track1);

  // excercise
  StjTrackList listActual = (*cut)(listIn);
  
  CPPUNIT_ASSERT_EQUAL( listExpected, listActual );

  delete cut;
}


void StjTrackListCutTest::test2006()
{
  StjTrackListCut *cut = new StjTrackListCut();
  cut->addCut(new StjTrackCutDca());
  cut->addCut(new StjTrackCutDcaPtDependent());
  cut->addCut(new StjTrackCutEta());
  //  cut->addCut(new StjTrackCutPossibleHitRatio());

  StjTrackList listIn;

  StjTrack track1;
  track1.pt   = 0.8;
  track1.Tdca = 2.9;
  track1.dcaD = 2.9;
  track1.eta  = 0.2;
  listIn.push_back(track1);

  StjTrack track2;
  track2.pt   = 0.8;
  track2.Tdca = 2.9;
  track2.dcaD = 3.1;
  track2.eta  = 0.2;
  listIn.push_back(track2);

  StjTrack track3;
  track3.pt   = 0.8;
  track3.Tdca = 2.9;
  track3.dcaD = 2.9;
  track3.eta  = 2.1;
  listIn.push_back(track3);

  StjTrack track4;
  track4.pt   = 0.8;
  track4.Tdca = 2.9;
  track4.dcaD = 2.9;
  track4.eta  = -2.1;
  listIn.push_back(track4);

  StjTrack track5;
  track5.pt   = 0.4;
  track5.Tdca = 2.9;
  track5.dcaD = 1.9;
  track5.eta  = 1.1;
  listIn.push_back(track5);

  StjTrack track6;
  track6.pt   = 0.4;
  track6.Tdca = 2.9;
  track6.dcaD = 2.1;
  track6.eta  = 1.1;
  listIn.push_back(track6);

  StjTrack track7;
  track7.pt   = 0.8;
  track7.Tdca = 2.9;
  track7.dcaD = 1.1;
  track7.eta  = 1.1;
  listIn.push_back(track7);

  StjTrack track8;
  track8.pt   = 0.8;
  track8.Tdca = 2.9;
  track8.dcaD = 1.5;
  track8.eta  = 1.1;
  listIn.push_back(track8);

  StjTrack track9;
  track9.pt   = 1.1;
  track9.Tdca = 2.9;
  track9.dcaD = 0.9;
  track9.eta  = 1.1;
  track9.nHits = 10;
  track9.nHitsPoss = 11;
  listIn.push_back(track9);

  StjTrack track10;
  track10.pt   = 1.1;
  track10.Tdca = 2.9;
  track10.dcaD = 1.1;
  track10.eta  = 1.1;
  listIn.push_back(track10);

  StjTrackList listExpected;
  listExpected.push_back(track5);
  listExpected.push_back(track7);
  listExpected.push_back(track9);

  // excercise
  StjTrackList listActual = (*cut)(listIn);
  
  CPPUNIT_ASSERT_EQUAL( listExpected, listActual );

  delete cut;
}
