// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STBET4PMAKERIMPTEST_HH
#define STBET4PMAKERIMPTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StBET4pMakerImpTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StBET4pMakerImpTest );
  CPPUNIT_TEST( testMake2003 );
  CPPUNIT_TEST( testMake2005 );
  CPPUNIT_TEST( testMake2006 );
  //  CPPUNIT_TEST( testMakeExpected2003 );
  //  CPPUNIT_TEST( testMakeExpected2005 );
  //  CPPUNIT_TEST( testMakeExpected2006 );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testMake2003();
  void testMake2005();
  void testMake2006();
  void testMakeExpected2003();
  void testMakeExpected2005();
  void testMakeExpected2006();

private:

  void assertResults(const char *path);
  void writeExpected(const char *path);

  StSpinJet::StJetTPC* tpc;
  StSpinJet::StJetTPCTrackCut* tpcCut;
  StSpinJet::StJetBEMC* bemc;
  StSpinJet::StJetBEMCEnergyCut *bemcCut;
  StSpinJet::StJetEEMC* eemc;

  StSpinJet::CorrectTowerEnergyForTracks* corr;
  StBET4pMakerImp *imp;

};

#endif // STBET4PMAKERIMPTEST_HH
