// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETCUTTEST_HH
#define STJJETCUTTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StjJetCutTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StjJetCutTest );
  CPPUNIT_TEST( testPt );
  CPPUNIT_TEST( testEta );
  CPPUNIT_TEST( testNFourVecs );
  CPPUNIT_TEST( testDetectorEta );
  CPPUNIT_TEST( testTrgBHT_notPass );
  CPPUNIT_TEST( testTrgBHT_pass );
  CPPUNIT_TEST( testTrgBHT_type );
  CPPUNIT_TEST( testTrgBHT_detectorId );
  CPPUNIT_TEST( testTrgBHT_towerId );
  CPPUNIT_TEST( testTrgBJP_notPass );
  CPPUNIT_TEST( testTrgBJP_pass );
  CPPUNIT_TEST( testTrgBJP_type );
  CPPUNIT_TEST( testTrgBJP_detectorId );
  CPPUNIT_TEST( testTrgBJP_jetPatchId );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testPt();
  void testEta();
  void testDetectorEta();
  void testNFourVecs();
  void testTrgBHT_notPass();
  void testTrgBHT_pass();
  void testTrgBHT_type();
  void testTrgBHT_detectorId();
  void testTrgBHT_towerId();
  void testTrgBJP_notPass();
  void testTrgBJP_pass();
  void testTrgBJP_type();
  void testTrgBJP_detectorId();
  void testTrgBJP_jetPatchId();

private:

};

#endif // STJJETCUTTEST_HH
