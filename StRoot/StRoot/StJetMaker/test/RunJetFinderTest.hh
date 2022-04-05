// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef RUNJETFINDERTEST_HH
#define RUNJETFINDERTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class RunJetFinderTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( RunJetFinderTest );
  CPPUNIT_TEST( testRun );
  //  CPPUNIT_TEST( testRunRepeat );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testRun();
  void testRunRepeat();

private:

  StjFourVecList createFourVecList();
  StjFourVec createFourVec(int runNumber, int eventId,
			   int fourvecId, int type, int detectorId,  short trackId, int towerId, int mcparticleId,
			   double pt, double eta, double phi, double m,
			   double vertexZ);

  StjJetList createJetList();

  StjJet createJet(int runNumber, int eventId,
		   int jetId,
		   double pt, double eta, double phi, double m,
		   double neuRt,
		   double vertexZ, double detectorEta,
		   const StjFourVecList&    fourVecList);

};


#endif // RUNJETFINDERTEST_HH
