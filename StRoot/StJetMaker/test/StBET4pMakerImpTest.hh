// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STBET4PMAKERIMPTEST_HH
#define STBET4PMAKERIMPTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StBET4pMakerImpTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StBET4pMakerImpTest );
  CPPUNIT_TEST( testGetTrackAndEnergyList_withCut );
  CPPUNIT_TEST( testGetTrackAndEnergyList_withoutCut );
  CPPUNIT_TEST( testMake );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testGetTrackAndEnergyList_withCut();
  void testGetTrackAndEnergyList_withoutCut();
  void testMake();

private:

  class StjTPCMock : public StjTPC {
  public:
    StjTPCMock(int n) : _n(n) { }
    StjTrackList getTrackList()
    { 
      StjTrackList ret;
      for(int i = 0; i < _n; ++i)
	ret.push_back(StjTrack());
      return ret; 
    };
  private:
    int _n;
  };

  class StjBEMCMock : public StjBEMC {
  public:
    StjBEMCMock(int n) : _n(n) { }
    StjTowerEnergyList getEnergyList()
    { 
      StjTowerEnergyList ret;
      for(int i = 0; i < _n; ++i)
	ret.push_back(StjTowerEnergy());
      return ret; 
    };
  private:
    int _n;
  };

  class StjEEMCMock : public StjEEMC {
  public:
    StjEEMCMock(int n) : _n(n) { }
    StjTowerEnergyList getEnergyList()
    { 
      StjTowerEnergyList ret;
      for(int i = 0; i < _n; ++i)
	ret.push_back(StjTowerEnergy());
      return ret; 
    };
  private:
    int _n;
  };

  class StjTrackCutMock : public StjTrackCut {
    bool operator()(const StjTrack& track)
    {
      return true;
    }
  };

  class StjTowerEnergyCutMock : public StjTowerEnergyCut {
    bool operator()(const StjTowerEnergy& energy)
    {
      return true;
    }
  };

  void assertResults(const char *path);
  void writeExpected(const char *path);

  StjTPC* tpc;
  StjTrackListCut* tpcCut;
  StjBEMC* bemc;
  StjTowerEnergyListCut *bemcCut;
  StjEEMC* eemc;

  StjTowerEnergyCorrectionForTracks* corr;
  StBET4pMakerImp *imp;

};

#endif // STBET4PMAKERIMPTEST_HH
