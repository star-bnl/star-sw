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

  class StjTPCMock : public StSpinJet::StjTPC {
  public:
    StjTPCMock(int n) : _n(n) { }
    StSpinJet::StjTrackList getTrackList()
    { 
      StSpinJet::StjTrackList ret;
      for(int i = 0; i < _n; ++i)
	ret.push_back(StSpinJet::StjTrack());
      return ret; 
    };
  private:
    int _n;
  };

  class StjBEMCMock : public StSpinJet::StjBEMC {
  public:
    StjBEMCMock(int n) : _n(n) { }
    StSpinJet::StjTowerEnergyList getEnergyList()
    { 
      StSpinJet::StjTowerEnergyList ret;
      for(int i = 0; i < _n; ++i)
	ret.push_back(StSpinJet::StjTowerEnergy());
      return ret; 
    };
  private:
    int _n;
  };

  class StjEEMCMock : public StSpinJet::StjEEMC {
  public:
    StjEEMCMock(int n) : _n(n) { }
    StSpinJet::StjTowerEnergyList getEnergyList()
    { 
      StSpinJet::StjTowerEnergyList ret;
      for(int i = 0; i < _n; ++i)
	ret.push_back(StSpinJet::StjTowerEnergy());
      return ret; 
    };
  private:
    int _n;
  };

  class StjTrackCutMock : public StJetTrackCut::StjTrackCut {
    bool operator()(const StSpinJet::StjTrack& track)
    {
      return true;
    }
  };

  class StjTowerEnergyCutMock : public StJetTowerEnergyCut::StjTowerEnergyCut {
    bool operator()(const StSpinJet::StjTowerEnergy& energy)
    {
      return true;
    }
  };

  void assertResults(const char *path);
  void writeExpected(const char *path);

  StSpinJet::StjTPC* tpc;
  StSpinJet::StjTrackListCut* tpcCut;
  StSpinJet::StjBEMC* bemc;
  StSpinJet::StjTowerEnergyListCut *bemcCut;
  StSpinJet::StjEEMC* eemc;

  StSpinJet::StjTowerEnergyCorrectionForTracks* corr;
  StBET4pMakerImp *imp;

};

#endif // STBET4PMAKERIMPTEST_HH
