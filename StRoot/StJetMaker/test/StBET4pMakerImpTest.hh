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

  class StJetTPCMock : public StSpinJet::StJetTPC {
  public:
    StJetTPCMock(int n) : _n(n) { }
    StSpinJet::TrackList getTrackList()
    { 
      StSpinJet::TrackList ret;
      for(int i = 0; i < _n; ++i)
	ret.push_back(StSpinJet::Track());
      return ret; 
    };
  private:
    int _n;
  };

  class StJetBEMCMock : public StSpinJet::StJetBEMC {
  public:
    StJetBEMCMock(int n) : _n(n) { }
    StSpinJet::TowerEnergyList getEnergyList()
    { 
      StSpinJet::TowerEnergyList ret;
      for(int i = 0; i < _n; ++i)
	ret.push_back(StSpinJet::TowerEnergy());
      return ret; 
    };
  private:
    int _n;
  };

  class StJetEEMCMock : public StSpinJet::StJetEEMC {
  public:
    StJetEEMCMock(int n) : _n(n) { }
    StSpinJet::TowerEnergyList getEnergyList()
    { 
      StSpinJet::TowerEnergyList ret;
      for(int i = 0; i < _n; ++i)
	ret.push_back(StSpinJet::TowerEnergy());
      return ret; 
    };
  private:
    int _n;
  };

  class TrackCutMock : public StJetTrackCut::TrackCut {
    bool operator()(const StSpinJet::Track& track)
    {
      return true;
    }
  };

  class TowerEnergyCutMock : public StJetTowerEnergyCut::TowerEnergyCut {
    bool operator()(const StSpinJet::TowerEnergy& energy)
    {
      return true;
    }
  };

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
