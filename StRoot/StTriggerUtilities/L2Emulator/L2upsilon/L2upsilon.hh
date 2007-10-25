//
// Pibero Djawwotho <pibero@iucf.indiana.edu>
// Indiana University
// October 23, 2007
//

#ifndef L2upsilon_hh
#define L2upsilon_hh

#ifdef __ROOT__ //in root4star environment
#include "StTriggerUtilities/L2Emulator/L2algoUtil/L2VirtualAlgo.h"
#else
#include "L2VirtualAlgo.h"
#endif

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <ctime>
using namespace std;

#include "StDaqLib/TRG/trgStructures.h"
#include "bemcTower.h"

class Histogram;

struct BemcCluster {
  float x;
  float y;
  float z;
  float E;
};

class Timer {
public:
  void start();
  time_t time() const;

private:
  time_t startTime;
};

class L2upsilon : public L2VirtualAlgo {
  int   par_L2ResOff;
public:
  L2upsilon( int resOff,const string& name = "quarkonium");
  //Jan: consider  L2upsilon(L2EmcDb* db,char *outDir, int resOff); 

  ~L2upsilon();

  string name() const;

  int initRun(char* name, int runNumber, int* userInt, float* userFloat);
  bool doEvent(int L0trg, int eventNumber, TrgDataType* trgData,
	       int bemcIn, unsigned short* bemcData,
	       int eemcIn, unsigned short* eemcData);
  void finishRun();
  void print();

private:
  void  findSeedTowers(vector<int>& L0Seeds, vector<int>& L2Seeds);
  void  calcCluster(int daqId);
  bool  checkClusterCtbHit(int daqId) const;
  float bbcVertexZ() const;
  void  readBemcTower();
  void  readBemcStatus();
  void  readBemcCtbMap();
  void  makeBemcTables();
  void  makeBemcGainTable();
  void  makeBemcHwPedestalTable();
  void  makeBemcPedestalTable();
  void  makeBemcKillTable();
  void  makeBemcPositionTable();
  void  makeBemcNeighborTable();

  void  createHistograms();
  void  writeHistograms();
  void  resetHistograms();
  void  deleteHistograms();

  void setBaseFileName(int runNumber);
  string timeString() const;

  void readBtowDbFile();
  void readTowerMaskFile();
  void readBemcKillTable();
  void readCtbKillTable();

  static BemcTower bemcTower[4800];
  static BemcCluster bemcCluster[4800];
  static int daqIdFromSoftId[4801];
  static int bemcCtbMap[4800];
  static TrgDataType* trgData;
  static unsigned short* bemcData;
  static int ctbKill[256];

  string fName;
  string fBaseFileName;
  ofstream fLogfile;

  int fEventsSeen;
  int fEventsAccepted;
  int fRunNumber;

  // float parameters
  float fMinL0ClusterEnergy;
  float fMinL2ClusterEnergy;
  float fMinInvMass;
  float fMaxInvMass;
  float fMaxCosTheta;

  // int parameters
  int fL0SeedThreshold;
  int fL2SeedThreshold;
  int fUseCtb;
  int fUseVertexZ;
  int fNumberOfTowersPerCluster;

  // Histograms
  vector<Histogram*> fHistograms;
  Histogram* hL0SeedTowers;
  Histogram* hL2SeedTowers;
  Histogram* hNumberOfL0Seeds;
  Histogram* hNumberOfL2Seeds;
  Histogram* hInvMass;
  Histogram* hTime;
  Histogram* hEnergyOfL0Cluster;
  Histogram* hEnergyOfL2Cluster;
  Histogram* hCosTheta;
  Histogram* hVertexZ;
  Histogram* hCtbIndex;
  Histogram* hHighTowers;
  Histogram* hL0rate;
  Histogram* hL2rate;

  // Timer
  Timer timer;
};

inline void Timer::start() { startTime = std::time(0); }
inline time_t Timer::time() const { return std::time(0) - startTime; }

inline L2upsilon::L2upsilon( int x,const string& name) : fName(name) { createHistograms(); par_L2ResOff=x; }

inline L2upsilon::~L2upsilon()
{
  if (fLogfile.is_open()) finishRun();
  deleteHistograms();
}

inline string L2upsilon::name() const { return fName; }

// extern L2upsilon jpsi;
// extern L2upsilon ups;

#endif
