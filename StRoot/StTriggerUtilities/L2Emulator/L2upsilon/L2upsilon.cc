//
// Pibero Djawwotho <pibero@iucf.indiana.edu>
// Indiana University
// October 23, 2007
//

#include <iomanip>
#include <fstream>
#include <functional>
#include <cmath>
#include <cstdlib>

#include "/usr/src/kernels/2.6.9-42.0.10.EL-smp-i686/include/asm-i386/msr.h"

#include "L2Result.h"
#include "Histogram.hh"
#include "L2upsilon.hh"

#define L0_HT_TRIGGER_BIT_SHIFT 4
#define CPU_MHZ 1603.680

// L2upsilon jpsi("jpsi");
// L2upsilon ups("ups");

BemcTower       L2upsilon::bemcTower[4800];
BemcCluster     L2upsilon::bemcCluster[4800];
int             L2upsilon::daqIdFromSoftId[4801];
int             L2upsilon::bemcCtbMap[4800];
TrgDataType*    L2upsilon::trgData = 0;
unsigned short* L2upsilon::bemcData = 0;
int             L2upsilon::ctbKill[256];

int L2upsilon::initRun(char* name, int runNumber, int* userInt, float* userFloat)
{
  // Debug
  cout << "L2upsilon::initRun(char* name=\"" << name << "\""
       << ", int runNumber=" << runNumber
       << ", int* userInt=" << userInt
       << ", float* userFloat=" << userFloat
       << ")"
       << endl;

  setBaseFileName(runNumber);

  //
  // Set float and int run control parameters
  //
  fMinL0ClusterEnergy = userFloat[0];
  fMinL2ClusterEnergy = userFloat[1];
  fMinInvMass         = userFloat[2];
  fMaxInvMass         = userFloat[3];
  fMaxCosTheta        = userFloat[4];

  fL0SeedThreshold          = userInt[0];
  fL2SeedThreshold          = userInt[1];
  fUseCtb                   = userInt[2];
  fUseVertexZ               = userInt[3];
  fNumberOfTowersPerCluster = userInt[4];

  //
  // Reset event counters
  //
  fEventsSeen = 0;
  fEventsAccepted = 0;

  //
  // Open log file and write out algorithm parameters
  //
  if (fLogfile.is_open()) finishRun();
  const string filename = fBaseFileName + ".log";
  fLogfile.open(filename.c_str());
  fLogfile << "Run Number: " << runNumber << '\n'
	   << "Start Time: " << timeString()
	   << "L0 seed threshold: " << fL0SeedThreshold << '\n'
	   << "L2 seed threshold: " << fL2SeedThreshold << '\n'
	   << "L0 cluster energy threshold (GeV): " << fMinL0ClusterEnergy << '\n'
	   << "L2 cluster energy threshold (GeV): " << fMinL2ClusterEnergy << '\n'
	   << "Number of towers per cluster: " << fNumberOfTowersPerCluster  << '\n'
	   << "Use CTB: " << fUseCtb << '\n'
	   << "Use vertex Z: " << fUseVertexZ << '\n'
	   << "Minimum invariant mass (GeV): " << fMinInvMass << '\n'
	   << "Maximum invariant mass (GeV): " << fMaxInvMass << '\n'
	   << "Maximum cos(theta): " << fMaxCosTheta << endl;
  fL0SeedThreshold <<= L0_HT_TRIGGER_BIT_SHIFT;
  readBemcTower();
  readBemcStatus();
  readBtowDbFile();
  readTowerMaskFile();
  readBemcKillTable();
  if (fUseCtb) {
    readBemcCtbMap();
    memset(ctbKill, 0, sizeof(ctbKill));
    readCtbKillTable();
  }
  resetHistograms();
  timer.start();
  return 0;
}

bool L2upsilon::doEvent(int L0trg, int eventNumber, TrgDataType* trgData,
			int bemcIn, unsigned short* bemcData,
			int eemcIn, unsigned short* eemcData)
{
  // Debug
  cout << "L2upsilon::doEvent(int L0trg=" << L0trg
       << ", int eventNumber=" << eventNumber
       << ", TrgDataType* trgData=" << trgData
       << ", int bemcIn=" << bemcIn
       << ", unsigned short* bemcData=" << bemcData
       << ", int eemcIn=" << eemcIn
       << ", unsigned short* eemcData=" << eemcData
       << ")"
       << endl;

  if (!trgData || !bemcData) return false;

  //
  // We have a valid event
  //
  unsigned int timeStart;
  unsigned int timeStop;
  rdtscl(timeStart);
  hL0rate->fill(timer.time());
  ++fEventsSeen;

  //
  // Save the trigger data and bemc data pointers for use in member functions
  //
  L2upsilon::trgData = trgData;
  L2upsilon::bemcData = bemcData;

  vector<int> L0Seeds;
  vector<int> L2Seeds;
  findSeedTowers(L0Seeds, L2Seeds);

  L2Result l2result;
  for (unsigned int i = 0; i < L0Seeds.size(); ++i) {
    const int id1 = L0Seeds[i];
    calcCluster(id1);
    if (bemcCluster[id1].E < fMinL0ClusterEnergy || bemcCluster[id1].E == 0) continue;
    for (unsigned int j = 0; j < L2Seeds.size(); ++j) {
      const int id2 = L2Seeds[j];
      if (id1 == id2) continue;
      calcCluster(id2);
      if (bemcCluster[id2].E < fMinL2ClusterEnergy || bemcCluster[id2].E == 0) continue;
      float cosTheta = (bemcCluster[id1].x * bemcCluster[id2].x +
                        bemcCluster[id1].y * bemcCluster[id2].y +
                        bemcCluster[id1].z * bemcCluster[id2].z);
      if (cosTheta > fMaxCosTheta) continue;
      float invMass = sqrt(2 * bemcCluster[id1].E * bemcCluster[id2].E * (1 - cosTheta));
      if (fMinInvMass < invMass && invMass < fMaxInvMass) {
	//
        // We have a candidate
	//
	hL2rate->fill(timer.time());
        l2result.numberOfL0SeedTowers = L0Seeds.size();
        l2result.numberOfL2SeedTowers = L2Seeds.size();
        l2result.invariantMass = invMass;
        l2result.eventsSeen = fEventsSeen;
        l2result.eventsAccepted = ++fEventsAccepted;
        l2result.energyOfL0Cluster = bemcCluster[id1].E;
        l2result.energyOfL2Cluster = bemcCluster[id2].E;
        rdtscl(timeStop);
        l2result.processingTime = (timeStop > timeStart) ? timeStop - timeStart : 0;

	//
	// Fill histograms
	//
	hL0SeedTowers->fill(bemcTower[id1].softId);
	hL2SeedTowers->fill(bemcTower[id2].softId);
        hNumberOfL0Seeds->fill(L0Seeds.size());
        hNumberOfL2Seeds->fill(L2Seeds.size());
        hInvMass->fill(invMass);
        hTime->fill(l2result.processingTime / CPU_MHZ);
        hEnergyOfL0Cluster->fill(bemcCluster[id1].E);
        hEnergyOfL2Cluster->fill(bemcCluster[id2].E);
        hCosTheta->fill(cosTheta);
        memcpy(&trgData->TrgSum.L2Result[par_L2ResOff], &l2result, sizeof(L2Result));
        return true;
      }
    }
  }

  //
  // No candidate found
  //
  l2result.numberOfL0SeedTowers = L0Seeds.size();
  l2result.numberOfL2SeedTowers = L2Seeds.size();
  l2result.invariantMass = 0;
  l2result.eventsSeen = fEventsSeen;
  l2result.eventsAccepted = fEventsAccepted;
  l2result.energyOfL0Cluster = 0;
  l2result.energyOfL2Cluster = 0;
  rdtscl(timeStop);
  l2result.processingTime = (timeStop > timeStart) ? timeStop - timeStart : 0;
  memcpy(&trgData->TrgSum.L2Result[par_L2ResOff], &l2result, sizeof(L2Result));
  return false;
}

void L2upsilon::finishRun()
{
  // Debug
  cout << "L2upsilon::finishRun()" << endl;

  //
  // Write out events summary and close log file
  //
  fLogfile << "Stop Time: " << timeString()
	   << "Events seen: " << fEventsSeen << '\n'
	   << "Events accepted: " << fEventsAccepted << endl;
  fLogfile.close();
  writeHistograms();
}

void L2upsilon::findSeedTowers(vector<int>& L0Seeds, vector<int>& L2Seeds)
{
  for (int daqId = 0; daqId < 4800; ++daqId) {
    if (bemcTower[daqId].kill) continue;
    if (fL2SeedThreshold < fL0SeedThreshold) {
      if (bemcData[daqId] - bemcTower[daqId].pedestal >= fL2SeedThreshold) {
	hHighTowers->fill(daqId);
        L2Seeds.push_back(daqId);
        if (bemcData[daqId] >= fL0SeedThreshold)
          L0Seeds.push_back(daqId);
      }
    }
    else {
      if (bemcData[daqId] >= fL0SeedThreshold) {
	hHighTowers->fill(daqId);
        L0Seeds.push_back(daqId);
        if (bemcData[daqId] - bemcTower[daqId].pedestal >= fL2SeedThreshold)
          L2Seeds.push_back(daqId);
      }
    }
  }
}

void L2upsilon::calcCluster(int daqId)
{
  bemcCluster[daqId].x = 0;
  bemcCluster[daqId].y = 0;
  bemcCluster[daqId].z = 0;
  bemcCluster[daqId].E = 0;

  //
  // Check CTB matching
  //
  if (fUseCtb && !checkClusterCtbHit(daqId)) {
    bemcCluster[daqId].E = 0;
    return;
  }

  //
  // Calculate the energies of all neighbors
  //
  float energies[8];
  for (int i = 0; i < bemcTower[daqId].numberOfNeighbors; ++i) {
    int j = bemcTower[daqId].neighbor[i];
    if (bemcTower[j].kill || bemcData[j] < bemcTower[j].pedestal) {
      energies[i] = 0;
    }
    else {
      energies[i] = (bemcData[j] - bemcTower[j].pedestal) * bemcTower[j].gain;
    }
  }

  //
  // Sort the high tower neighbors in order of descending energy.
  // Use an insertion sort algorithm which is fairly efficient
  // for small arrays.
  //
  for (int i = 1; i < bemcTower[daqId].numberOfNeighbors; ++i) {
    int j = i;
    float indexE = energies[j];
    int indexId = bemcTower[daqId].neighbor[j];
    while (j > 0 && indexE > energies[j-1]) {
      energies[j] = energies[j-1];
      bemcTower[daqId].neighbor[j] = bemcTower[daqId].neighbor[j-1];
      --j;
    }
    energies[j] = indexE;
    bemcTower[daqId].neighbor[j] = indexId;
  }

  //
  // Calculate energy-weighted centroid of cluster
  //
  bemcCluster[daqId].E = (bemcData[daqId] - bemcTower[daqId].pedestal) * bemcTower[daqId].gain;
  bemcCluster[daqId].x = bemcTower[daqId].x * bemcCluster[daqId].E;
  bemcCluster[daqId].y = bemcTower[daqId].y * bemcCluster[daqId].E;
  bemcCluster[daqId].z = bemcTower[daqId].z * bemcCluster[daqId].E;
  for (int i = 0; i < fNumberOfTowersPerCluster - 1; ++i) {
    bemcCluster[daqId].E += energies[i];
    bemcCluster[daqId].x += bemcTower[bemcTower[daqId].neighbor[i]].x * energies[i];
    bemcCluster[daqId].y += bemcTower[bemcTower[daqId].neighbor[i]].y * energies[i];
    bemcCluster[daqId].z += bemcTower[bemcTower[daqId].neighbor[i]].z * energies[i];
  }
  bemcCluster[daqId].x /= bemcCluster[daqId].E;
  bemcCluster[daqId].y /= bemcCluster[daqId].E;
  bemcCluster[daqId].z /= bemcCluster[daqId].E;
  if (fUseVertexZ) {
    float vertexZ = bbcVertexZ();
    bemcCluster[daqId].z -= vertexZ;
    hVertexZ->fill(vertexZ);
  }

  //
  // Normalize (x, y, z) vector to unity
  //
  float norm = sqrt(bemcCluster[daqId].x * bemcCluster[daqId].x +
		    bemcCluster[daqId].y * bemcCluster[daqId].y +
		    bemcCluster[daqId].z * bemcCluster[daqId].z);
  if (norm) {
    bemcCluster[daqId].x /= norm;
    bemcCluster[daqId].y /= norm;
    bemcCluster[daqId].z /= norm;
  }
}

bool L2upsilon::checkClusterCtbHit(int daqId) const
{
  int i = bemcCtbMap[daqId];
  if (ctbKill[i]) return false;
  int adc;
#if 0
  switch (i) {
  case 76:
    adc = trgData->rawTriggerDet[0].ZDCSMD[7];
    break;
  case 75:
    adc = trgData->rawTriggerDet[0].ZDCSMD[6];
    break;
  case 74:
    adc = trgData->rawTriggerDet[0].ZDCSMD[5];
    break;
  case 166:
    adc = trgData->rawTriggerDet[0].ZDCSMD[4];
    break;
  case 165:
    adc = trgData->rawTriggerDet[0].ZDCSMD[3];
    break;
  case 164:
    adc = trgData->rawTriggerDet[0].ZDCSMD[2];
    break;
  default:
    adc = trgData->rawTriggerDet[0].CTB[i];
    break;
  }
#endif
#if 0 // JAN
  //
  // 2006/05/31 - Cables swapped by Zhangbu Xu
  //
  switch (i) {
  case 4:
    adc = trgData->rawTriggerDet[0].ZDCSMD[7];
    break;
  case 3:
    adc = trgData->rawTriggerDet[0].ZDCSMD[6];
    break;
  case 2:
    adc = trgData->rawTriggerDet[0].ZDCSMD[5];
    break;
  case 238:
    adc = trgData->rawTriggerDet[0].ZDCSMD[4];
    break;
  case 237:
    adc = trgData->rawTriggerDet[0].ZDCSMD[3];
    break;
  case 236:
    adc = trgData->rawTriggerDet[0].ZDCSMD[2];
    break;
  default:
    adc = trgData->rawTriggerDet[0].CTB[i];
    break;
  }
#endif
 if (adc > 3) {
    hCtbIndex->fill(i);
    return true;
  }
  return false;
}

float L2upsilon::bbcVertexZ() const
{
  //
  // Taken from $STAR/StRoot/StEvent/StTriggerData2005.cxx
  //
  static const int q_map[2][24] = {
    { 8  , 5  , 4  , 40 , 37 , 36 , 7  , 6  ,
      3  , 2  , 1  , 39 , 38 , 35 , 34 , 33 ,
      72 , 71 , 70 , 69 , 68 , 67 , 66 , 65 },
    { 24 , 21 , 20 , 56 , 53 , 52 , 23 , 22 ,
      19 , 18 , 17 , 55 , 54 , 51 , 50 , 49 ,
      88 , 87 , 86 , 85 , 84 , 83 , 82 , 81 }
  };

  static const int t_map[2][24] = {
    { 16 , 13 , 12 , 48 , 45 , 44 , 15 , 14 ,
      11 , 10 , 9  , 47 , 46 , 43 , 42 , 41 ,
      80 , 79 , 78 , 77 , 76 , 75 , 74 , 73 },
    { 32 , 29 , 28 , 64 , 61 , 60 , 31 , 30 ,
      27 , 26 , 25 , 63 , 62 , 59 , 58 , 57 ,
      96 , 95 , 94 , 93 , 92 , 91 , 90 , 89 }
  };

  //
  // Taken from $STAR/StRoot/StEvent/StBbcTriggerDetector.cxx
  //
  enum { east = 0, west = 1 };
  unsigned short tdcEarliestEast = 0;
  unsigned short tdcEarliestWest = 0;

#if 0 // JAN2
  for (int i = 0; i < 16; ++i) {
    unsigned short adc = trgData->rawTriggerDet[0].BBC[q_map[east][i]-1];
    unsigned short tdc = trgData->rawTriggerDet[0].BBC[t_map[east][i]-1];

    if (tdc < 245 && tdc > tdcEarliestEast && adc > 5) tdcEarliestEast = tdc;
  }

  for (int i = 24; i < 40; ++i) {
    unsigned short adc = trgData->rawTriggerDet[0].BBC[q_map[west][i-24]-1];
    unsigned short tdc = trgData->rawTriggerDet[0].BBC[t_map[west][i-24]-1];
    if (tdc < 245 && tdc > tdcEarliestWest && adc > 5) tdcEarliestWest = tdc;
  }
#endif

  return 2 * (tdcEarliestWest - tdcEarliestEast);
}

void L2upsilon::readBemcTower()
{
  const string filename = "bemcTower.dat";
  ifstream in(filename.c_str());
  if (!in) {
    fLogfile << "Can't open " << filename << endl;
    return;
  }
  string line;
  while (getline(in, line)) {
    if (line[0] == '#') continue;
    istringstream in(line);
    int daqId;
    in >> daqId;
    in >> bemcTower[daqId].softId
       >> bemcTower[daqId].crate
       >> bemcTower[daqId].crateSeq
       >> bemcTower[daqId].gain
       >> bemcTower[daqId].x
       >> bemcTower[daqId].y
       >> bemcTower[daqId].z
       >> bemcTower[daqId].eta
       >> bemcTower[daqId].phi
       >> bemcTower[daqId].numberOfNeighbors;
    for (int i = 0; i < bemcTower[daqId].numberOfNeighbors; ++i)
      in >> bemcTower[daqId].neighbor[i];
    daqIdFromSoftId[bemcTower[daqId].softId] = daqId;
  }
  in.close();
}

void L2upsilon::readBemcStatus()
{
  //
  // Open global bemcStatus.txt if available, otherwise revert to local copy
  //
  string filename = "/home/startrg/trg/cfg/Tier1/DSM_LUT/bemcStatus.txt";
  ifstream in(filename.c_str());
  if (!in) {
    fLogfile << "Can't open " << filename << endl;
    filename = "bemcStatus.txt";
    in.clear();
    in.open(filename.c_str());
    if (!in) {
      fLogfile << "Can't open " << filename << endl;
      return;
    }
  }

  string line;
  while (getline(in, line)) {
    if (line[0] == '#') {
      //
      // Check timestamp of bemcStatus.txt against today's date
      //
      if (line.find("Timestamp") != string::npos) {
	struct DateTime {
	  int year;
	  int month;
	  int day;
	  int hour;
	  int min;
	  int sec;
	};
	DateTime bemcStatus;
	sscanf(line.c_str(), "# Timestamp: %d-%d-%d %d:%d:%d",
	       &bemcStatus.year, &bemcStatus.month, &bemcStatus.day,
	       &bemcStatus.hour, &bemcStatus.min  , &bemcStatus.sec);
	time_t t = time(0);
	tm* tm = localtime(&t);
	DateTime today = {
	  tm->tm_year + 1900,
	  tm->tm_mon + 1,
	  tm->tm_mday,
	  tm->tm_hour,
	  tm->tm_min,
	  tm->tm_sec
	};
	if (bemcStatus.year  != today.year  ||
	    bemcStatus.month != today.month ||
	    bemcStatus.day   != today.day) {
	  char fillChar = fLogfile.fill();
	  fLogfile.fill('0');
	  fLogfile << filename << " is out of date!\n";
	  fLogfile << "- Timestamp is " << bemcStatus.year << '-'
		   << setw(2) << bemcStatus.month << '-'
		   << setw(2) << bemcStatus.day   << ' '
		   << setw(2) << bemcStatus.hour  << ':'
		   << setw(2) << bemcStatus.min   << ':'
		   << setw(2) << bemcStatus.sec   << endl;
	  fLogfile << "- Today is " << today.year << '-'
		   << setw(2) << today.month << '-'
		   << setw(2) << today.day   << ' '
		   << setw(2) << today.hour  << ':'
		   << setw(2) << today.min   << ':'
		   << setw(2) << today.sec   << endl;
	  fLogfile.fill(fillChar);
	}
      }
      continue;
    }
    if (line.compare(0, 6, "SoftId") == 0) {
      int   softId;
      int   crate;
      int   crateSeq;
      int   towerUnmasked;
      int   patchMaskedInHT;
      int   patchMaskedInSum;
      float pedestal;
      int   triggerPatch;
      istringstream in(line);
      in.ignore(6);
      in >> softId
         >> crate
         >> crateSeq
         >> towerUnmasked
         >> patchMaskedInHT
         >> patchMaskedInSum
         >> pedestal
         >> triggerPatch;
      int daqId = daqIdFromSoftId[softId];
      bemcTower[daqId].kill = !towerUnmasked;
      bemcTower[daqId].pedestal = pedestal;
    }
  }
  in.close();

  //
  // Save bemcSatus.txt
  //
  const string command = "/bin/cp " + filename + " " + fBaseFileName + ".bemcStatus.txt";
  system(command.c_str());
}

void L2upsilon::readBemcCtbMap()
{
  const string filename = "bemcCtbMap.dat";
  ifstream in(filename.c_str());
  if (!in) {
    fLogfile << "Can't open " << filename << endl;
    return;
  }
  string line;
  while (getline(in, line)) {
    if (line[0] == '#') continue;
    istringstream in(line);
    int daqId;
    int softId;
    in >> daqId >> softId;
    in >> bemcCtbMap[daqId];
  }
  in.close();
}

//
// The functions below generate BEMC tables in the old format
// (2004 & 2005) and saves them in the directory "bemcTables".
//
void L2upsilon::makeBemcTables()
{
  makeBemcGainTable();
  makeBemcHwPedestalTable();
  makeBemcPedestalTable();
  makeBemcKillTable();
  makeBemcPositionTable();
  makeBemcNeighborTable();
}

void L2upsilon::makeBemcGainTable()
{
  const string filename = "bemcTables/bemcGain.dat";
  ofstream out(filename.c_str());
  if (!out) {
    fLogfile << "Can't open " << filename << endl;
    return;
  }
  out << "daqId\tsoftId\tadc->E[0]\tadc->E[1]\n";
  for (int daqId = 0; daqId < 4800; ++daqId) {
    out << daqId << '\t'
	<< bemcTower[daqId].softId << '\t'
	<< 0 << '\t'
	<< bemcTower[daqId].gain << '\n';
  }
  out.close();
}

void L2upsilon::makeBemcHwPedestalTable()
{
  const string filename = "bemcTables/bemcHwPedestal.dat";
  ofstream out(filename.c_str());
  if (!out) {
    fLogfile << "Can't open " << filename << endl;
    return;
  }
  out << "daqId\tsoftId\thwPedestal\n";
  for (int daqId = 0; daqId < 4800; ++daqId) {
    out << daqId                    << '\t'
	<< bemcTower[daqId].softId  << '\t'
	<< 0 << '\n';
  } 
  out.close();
}

void L2upsilon::makeBemcPedestalTable()
{
  const string filename = "bemcTables/bemcPedestal.dat";
  ofstream out(filename.c_str());
  if (!out) {
    fLogfile << "Can't open " << filename << endl;
    return;
  }
  out << "daqId\tsoftId\tpedestal\n";
  for (int daqId = 0; daqId < 4800; ++daqId) {
    out << daqId                    << '\t'
	<< bemcTower[daqId].softId  << '\t'
	<< bemcTower[daqId].pedestal << '\n';
  } 
  out.close();
}

void L2upsilon::makeBemcKillTable()
{
  const string filename = "bemcTables/bemcKill.dat";
  ofstream out(filename.c_str());
  if (!out) {
    fLogfile << "Can't open " << filename << endl;
    return;
  }
  out << "daqId\tsoftId\tkill\n";
  for (int daqId = 0; daqId < 4800; ++daqId) {
    out << daqId                    << '\t'
	<< bemcTower[daqId].softId  << '\t'
	<< bemcTower[daqId].kill << '\n';
  } 
  out.close();
}

void L2upsilon::makeBemcPositionTable()
{
  const string filename = "bemcTables/bemcPosition.dat";
  ofstream out(filename.c_str());
  if (!out) {
    fLogfile << "Can't open " << filename << endl;
    return;
  }
  out << "daqId\tsoftId\tx\ty\tz\teta\tphi\n";
  for (int daqId = 0; daqId < 4800; ++daqId) {
    out << daqId                    << '\t'
	<< bemcTower[daqId].softId  << '\t'
	<< bemcTower[daqId].x << '\t'
	<< bemcTower[daqId].y << '\t'
	<< bemcTower[daqId].z << '\t'
	<< bemcTower[daqId].eta << '\t'
	<< bemcTower[daqId].phi << '\n';
  } 
  out.close();
}

void L2upsilon::makeBemcNeighborTable()
{
  const string filename = "bemcTables/upsBemcNeighbor.dat";
  ofstream out(filename.c_str());
  if (!out) {
    fLogfile << "Can't open " << filename << endl;
    return;
  }
  out << "daqId\tsoftId\tnum neigh.\tneighbors\n";
  for (int daqId = 0; daqId < 4800; ++daqId) {
    out << daqId << '\t'
	<< bemcTower[daqId].softId << '\t'
	<< bemcTower[daqId].numberOfNeighbors << '\t';
    for (int i = 0; i < bemcTower[daqId].numberOfNeighbors; ++i)
      out << bemcTower[daqId].neighbor[i] << '\t';
    out << '\n';
  } 
  out.close();
}

void L2upsilon::createHistograms()
{
  hL0SeedTowers = new Histogram("L0SeedTowers", "L0 seed towers;softId", 4800, 1, 4801);
  hL2SeedTowers = new Histogram("L2SeedTowers", "L2 seed towers;softId", 4800, 1, 4801);
  hNumberOfL0Seeds = new Histogram("numberOfL0Seeds", ";Number of L0 seeds", 10, 0, 10);
  hNumberOfL2Seeds = new Histogram("numberOfL2Seeds", ";Number of L2 seeds", 10, 0, 10);
  hInvMass = new Histogram("invMass", ";m_{ee} [GeV]", 200, 0, 20);
  hTime = new Histogram("timing", ";processing time [#mus]", 200, 0, 400);
  hEnergyOfL0Cluster = new Histogram("energyOfL0Cluster", ";Energy of L0 cluster [GeV]",
				     200, 0, 20);
  hEnergyOfL2Cluster = new Histogram("energyOfL2Cluster", ";Energy of L2 cluster [GeV]",
				     200, 0, 20);
  hCosTheta = new Histogram("cosTheta", ";cos(#Theta)", 100, -1, 1);
  hVertexZ = new Histogram("vertexZ", ";z_{vertex} (cm)", 100, -200, 200);
  hCtbIndex = new Histogram("ctbIndex", ";CTB index", 256, 0, 256);
  hHighTowers = new Histogram("highTowers", ";daqId", 4800, 0, 4800);
  hL0rate = new Histogram("l0rate", "L0;time (sec);rate (Hz)", 1800, 0, 1800);
  hL2rate = new Histogram("l2rate", "L2;time (sec);rate (Hz)", 1800, 0, 1800);

  fHistograms.push_back(hL0SeedTowers);
  fHistograms.push_back(hL2SeedTowers);
  fHistograms.push_back(hNumberOfL0Seeds);
  fHistograms.push_back(hNumberOfL2Seeds);
  fHistograms.push_back(hInvMass);
  fHistograms.push_back(hTime);
  fHistograms.push_back(hEnergyOfL0Cluster);
  fHistograms.push_back(hEnergyOfL2Cluster);
  fHistograms.push_back(hCosTheta);
  fHistograms.push_back(hVertexZ);
  fHistograms.push_back(hCtbIndex);
  fHistograms.push_back(hHighTowers);
  fHistograms.push_back(hL0rate);
  fHistograms.push_back(hL2rate);
}

struct writeHistogram : public unary_function<ostream&, Histogram*> {
  ostream& out;

  explicit writeHistogram(ostream& out) : out(out) {}

  ostream& operator()(Histogram* h) const
  {
    return h->write(out);
  }
};

void L2upsilon::writeHistograms()
{
  const string name = fBaseFileName + ".histo.bin";
  ofstream out(name.c_str());
  if (!out) {
    fLogfile << "Can't open " << name << endl;
  }
  for_each(fHistograms.begin(), fHistograms.end(), writeHistogram(out));
  out.close();
}

void L2upsilon::resetHistograms()
{
  for_each(fHistograms.begin(), fHistograms.end(), mem_fun(&Histogram::reset));
}

struct deleteHistogram : public unary_function<Histogram*, Histogram*> {
  Histogram* operator()(Histogram* h) const
  {
    delete h;
    return 0;
  }
};

void L2upsilon::deleteHistograms()
{
  transform(fHistograms.begin(), fHistograms.end(), fHistograms.begin(), deleteHistogram());
}

#if 0
void L2upsilon::print()
{
  const int L2RESULT_OFFSET = (name() == "ups") ? L2RESULTS_OFFSET_UPS : L2RESULTS_OFFSET_JPSI;
  L2Result& l2result = (L2Result&)trgData->TrgSum.L2Result[L2RESULT_OFFSET];
  fLogfile << l2result.numberOfL0SeedTowers << '\t'
	   << l2result.numberOfL2SeedTowers << '\t'
	   << l2result.invariantMass << '\t'
	   << l2result.eventsSeen << '\t'
	   << l2result.eventsAccepted << '\t'
	   << l2result.processingTime / CPU_MHZ << '\t'
	   << l2result.energyOfL0Cluster << '\t'
	   << l2result.energyOfL2Cluster << endl;
}
#endif
void L2upsilon::setBaseFileName(int runNumber)
{
  ostringstream s;
  s << "data/run" << runNumber << ".l2" << fName;
  fBaseFileName = s.str();
}

string L2upsilon::timeString() const
{
  time_t t = time(0);
  return ctime(&t);
}

//
// Utilities to read Jan's BEMC status table
//

#include <map>
map<string, int> daqIdFromName;

void L2upsilon::readBtowDbFile()
{
  const string filename = "emc_setup/btowDb.current";
  ifstream in(filename.c_str());
  if (!in) {
    fLogfile << "Can't open " << filename << endl;
    return;
  }
  string line;
  while (getline(in, line)) {
    if (line[0] == '#') continue;
    char  name[10];
    int   crate;
    int   chan;
    int   sec;
    char  sub;
    int   eta;
    float gain;
    float ped;
    float thr;
    int   stat;
    int   fail;
    int   softId;
    int   m, s, e;
    int   daqId;
    sscanf(line.c_str(), "%s %x %x %d %c %d %f %f %f %x %x id%d-%d-%d-%d %d",
	   name, &crate, &chan, &sec, &sub, &eta, &gain, &ped, &thr, &stat, &fail,
	   &softId, &m, &s, &e, &daqId);
    if (softId != bemcTower[daqId].softId) {
      fLogfile << "SoftIds differ for daqId = " << daqId << '\n'
	       << "- Jan: " << softId << '\n'
	       << "- Official:" << bemcTower[daqId].softId << endl;
    }
    if (crate != bemcTower[daqId].crate) {
      fLogfile << "Crates differ for daqId = " << daqId
	       << " and softId = " << bemcTower[daqId].softId << '\n'
	       << "- Jan: " << crate << '\n'
	       << "- Official: " << bemcTower[daqId].crate << endl;
    }
    if (chan != bemcTower[daqId].crateSeq) {
      fLogfile << "Channels differ for daqId = " << daqId
	       << " and softId = " << bemcTower[daqId].softId << '\n'
	       << "- Jan:" << chan << '\n'
	       << "- Official: " << bemcTower[daqId].crateSeq << endl;
    }
    if (gain) {
      gain = 1 / gain;
      const float gainDeviation = 0.1;
      if (fabs((gain - bemcTower[daqId].gain) / gain) > gainDeviation) {
	fLogfile << "Gains differ by more than " << gainDeviation * 100
		 << "% for daqId = " << daqId
		 << " and softId = " << bemcTower[daqId].softId << '\n'
		 << "- Jan: " << gain << '\n'
		 << "- Official: " << bemcTower[daqId].gain << endl;
      }
    }
    const float channelDeviation = 10;
    if (fabs(ped - bemcTower[daqId].pedestal) > channelDeviation) {
      fLogfile << "Pedestals differ by more than " << channelDeviation
	       << " channels for daqId = " << daqId
	       << " and softId = " << bemcTower[daqId].softId << '\n'
	       << "- Jan: " << ped << '\n'
	       << "- Official: " << bemcTower[daqId].pedestal << endl;
    }
    daqIdFromName[name] = daqId;
  }
  in.close();
}

void L2upsilon::readTowerMaskFile()
{
  const string filename = "emc_setup/towerMask.current";
  ifstream in(filename.c_str());
  if (!in) {
    fLogfile << "Can't open " << filename << endl;
    return;
  }
  string line;
  bool btow = false;
  fLogfile << "********************** BEMC HOT TOWERS **********************" << endl;
  while (getline(in, line)) {
    if (line[0] == '#') {
      if (line.find("BTOW") != string::npos)
	btow = true;
      continue;
    }
    if (btow) {
      char name[10];
      int  stat;
      int  fatal;
      sscanf(line.c_str(), "%s %x %x", name, &stat, &fatal);
      int daqId = daqIdFromName[name];
      bemcTower[daqId].kill = stat != 0 || fatal != 0;
      fLogfile << "daqId = " << daqId << "\tsoftId = " << bemcTower[daqId].softId;
      ios_base::fmtflags options = fLogfile.flags(ios_base::hex | ios_base::showbase);
      fLogfile << "\tstat = " << stat << "\tfatal = " << fatal << endl;
      fLogfile.flags(options);
    }
  }
  fLogfile << "*************************************************************" << endl;
  in.close();
}

void L2upsilon::readBemcKillTable()
{
  const string filename = "bemcKill.dat";
  ifstream in(filename.c_str());
  if (!in) return;
  string line;
  while (getline(in, line)) {
    if (line[0] == '#') continue;
    int daqId;
    int softId;
    int kill;
    sscanf(line.c_str(), "%d %d %d", &daqId, &softId, &kill);
    if (softId != bemcTower[daqId].softId) {
      fLogfile << "softId = " << softId
	       << " from bemcKill.dat does not match softId = "
	       << bemcTower[daqId].softId
	       << " from bemcTower.dat for daqId = " << daqId << endl;
    }
    else
      bemcTower[daqId].kill = kill;
  }
  in.close();
}

void L2upsilon::readCtbKillTable()
{
  const string filename = "ctbKill.dat";
  ifstream in(filename.c_str());
  if (!in) return;
  string line;
  while (getline(in, line)) {
    if (line[0] == '#') continue;
    int ctbId, tray, slat, kill;
    sscanf(line.c_str(), "%d %d %d %d", &ctbId, &tray, &slat, &kill);
    ctbKill[ctbId] = kill;
  }
  in.close();
}
