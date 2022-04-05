//
// Pibero Djawwotho <pibero@iucf.indiana.edu>
// Indiana University
// October 23, 2007
//

#include <cmath>
#include <cstdlib>
#include <algorithm>
#include <functional>
#include <cstring>
#include <cassert>

#include "L2upsilonResult2006.h"
#include "L2upsilon2006.hh"
#ifdef  IS_REAL_L2  //in l2-ana  environment
  #include "trgStructures.h"
  #include "../L2algoUtil/L2EmcDb.h"
  #include "../L2algoUtil/L2Histo.h"
#else
  #include "StDaqLib/TRG/trgStructures.h"
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2EmcDb.h"
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2Histo.h"
#endif

#define L0_HT_TRIGGER_BIT_SHIFT 4
#define CPU_MHZ 1603.680

L2upsilon2006::L2upsilon2006(const char* name, L2EmcDb* db, char* outDir, int resOff) :
  L2VirtualAlgo(name, db, outDir, resOff), mLogFile(stdout), mRunNumber(0), mUnfinished(false)
{
  createHistograms();
}

L2upsilon2006::~L2upsilon2006()
{
  deleteHistograms();
}

int L2upsilon2006::initRun(int runNumber, int* userInt, float* userFloat)
{
  //
  // Open log file and write out algorithm parameters
  //
  if (mUnfinished) {
    fprintf(mLogFile, "L2upsilon2006: WARNING - finishRun() was not called for last run\n");
    finishRun();
  }

  mRunNumber = runNumber;
  mUnfinished = true;

  char filename[FILENAME_MAX];
  sprintf(filename, "%s/run%d.l2ups.log", mOutDir, runNumber);

  if (FILE* fp = fopen(filename, "w"))
    mLogFile = fp;
  else
    printf("L2upsilon2006: WARNING - Can't open log file %s\n", filename);

  fprintf(mLogFile, "L2upsilon2006::initRun(char* name=\"%s\", int runNumber=%d, int* userInt=%p, float* userFloat=%p)\n", mName, runNumber, userInt, userFloat);

  //
  // Set float and int run control parameters
  //
  mMinL0ClusterEnergy = userFloat[0];
  mMinL2ClusterEnergy = userFloat[1];
  mMinInvMass         = userFloat[2];
  mMaxInvMass         = userFloat[3];
  mMaxCosTheta        = userFloat[4];

  mL0SeedThreshold          = userInt[0];
  mL2SeedThreshold          = userInt[1];
  mUseCtb                   = userInt[2];
  mUseVertexZ               = userInt[3];
  mNumberOfTowersPerCluster = userInt[4];

  fprintf(mLogFile, "Run Number: %d\n", runNumber);
  fprintf(mLogFile, "Start Time: %s", timeString());
  fprintf(mLogFile, "L0 seed threshold: %d\n", mL0SeedThreshold);
  fprintf(mLogFile, "L2 seed threshold: %d\n", mL2SeedThreshold);
  fprintf(mLogFile, "L0 cluster energy threshold [GeV]: %f\n", mMinL0ClusterEnergy);
  fprintf(mLogFile, "L2 cluster energy threshold [GeV]: %f\n", mMinL2ClusterEnergy);
  fprintf(mLogFile, "Number of towers per cluster: %d\n", mNumberOfTowersPerCluster);
  fprintf(mLogFile, "Use CTB: %d\n", mUseCtb);
  fprintf(mLogFile, "Use vertex Z: %d\n", mUseVertexZ);
  fprintf(mLogFile, "Minimum invariant mass [GeV]: %f\n", mMinInvMass);
  fprintf(mLogFile, "Maximum invariant mass [GeV]: %f\n", mMaxInvMass);
  fprintf(mLogFile, "Maximum cos(theta): %f\n", mMaxCosTheta);

  //
  // Reset event counters
  //
  mEventsSeen = 0;
  mEventsAccepted = 0;

  mL0SeedThreshold <<= L0_HT_TRIGGER_BIT_SHIFT;

  // Get pedestals, gains, status, ...
  mDb->initRun(runNumber);
  memset(bemcTower, 0, sizeof(bemcTower));
  memset(mSoftIdToRdo, 0, sizeof(mSoftIdToRdo));
  memset(mPhiEtaToRdo, 0, sizeof(mPhiEtaToRdo));
  for (int i = 0; i < EmcDbIndexMax; ++i) {
    const L2EmcDb::EmcCDbItem* x = mDb->getByIndex(i);
    if (mDb->isBTOW(x)) {
      int softId = atoi(x->tube + 2); // 1-4800
      int eta = x->eta - 1; // 0-39
      int phi = (x->sec - 1) * 10 + x->sub - 'a'; // 0-119
      bemcTower[x->rdo].softId = softId;
      bemcTower[x->rdo].gain = x->gain;
      bemcTower[x->rdo].eta = eta;
      bemcTower[x->rdo].phi = phi;
      bemcTower[x->rdo].pedestal = x->ped;
      bemcTower[x->rdo].stat = x->stat;
      bemcTower[x->rdo].fail = x->fail;
      mSoftIdToRdo[softId] = x->rdo;
      mPhiEtaToRdo[phi][eta] = x->rdo;
    }
  }

  // Get neighbors
  for (int i = 0; i < EmcDbIndexMax; ++i) {
    const L2EmcDb::EmcCDbItem* x = mDb->getByIndex(i);
    if (mDb->isBTOW(x)) {
      int eta = x->eta - 1; // 0-39
      int phi = (x->sec - 1) * 10 + x->sub - 'a'; // 0-119      
      int n = 0;
      for (int deta = -1; deta <= 1; ++deta) {
	int eta2 = eta + deta;
	if (eta2 < 0 || eta2 >= 40) continue;
	for (int dphi = -1; dphi <= 1; ++dphi) {
	  if (deta == 0 && dphi == 0) continue;
	  int phi2 = phi + dphi;
	  phi2 += 120;
	  phi2 %= 120;
	  int rdo2 = mPhiEtaToRdo[phi2][eta2];
	  bemcTower[x->rdo].neighbor[n++] = rdo2;
	}
      }
      bemcTower[x->rdo].numberOfNeighbors = n;
    }
  }

  resetHistograms();
  timer.start();

  return 0;
}

void L2upsilon2006::readGeomXYZ(const char *fname){
  printf(" Get BTOW towers x, y, z from=%s=\n",fname);

  FILE* fp = fopen(fname, "r");
  assert(fp);
  int id;
  float x, y, z;
  int ret;
  while ((ret = fscanf(fp, "%d %f %f %f", &id, &x, &y, &z)) != EOF) {
    assert(ret==4);
    int rdo = mSoftIdToRdo[id];
    bemcTower[rdo].x = x;
    bemcTower[rdo].y = y;
    bemcTower[rdo].z = z;
  }
  fclose(fp);
}


bool L2upsilon2006::doEvent(int L0trg, int eventNumber, TrgDataType* trgData,
			int bemcIn, unsigned short* bemcData,
			int eemcIn, unsigned short* eemcData)
{

#if 0 
  printf("L2upsilon2006::doEvent(int L0trg=%d, int eventNumber=%d, TrgDataType* trgData=%p, int bemcIn=%d, unsigned short* bemcData=%p, int eemcIn=%d, unsigned short* eemcData=%p)\n", L0trg, eventNumber, trgData, bemcIn, bemcData, eemcIn, eemcData);
#endif
  rdtscl_macro(mEveTimeStart);
  mAccept=false;
  if (trgData && bemcData) {// UUUU , I inverted  logic to positive

  //
  // We have a valid event
  //
  unsigned int timeStart;
  unsigned int timeStop;

  rdtscl_macro(timeStart);
  hL0rate->fill(timer.time());
  ++mEventsSeen;
  this->trgData = trgData;
  this->bemcData = bemcData;

  //
  // Save the trigger data and BEMC data pointers for use in member functions
  //
  vector<int> L0Seeds;
  vector<int> L2Seeds;

  L0Seeds.reserve(100);
  L2Seeds.reserve(100);
  findSeedTowers(L0Seeds, L2Seeds);

  L2upsilonResult2006 result;
  for (size_t i = 0; i < L0Seeds.size(); ++i) {
    const int id1 = L0Seeds[i];
    calcCluster(id1);
    if (bemcCluster[id1].E < mMinL0ClusterEnergy || bemcCluster[id1].E == 0) continue;
    for (size_t j = 0; j < L2Seeds.size(); ++j) {
      const int id2 = L2Seeds[j];
      if (id1 == id2) continue;
      calcCluster(id2);
      if (bemcCluster[id2].E < mMinL2ClusterEnergy || bemcCluster[id2].E == 0) continue;
      float cosTheta = (bemcCluster[id1].x * bemcCluster[id2].x +
                        bemcCluster[id1].y * bemcCluster[id2].y +
                        bemcCluster[id1].z * bemcCluster[id2].z);
      if (cosTheta > mMaxCosTheta) continue;
      float invMass = sqrt(2 * bemcCluster[id1].E * bemcCluster[id2].E * (1 - cosTheta));
      if (mMinInvMass < invMass && invMass < mMaxInvMass) {
	//
        // We have a candidate
	//
	hL2rate->fill(timer.time());

        result.numberOfL0SeedTowers = L0Seeds.size();
        result.numberOfL2SeedTowers = L2Seeds.size();
        result.invariantMass = invMass;
        result.eventsSeen = mEventsSeen;
        result.eventsAccepted = ++mEventsAccepted;
        result.energyOfL0Cluster = bemcCluster[id1].E;
        result.energyOfL2Cluster = bemcCluster[id2].E;
        rdtscl_macro(timeStop);
        result.processingTime = (timeStop > timeStart) ? timeStop - timeStart : 0;

	//
	// Fill histograms
	//
	hL0SeedTowers->fill(bemcTower[id1].softId-1);
	hL2SeedTowers->fill(bemcTower[id2].softId-1);
        hNumberOfL0Seeds->fill(L0Seeds.size());
        hNumberOfL2Seeds->fill(L2Seeds.size());
        hInvMass->fill(int(invMass*10));
        hTime->fill(int(0.5*result.processingTime/CPU_MHZ));
        hEnergyOfL0Cluster->fill(int(bemcCluster[id1].E*10));
        hEnergyOfL2Cluster->fill(int(bemcCluster[id2].E*10));
        hCosTheta->fill(int((cosTheta+1)*50));

        memcpy(&trgData->TrgSum.L2Result[mResultOffset], &result, sizeof(result));

#ifndef IS_REAL_L2
	print();
#endif
	mAccept=true;
        goto doEventEnd;
      }
    }
  }

  //
  // No candidate found
  //
  result.numberOfL0SeedTowers = L0Seeds.size();
  result.numberOfL2SeedTowers = L2Seeds.size();
  result.invariantMass = 0;
  result.eventsSeen = mEventsSeen;
  result.eventsAccepted = mEventsAccepted;
  result.energyOfL0Cluster = 0;
  result.energyOfL2Cluster = 0;
  rdtscl_macro(timeStop);
  result.processingTime = (timeStop > timeStart) ? timeStop - timeStart : 0;
  memcpy(&trgData->TrgSum.L2Result[mResultOffset], &result, sizeof(result));

#ifndef IS_REAL_L2
  print();
#endif
  } // end of UUUU
  doEventEnd:
  rdtscl_macro(mEveTimeStop);
  mEveTimeDiff=mEveTimeStop-mEveTimeStart;
  int  kTick=mEveTimeDiff/1000;
  //   printf("uu=%f t1=%d t2=%d \n",mEveTimeDiff/1000.,mEveTimeStart,mEveTimeStop);
  mhT->fill(kTick);
 
  return  mAccept;
}

void L2upsilon2006::finishRun()
{
  mUnfinished = false;
  fprintf(mLogFile, (char*)"L2upsilon2006::finishRun()\n");
  finishCommonHistos() ;
  //
  // Write out events summary and close log file
  //
  fprintf(mLogFile, "Stop Time: %s", timeString());
  fprintf(mLogFile, "Events seen: %d\n", mEventsSeen);
  fprintf(mLogFile, "Events accepted: %d\n", mEventsAccepted);

  writeHistograms();

  if (mLogFile != stdout) fclose(mLogFile);
}

void L2upsilon2006::findSeedTowers(vector<int>& L0Seeds, vector<int>& L2Seeds)
{
  for (int rdo = 0; rdo < 4800; ++rdo) {
    if (bemcTower[rdo].fail) continue;
    if (bemcData[rdo] - bemcTower[rdo].pedestal >= mL2SeedThreshold) {
      hHighTowers->fill(bemcTower[rdo].softId-1);
      L2Seeds.push_back(rdo);
      if (bemcData[rdo] - bemcTower[rdo].pedestal >= mL0SeedThreshold)
	L0Seeds.push_back(rdo);
    }
  }
}

void L2upsilon2006::calcCluster(int rdo)
{
  bemcCluster[rdo].x = 0;
  bemcCluster[rdo].y = 0;
  bemcCluster[rdo].z = 0;
  bemcCluster[rdo].E = 0;

  //
  // Calculate the energies of all neighbors
  //
  float energies[8];
  for (int i = 0; i < bemcTower[rdo].numberOfNeighbors; ++i) {
    int j = bemcTower[rdo].neighbor[i];
    if (bemcTower[j].fail || bemcData[j] < bemcTower[j].pedestal) {
      energies[i] = 0;
    }
    else {
      energies[i] = (bemcData[j] - bemcTower[j].pedestal) / bemcTower[j].gain;
    }
  }

  //
  // Sort the high tower neighbors in order of descending energy.
  // Use an insertion sort algorithm which is fairly efficient
  // for small arrays.
  //
  for (int i = 1; i < bemcTower[rdo].numberOfNeighbors; ++i) {
    int j = i;
    float indexE = energies[j];
    int indexId = bemcTower[rdo].neighbor[j];
    while (j > 0 && indexE > energies[j-1]) {
      energies[j] = energies[j-1];
      bemcTower[rdo].neighbor[j] = bemcTower[rdo].neighbor[j-1];
      --j;
    }
    energies[j] = indexE;
    bemcTower[rdo].neighbor[j] = indexId;
  }

  //
  // Calculate energy-weighted centroid of cluster
  //
  bemcCluster[rdo].E = (bemcData[rdo] - bemcTower[rdo].pedestal) / bemcTower[rdo].gain;
  bemcCluster[rdo].x = bemcTower[rdo].x * bemcCluster[rdo].E;
  bemcCluster[rdo].y = bemcTower[rdo].y * bemcCluster[rdo].E;
  bemcCluster[rdo].z = bemcTower[rdo].z * bemcCluster[rdo].E;

  for (int i = 0; i < mNumberOfTowersPerCluster - 1; ++i) {
    bemcCluster[rdo].E += energies[i];
    bemcCluster[rdo].x += bemcTower[bemcTower[rdo].neighbor[i]].x * energies[i];
    bemcCluster[rdo].y += bemcTower[bemcTower[rdo].neighbor[i]].y * energies[i];
    bemcCluster[rdo].z += bemcTower[bemcTower[rdo].neighbor[i]].z * energies[i];
  }

  bemcCluster[rdo].x /= bemcCluster[rdo].E;
  bemcCluster[rdo].y /= bemcCluster[rdo].E;
  bemcCluster[rdo].z /= bemcCluster[rdo].E;

  //
  // Normalize (x, y, z) vector to unity
  //
  float norm = sqrt(bemcCluster[rdo].x * bemcCluster[rdo].x +
		    bemcCluster[rdo].y * bemcCluster[rdo].y +
		    bemcCluster[rdo].z * bemcCluster[rdo].z);

  if (norm) {
    bemcCluster[rdo].x /= norm;
    bemcCluster[rdo].y /= norm;
    bemcCluster[rdo].z /= norm;
  }
}

void L2upsilon2006::createHistograms()
{
  hL0SeedTowers = new L2Histo(100,  (char*)"L0 seed towers;softId", 4800);
  hL2SeedTowers = new L2Histo(101,  (char*)"L2 seed towers;softId", 4800);
  hNumberOfL0Seeds = new L2Histo(102,  (char*)";Number of L0 seeds", 10);
  hNumberOfL2Seeds = new L2Histo(103, (char*) ";Number of L2 seeds", 10);
  hInvMass = new L2Histo(104,  (char*)";m_{ee} [GeV]", 200);
  hTime = new L2Histo(105,  (char*)";processing time [#mus]", 200);
  hEnergyOfL0Cluster = new L2Histo(106,  (char*)";Energy of L0 cluster [GeV]", 200);
  hEnergyOfL2Cluster = new L2Histo(107,  (char*)";Energy of L2 cluster [GeV]", 200);
  hCosTheta = new L2Histo(108,  (char*)";cos(#Theta)", 100);
  hVertexZ = new L2Histo(109,  (char*)";z_{vertex} (cm)", 100);
  hCtbIndex = new L2Histo(110,  (char*)";CTB index", 256);
  hHighTowers = new L2Histo(111,  (char*)";softId", 4800);
  hL0rate = new L2Histo(112,  (char*)"L0;time (sec);rate (Hz)", 1800);
  hL2rate = new L2Histo(113,  (char*)"L2;time (sec);rate (Hz)", 1800);

  mHistograms.push_back(hL0SeedTowers);
  mHistograms.push_back(hL2SeedTowers);
  mHistograms.push_back(hNumberOfL0Seeds);
  mHistograms.push_back(hNumberOfL2Seeds);
  mHistograms.push_back(hInvMass);
  mHistograms.push_back(hTime);
  mHistograms.push_back(hEnergyOfL0Cluster);
  mHistograms.push_back(hEnergyOfL2Cluster);
  mHistograms.push_back(hCosTheta);
  mHistograms.push_back(hVertexZ);
  mHistograms.push_back(hCtbIndex);
  mHistograms.push_back(hHighTowers);
  mHistograms.push_back(hL0rate);
  mHistograms.push_back(hL2rate);
}

void L2upsilon2006::writeHistograms()
{
  char filename[FILENAME_MAX];
  sprintf(filename, "%s/run%d.l2ups.histo.bin", mOutDir, mRunNumber);
  FILE* fp = fopen(filename, "w");
  if (!fp) {
    fprintf(mLogFile, "L2upsilon2006: WARNING - Can't open histogram file %s\n", filename);
    return;
  }
  for (list<L2Histo*>::iterator i = mHistograms.begin(); i != mHistograms.end(); ++i)
    (*i)->write(fp);
  fclose(fp);
}

void L2upsilon2006::resetHistograms()
{
  for_each(mHistograms.begin(), mHistograms.end(), mem_fun(&L2Histo::reset));
}

void L2upsilon2006::deleteHistograms()
{
  for (list<L2Histo*>::iterator i = mHistograms.begin(); i != mHistograms.end(); ++i) {
    delete *i;
    *i = 0;
  }
}

void L2upsilon2006::print()
{
  L2upsilonResult2006* result = (L2upsilonResult2006*)&trgData->TrgSum.L2Result[mResultOffset];
  fprintf(mLogFile, "-----------------------------------------------\n");
  fprintf(mLogFile, "Number of L0 seed towers: %d\n", result->numberOfL0SeedTowers);
  fprintf(mLogFile, "Number of L2 seed towers: %d\n", result->numberOfL2SeedTowers);
  fprintf(mLogFile, "Invariant mass [GeV]: %f\n", result->invariantMass);
  fprintf(mLogFile, "Events seen: %d\n", result->eventsSeen);
  fprintf(mLogFile, "Events accepted: %d\n", result->eventsAccepted);
  fprintf(mLogFile, "Processing time [us]: %f\n", result->processingTime / CPU_MHZ);
  fprintf(mLogFile, "Energy of L0 cluster [GeV]: %f\n", result->energyOfL0Cluster);
  fprintf(mLogFile, "Energy of L2 cluster [GeV]: %f\n", result->energyOfL2Cluster);
}
