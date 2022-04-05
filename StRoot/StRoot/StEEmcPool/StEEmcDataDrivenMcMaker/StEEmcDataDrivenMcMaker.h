// -*- mode: C++ -*-
//
// Hal Spinka <hms@anl.gov>
// Argonne National Laboratory
//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University Cyclotron Facility
//
// Ilya Selyuzhenkov <ilya.selyuzhenkov@gmail.com>
// Indiana University Cyclotron Facility
//

#ifndef ST_EEMC_DATA_DRIVEN_MC_MAKER_H
#define ST_EEMC_DATA_DRIVEN_MC_MAKER_H
#include <vector>
// ROOT
class TClonesArray;
class TH2F;
class TTree;

// STAR
class StEEmcDb;
class StMuEmcHit;
class StEEmcSmdResponse;
class StMuEmcUtil;
class StMcEvent;
class StMcVertex;
class StMcTrack;
class StMcCalorimeterHit;
class StMcEmcHitCollection;
class StEEmcDataDrivenMcEventInfo;
class StEEmcDataDrivenMcReplaceInfo;
class StEEmcA2EMaker;

// Local
class StEEmcShowerShape;

// STAR
#include "StMaker.h"
#include <vector>
class StEEmcDataDrivenMcMaker : public StMaker {
public:
  StEEmcDataDrivenMcMaker(const char* name = "StEEmcDataDrivenMcMaker");
  ~StEEmcDataDrivenMcMaker() {}

  void Clear(Option_t* option = "");
  int  Init();
  int  InitRun(int runNumber);
  int  Make();
  int  Finish();

  StEEmcDataDrivenMcEventInfo* GetDataDrivenMcEventInfo();
  void SetLibraryFile(const char* filename);
  void SetLogFileName(const char* filename);
  void SetNumberOfStripsReplaced(int n);

  void SetShowerShapeScalingMethod(int id);
	// defaul is method 1
	// method 1: preserve SMD integrated energy for the replaced strips within +/- mNumberOfStripsReplaced:
	// method 1: scale = E_smd^geant / E_smd^library
	// method 2: use photon energies ratio from geant and library records:
	// method 2: scale = E_gamma^geant / E_gamma^library

  void UsePed(bool value = true) { mUsePed = value; }

private:
  enum { NUMBER_OF_ENERGY_BINS = 2, NUMBER_OF_PRESHOWER_BINS = 4 };

  void processVertex(StMcVertex* mcVertex);
  void processTrack(StMcTrack* mcTrack);
  bool multiSector(const vector<StMcCalorimeterHit*>& hits) const;
  int  getEnergyBin(StEEmcShowerShape* showerShape) const;
  int  getPreshowerBin(StEEmcShowerShape* showerShape) const;
  void getEnergies(StMcTrack* mcTrack, StEEmcDataDrivenMcReplaceInfo* replaceInfo);
  float GetShowerShapeScale(StMcTrack *mcTrack, StEEmcShowerShape* showerShape, int sector, int plane, int geantPhotonCentralStrip);

  bool mUsePed;
  float mPed[12][2][288]; // [sector][plane][strip]
  float mGain[12][2][288]; // [sector][plane][strip]
  TString mLibraryFile;
  TString mLogFileName;
  TFile* mLogFile;
  StEEmcDb* mEEmcDb;
  StMuEmcHit* mStrips[12][2][288]; // [sector][plane][strip]
  StMcEvent* mMcEvent;
  StMuEmcUtil* mMuEmcUtil;

  // Each SMD sector follows one of 3 SMD order
  //   S=Spacer, U=U-plane, V=V-plane
  // Sector 3, 6, 9, 12: SUV (mResponses[0])
  // Sector 1, 4, 7, 10: VSU (mResponses[1])
  // Sector 2, 5, 8, 11: UVS (mResponses[2])
  // See http://www.star.bnl.gov/public/eemc/geom/geom.html
  // The shower shapes are also binned in energy (E < 8 GeV, E > 8 GeV)
  // and in preshower energies (preshower1 == 0 && preshower2 == 0, or otherwise).
  TClonesArray* mShowerShapes[2][4]; // [energy][preshower]

  // QA tree
  TTree* mTree;
  StEEmcDataDrivenMcEventInfo* mDataDrivenMcEventInfo;
  int mNumberOfStripsReplaced;
  int mShowerShapeScalingMethod;
  map<StEEmcShowerShape*, int> mLibraryMap;
  StEEmcA2EMaker* mA2E;

  ClassDef(StEEmcDataDrivenMcMaker, 1);
};

inline StEEmcDataDrivenMcEventInfo* StEEmcDataDrivenMcMaker::GetDataDrivenMcEventInfo()
{
  return mDataDrivenMcEventInfo;
}

inline void StEEmcDataDrivenMcMaker::SetLibraryFile(const char* filename)
{
  mLibraryFile = filename;
}

inline void StEEmcDataDrivenMcMaker::SetLogFileName(const char* filename)
{
  mLogFileName = filename;
}

inline void StEEmcDataDrivenMcMaker::SetNumberOfStripsReplaced(int n)
{
  mNumberOfStripsReplaced = n;
}

inline void StEEmcDataDrivenMcMaker::SetShowerShapeScalingMethod(int id)
{
  mShowerShapeScalingMethod = id;
}

#endif // ST_EEMC_DATA_DRIVEN_MC_MAKER_H
