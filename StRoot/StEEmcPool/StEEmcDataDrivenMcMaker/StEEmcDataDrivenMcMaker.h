// -*- mode: C++ -*-
//
// Hal Spinka <hms@anl.gov>
// Argonne National Laboratory
// Feb. 12, 2008
//

#ifndef ST_EEMC_DATA_DRIVEN_MC_MAKER_H
#define ST_EEMC_DATA_DRIVEN_MC_MAKER_H

// ROOT
class TClonesArray;
class TH2F;

// STAR
class StEEmcDbMaker;
class StMuEmcHit;
class StEEmcSmdResponse;
class StMuEmcUtil;
class StMcEvent;
class StMcVertex;
class StMcTrack;
class StMcCalorimeterHit;

// Local
class StEEmcShowerShape;

// STAR
#include "StMaker.h"

class StEEmcDataDrivenMcMaker : public StMaker {
public:
  StEEmcDataDrivenMcMaker(const char* name = "StEEmcDataDrivenMcMaker") : StMaker(name) {}
  ~StEEmcDataDrivenMcMaker() {}

  void Clear(Option_t* option = "");
  int  Init();
  int  Make();
  int  Finish();
  void SetLibraryFile(const char* filename);
  void SetLogFileName(const char* filename);

private:
  void processVertex(StMcVertex* mcVertex);
  void processTrack(StMcTrack* mcTrack);
  bool multiSector(const vector<StMcCalorimeterHit*>& hits) const;
  int  getEnergyBin(StEEmcShowerShape* showerShape) const;
  int  getPreshowerBin(StEEmcShowerShape* showerShape) const;

  TString mLibraryFile;
  TString mLogFileName;
  TFile* mLogFile;
  StEEmcDbMaker* mEEmcDb;
  StMuEmcHit* mStrips[12][2][288];
  StMcEvent* mcEvent;
  StMuEmcUtil* mMuEmcUtil;
  int mNumberOfPhotons;

  // Each SMD sector follows one of 3 SMD order
  //   S=Spacer, U=U-plane, V=V-plane
  // Sector 3, 6, 9, 12: SUV (mResponses[0])
  // Sector 1, 4, 7, 10: VSU (mResponses[1])
  // Sector 2, 5, 8, 11: UVS (mResponses[2])
  // See http://www.star.bnl.gov/public/eemc/geom/geom.html
  // The shower shapes are also binned in energy (E < 8 GeV, E > 8 GeV)
  // and in preshower energies (preshower1 == 0 && preshower2 == 0, or otherwise).
  TClonesArray* mShowerShapes[2][4]; // [energy][preshower]

  // QA histograms
  TH1F* hLibEntry;
  TH1F* hNumberOfPhotons;
  TH2F* hMcPhotonEnergyEta;
  TH2F* hMcPhotonEnergyPt;
  TH1F* hMcPhotonParent;
  TH2F* hMcPhotonXY;
  TH1F* hResidualSmdu;
  TH1F* hResidualSmdv;
  TH2F* hCorrSmdu;
  TH2F* hCorrSmdv;
  TH2F* hDiffStripSmdu;
  TH2F* hDiffStripSmdv;
  TH2F* hAsymStripSmdu;
  TH2F* hAsymStripSmdv;
  TH2F* hHighStripsUVid;
  TH2F* hHighStripsUVenergy;

  ClassDef(StEEmcDataDrivenMcMaker, 1);
};

inline void StEEmcDataDrivenMcMaker::SetLibraryFile(const char* filename)
{
  mLibraryFile = filename;
}

inline void StEEmcDataDrivenMcMaker::SetLogFileName(const char* filename)
{
  mLogFileName = filename;
}

#endif // ST_EEMC_DATA_DRIVEN_MC_MAKER_H
