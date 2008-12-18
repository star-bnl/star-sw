//
// Ilya Selyuzhenkov <ilya.selyuzhenkov@gmail.com>
// Indiana University Cyclotron Facility
//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University Cyclotron Facility
//

#ifndef __StEEmcDataDrivenMcReplaceInfo_h__
#define __StEEmcDataDrivenMcReplaceInfo_h__

#include "TClonesArray.h"
#include "TVector3.h"

#include "StMcCalorimeterHit.hh"

class StEEmcDataDrivenMcReplaceInfo : public TObject {
public:
  enum { numEemcTowerLayers = 6 }; // 0 tower,  1 pre-shower1, 2 pre-shower2, 3 post-shower , 4 smdU, 5 smdV

  StEEmcDataDrivenMcReplaceInfo();
  virtual ~StEEmcDataDrivenMcReplaceInfo() {}

  void Clear(Option_t* options = "");

  int  pid; // particle's Geant Id
  int  parentPid;// particle's parent Geant Id
  int  firstHadronPid;// particle's first hadron Geant Id  [hadron which has gluon or quark parents]
  int  libraryShapeId; // library shape id (currently entry number from library tree)
  TVector3 momentum; // particle 3-momentum MC
  float energy; // particle energy from MC
  float energyScaleU; // (dE/totalE)*totalEscaled / EnergyLibrary
  float energyScaleV; // (dE/totalE)*totalEscaled / EnergyLibrary
  int   highStripShiftU; // shift for high u-strip from extrapolated photon position
  int   highStripShiftV; //shift for high v-strip from extrapolated photon position
  int   libraryBinId; // Data-driven librry bin id: nPre12bins = 4; nEbins=2; binId = pre12bin+eBins*nPre12bins

  int   nTowerFired[numEemcTowerLayers]; // number of towers fired by the particle (cluster size)
  float dEnergy[numEemcTowerLayers]; // energy in tower cluster from Geant record
  float totalEnergy[numEemcTowerLayers]; // total energy in tower cluster from all particles from Geant record
  float totalEnergyScaled[numEemcTowerLayers]; // sampled energy in tower cluster from MuDst from all particles

  StMcCalorimeterHit* newMcHitEsmdU();
  StMcCalorimeterHit* addMcHitEsmdU(StMcCalorimeterHit *hit);
  StMcCalorimeterHit* getMcHitEsmdU(int i) { return (StMcCalorimeterHit*)mMcHitsEsmdU->At(i); }
  int getNumberOfMcHitsEsmdU() { return mMcHitsEsmdU->GetEntriesFast(); }

  StMcCalorimeterHit* newMcHitEsmdV();
  StMcCalorimeterHit* getMcHitEsmdV(int i) { return (StMcCalorimeterHit*)mMcHitsEsmdV->At(i); }
  StMcCalorimeterHit* addMcHitEsmdV(StMcCalorimeterHit *hit);
  int getNumberOfMcHitsEsmdV() { return mMcHitsEsmdV->GetEntriesFast(); }

private:
  void InitArrays();

  TClonesArray* mMcHitsEsmdU;
  TClonesArray* mMcHitsEsmdV;

  ClassDef(StEEmcDataDrivenMcReplaceInfo,1);
};

#endif
