/*
  AUTHOR
  David Kapukchyan

  PURPOSE
  The purpoe of this class is to fill the cluster and point information into a #TClonesArray of #FcsPhotonCandidate and do some basic QA on the filled cluster and point information.

  DESCRIPTION
  Grab the cluster and point collection from #StMuFcsAnaData::mMuFcsColl and save the information to #FcsEventInfo and #StMuFcsAnaData::mPhArr.

  CAVEATS
  Good to have #StMuFcsAnaData::mFoundVertex and #StMuFcsAnaData::mUseVertex set otherwise will use a vertex of zero

  LOG
  @[January 14, 2026] > First instance where relevant functionality was copied from #StMuFcsTreeMaker
  @[June 4, 2026] > Fixed error printing to print this class's name and not old 'StMuFcsTreeMaker'
  @[July 1, 2026] > Changed name from StMuFcsAnaFillClusPoint to StFwdAnaFillEcalClusPoint
*/


#ifndef STFWDANA_STFWDANAFILLECALCLUSPOINT_HH
#define STFWDANA_STFWDANAFILLECALCLUSPOINT_HH

#include "StFwdAnaVirtual.h"

class StFwdAnaFillEcalClusPoint : public StFwdAnaVirtual
{
public:
  StFwdAnaFillEcalClusPoint();
  ~StFwdAnaFillEcalClusPoint();

  virtual UInt_t LoadHists(TFile* file, HistManager* histman, StFwdAnaData* anadata);
  virtual Int_t DoMake(StFwdAnaData* anadata);

  void PaintHeatMap(TCanvas* canv, const char* savename) const;
  void PaintClusPointQa(TCanvas* canv, const char* savename)   const;
  void PaintEnergyZoom(TCanvas* canv, const char* savename) const;

  
protected:
  TH1* mH2F_PhotonHeatMap = 0;          ///< Distribution of photons in STAR x,y space
  TH1* mH2F_PhotonHeatMapG = 0;         ///< Distribution of photons in STAR x,y space when energy has a specific value near an energy spike
  TH1* mH2F_PhotonHeatMapB = 0;         ///< Distribution of photons in STAR x,y space when energy has a specific value on an energy spike

  TH1* mH1F_ClusterEnergy = 0;          ///< All Cluster energy
  TH1* mH1F_PointEnergy = 0;            ///< All Point energy
  TH1* mH1F_ClusterMult = 0;            ///< Raw cluster multiplicity in event
  TH1* mH1F_PointMult = 0;              ///< Raw point multiplicity in event
  
  ClassDef(StFwdAnaFillEcalClusPoint,1)
};

#endif

