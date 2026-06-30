/*
  AUTHOR
  David Kapukchyan

  PURPOSE
  Cross check that the FCS clusters and points in StEvent matches the FCS clusters and points in StMuEvent since the cluster parent information is missing from #StMuFcsCollection

  DESCRIPTION
  Grab the cluster and point collection from StEvent and print out parent cluster information. This way I can try to  match with the parent cluster information from #StMuFcsCollection

  CONCLUSIONS
  @[February 23, 2026] > Sometimes, looking at StMuFcsPoint->nParentPoints() is correct but StMuFcsCluster->nPoints() is not correct


  LOG
  @[Februrary 23, 2026] > First instance where relevant functionality was copied from #StMuFcsAnaFillClusPoint and modified to grab information from StEvent
  @[May 29, 2026] > Commented out printing for when I needed to test why the MuDsts did not contain the point-cluster associations

*/


#ifndef STMUFCSANACHECKFILLCLUSPOINT_HH
#define STMUFCSANACHECKFILLCLUSPOINT_HH

//#include "StEnumerations.h"
//#include "StContainers.h"

#include "StMuFcsVirtualAna.h"

class StMuFcsAnaCheckFillClusPoint : public StMuFcsVirtualAna
{
public:
  StMuFcsAnaCheckFillClusPoint();
  ~StMuFcsAnaCheckFillClusPoint();

  virtual UInt_t LoadHists(TFile* file, HistManager* histman, StMuFcsAnaData* anadata);
  virtual Int_t DoMake(StMuFcsAnaData* mufcsdata);
  
  ClassDef(StMuFcsAnaCheckFillClusPoint,1)
};

#endif

