// $Id: StSvtClusterMaker.h,v 1.1 2000/07/06 03:50:34 caines Exp $
// $Log: StSvtClusterMaker.h,v $
// Revision 1.1  2000/07/06 03:50:34  caines
// First version of cluster finder and fitter
//
//
#ifndef STAR_StSvtClusterMaker
#define STAR_StSvtClusterMaker
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StSvtClusterObjAnalMaker virtual base class for Maker                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif 

#include "StSvtCluster.hh"
#include "StSvtClusterFinder.hh"

class TH1F;
class TH2F;
class TObjectSet;

class StSvtData;
class StSvtHybridData;
class StSvtHybridCluster;
class StSvtClusterFinder;

class StSvtClusterMaker : public StMaker 
{
 public: 
  StSvtClusterMaker(const char *name="SvtCluster");
  StSvtClusterMaker( StSvtClusterMaker& clumaker);
  virtual ~StSvtClusterMaker();

  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();

  Int_t SetSvtCluster();
  Int_t SetHybridClusters();

 protected:

  StSvtData *mSvtEvent;               //!  
  StSvtHybridData* mHybridData ;           //!
  StSvtHybridCluster* mHybridCluster ;      //!
  StSvtClusterFinder* mClusterFinder;   //!
  StSvtCluster* mCluster;         //!
  St_ObjectSet* mClusterSet;   //!
 
  //TH1F *m_n_seq; //! No. of seq on a cluster
  // TH2F **m_time_anode_clu; //! Timebucket vs anode for clusters
  //int numOfClusters, numOfMembers;
  //Int_t mTotalNumberOfHybrids;
 
 private:

  ClassDef(StSvtClusterMaker,1)   //virtual base class for Makers

};

#endif


