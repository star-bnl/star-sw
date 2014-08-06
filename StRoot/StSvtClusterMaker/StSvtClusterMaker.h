// $Id: StSvtClusterMaker.h,v 1.9 2014/08/06 11:43:45 jeromel Exp $
// $Log: StSvtClusterMaker.h,v $
// Revision 1.9  2014/08/06 11:43:45  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.8  2005/08/04 04:06:54  perev
// clear of collection added
//
// Revision 1.7  2003/09/10 19:47:35  perev
// ansi corrs
//
// Revision 1.6  2003/01/28 20:28:44  munhoz
// new filters for clusters
//
// Revision 1.5  2001/09/22 01:07:09  caines
// Fixes now that AddData() is cleared everyevent
//
// Revision 1.4  2001/08/07 20:52:15  caines
// Implement better packing of svt hardware and charge values
//
// Revision 1.3  2001/04/29 20:11:57  caines
// Added reset command for Online monitor
//
// Revision 1.2  2000/08/21 13:06:58  caines
// Much improved hit finding and fitting
//
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

#include "StSvtClusterFinder.hh"

class TH1F;
class TH2F;
class TObjectSet;

class StSvtData;
class StSvtHybridData;
class StSvtClusterFinder;
class StSvtHybridCollection;
class StSvtHybridCluster;

class StSvtClusterMaker : public StMaker 
{
 public: 
  StSvtClusterMaker(const char *name="SvtCluster");
  StSvtClusterMaker( StSvtClusterMaker& clumaker);
  virtual ~StSvtClusterMaker();

  Int_t Init();
  Int_t Make();
  Int_t Finish();
  void  Clear(const char *opt="");
  
  Int_t Reset();
  Int_t GetSvtRawData();
  Int_t SetSvtCluster();
  Int_t SetHybridClusters();
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StSvtClusterMaker.h,v 1.9 2014/08/06 11:43:45 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}


 protected:

  StSvtData *mSvtEvent;               //!  
  StSvtHybridData* mHybridData ;           //!
  StSvtHybridCluster* mHybridCluster;      //!
  StSvtClusterFinder* mClusterFinder;   //!
  StSvtHybridCollection* mClusterColl;         //!
  St_ObjectSet* mClusterSet;   //!
 
  //TH1F *m_n_seq; //! No. of seq on a cluster
  // TH2F **m_time_anode_clu; //! Timebucket vs anode for clusters
  //int numOfClusters, numOfMembers;
  //Int_t mTotalNumberOfHybrids;
 
 private:

  ClassDef(StSvtClusterMaker,0)   //virtual base class for Makers

};

#endif


