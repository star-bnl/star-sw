// $Id: StSvtClusterAnalysisMaker.h,v 1.1 2000/07/06 03:50:33 caines Exp $
// $Log: StSvtClusterAnalysisMaker.h,v $
// Revision 1.1  2000/07/06 03:50:33  caines
// First version of cluster finder and fitter
//
//
#ifndef STAR_StSvtClusterAnalysis
#define STAR_StSvtClusterAnalysis
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StSvtClusterAnalysis  base class                                     //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "tables/St_scs_spt_Table.h"

class St_scs_spt;

class TH1F;
class TH2F;

class StSvtHybridCluster;
class StSvtHybridData;
class StSvtCluster;
class StSvtData;
class StSvtAnalysis;
 
class StSvtClusterAnalysisMaker : public StMaker
{
 public: 
  StSvtClusterAnalysisMaker(const char *name = "SvtAnalysis");
  StSvtClusterAnalysisMaker(StSvtClusterAnalysisMaker& analmaker);
  ~StSvtClusterAnalysisMaker();

  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();


  Int_t GetSvtEvent();
  Int_t GetSvtCluster();

  Int_t CreateClusterHist(Int_t tNuOfHyb); // Tracking histograms 
  Int_t SetClusterAnalysis();
  void MakeHistograms(); // Tracking histograms

  void SaveIntoTable(int numOfCluster, int barrel, int ladder, int wafer,
		     int hybrid);
   
    
 protected:
  StSvtData* mSvtEvent;      //!     
  StSvtHybridData* mHybridData ;           //!
  StSvtHybridCluster* mHybridCluster ;      //!
  StSvtCluster* mSvtCluster;         //!
  StSvtAnalysis* mSvtAnalysis;       //!
 
  TH1F *m_n_seq; //! No. of seq on a cluster
  TH1F **m_sumADC; //! Sum of ADC on hits
  TH2F *m_nClust; //! No. of  clusters per event
  TH2F **m_time_anode_clu; //! Timebucket vs anode for clusters
  TH2F **m_time_anode_raw; //! Timebucket vs anode for raw sequences
  TH2F *m_SumADCvsTime; //! Timebucket vs SUM ADC of clusters

  int numOfClust, numOfMembers;
  int mTotalNumberOfHybrids;
  int mNoEvents;



 private:

  ClassDef(StSvtClusterAnalysisMaker,1)   //virtual base class for Makers

};


#endif
