// $Id: StSvtClusterAnalysisMaker.h,v 1.3 2001/04/04 19:12:01 didenko Exp $
// $Log: StSvtClusterAnalysisMaker.h,v $
// Revision 1.3  2001/04/04 19:12:01  didenko
// remove ! from comments to create Streamer
//
// Revision 1.2  2000/08/21 13:06:58  caines
// Much improved hit finding and fitting
//
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


class TH1F;
class TH2F;
class TObjectSet;

class StSvtHit;
class StSvtHybridCollection;
class StSvtHybridPixels;
class StSvtHybridCluster;
class StSvtHybridData;
class StSvtData;
class StSvtAnalysis;
class StSvtAnalysedHybridClusters;
 
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
  Int_t GetSvtPixels();
  Int_t GetSvtCluster();
  Int_t SetSvtAnalysis();

  Int_t GetRawData(int index);
  Int_t GetPixelData(int index); 
  Int_t GetTotNumOfClu();

  Int_t SetThreshOld(Int_t thresh, Int_t offset);
  Int_t CreateClusterHist(Int_t tNuOfHyb); // Tracking histograms 
  Int_t SetClusterAnalysis();
  void  printClusterInfo();
  void MakeHistograms(); // Tracking histograms
    
 protected:

  Int_t mThreshOld;
  Int_t mOffSet;
  Int_t mEventNum;
  Int_t mNumOfClusters;
  Int_t mNumOfMembers;
  Int_t mTotNumOfClusters;
  Int_t mTotNumOfGoodClusters;
  Int_t mTotNumOfBadClusters;
  Int_t mTotalNumberOfHybrids;

  Int_t mNoEvents;


  Float_t adcArray[128*240];
  Char_t* mDataType;

  StSvtData* mSvtAdjEvent;                    //! 
  StSvtHybridData* mHybridRawData ;           //!
  StSvtHybridData* mHybridAdjData ;           //!
  StSvtHybridPixels* mHybridPixelData ;       //!
  StSvtHybridCluster* mHybridCluster ;        //!
  StSvtHybridCollection* mSvtPixelColl;       //!
  StSvtHybridCollection* mSvtClusterColl;     //!
  StSvtHybridCollection* mSvtRawEventColl;    //! 
  //StSvtData* mSvtClusterColl;     //!
  //StSvtData* mSvtRawEventColl;    //! 
  StSvtHybridCollection* mSvtAnalColl;        //! 
  StSvtAnalysis* mSvtAnalysis;                //!
  StSvtAnalysedHybridClusters* mSvtAnalClusters;      //! 
  StSvtHit* mSvtHit;                          //!
 
  St_ObjectSet* mSvtAnalSet;                  //!

  TH1F *m_n_seq;                              //! No. of seq on a cluster
  TH1F **m_sumADC;                            //! Sum of ADC on hits
  TH2F *m_nClust;                             //! No. of  clusters per event
  TH2F **m_time_anode_clu;                    //! Timebucket vs anode for clusters
  TH2F **m_time_anode_raw;                    //! Timebucket vs anode for raw sequences
  TH2F *m_SumADCvsTime;                       //! Timebucket vs SUM ADC of clusters



 private:

  ClassDef(StSvtClusterAnalysisMaker,1)       //virtual base class for Makers

};


#endif
