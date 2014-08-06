// $Id: StSvtClusterAnalysisMaker.h,v 1.14 2014/08/06 11:43:45 jeromel Exp $
// $Log: StSvtClusterAnalysisMaker.h,v $
// Revision 1.14  2014/08/06 11:43:45  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.13  2007/07/12 20:06:50  fisyak
// Move initialization to IntRun from Init, empty GetSvtDriftCurve, clean up
//
// Revision 1.12  2004/03/18 04:02:56  caines
// Remove from global scope variables used in debug mode as they shouldnt be there and caused erratic behaviour
//
// Revision 1.11  2003/09/10 19:47:35  perev
// ansi corrs
//
// Revision 1.10  2003/01/28 20:28:09  munhoz
// new filters for clusters
//
// Revision 1.9  2002/05/09 16:55:40  munhoz
// add reading bad anodes from DB
//
// Revision 1.8  2002/04/25 20:34:51  caines
// Pass bad anode information into cluster fitter
//
// Revision 1.7  2001/09/22 01:07:09  caines
// Fixes now that AddData() is cleared everyevent
//
// Revision 1.6  2001/09/16 22:09:31  caines
// Add extra checks for when SVT isnt in every event
//
// Revision 1.5  2001/08/07 20:52:15  caines
// Implement better packing of svt hardware and charge values
//
// Revision 1.4  2001/07/19 20:42:24  caines
//  Add Reset functions
//
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
class TFile;
class TNtuple;

class StSvtHit;
class StSvtHybridCollection;
class StSvtHybridPixels;
class StSvtHybridCluster;
class StSvtHybridData;
class StSvtData;
class StSvtAnalysis;
class StSvtAnalysedHybridClusters;
class StSvtHybridBadAnodes;
 
class StSvtClusterAnalysisMaker : public StMaker
{
 public: 
  StSvtClusterAnalysisMaker(const char *name = "SvtAnalysis");
  StSvtClusterAnalysisMaker(StSvtClusterAnalysisMaker& analmaker);
  ~StSvtClusterAnalysisMaker();

  virtual Int_t InitRun(int runumber);
  virtual Int_t Make();
  virtual Int_t Finish();
  virtual void Clear(Option_t *option="");

  Int_t Reset();

  Int_t GetSvtEvent();
  Int_t GetSvtRawEvent();
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
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StSvtClusterAnalysisMaker.h,v 1.14 2014/08/06 11:43:45 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

    
 protected:

  Int_t mThreshOld;
  Int_t mOffSet;
  Int_t mNumOfClusters;
  Int_t mNumOfMembers;
  Int_t mTotNumOfClusters;
  Int_t mTotNumOfGoodClusters;
  Int_t mTotNumOfBadClusters;
  Int_t mTotalNumberOfHybrids;

  Int_t mNoEvents;


  Float_t adcArray[128*240];

  TFile *hfile;  //!
  TNtuple* ntpl;  //!

  StSvtData* mSvtAdjEvent;                    //! 
  StSvtHybridData* mHybridRawData ;           //!
  StSvtHybridData* mHybridAdjData ;           //!
  StSvtHybridPixels* mHybridPixelData ;       //!
  StSvtHybridCluster* mHybridCluster ;        //!
  StSvtHybridCollection* mSvtPixelColl;       //!
  StSvtHybridCollection* mSvtClusterColl;     //!
  StSvtHybridCollection* mSvtRawEventColl;    //! 
  StSvtHybridCollection* mSvtBadAnodeSet;     //!
  //StSvtData* mSvtClusterColl;     //!
  //StSvtData* mSvtRawEventColl;    //! 
  StSvtHybridCollection* mSvtAnalColl;        //! 
  StSvtAnalysis* mSvtAnalysis;                //!
  StSvtAnalysedHybridClusters* mSvtAnalClusters;      //! 
  StSvtHit* mSvtHit;                          //!
  StSvtHybridBadAnodes* mSvtBadAnode;                //!

  St_ObjectSet* mSvtAnalSet;                  //!

  TH1F *m_n_seq;                              //! No. of seq on a cluster
  TH1F **m_sumADC;                            //! Sum of ADC on hits
  TH1F *m_sumADC_all;                            //! Sum of ADC on hits
  TH2F *m_nClust;                             //! No. of  clusters per event
  TH2F **m_time_anode_clu;                    //! Timebucket vs anode for clusters
  TH2F **m_time_anode_raw;                    //! Timebucket vs anode for raw sequences
  TH2F *m_SumADCvsTime;                       //! Timebucket vs SUM ADC of clusters
  TH2F *m_PeakADCvsTime;                       //! Timebucket vs Peak ADC of clusters


 private:

  ClassDef(StSvtClusterAnalysisMaker,0)       //virtual base class for Makers

};


#endif
