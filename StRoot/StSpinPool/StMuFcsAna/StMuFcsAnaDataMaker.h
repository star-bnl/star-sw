/*
  AUTHOR
  David Kapukchyan

  PURPOSE
  The purpose of this class is to act as a bridge between the STAR makers and the "FcsAna" strategy framework. It will also fill the basic data structure of #StMuFcsAnaData and handle the calls of different sub-analysis modules (#StMuFcsVirtualAna). It will also manage the histogram objects created by the various sub-analysis modules by utilizing #HistManager.
  
  DESCRIPTION
  In the "FcsAna" strategy framework this class contains a #HistManager, and a #StMuFcsAnaData that sub-analysis modules will utilize. Sub-analysis modules will inherit from #StMuFcsVirtualAna and should be added to this class. Contains a #StMuFcsAnaData, a #HistManager and a vector of #StMuFcsVirtualAna objects. It will fill #StMuFcsAnaData with the relevant information by utilizing the filenames #mPolDataFilename (For polarization information taken from https://wiki.bnl.gov/rhicspin/Results_(Polarimetry)), #mFcsTrigFilename (Generated from GetTrigList.csh which is a text file of all FCS triggers names and their offline id as well as the starting and ending run number)  , #mFilename (name of file to save histograms to, passed to #mHistManager). Inside of #Init(), it will call all the #StMuFcsVirtualAna::LoadHists(). Inside of #Make() it will call all the #StMuFcsVirtualAna::DoMake() methods. The rest of the code handles everything else so all you need to do is insert your algorithm for what you want "#Make()" to do using the #addAna() call. #InitRun() will grab the FCS database object.

    Creates a histogram to keep track of make calls. Grabs the MuDstMaker pointer and the related pointers and stores them in #StMuFcsAnaData. Fills the #FcsEventInfo object in #StMuFcsAnaData

  LOG
  @[January 9, 2026] > Copied from *StMuFcsTreeMaker* and modified to do the relevant things. Since I am keeping *StMuFcsTreeMaker* please see that log for all relevant developments of the analysis code.
  @[June 8, 2026] > Implemented #StMuFcsAnaData::mEvent

*/


#ifndef STMUFCSANADATAMAKER_HH
#define STMUFCSANADATAMAKER_HH

//C/C++ Headers
#include <iostream>

//ROOT Headers
#include "TString.h"
#include "TPolyLine.h"
#include "TEllipse.h"
#include "TFile.h"
#include "TTree.h"
#include "TLeaf.h"
#include "TH1F.h"
#include "TLegend.h"
#include "TF1.h"
#include "TGeoPolygon.h"

//STAR Headers
#include "StEnumerations.h"
#include "StMaker.h"
#include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuTriggerIdCollection.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StEvent/StTriggerData.h"
#include "StEvent/StTriggerId.h"
#include "StMessMgr.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "Stypes.h"
#include "StFcsDbMaker/StFcsDbMaker.h"
#include "StFcsDbMaker/StFcsDb.h"
#include "StMuDSTMaker/COMMON/StMuFcsCollection.h"
#include "StMuDSTMaker/COMMON/StMuFcsHit.h"
#include "StMuDSTMaker/COMMON/StMuFcsCluster.h"
#include "StMuDSTMaker/COMMON/StMuFcsPoint.h"

#include "StSpinPool/StFcsTreeManager/StMuFcsPi0Data.h"
//#include "StSpinPool/StFcsPi0Ana/StMuEpdRun22QaMaker.h"
//#include "StSpinPool/StFcsPi0Ana/StFcsRun22TriggerMap.h"

#include "StMuFcsVirtualAna.h"
#include "StMuFcsAnaData.h"

class StMuFcsAnaDataMaker : public StMaker {
public:
  
  StMuFcsAnaDataMaker(const Char_t* name = "MuFcsAnaData");
  ~StMuFcsAnaDataMaker();
  
  UInt_t addAna(StMuFcsVirtualAna* ana);
  
  StMuFcsAnaData* anaData()const{ return mAnaData; }
  HistManager* getHists()const{ return mHists; }
  void setPolDataFilename(const char* name) { mPolDataFilename = name; }
  void setFcsTrigFilename(const char* name) { mFcsTrigFilename = name; }
  void setOutFilename(const char* name) { mFilename = name; }
  void setAnaData( StMuFcsAnaData* anadata);
  void setHistManager( HistManager* hm );
  
  UInt_t LoadDataFromFile(TFile* file);       ///< Load tree and histograms from file, calls #StMuFcsVirtualAna::LoadHists()

  virtual Int_t Init();
  virtual Int_t InitRun(int runnumber);
  virtual void Clear(Option_t* option="");    ///< Gets called before #Make() in #StChain::EventLoop()
  virtual Int_t Make();                       ///< Call #StMuFcsVirtualAna::DoMake() on internal analysis modules
  virtual Int_t Finish();
  
protected:
  TString mPolDataFilename = "";                ///< File to read Polarization information
  TString mFcsTrigFilename = "";                ///< File to read Fcs trigger information
  TString mFilename = "";                       ///< File name to save to. Passed to #mHists
  StMuFcsAnaData* mAnaData = 0;                 ///< Data structure that contains the pointers to the needed makers
  HistManager* mHists = 0;                      ///< Manage loading and saving histograms

  TH1* mH1D_Entries = 0;                       ///< Number of events processed no cuts (i.e. "Make" calls)
  TH1* mH1F_RndmSpin = 0;               ///< Single bin histogram to know if random spins are being used or grabbing from database
  
private:
  bool mInternalHists = false;                  ///< Boolean to keep track if mHists was added externally or an internal one was created
  std::vector<StMuFcsVirtualAna*> mAnaList;     ///< List of analysis modules whose calls are executed sequentially

  ClassDef(StMuFcsAnaDataMaker, 1)
};

#endif
