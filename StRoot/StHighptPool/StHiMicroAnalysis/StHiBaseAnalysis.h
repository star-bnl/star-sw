/***************************************************************************
 *
 * $Id: StHiBaseAnalysis.h,v 1.1 2002/04/02 20:05:18 jklay Exp $                                    
 *
 * Author: Bum Choi, UT Austin, Apr 2002
 *
 ***************************************************************************
 *
 * Description:  Class to perform basic I/O and functions for doing
 *		 highpt Analysis on highpt uDSTs
 *
 *
 ***************************************************************************
 * 
 * $Log: StHiBaseAnalysis.h,v $
 * Revision 1.1  2002/04/02 20:05:18  jklay
 * Bums analysis tools for highpt uDSTs
 *
 *
 **************************************************************************/
#ifndef StHiBaseAnalysis_H
#define StHiBaseAnalysis_H

#include <iostream>
#include <utility>
#include <cstdlib>

#include "StHiMicroEvent/StHiMicroEvent.h"
#include "Centrality.h"
#include "Name.h"
#include "CutRc.h"
#include "IO.h"
#include "Bin.h"
#include "TObject.h"
#include "TString.h"
#include "TArrayD.h"
#include "TSystem.h"
#include "TChain.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"
#include "TProfile2D.h"
#include "TFile.h"
#include "TStyle.h"
#include "TBenchmark.h"
#include "TString.h"
#include "TF1.h"
#include "TF2.h"
#include "TRandom.h"
#include "TGraphAsymmErrors.h"



#include <utility>
#include <fstream.h>
#include <cstdlib>


class StHiBaseAnalysis : public TObject{
 public:
  StHiBaseAnalysis(const char* inputDir="./", 
	       const char* outRootName="hianalysis.hist.root");
  virtual ~StHiBaseAnalysis();

  virtual Int_t Init();
  virtual void  Run();
  virtual void  Finish();

  void setNEvent(Int_t n) { mNEvent = n; }
  void setNFile(Int_t n) {mNFile = n; }
  void setDebug(Int_t a=1) { mDebug = a; }

 protected:

  virtual Int_t initMore();
  virtual void initChain();
  //  virtual void initHalf();
  virtual void initHistograms() = 0; // must override
  virtual void fillEventHistograms();
  virtual void finishHistograms();

  Bool_t trackOk(StHiMicroTrack*);
  virtual Bool_t acceptEvent(StHiMicroEvent*);

  virtual void trackLoop();

  TChain*            mHiMicroChain; //!
  StHiMicroEvent*    mHiMicroEvent; //!
  TString            mInputDir; //!
  TFile*             mOutRootFile; //!
  TString            mOutRootName; //!   
  TBenchmark*        mBenchmark; //!

  Int_t              mNEvent; //!
  Int_t              mNFile;  //!
  
  Int_t              mDebug; //!
  Int_t              mNEventAccepted; //!
  Int_t              mNHiPtTrack; //!
    
  
  

  ClassDef(StHiBaseAnalysis,1)
};

#endif
