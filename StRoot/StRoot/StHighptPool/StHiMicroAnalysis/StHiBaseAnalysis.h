#ifndef StHiBaseAnalysis_H
#define StHiBaseAnalysis_H

#include "Stiostream.h"
#include <utility>
#include <cstdlib>

#include "StHighptPool/StHiMicroEvent/StHiMicroEvent.h"
#include "StHighptPool/Common/Centrality.h"
#include "StHighptPool/Common/Name.h"
#include "StHighptPool/Common/CutRc.h"
#include "StHighptPool/Common/Cut.h"
#include "StHighptPool/Common/IO.h"
#include "StHighptPool/Common/Bin.h"
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
#include "Stiostream.h"
#include <cstdlib>


class StHiBaseAnalysis : public TObject{
 public:
  StHiBaseAnalysis(const char* inDir, const char* outRootName);
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
