//#Based Qinghua's StLamPhiMaker.h, I add the lambda tree("tlam") to store the information of lambda reconstruction. The production is a nano root file with some naive cuts. It is used for further analysis.

#ifndef StV0sMaker_h
#define StV0sMaker_h
#include "StMaker.h"
#include <string>
#include "StTriggerIdCollection.h"
#include "StTriggerId.h"
#include "TTree.h"
#include "TChain.h"
#include "TSystem.h"
#include "TFile.h"
#include "TMemStat.h"
#include <iostream>
#include <fstream>
#include <stdlib.h>
#include "StarClassLibrary/SystemOfUnits.h"
#include "StarClassLibrary/StHelix.hh"
#include "StarClassLibrary/StThreeVectorD.hh"
#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"

#include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"
#include "StEmcTriggerMaker/StEmcTriggerMaker.h"
#include "StEmcTriggerMaker/StBemcTrigger.h"

class TFile;
class TTree;
class StMuDst;
class StMuDstMaker;
class StSpinDbMaker;
//class StEmcTriggerMaker;

using namespace std;

class StV0sMaker : public StMaker {
 public:
  enum V0Type {kUnknown, kLambda, kAntiLambda, kK0s};
  StV0sMaker(const Char_t *name, StMuDstMaker* uDstMaker, const char *outputFile, V0Type type = kUnknown) :
    StMaker(name), muDstMaker(uDstMaker),    outName(outputFile), mType(type){    }
    virtual ~StV0sMaker() {}
  
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();

  StSpinDbMaker *spDb;
  //StEmcTriggerMaker *trgMaker;
#if 0
  static Double_t TpcnSigma(StMuTrack *gTrack, Double_t mass = 0.13956995, Double_t charge = 1);
  static Double_t nSigmaProton(StMuTrack *gTrack) {return TpcnSigma(gTrack,0.93827231);}
  static Double_t nSigmaPion(StMuTrack *gTrack) {return TpcnSigma(gTrack);}
#endif
 protected:
  void InitFile(void);
  void FinishFile(void);
  
 protected:
  StMuDstMaker*   muDstMaker;  
  
 private:
  const char*  outName;    
  V0Type       mType;
  StMuDst*     mu;      
  TFile        *m_outfile; 
  TTree        *picoTree;
  
  TH2D         *Hpdedx;
  TH2D         *Hgdedx;
  TH1D         *Hptrack;
  TH1D         *Hgtrack;
  TH1D         *Hpeta;
  TH1D         *Hgeta;
  TH1D         *Hppt;
  TH1D         *Hgpt;
  TH1D         *Hphit;
  TH1D         *Hghit;
  TH1D         *Hpdca;
  TH1D         *Hgdca;
  
  static const int kMaxLam = 200;
  //event information
  int runID,eventID,eventNum;
  int bunchID,spin,spinbit,spinbit8,bx7,bx48;
  float magn;
  float bbcrate,zdcrate;
  int bbcTimebin;  
  float pvx,pvy,pvz;
  int npv,ngtra,nptra;
  int nlam;

  int numtrig;
  int trigger[kMaxLam];
   
  //Vertex's information
  int pvrank[kMaxLam];
  float npvx[kMaxLam];
  float npvy[kMaxLam];
  float npvz[kMaxLam];

  //p track's information
  int phit[kMaxLam];
  int phitpos[kMaxLam];
  float pdca[kMaxLam];
  float psigma[kMaxLam];
  float pdedx[kMaxLam];
  float ppx[kMaxLam];
  float ppy[kMaxLam];
  float ppz[kMaxLam];
  float ptrackpx[kMaxLam];
  float ptrackpy[kMaxLam];
  float ptrackpz[kMaxLam];
  float pLength[kMaxLam];
  float pMom[kMaxLam];
  
  //pi track's information
  int pihit[kMaxLam];
  int pihitpos[kMaxLam];
  float pidca[kMaxLam];
  float pisigma[kMaxLam];
  float pidedx[kMaxLam];
  float pipx[kMaxLam];
  float pipy[kMaxLam];
  float pipz[kMaxLam];
  float pitrackpx[kMaxLam];
  float pitrackpy[kMaxLam];
  float pitrackpz[kMaxLam];
  float piLength[kMaxLam];
  float piMom[kMaxLam];
  //lambda candidate's information
  float mass[kMaxLam];
  float dca2[kMaxLam];
  float length[kMaxLam];
  float vleng[kMaxLam];
  float dcaV0[kMaxLam];
  float V0x[kMaxLam];
  float V0y[kMaxLam];
  float V0z[kMaxLam];
  float V0px[kMaxLam];
  float V0py[kMaxLam];
  float V0pz[kMaxLam];
    
  ClassDef(StV0sMaker,0)
    };

class StAntilamMaker : public StV0sMaker {
 public:
  StAntilamMaker(const Char_t *name, StMuDstMaker* uDstMaker, const char *outputFile): StV0sMaker(name, uDstMaker, outputFile, kAntiLambda) {}
    virtual ~StAntilamMaker() {}
    
  ClassDef(StAntilamMaker,0)
    };
class StLambdaMaker : public StV0sMaker {
 public:
  StLambdaMaker(const Char_t *name, StMuDstMaker* uDstMaker, const char *outputFile): StV0sMaker(name, uDstMaker, outputFile, kLambda) {}
    virtual ~StLambdaMaker() {}
    
  ClassDef(StLambdaMaker,0)
    };
class StK0sMaker : public StV0sMaker {
 public:
  StK0sMaker(const Char_t *name, StMuDstMaker* uDstMaker, const char *outputFile): StV0sMaker(name, uDstMaker, outputFile, kK0s) {}
    virtual ~StK0sMaker() {}
    
  ClassDef(StK0sMaker,0)
    };

#endif
