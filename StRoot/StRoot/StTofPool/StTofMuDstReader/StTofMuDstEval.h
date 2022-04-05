#ifndef StTofMuDstEval_HH
#define StTofMuDstEval_HH

#ifndef StMaker_H
#include "StMaker.h"
#endif

#define VHRBIN2PS 24.414  // Very High resolution mode, pico-second per bin
                          // 1000*25/1024 (ps/chn)
#define HRBIN2PS 97.656   // High resolution mode, pico-second per bin
                          // 97.65625= 1000*100/1024  (ps/chn)


class StMuTrack;
class StMuEvent;
class StMuTofHit;
class TRandom;
class StMuEvent;
class StMuDst;
class StFileI;
class TChain;
class TClonesArray;
class StMuDstMaker;
class StEvent;
class StTofCollection;
class StSortTofRawData;
class StTofrGeometry;

#include "TFile.h"
#include "TObject.h"
#include "TTree.h"
#include "TBranch.h"
#include "TClonesArray.h"


#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif



class StTofMuDstEval : public StMaker {
  
 protected:
  
 public:

  StMaker* currentChain;
  StTofMuDstEval(const char* name = "StTofMuDstEval",
		 StMuDstMaker *maker = 0);
  ~StTofMuDstEval();
  void  Clear(const char* opt="");
  Int_t Init();
  
  
  Int_t Make();
  Int_t Finish();
  
  void  GetPvpdNHits(int &nEast, int &nWest);
  Float_t  GetUncorrectedTot(StMuTofHit *tofHit);
  void  GetLocalHitPosition(StMuTofHit *tofHit, Double_t* local);
  
 private:
  static const Int_t mNPVPD = 6;
  static const Int_t mNTOFr5 = 192;
  
  StMuDstMaker  *mMuDstMaker;
  StMuDst       *mMuDst;
  
  StEvent *mEvent;
  StTofCollection *mTofCollection;
  StSortTofRawData *mSortTofRawData;
  StTofrGeometry   *mTofrGeom;
  
  virtual const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StTofMuDstEval.h,v 1.2 2014/08/06 11:43:47 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;}	
  
  // the following is a ROOT macro  that is needed in all ROOT accessible code
  ClassDef(StTofMuDstEval, 1)
    
};

#endif
 
