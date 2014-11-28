#ifndef Cut_H
#define Cut_H

#include "TObject.h"
#include "Centrality.h"
#include "TMath.h"

//class StMiniMcEvent;
//class StTinyRcTrack;
//class StTinyMcTrack;
//class StMiniMcPair;
#ifndef ST_NO_NAMESPACES
using namespace std;
#endif

const float geomCut=198.;	//Central membrane info?

class Cut {
 public:

  //  Cut();
  // virtual ~Cut();

  //
  // event cuts
  //
  //--- centrality

  static const char* SetCuts(const char* cutList);

  static void DecodeCutList(const char* cutList);
  static void ShowCuts();

  static void SetCentCut(const char centType);
  static void SetZVertexCut(const char zVtxCut);
  static void SetEtaCut(const char etaCut);
  static void SetFitPtsCut(const char fitPtsCut);
  static void SetDCACut(const char dcaCut);
  static void SetSameSectorCut(const char sameSectorCut);
  static void SetCrossCMCut(const char crossCMCut);

  static void SetFlowCent(int min, int max) {
    mFlowCent[0] = min; mFlowCent[1] = max;
  }

  static void SetZDCSum(int min, int max) {
    mZDCSum[0] = min; mZDCSum[1] = max;
  }

  static void SetVertexZ(Float_t min,Float_t max) {
    mVertexZ[0] = min; mVertexZ[1] = max;
  }

  //
  // track cuts
  //

  static void SetEta(Float_t min,Float_t max) {
    mEta[0] = min; mEta[1] = max;
  }

  static void SetFitPts(Int_t min, Int_t max) {
    mFitPts[0] = min; mFitPts[1] = max;
  }

  static void SetSDcaGl(Float_t min, Float_t max) {
    mSDcaGl[0] = min; mSDcaGl[1] = max;
  }

  static void SetDcaXYGl(Float_t min, Float_t max) {
    mDcaXYGl[0] = min; mDcaXYGl[1] = max;
  }

  //
  // primitives
  //
  static Int_t   mFlowCent[2]; //!
  static Float_t mVertexZ[2]; //!
  static Float_t mZDCSum[2]; //!
  
  static Float_t mEta[2]; //!
  static Int_t   mFitPts[2]; //!
  static Float_t mSDcaGl[2]; //!
  static Float_t mDcaXYGl[2]; //!
  static Float_t mCross[2]; //!
  static Int_t   mFirstPadrow[2]; //!
  static Bool_t   mSameSector; //!
  static Bool_t   mCrossCM; //!

 private:

  ClassDef(Cut,1)

};

#endif
