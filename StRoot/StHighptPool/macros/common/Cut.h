#ifndef Cut_H
#define Cut_H

#include "TObject.h"
//#include "Rtypes.h"
#include "Centrality.h"
#include "TMath.h"

class StMiniMcEvent;
class StTinyRcTrack;
class StTinyMcTrack;
class StMiniMcPair;

class Cut {
 public:

  //  Cut();
  // virtual ~Cut();

  //
  // event cuts
  //
  //--- centrality
  static void SetZdcCtbCent(NchCentrality min, NchCentrality max) {
    mZdcCtbCent[0] = min; mZdcCtbCent[1] = max; 
  }

  static void SetFlowCent(int min, int max) {
    mFlowCent[0] = min; mFlowCent[1] = max;
  }

  static void SetHMinusCent(int min, int max) {
    mHMinusCent[0]= min; mHMinusCent[1]=max;
  }

  static void SetNchCent(int min, int max) {
    mNchCent[0] = min; mNchCent[1] = max;
  }

  static bool DoZdcCtbCent() { return mDoZdcCtbCent; }
  static bool DoFlowCent() { return mDoFlowCent; }
  static bool DoHMinusCent() {  return mDoHMinusCent; }

  // have both the spectra centrality and the mc centrality 
  // in the cut definition.
  //
  static void SetDoSpectraCent(bool a=true) { mDoSpectraCent=a; }
  static void SetDoOtherCent(bool a=true) { mDoSpectraCent= !a; }

  //----

  static void SetVertexZ(Float_t min,Float_t max) {
    mVertexZ[0] = min; mVertexZ[1] = max;
  }

  static void SetVertexZSkip(Float_t val){
    mVertexZSkip=fabs(val); 
  }
  //
  // track cuts
  //

  static void SetHalf(char half);
  static void SetHitHalf(char half);
  static void SetGeomHalf(char half);

  static char Half() { return mHalf; }
  static char HitHalf() { return mHitHalf; }
  static char GeomHalf() { return mGeomHalf; }
  static bool VertexSkipOn() { return (mVertexZSkip); }

  
  static void SetHitAvoid(Float_t val){
    mHitAvoid=val;
  }
  //  static Float_t HitVoid() { return mHitAvoid; }

  static void SetEta(Float_t min,Float_t max) {
    mEta[0] = min; mEta[1] = max;
  }

  static void SetFitPts(Int_t min, Int_t max) {
    mFitPts[0] = min; mFitPts[1] = max;
  }

  static void SetSDcaGl(Float_t min, Float_t max) {
    mSDcaGl[0] = min; mSDcaGl[1] = max;
  }

  static void SetDcaPr(Float_t min, Float_t max) {
    mDcaPr[0] = min; mDcaPr[1] = max;
  }

  static void SetDcaXYGl(Float_t min, Float_t max) {
    mDcaXYGl[0] = min; mDcaXYGl[1] = max;
  }

  static void SetMcPts(Int_t min, Int_t max){
    mMcPts[0] = min, mMcPts[1] = max;
  }

  static void SetEtaTight(Float_t min, Float_t max){
    mEtaTight[0] = min, mEtaTight[1] = max;
  }

  
  static void SetCut(Int_t);


  //static bool IsSameSide(Float_t vertexZ, Float_t dip){
  //   return(vertexZ*(vertexZ + 200*TMath::Tan(dip))>0);
  // }  

  // for east west
  static bool IsSameSide(Float_t vertexZ, Float_t firstZ,Float_t lastZ){
    bool isOut = !(mVertexZSkip && !IsOutSide(firstZ,lastZ,mVertexZSkip));
    return (vertexZ*lastZ>0 && vertexZ*firstZ>0 && isOut);
  }

  static bool IsGeomSameSide(Float_t vertexZ,Float_t dipAngle){
    float z = vertexZ+192*tan(dipAngle);
    bool isSame=false;
    switch(mGeomHalf){
    case 'e': if(z<0 && vertexZ<0) isSame=true; break;
    case 'w' : if(z>0 && vertexZ>0) isSame=true; break;
    case 's' : if(z*vertexZ>0) isSame=true; break;
    default : exit(-1);
    }
    return isSame;
  }

  static bool IsHitSameSide(Float_t firstZ,Float_t lastZ){
    bool isSame=false;
    switch(mHitHalf){
    case 'e': if(firstZ<0 && lastZ<0) isSame=true; break;
    case 'w' : if(firstZ>0 && lastZ>0) isSame=true; break;
    case 's' : if(firstZ*lastZ>0) isSame=true; break;
    default : exit(-1);
    }
    return isSame;
    
  }

  static bool IsOutSide(Float_t firstZ, Float_t lastZ, Float_t val){
    return (fabs(firstZ)>fabs(val) && fabs(lastZ)>fabs(val) 
	    && firstZ*lastZ>0); 
  }

  
  static void ShowCuts();


  //
  // primitives
  //

  static NchCentrality   mZdcCtbCent[2]; //!
  static Int_t   mFlowCent[2]; //!
  static Int_t   mHMinusCent[2]; //!
  //  static Int_t   mZdcHMinusCent[2] ; //!
  static Int_t   mNchCent[2];//!

  static bool    mDoZdcCtbCent; //!
  static bool    mDoFlowCent; //!
  static bool    mDoHMinusCent; //!
  //  static bool    mDoZdcHMinusCent; //!
  static bool    mDoNchCent; //!
  static bool    mDoNchCentKludge; //!

  static bool mDoSpectraCent; //!


  static Float_t mVertexZ[2]; //!
  static Float_t mVertexZSkip; //!
  
  static Float_t mEta[2]; //!
  static Int_t   mFitPts[2]; //!
  static Float_t mSDcaGl[2]; //!
  static Float_t mDcaPr[2]; //!
  static Float_t mDcaXYGl[2]; //!
  static Int_t   mMcPts[2]; //!
  static Float_t mEtaTight[2]; //!
  
  static char    mHalf; //!
  static char    mHitHalf; //!
  static char    mGeomHalf; //!
  static Float_t mHitAvoid; //!

  static Int_t   mCut; //!

 private:

  ClassDef(Cut,1)

};






#endif
