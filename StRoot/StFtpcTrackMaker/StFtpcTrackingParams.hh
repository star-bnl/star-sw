#ifndef STAR_StFtpcTrackingParams
#define STAR_StFtpcTrackingParams

#ifndef ROOT_Rtypes
#include "Rtypes.h"
#endif

class StFtpcTrackingParams
{
 private:

  static StFtpcTrackingParams* mInstance;
  
  // Pion mass
  Float_t mM_pi;

  // FTPC geometry
  Float_t mInnerRadius;
  Float_t mOuterRadius;
  Float_t mPadRowPosZ[10];

  // Vertex position
  Float_t mMaxVertexPosZWarning;
  Float_t mMaxVertexPosZError;

  // Vertex reconstruction
    Int_t mHistoBins;
  Float_t mHistoMin;
  Float_t mHistoMax;

  // Tracker
  Int_t mRowSegments;
  Int_t mPhiSegments;
  Int_t mEtaSegments;
  
  // Tracking
   Bool_t mLaser[4];
   Bool_t mVertexConstraint[4];
    Int_t mMaxTrackletLength[4];
    Int_t mMinTrackLength[4];
    Int_t mRowScopeTracklet[4];
    Int_t mRowScopeTrack[4];
    Int_t mPhiScope[4];
    Int_t mEtaScope[4];
  Float_t mMaxDca[4];

  // Tracklets
  Float_t mMaxAngleTracklet[4];

  // Tracks
  Float_t mMaxAngleTrack[4];
  Float_t mMaxCircleDist[4];
  Float_t mMaxLengthDist[4];
  
  // Split tracks
  Float_t mMaxDist;
  Float_t mMinPointRatio;
  Float_t mMaxPointRatio;

  // dE/dx
  Int_t mDebugLevel;
  Int_t mNoAngle;
  Int_t mMaxHit; 
  Int_t mMinHit;

  Double_t mPadLength;
  Double_t mFracTrunc;
  Double_t mALargeNumber;


 protected:
  
  StFtpcTrackingParams();
  virtual ~StFtpcTrackingParams();

  void PrintParams();
  Int_t InitdEdx();

 public:

  static StFtpcTrackingParams* Instance(Bool_t debug = kFALSE);

  Int_t Init();
  Int_t InitFromFile();
  
  // Pion mass
  Float_t m_pi();

  // FTPC geometry
  Float_t InnerRadius();
  Float_t OuterRadius();
  Float_t PadRowPosZ(Int_t row);

  // Vertex position
  Float_t MaxVertexPosZWarning();
  Float_t MaxVertexPosZError();

  // Vertex reconstruction
    Int_t HistoBins();
  Float_t HistoMin();
  Float_t HistoMax();

  // Tracker
  Int_t RowSegments();
  Int_t PhiSegments();
  Int_t EtaSegments();
  
  // Tracking
   Bool_t Laser(Int_t tracking_method);
   Bool_t VertexConstraint(Int_t tracking_method);
    Int_t MaxTrackletLength(Int_t tracking_method);
    Int_t MinTrackLength(Int_t tracking_method);
    Int_t RowScopeTracklet(Int_t tracking_method);
    Int_t RowScopeTrack(Int_t tracking_method);
    Int_t PhiScope(Int_t tracking_method);
    Int_t EtaScope(Int_t tracking_method);
  Float_t MaxDca(Int_t tracking_method);

  // Tracklets
  Float_t MaxAngleTracklet(Int_t tracking_method);

  // Tracks
  Float_t MaxAngleTrack(Int_t tracking_method);
  Float_t MaxCircleDist(Int_t tracking_method);
  Float_t MaxLengthDist(Int_t tracking_method);
  
  // Split tracks
  Float_t MaxDist();
  Float_t MinPointRatio();
  Float_t MaxPointRatio();

  // dE/dx
  Int_t DebugLevel();
  Int_t NoAngle();
  Int_t MaxHit();
  Int_t MinHit();
  
  Double_t PadLength();
  Double_t FracTrunc();
  Double_t ALargeNumber();

  ClassDef(StFtpcTrackingParams,0)  // Parameters for FTPC tracking
};    

#endif
