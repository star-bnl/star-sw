// $Id: StFtpcTrackingParams.hh,v 1.5 2002/10/11 15:45:47 oldi Exp $
// $Log: StFtpcTrackingParams.hh,v $
// Revision 1.5  2002/10/11 15:45:47  oldi
// Get FTPC geometry and dimensions from database.
// No field fit activated: Returns momentum = 0 but fits a helix.
// Bug in TrackMaker fixed (events with z_vertex > outer_ftpc_radius were cut).
// QA histograms corrected (0 was supressed).
// Code cleanup (several lines of code changed due to *params -> Instance()).
// cout -> gMessMgr.
//
// Revision 1.4  2002/10/03 10:34:08  oldi
// Usage of gufld removed.
// Magnetic field is read by StMagUtilities, now.
//
// Revision 1.3  2002/06/07 06:00:40  oldi
// New value for rotation angle of FTPC east after temperature offset was corrected.
//

#ifndef STAR_StFtpcTrackingParams
#define STAR_StFtpcTrackingParams

#ifndef ROOT_Rtypes
#include "Rtypes.h"
#endif

// SCL
#include "SystemOfUnits.h"
#include "StThreeVector.hh"
#include "StMatrixD.hh"

#include "tables/St_ftpcDimensions_Table.h"
#include "tables/St_ftpcPadrowZ_Table.h"

#include "StDbUtilities/StMagUtilities.h"
//#include "StTpcDb/StTpcDb.h"

class St_ftpcDimensions;
class St_ftpcPadrowZ;
class StGlobalCoordinate;
class StFtpcLocalCoordinate;
class StTpcDb;

class StFtpcTrackingParams
{
 private:

  static StFtpcTrackingParams* mInstance;
  
  // FTPC geometry
  Float_t  mInnerRadius;
  Float_t  mOuterRadius;
  Int_t    mNumberOfPadRows;
  Int_t    mNumberOfPadRowsPerSide;
  Float_t *mPadRowPosZ;

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

  // transformation due to rotated and displaced TPC
  StMatrixD mTpcToGlobalRotation; // (3X3)
  StMatrixD mGlobalToTpcRotation; // (3X3)
  StThreeVectorD mTpcPositionInGlobal; 

  // internal FTPC rotation (East only)
  StMatrixD mFtpcRotation;
  StMatrixD mFtpcRotationInverse;
  Double_t mInstallationPointZ;
  Double_t mObservedVertexOffsetY;
  
  //StTpcDb*                gTpcDbPtr; // pointer to TPC database
  StMagUtilities *mMagField;  // pointer to magnetic field table
  Float_t mMagFieldFactor;

 protected:
  
  StFtpcTrackingParams(St_ftpcDimensions *dimensions = 0, 
		       St_ftpcPadrowZ *padrow_z = 0);

  void PrintParams();
  Int_t InitdEdx();
  Int_t ResetMagField(TDataSet *RunLog = 0);

 public:

  static StFtpcTrackingParams* Instance(Bool_t debug, 
					St_ftpcDimensions *dimensions, 
					St_ftpcPadrowZ *padrow_z, 
					TDataSet *RunLog);
  static StFtpcTrackingParams* Instance(Bool_t debug, 
					TDataSet *RunLog);
  static StFtpcTrackingParams* Instance();

  virtual ~StFtpcTrackingParams();

  Int_t Init();
  Int_t InitFromFile();
  
  // FTPC geometry
  Float_t InnerRadius();
  Float_t OuterRadius();
  Int_t NumberOfPadRows();
  Int_t NumberOfPadRowsPerSide();
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

  // transformation due to rotated and displaced TPC
  StMatrixD TpcToGlobalRotation();
  StMatrixD GlobalToTpcRotation();
  StThreeVectorD TpcPositionInGlobal(); 

  StMatrixD FtpcRotation();
  StMatrixD FtpcRotationInverse();
  Double_t InstallationPointZ();
  Double_t ObservedVertexOffsetY();

  // magnetic field table
  StMagUtilities *MagField();
  Float_t MagFieldFactor();

  ClassDef(StFtpcTrackingParams,0)  // Parameters for FTPC tracking
};    

#endif
