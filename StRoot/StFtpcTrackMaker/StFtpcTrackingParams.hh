// $Id: StFtpcTrackingParams.hh,v 1.10 2003/01/16 18:04:35 oldi Exp $
// $Log: StFtpcTrackingParams.hh,v $
// Revision 1.10  2003/01/16 18:04:35  oldi
// Bugs eliminated. Now it compiles on Solaris again.
// Split residuals for global and primary fit.
//
// Revision 1.9  2002/11/21 15:46:30  oldi
// Enabled rotation for FTPC west. If there is an observed shift of the vertex
// position in y-direction (in FTPC west), just fill this offset into the Db.
// Up to now this offset is set to 0., i.e. only FTPC east is rotated (because
// the offset is at 0.3427 cm).
//
// Revision 1.8  2002/11/19 12:45:11  oldi
// A new database entry (installationPointY[east/west]) was introduced. Now
// the rotation of FTPC east is done around the correct axis, which isn't
// measured but comes from the drawings. The measurements used before were true
// measurements but had nothing to do with the rotation axis, unfortunately.
// Anyway, the difference is rather small since a typical cluster is rotated
// by less than 0.1mm.
// Some code cleanup done.
//
// Revision 1.7  2002/11/06 13:47:40  oldi
// All current database values hardcoded (for stand alone usage).
// Code clean ups.
//
// Revision 1.6  2002/10/31 13:42:31  oldi
// Everything read from database now.
//
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

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

#include "tables/St_ftpcTrackingPars_Table.h"
#include "tables/St_ftpcDimensions_Table.h"
#include "tables/St_ftpcPadrowZ_Table.h"
#include "tables/St_fde_fdepar_Table.h"
#include "tables/St_ftpcCoordTrans_Table.h"

#include "StDbUtilities/StMagUtilities.h"
#include "StTpcDb/StTpcDb.h"

class St_ftpcDimensions;
class St_ftpcPadrowZ;
class St_fde_fdepar;
class StGlobalCoordinate;
class StFtpcLocalCoordinate;
class StTpcDb;

class StFtpcTrackingParams
{
private:
  
  static StFtpcTrackingParams* mInstance;
  
  // FTPC geometry
  Double_t  mInnerRadius;
  Double_t  mOuterRadius;
     Int_t  mNumberOfPadRows;
     Int_t  mNumberOfPadRowsPerSide;
  Double_t *mPadRowPosZ;

  // Vertex position
  Double_t mMaxVertexPosZWarning;
  Double_t mMaxVertexPosZError;

  // Vertex reconstruction
     Int_t mHistoBins;
  Double_t mHistoMin;
  Double_t mHistoMax;

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
  Double_t mMaxDca[4];

  // Tracklets
  Double_t mMaxAngleTracklet[4];

  // Tracks
  Double_t mMaxAngleTrack[4];
  Double_t mMaxCircleDist[4];
  Double_t mMaxLengthDist[4];
  
  // Split tracks
  Double_t mMaxDist;
  Double_t mMinPointRatio;
  Double_t mMaxPointRatio;

  // dE/dx
  Int_t mDebugLevel;
  Int_t mIdMethod;
  Int_t mNoAngle;
  Int_t mMaxHit; 
  Int_t mMinHit;
  Int_t mMaxTrack;

  Double_t mPadLength;
  Double_t mFracTrunc;
  Double_t mAip;
  Double_t mALargeNumber;

  // transformation due to rotated and displaced TPC
       StMatrixD mTpcToGlobalRotation; // (3X3)
       StMatrixD mGlobalToTpcRotation; // (3X3)
  StThreeVectorD mTpcPositionInGlobal; 

  // internal FTPC rotation
  StMatrixD *mFtpcRotation[2];
  StMatrixD *mFtpcRotationInverse[2];
   Double_t  mInstallationPointY[2];
   Double_t  mInstallationPointZ[2];
   Double_t  mObservedVertexOffsetY[2];
  
  StMagUtilities *mMagField;       // pointer to magnetic field table
        Double_t  mMagFieldFactor;

protected:
  
  StFtpcTrackingParams(St_ftpcTrackingPars *trackPars = 0,
		       St_fde_fdepar *dEdxPars = 0,
		       St_ftpcDimensions *dimensions = 0,
		       St_ftpcPadrowZ *padrow_z = 0);
  StFtpcTrackingParams(Double_t magFieldFactor);
  
  Int_t InitTrackingParams(ftpcTrackingPars_st *trackParsTable);
  Int_t InitdEdx(FDE_FDEPAR_ST *dEdxParsTable);
  Int_t InitDimensions(ftpcDimensions_st* dimensionsTable);
  Int_t InitPadRows(ftpcPadrowZ_st* padrowzTable);
  Int_t InitCoordTransformation();
  Int_t InitCoordTransformation(ftpcCoordTrans_st* ftpcCoordTrans);
  Int_t InitSpaceTransformation();
  Int_t ResetMagField(TDataSet *RunLog = 0);
  
public:
  
  static StFtpcTrackingParams* Instance(Bool_t debug, 
					St_ftpcTrackingPars *trackPars,
					St_fde_fdepar *dEdxPars,
					St_ftpcDimensions *dimensions,
					St_ftpcPadrowZ *padrow_z);
  static StFtpcTrackingParams* Instance(Bool_t debug, 
					St_ftpcCoordTrans *ftpcCoordTrans, 
					TDataSet *RunLog);
  static StFtpcTrackingParams* Instance(Bool_t debug, Double_t magFieldFactor);
  static StFtpcTrackingParams* Instance();
  
  virtual ~StFtpcTrackingParams();
  
  void PrintParams();
  
  // FTPC geometry
  Double_t InnerRadius();
  Double_t OuterRadius();
     Int_t NumberOfPadRows();
     Int_t NumberOfPadRowsPerSide();
  Double_t PadRowPosZ(Int_t row);

  // Vertex position
  Double_t MaxVertexPosZWarning();
  Double_t MaxVertexPosZError();

  // Vertex reconstruction
     Int_t HistoBins();
  Double_t HistoMin();
  Double_t HistoMax();

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
  Double_t MaxDca(Int_t tracking_method);

  // Tracklets
  Double_t MaxAngleTracklet(Int_t tracking_method);

  // Tracks
  Double_t MaxAngleTrack(Int_t tracking_method);
  Double_t MaxCircleDist(Int_t tracking_method);
  Double_t MaxLengthDist(Int_t tracking_method);
  
  // Split tracks
  Double_t MaxDist();
  Double_t MinPointRatio();
  Double_t MaxPointRatio();

  // dE/dx
  Int_t DebugLevel();
  Int_t IdMethod();
  Int_t NoAngle();
  Int_t MaxHit();
  Int_t MinHit();
  Int_t MaxTrack();
  
  Double_t PadLength();
  Double_t FracTrunc();
  Double_t Aip();
  Double_t ALargeNumber();

  // transformation due to rotated and displaced TPC
       StMatrixD TpcToGlobalRotation();
       StMatrixD GlobalToTpcRotation();
  StThreeVectorD TpcPositionInGlobal(); 

  StMatrixD FtpcRotation(Int_t i);
  StMatrixD FtpcRotationInverse(Int_t i);
   Double_t InstallationPointY(Int_t i);
   Double_t InstallationPointZ(Int_t i);
   Double_t ObservedVertexOffsetY(Int_t i);

  // magnetic field table
  StMagUtilities *MagField();
        Double_t  MagFieldFactor();

  ClassDef(StFtpcTrackingParams,0)  // Parameters for FTPC tracking
};    

#endif
