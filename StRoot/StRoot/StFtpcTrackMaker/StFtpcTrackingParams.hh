// $Id: StFtpcTrackingParams.hh,v 1.22 2012/11/07 23:30:18 fisyak Exp $
// $Log: StFtpcTrackingParams.hh,v $
// Revision 1.22  2012/11/07 23:30:18  fisyak
// Supress warnings
//
// Revision 1.21  2007/12/12 12:55:19  jcs
// Markus Oldenburg replaced assert() with a return code which can be tested in StFtpcTrackMaker
// replaced 'return 1' with 'return kStOK'
// replaced 'return 0' with 'return KStErr'
//
// Revision 1.20  2007/05/14 19:44:16  jcs
// remove obsolete include statement
//
// Revision 1.19  2007/05/08 10:47:33  jcs
// replace StMagUtilities with StarMagField as requested by Yuri
//
// Revision 1.18  2005/07/06 19:40:07  fisyak
// use templated version of StThreeVectorF and StPhysicalHelixD
//
// Revision 1.17  2004/06/04 11:05:26  jcs
// replaced StarDb/ftpc/fdepars/fdepar with StarDb/ftpc/ftpcdEdxPars
//
// Revision 1.16  2003/10/07 14:11:10  jcs
// remove previous fix for determining magnetic field
//
// Revision 1.15  2003/10/02 00:10:37  perev
// Zeroing of members added and bug in ResetMagField fixed
//
// Revision 1.14  2003/09/26 06:08:58  oldi
// Check if the magentic field was reversed 'by hand' with a chain option.
// If yes, multiply the scaleFactor of the field with -1.
//
// Revision 1.13  2003/09/11 21:31:30  jeromel
// removed inline as it would leave a few undefined reference
//
// Revision 1.12  2003/05/21 09:47:10  putschke
// Include rotation around y-axis for FTPC east and west
//
// Revision 1.11  2003/05/20 18:35:08  oldi
// Cuts for vertex estimation introduced (globDca < 1 cm, multiplicity >= 200).
//
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
#include "StThreeVectorD.hh"
#include "StMatrixD.hh"
#include "Stypes.h"

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

#include "tables/St_ftpcTrackingPars_Table.h"
#include "tables/St_ftpcDimensions_Table.h"
#include "tables/St_ftpcPadrowZ_Table.h"
#include "tables/St_ftpcdEdxPars_Table.h"
#include "tables/St_ftpcCoordTrans_Table.h"

#include "StTpcDb/StTpcDb.h"

#include "StarMagField.h"

class St_ftpcDimensions;
class St_ftpcPadrowZ;
class St_ftpcdEdxPars;
class StGlobalCoordinate;
class StFtpcLocalCoordinate;
class StTpcDb;

class StFtpcTrackingParams
{
private:
  
  static StFtpcTrackingParams* mInstance;

  char mStart;  //  start of simple variables

  Int_t mReturnCode;

  /// FTPC geometry
  Double_t  mInnerRadius;
  Double_t  mOuterRadius;
     Int_t  mNumberOfPadRows;
     Int_t  mNumberOfPadRowsPerSide;
  Double_t *mPadRowPosZ;

  /// Vertex position
  Double_t mMaxVertexPosZWarning;
  Double_t mMaxVertexPosZError;

  /// Vertex reconstruction
     Int_t mHistoBins;
  Double_t mHistoMin;
  Double_t mHistoMax;
  Double_t mMaxDcaVertex;
     Int_t mMinNumTracks;

  /// Tracker
  Int_t mRowSegments;
  Int_t mPhiSegments;
  Int_t mEtaSegments;
  
  /// Tracking
    Bool_t mLaser[4];
    Bool_t mVertexConstraint[4];
     Int_t mMaxTrackletLength[4];
     Int_t mMinTrackLength[4];
     Int_t mRowScopeTracklet[4];
     Int_t mRowScopeTrack[4];
     Int_t mPhiScope[4];
     Int_t mEtaScope[4];
  Double_t mMaxDca[4];

  /// Tracklets
  Double_t mMaxAngleTracklet[4];

  /// Tracks
  Double_t mMaxAngleTrack[4];
  Double_t mMaxCircleDist[4];
  Double_t mMaxLengthDist[4];
  
  /// Split tracks
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
  
  // internal FTPC rotation
  StMatrixD *mFtpcRotationY[2];
  StMatrixD *mFtpcRotationYInverse[2];
  StMatrixD *mFtpcRotationX[2];
  StMatrixD *mFtpcRotationXInverse[2];
  StMatrixD *mFtpcRotation[2];
  StMatrixD *mFtpcRotationInverse[2];
   Double_t  mInstallationPointY[2];
   Double_t  mInstallationPointX[2];
   Double_t  mInstallationPointZ[2];
   Double_t  mObservedVertexOffsetY[2];
   Double_t  mObservedVertexOffsetX[2];

   char mEnd; //End of simple variables

  // transformation due to rotated and displaced TPC
       StMatrixD mTpcToGlobalRotation; // (3X3)
       StMatrixD mGlobalToTpcRotation; // (3X3)
  StThreeVectorD mTpcPositionInGlobal; 

protected:
  
  StFtpcTrackingParams(St_ftpcTrackingPars *trackPars = 0,
		       St_ftpcdEdxPars *dEdxPars = 0,
		       St_ftpcDimensions *dimensions = 0,
		       St_ftpcPadrowZ *padrow_z = 0);
  StFtpcTrackingParams();
  
  Int_t InitTrackingParams(ftpcTrackingPars_st *trackParsTable);
  Int_t InitdEdx(ftpcdEdxPars_st *dEdxParsTable);
  Int_t InitDimensions(ftpcDimensions_st* dimensionsTable);
  Int_t InitPadRows(ftpcPadrowZ_st* padrowzTable);
  Int_t InitCoordTransformation();
  Int_t InitCoordTransformation(ftpcCoordTrans_st* ftpcCoordTrans);
  Int_t InitSpaceTransformation();
  
public:
  
  static StFtpcTrackingParams* Instance(Bool_t debug, 
					St_ftpcTrackingPars *trackPars,
					St_ftpcdEdxPars *dEdxPars,
					St_ftpcDimensions *dimensions,
					St_ftpcPadrowZ *padrow_z);
  static StFtpcTrackingParams* Instance(Bool_t debug, 
					St_ftpcCoordTrans *ftpcCoordTrans);
  static StFtpcTrackingParams* Instance();
  
  virtual ~StFtpcTrackingParams();
  
  void PrintParams();

  Int_t GetReturnCode() { return mReturnCode; }

  /// @name FTPC geometry
  //@{
  Double_t InnerRadius();
  Double_t OuterRadius();
     Int_t NumberOfPadRows();
     Int_t NumberOfPadRowsPerSide();
  Double_t PadRowPosZ(Int_t row);
  //@}


  /// @name Vertex position
  //@{
  Double_t MaxVertexPosZWarning();
  Double_t MaxVertexPosZError();
  //@}


  /// @name Vertex reconstruction
  //@{
     Int_t HistoBins();
  Double_t HistoMin();
  Double_t HistoMax();
  Double_t MaxDcaVertex();
     Int_t MinNumTracks();
  //@}

  /// @name Tracker
  //@{
  Int_t RowSegments();
  Int_t PhiSegments();
  Int_t EtaSegments();
  //@}

  
  /// @name Tracking
  //@{
  Bool_t Laser(Int_t tracking_method);
  Bool_t VertexConstraint(Int_t tracking_method);
  Int_t MaxTrackletLength(Int_t tracking_method);
  Int_t MinTrackLength(Int_t tracking_method);
  Int_t RowScopeTracklet(Int_t tracking_method);
  Int_t RowScopeTrack(Int_t tracking_method);
  Int_t PhiScope(Int_t tracking_method);
  Int_t EtaScope(Int_t tracking_method);
  Double_t MaxDca(Int_t tracking_method);
  //@}

  /// Tracklets
  Double_t MaxAngleTracklet(Int_t tracking_method);


  /// @name Tracks
  //@{
  Double_t MaxAngleTrack(Int_t tracking_method);
  Double_t MaxCircleDist(Int_t tracking_method);
  Double_t MaxLengthDist(Int_t tracking_method);
  //@}

  
  /// @name Split tracks
  //@{
  Double_t MaxDist();
  Double_t MinPointRatio();
  Double_t MaxPointRatio();
  //@}


  /// @name dE/dx
  //@{
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
  //@}


  /// transformation due to rotated and displaced TPC
  StMatrixD TpcToGlobalRotation();
  StMatrixD GlobalToTpcRotation();
  StThreeVectorD TpcPositionInGlobal(); 


  /// @name internal FTPC rotation
  //@{
  StMatrixD FtpcRotation(Int_t i);
  StMatrixD FtpcRotationInverse(Int_t i);
  StMatrixD FtpcRotationX(Int_t i);
  StMatrixD FtpcRotationXInverse(Int_t i);
  StMatrixD FtpcRotationY(Int_t i);
  StMatrixD FtpcRotationYInverse(Int_t i);
  Double_t InstallationPointX(Int_t i);
  Double_t InstallationPointY(Int_t i);
  Double_t InstallationPointZ(Int_t i);
  Double_t ObservedVertexOffsetY(Int_t i);
  Double_t ObservedVertexOffsetX(Int_t i);
  //@}


  StarMagField *MagField() {return StarMagField::Instance();}
  Double_t  MagFieldFactor(){ return StarMagField::Instance()->GetFactor();}

  ClassDef(StFtpcTrackingParams,0)  // Parameters for FTPC tracking
};    

#endif
