// $Id: StFtpcTrackingParams.cc,v 1.20 2003/09/11 21:31:30 jeromel Exp $
// $Log: StFtpcTrackingParams.cc,v $
// Revision 1.20  2003/09/11 21:31:30  jeromel
// removed inline as it would leave a few undefined reference
//
// Revision 1.19  2003/09/02 17:58:17  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.18  2003/06/27 13:11:25  putschke
// *** empty log message ***
//
// Revision 1.17  2003/05/23 21:10:44  oldi
// Cosmetics.
//
// Revision 1.16  2003/05/21 09:47:35  putschke
// Include rotation around y-axis for FTPC east and west
//
// Revision 1.15  2003/05/20 18:35:04  oldi
// Cuts for vertex estimation introduced (globDca < 1 cm, multiplicity >= 200).
//
// Revision 1.14  2003/01/29 17:24:37  oldi
// Message added which will be printed when StMagUtilities is initialized.
//
// Revision 1.13  2003/01/21 10:04:13  jcs
// initialize variables to eliminate compiler warnings for NODEBUG=yes
//
// Revision 1.12  2003/01/16 18:04:34  oldi
// Bugs eliminated. Now it compiles on Solaris again.
// Split residuals for global and primary fit.
//
// Revision 1.11  2002/11/21 15:46:25  oldi
// Enabled rotation for FTPC west. If there is an observed shift of the vertex
// position in y-direction (in FTPC west), just fill this offset into the Db.
// Up to now this offset is set to 0., i.e. only FTPC east is rotated (because
// the offset is at 0.3427 cm).
//
// Revision 1.10  2002/11/19 12:45:09  oldi
// A new database entry (installationPointY[east/west]) was introduced. Now
// the rotation of FTPC east is done around the correct axis, which isn't
// measured but comes from the drawings. The measurements used before were true
// measurements but had nothing to do with the rotation axis, unfortunately.
// Anyway, the difference is rather small since a typical cluster is rotated
// by less than 0.1mm.
// Some code cleanup done.
//
// Revision 1.9  2002/11/06 13:47:34  oldi
// All current database values hardcoded (for stand alone usage).
// Code clean ups.
//
// Revision 1.8  2002/10/31 13:42:26  oldi
// Everything read from database now.
//
// Revision 1.7  2002/10/11 15:45:43  oldi
// Get FTPC geometry and dimensions from database.
// No field fit activated: Returns momentum = 0 but fits a helix.
// Bug in TrackMaker fixed (events with z_vertex > outer_ftpc_radius were cut).
// QA histograms corrected (0 was supressed).
// Code cleanup (several lines of code changed due to *params -> Instance()).
// cout -> gMessMgr.
//
// Revision 1.6  2002/10/03 10:34:06  oldi
// Usage of gufld removed.
// Magnetic field is read by StMagUtilities, now.
//
// Revision 1.5  2002/08/02 11:19:32  oldi
// MaxDCA is set to 100 cm now. Therefore 'every' track is fitted with the
// primary vertex in addition to the global fit, which is perfpormed anyway.
// The cut, which was set to 2.5 cm before, has to be applied during the
// analysis, now.
// The y-offest of FTPC east changed slightly due to the 'new' t0 of 2.96 mus.
//
// Revision 1.4  2002/06/07 06:00:39  oldi
// New value for rotation angle of FTPC east after temperature offset was corrected.
//

//----------Author:        Markus D. Oldenburg
//----------Last Modified: 25.04.2002
//----------Copyright:     &copy MDO Production 2002

#include "StFtpcTrackingParams.hh"
#include "SystemOfUnits.h"
#include "StMessMgr.h"

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

#include "tables/St_MagFactor_Table.h"

#include "TMath.h"
#include "Stiostream.h"

////////////////////////////////////////////////////////////////////////
// StFtpcTrackingParams                                               //
//                                                                    //
// This class provides the necessary parameters for FTPC tracking.    //
////////////////////////////////////////////////////////////////////////

ClassImp(StFtpcTrackingParams)

class St_fde_fdepar;

// Initialization of instance
StFtpcTrackingParams* StFtpcTrackingParams::mInstance = 0;


// FTPC geometry
Double_t StFtpcTrackingParams::InnerRadius()            { return mInnerRadius;            }
Double_t StFtpcTrackingParams::OuterRadius()            { return mOuterRadius;            }
   Int_t StFtpcTrackingParams::NumberOfPadRows()        { return mNumberOfPadRows;        }
   Int_t StFtpcTrackingParams::NumberOfPadRowsPerSide() { return mNumberOfPadRowsPerSide; }
Double_t StFtpcTrackingParams::PadRowPosZ(Int_t row)    { return mPadRowPosZ[row];        }

// Vertex position
Double_t StFtpcTrackingParams::MaxVertexPosZWarning() { return mMaxVertexPosZWarning; }
Double_t StFtpcTrackingParams::MaxVertexPosZError()   { return mMaxVertexPosZError;   }

// Vertex reconstruction
   Int_t StFtpcTrackingParams::HistoBins()    { return mHistoBins;    }
Double_t StFtpcTrackingParams::HistoMin()     { return mHistoMin;     }
Double_t StFtpcTrackingParams::HistoMax()     { return mHistoMax;     }
Double_t StFtpcTrackingParams::MaxDcaVertex() { return mMaxDcaVertex; }
   Int_t StFtpcTrackingParams::MinNumTracks() { return mMinNumTracks; }

// Tracker
Int_t StFtpcTrackingParams::RowSegments() { return mRowSegments; }
Int_t StFtpcTrackingParams::PhiSegments() { return mPhiSegments; }
Int_t StFtpcTrackingParams::EtaSegments() { return mEtaSegments; }

// Tracking
  Bool_t StFtpcTrackingParams::Laser(Int_t tracking_method)             { return mLaser[tracking_method];             }
  Bool_t StFtpcTrackingParams::VertexConstraint(Int_t tracking_method)  { return mVertexConstraint[tracking_method];  }
   Int_t StFtpcTrackingParams::MaxTrackletLength(Int_t tracking_method) { return mMaxTrackletLength[tracking_method]; }
   Int_t StFtpcTrackingParams::MinTrackLength(Int_t tracking_method)    { return mMinTrackLength[tracking_method];    }
   Int_t StFtpcTrackingParams::RowScopeTracklet(Int_t tracking_method)  { return mRowScopeTracklet[tracking_method];  }
   Int_t StFtpcTrackingParams::RowScopeTrack(Int_t tracking_method)     { return mRowScopeTrack[tracking_method];     }
   Int_t StFtpcTrackingParams::PhiScope(Int_t tracking_method)          { return mPhiScope[tracking_method];          }
   Int_t StFtpcTrackingParams::EtaScope(Int_t tracking_method)          { return mEtaScope[tracking_method];          }
Double_t StFtpcTrackingParams::MaxDca(Int_t tracking_method)            { return mMaxDca[tracking_method];            }

// Tracklets
Double_t StFtpcTrackingParams::MaxAngleTracklet(Int_t tracking_method) { return mMaxAngleTracklet[tracking_method]; }

// Tracks
Double_t StFtpcTrackingParams::MaxAngleTrack(Int_t tracking_method) { return mMaxAngleTrack[tracking_method]; }
Double_t StFtpcTrackingParams::MaxCircleDist(Int_t tracking_method) { return mMaxCircleDist[tracking_method]; }
Double_t StFtpcTrackingParams::MaxLengthDist(Int_t tracking_method) { return mMaxLengthDist[tracking_method]; }

// Split tracks
Double_t StFtpcTrackingParams::MaxDist()       { return mMaxDist;       }
Double_t StFtpcTrackingParams::MinPointRatio() { return mMinPointRatio; }
Double_t StFtpcTrackingParams::MaxPointRatio() { return mMaxPointRatio; }

// dE/dx
Int_t StFtpcTrackingParams::DebugLevel() { return mDebugLevel;    }
Int_t StFtpcTrackingParams::IdMethod()   { return mIdMethod;      }
Int_t StFtpcTrackingParams::NoAngle()    { return mNoAngle;       }
Int_t StFtpcTrackingParams::MaxHit()     { return mMaxHit;        }
Int_t StFtpcTrackingParams::MinHit()     { return mMinHit;        }
Int_t StFtpcTrackingParams::MaxTrack()   { return mMaxTrack;      }

Double_t StFtpcTrackingParams::PadLength()    { return mPadLength;     }
Double_t StFtpcTrackingParams::FracTrunc()    { return mFracTrunc;     }
Double_t StFtpcTrackingParams::Aip()          { return mAip;           } 
Double_t StFtpcTrackingParams::ALargeNumber() { return mALargeNumber;  }

// transformation due to rotated and displaced TPC
     StMatrixD StFtpcTrackingParams::TpcToGlobalRotation() { return mTpcToGlobalRotation; }
     StMatrixD StFtpcTrackingParams::GlobalToTpcRotation() { return mGlobalToTpcRotation; }
StThreeVectorD StFtpcTrackingParams::TpcPositionInGlobal() { return mTpcPositionInGlobal; } 

StMatrixD StFtpcTrackingParams::FtpcRotation(Int_t i)          { return *mFtpcRotation[i];           }
StMatrixD StFtpcTrackingParams::FtpcRotationInverse(Int_t i)   { return *mFtpcRotationInverse[i];    }
StMatrixD StFtpcTrackingParams::FtpcRotationX(Int_t i)         { return *mFtpcRotationX[i];           }
StMatrixD StFtpcTrackingParams::FtpcRotationXInverse(Int_t i)  { return *mFtpcRotationXInverse[i];    }
StMatrixD StFtpcTrackingParams::FtpcRotationY(Int_t i)         { return *mFtpcRotationY[i];           }
StMatrixD StFtpcTrackingParams::FtpcRotationYInverse(Int_t i)  { return *mFtpcRotationYInverse[i];    }

 Double_t StFtpcTrackingParams::InstallationPointX(Int_t i)    { return (i>=0 && i<=1) ? mInstallationPointX[i] : 0.; }
 Double_t StFtpcTrackingParams::InstallationPointY(Int_t i)    { return (i>=0 && i<=1) ? mInstallationPointY[i] : 0.; }
 Double_t StFtpcTrackingParams::InstallationPointZ(Int_t i)    { return (i>=0 && i<=1) ? mInstallationPointZ[i] : 0.; }
 Double_t StFtpcTrackingParams::ObservedVertexOffsetY(Int_t i) { return (i>=0 && i<=1) ? mObservedVertexOffsetY[i] : 0.; }
 Double_t StFtpcTrackingParams::ObservedVertexOffsetX(Int_t i) { return (i>=0 && i<=1) ? mObservedVertexOffsetX[i] : 0.; }

// magnetic field table
Double_t StFtpcTrackingParams::MagFieldFactor()  { return mMagFieldFactor; }
StMagUtilities *StFtpcTrackingParams::MagField() { return mMagField;       }


StFtpcTrackingParams* StFtpcTrackingParams::Instance(Bool_t debug,
						     St_ftpcTrackingPars *trackPars,
						     St_fde_fdepar *dEdxPars,
						     St_ftpcDimensions *dimensions, 
						     St_ftpcPadrowZ *padrow_z) {
  // makes new instance or returns old one if it exists already
  
  if (!mInstance) {
    mInstance = new StFtpcTrackingParams(trackPars, dEdxPars, dimensions, padrow_z);
  }

  return mInstance;
}


StFtpcTrackingParams* StFtpcTrackingParams::Instance(Bool_t debug, St_ftpcCoordTrans *ftpcCoordTrans, TDataSet *RunLog) {
  // updates magnetic field, if necessary

  mInstance->InitCoordTransformation(ftpcCoordTrans->GetTable()); // Has to be invoked here, because it could change from run to run.
  mInstance->InitSpaceTransformation(); // Has to be invoked here, since gStTpcDb isn't set before.
  
  if (RunLog) {
    mInstance->ResetMagField(RunLog); // Has to be invoked here, because it could change from run to run.
  }
  
  if (debug) {
    mInstance->PrintParams();
  }
  
  return mInstance;
}


StFtpcTrackingParams* StFtpcTrackingParams::Instance(Bool_t debug, Double_t magFieldFactor) {
  // Initialization with hardcoded values (to be able to run stand alone).
  
  if (mInstance) {
    
    if (magFieldFactor != mInstance->MagFieldFactor()) {
      delete mInstance;
      mInstance = new StFtpcTrackingParams(magFieldFactor);
    }
  }

  else { // instance isn't there
    mInstance = new StFtpcTrackingParams(magFieldFactor);
  }

  if (debug) {
    mInstance->PrintParams();
  }

  return mInstance;
}


StFtpcTrackingParams* StFtpcTrackingParams::Instance() {
  // return instance only
  
  return mInstance;
}


StFtpcTrackingParams::StFtpcTrackingParams(St_ftpcTrackingPars *trackPars,
					   St_fde_fdepar *dEdxPars,
					   St_ftpcDimensions *dimensions, 
					   St_ftpcPadrowZ *zrow) 
  : mTpcToGlobalRotation(3, 3, 1), mGlobalToTpcRotation(3, 3, 1)
{
  // default constructor

  mFtpcRotation[0] = new StMatrixD(3, 3, 1);
  mFtpcRotation[1] = new StMatrixD(3, 3, 1);
  mFtpcRotationInverse[0] = new StMatrixD(3, 3, 1);
  mFtpcRotationInverse[1] = new StMatrixD(3, 3, 1);
  mFtpcRotationX[0] = new StMatrixD(3, 3, 1);
  mFtpcRotationX[1] = new StMatrixD(3, 3, 1);
  mFtpcRotationXInverse[0] = new StMatrixD(3, 3, 1);
  mFtpcRotationXInverse[1] = new StMatrixD(3, 3, 1);
  mFtpcRotationY[0] = new StMatrixD(3, 3, 1);
  mFtpcRotationY[1] = new StMatrixD(3, 3, 1);
  mFtpcRotationYInverse[0] = new StMatrixD(3, 3, 1);
  mFtpcRotationYInverse[1] = new StMatrixD(3, 3, 1);
  

  InitTrackingParams(trackPars->GetTable());
  InitdEdx(dEdxPars->GetTable());
  InitDimensions(dimensions->GetTable());
  InitPadRows(zrow->GetTable());
  InitCoordTransformation();
  ResetMagField();
}


StFtpcTrackingParams::StFtpcTrackingParams(Double_t magFieldFactor)
  : mTpcToGlobalRotation(3, 3, 1), mGlobalToTpcRotation(3, 3, 1) 
{
  // Initialization with hardcoded values.

  mFtpcRotation[0] = new StMatrixD(3, 3, 1);
  mFtpcRotation[1] = new StMatrixD(3, 3, 1);
  mFtpcRotationInverse[0] = new StMatrixD(3, 3, 1);
  mFtpcRotationInverse[1] = new StMatrixD(3, 3, 1);
  mFtpcRotationX[0] = new StMatrixD(3, 3, 1);
  mFtpcRotationX[1] = new StMatrixD(3, 3, 1);
  mFtpcRotationXInverse[0] = new StMatrixD(3, 3, 1);
  mFtpcRotationXInverse[1] = new StMatrixD(3, 3, 1);
  mFtpcRotationY[0] = new StMatrixD(3, 3, 1);
  mFtpcRotationY[1] = new StMatrixD(3, 3, 1);
  mFtpcRotationYInverse[0] = new StMatrixD(3, 3, 1);
  mFtpcRotationYInverse[1] = new StMatrixD(3, 3, 1);

  // FTPC geometry
  mInnerRadius =  7.73 * centimeter;
  mOuterRadius = 30.05 * centimeter;

  mNumberOfPadRows        = 20;
  mNumberOfPadRowsPerSide = 10;
  
  mPadRowPosZ = new Double_t[mNumberOfPadRows];
  
  mPadRowPosZ[0]  =  162.75 * centimeter;
  mPadRowPosZ[1]  =  171.25 * centimeter;
  mPadRowPosZ[2]  =  184.05 * centimeter;
  mPadRowPosZ[3]  =  192.55 * centimeter;
  mPadRowPosZ[4]  =  205.35 * centimeter;
  mPadRowPosZ[5]  =  213.85 * centimeter;
  mPadRowPosZ[6]  =  226.65 * centimeter;
  mPadRowPosZ[7]  =  235.15 * centimeter;
  mPadRowPosZ[8]  =  247.95 * centimeter;
  mPadRowPosZ[9]  =  256.45 * centimeter;
  mPadRowPosZ[10] = -162.75 * centimeter;
  mPadRowPosZ[11] = -171.25 * centimeter;
  mPadRowPosZ[12] = -184.05 * centimeter;
  mPadRowPosZ[13] = -192.55 * centimeter;
  mPadRowPosZ[14] = -205.35 * centimeter;
  mPadRowPosZ[15] = -213.85 * centimeter;
  mPadRowPosZ[16] = -226.65 * centimeter;
  mPadRowPosZ[17] = -235.15 * centimeter;
  mPadRowPosZ[18] = -247.95 * centimeter;
  mPadRowPosZ[19] = -256.45 * centimeter;

  // Vertex position
  mMaxVertexPosZWarning =  50 * centimeter;
  mMaxVertexPosZError   = 100 * centimeter;

  // Vertex reconstruction
  mHistoBins    = 300;
  mHistoMin     = -75. * centimeter;
  mHistoMax     =  75. * centimeter;
  mMaxDcaVertex =   100. * centimeter;
  mMinNumTracks = 1; // must be >0 !

  // Tracker
  mRowSegments =  20;
  mPhiSegments = 100; 
  mEtaSegments = 200;
  
  // Tracking
  mLaser[0] = 0;
  mLaser[1] = 0;
  mLaser[2] = 1;
  mLaser[3] = 1;

  mVertexConstraint[0] = 1;
  mVertexConstraint[1] = 0;
  mVertexConstraint[2] = 1;
  mVertexConstraint[3] = 0;

  mMaxTrackletLength[0] =  3;
  mMaxTrackletLength[1] =  3;
  mMaxTrackletLength[2] = 10;
  mMaxTrackletLength[3] = 10;

  mMinTrackLength[0] = 5;
  mMinTrackLength[1] = 5;
  mMinTrackLength[2] = 5;
  mMinTrackLength[3] = 5;

  mRowScopeTracklet[0] = 2;
  mRowScopeTracklet[1] = 2;
  mRowScopeTracklet[2] = 2;
  mRowScopeTracklet[3] = 3;

  mRowScopeTrack[0] = 3;
  mRowScopeTrack[1] = 3;
  mRowScopeTrack[2] = 3;
  mRowScopeTrack[3] = 3;

  mPhiScope[0] = 1;
  mPhiScope[1] = 1;
  mPhiScope[2] = 1;
  mPhiScope[3] = 2;

  mEtaScope[0] =  1;
  mEtaScope[1] =  3;
  mEtaScope[2] =  3;
  mEtaScope[3] = 15;

  mMaxDca[0] = 100. * centimeter;
  mMaxDca[1] = 100. * centimeter;
  mMaxDca[2] = 100. * centimeter;
  mMaxDca[3] = 100. * centimeter;

  // Tracklets
  mMaxAngleTracklet[0] = 0.015 * radian;
  mMaxAngleTracklet[1] = 0.03 * radian;
  mMaxAngleTracklet[2] = 0.03 * radian;
  mMaxAngleTracklet[3] = 0.05 * radian;

  // Tracks
  mMaxAngleTrack[0] = 0.03 * radian;
  mMaxAngleTrack[1] = 0.08 * radian;
  mMaxAngleTrack[2] = 0.007 * radian;
  mMaxAngleTrack[3] = 0.007 * radian;

  mMaxCircleDist[0] = 0.05 * 1./centimeter;
  mMaxCircleDist[1] = 0.05 * 1./centimeter;
  mMaxCircleDist[2] = 0.03 * 1./centimeter;
  mMaxCircleDist[3] = 0.03 * 1./centimeter;

  mMaxLengthDist[0] = 30. * centimeter;
  mMaxLengthDist[1] = 70. * centimeter;
  mMaxLengthDist[2] = 30. * centimeter;
  mMaxLengthDist[3] = 30. * centimeter;
  
  // Split tracks
  mMaxDist       = 0.11 * centimeter;
  mMinPointRatio = 0.5;
  mMaxPointRatio = 0.5;

  // dE/dx
  mDebugLevel =     10;
  mIdMethod   =      0;
  mNoAngle    =      0;
  mMaxHit     =     10; 
  mMinHit     =      4;
  mMaxTrack   = 100000;

  mPadLength    = 0.02;
  mFracTrunc    = 0.8;
  mAip          = 2.6e-08;
  mALargeNumber = 1.0e+10;

  // magnetic field
  mMagFieldFactor = magFieldFactor;
  gMessMgr->Message("", "I", "OST") << "Initializing StMagUtilities for FTPC!" << endm;
  mMagField = new StMagUtilities((EBField)2, mMagFieldFactor, 0);

  // transformation due to rotated and displaced TPC
  mTpcPositionInGlobal.setX(-0.256 * centimeter);
  mTpcPositionInGlobal.setY(-0.082 * centimeter);
  mTpcPositionInGlobal.setZ(-0.192 * centimeter);

  Double_t phi   =  0.0 * radian;  //large uncertainty, so set to 0
  Double_t theta = -0.000381 * radian;
  Double_t psi   = -0.000156 * radian;
  
  mGlobalToTpcRotation(1, 1) =   TMath::Cos(theta) * TMath::Cos(phi);
  mGlobalToTpcRotation(1, 2) =   TMath::Cos(theta) * TMath::Sin(phi);
  mGlobalToTpcRotation(1, 3) = - TMath::Sin(theta);
  mGlobalToTpcRotation(2, 1) =   TMath::Sin(psi)   * TMath::Sin(theta) * TMath::Cos(phi) - TMath::Cos(psi) * TMath::Sin(phi);
  mGlobalToTpcRotation(2, 2) =   TMath::Sin(psi)   * TMath::Sin(theta) * TMath::Sin(phi) + TMath::Cos(psi) * TMath::Cos(phi);
  mGlobalToTpcRotation(2, 3) =   TMath::Cos(theta) * TMath::Sin(psi);
  mGlobalToTpcRotation(3, 1) =   TMath::Cos(psi)   * TMath::Sin(theta) * TMath::Cos(phi) + TMath::Sin(psi) * TMath::Sin(phi);
  mGlobalToTpcRotation(3, 2) =   TMath::Cos(psi)   * TMath::Sin(theta) * TMath::Sin(phi) - TMath::Sin(psi) * TMath::Cos(phi);
  mGlobalToTpcRotation(3, 3) =   TMath::Cos(theta) * TMath::Cos(psi);
  
  UInt_t ierr;
  mTpcToGlobalRotation = mGlobalToTpcRotation.inverse(ierr);
  
  if (ierr!=0) { 
    gMessMgr->Message("", "E", "OST") << "Cant invert rotation matrix!" << endm;
    gMessMgr->Message("", "E", "OST") << "Global to TPC rotation matrix:" << mGlobalToTpcRotation << endm;
    gMessMgr->Message("", "E", "OST") << "TPC to global rotation matrix:" << mTpcToGlobalRotation << endm;
  }
    
  // internal FTPC rotation [has do be done before local -> global]
  mInstallationPointX[0] = 0.0 * centimeter;
  mInstallationPointX[1] = 0.0 * centimeter;
  mInstallationPointY[0] = -22.25 * centimeter;
  mInstallationPointY[1] = -22.25 * centimeter;
  mInstallationPointZ[0] = -234.325 * centimeter;
  mInstallationPointZ[1] =  234.325 * centimeter;
  
  mObservedVertexOffsetY[0] = 0.1 * centimeter;
  mObservedVertexOffsetY[1] = -0.034 * centimeter;
  
  for (Int_t i = 0; i <= 1; i++) { // east and west rotation

    // define rotation angle
    Double_t zShift, phi0, phi1, alpha;
    Double_t pq = TMath::Sqrt(TMath::Power(mObservedVertexOffsetY[i], 2.) 
			      - 2.*mObservedVertexOffsetY[i]*mInstallationPointY[i] 
			      + TMath::Power(mInstallationPointZ[i], 2.)); // p-q-formula
    
     // Initialize variable to avoid compiler warning
     alpha = 0;
     
    if (i == 0) { // east
      zShift = (mInstallationPointZ[i] + pq) * centimeter; // take correct solution of p-q-formula
      phi0 = TMath::ATan((mInstallationPointY[i] - mObservedVertexOffsetY[i]) / mInstallationPointZ[i]) * radian;
      phi1 = TMath::ATan(mInstallationPointY[i] / (mInstallationPointZ[i] - zShift)) * radian;
      alpha = phi0 - phi1;
    }
    
    else if (i == 1) { //west
      zShift = (mInstallationPointZ[i] - pq) * centimeter; // take correct solution of p-q-formula
      phi0 = TMath::ATan((mObservedVertexOffsetY[i] - mInstallationPointY[i]) / mInstallationPointZ[i]) * radian;
      phi1 = TMath::ATan(mInstallationPointY[i] / (zShift - mInstallationPointZ[i]));
      alpha = phi1 - phi0;
    }

    // define rotation axis
    // simplify to rotation about x-axis because of very small y-z-offset
    Double_t rx = 1.0 * centimeter;
    Double_t ry = 0.0 * centimeter;
    Double_t rz = 0.0 * centimeter;
    
    // take the normal vector as rotation vector
    Double_t norm_r = TMath::Sqrt(rx*rx + ry*ry + rz*rz);
    rx = rx/norm_r;
    ry = ry/norm_r;
    rz = rz/norm_r;
    
    // rotation maxtrix : rotation about r(rx, ry, rz) with angle alpha
    // before that the coordinate system has to be transformed to x_installation, 
    // y_installation, z_installation as new coordinate system origin
    (*mFtpcRotationY[i])(1, 1) = rx * rx * (1 - TMath::Cos(alpha)) +      TMath::Cos(alpha);
    (*mFtpcRotationY[i])(1, 2) = ry * rx * (1 - TMath::Cos(alpha)) - rz * TMath::Sin(alpha);
    (*mFtpcRotationY[i])(1, 3) = rz * rx * (1 - TMath::Cos(alpha)) + ry * TMath::Sin(alpha);
    (*mFtpcRotationY[i])(2, 1) = rx * ry * (1 - TMath::Cos(alpha)) + rz * TMath::Sin(alpha);
    (*mFtpcRotationY[i])(2, 2) = ry * ry * (1 - TMath::Cos(alpha)) +      TMath::Cos(alpha);
    (*mFtpcRotationY[i])(2, 3) = rz * ry * (1 - TMath::Cos(alpha)) - rx * TMath::Sin(alpha);
    (*mFtpcRotationY[i])(3, 1) = rx * ry * (1 - TMath::Cos(alpha)) - ry * TMath::Sin(alpha);
    (*mFtpcRotationY[i])(3, 2) = ry * rz * (1 - TMath::Cos(alpha)) + rx * TMath::Sin(alpha);
    (*mFtpcRotationY[i])(3, 3) = rz * rz * (1 - TMath::Cos(alpha)) +      TMath::Cos(alpha);
    
    (*mFtpcRotationYInverse[i]) = (*mFtpcRotationY[i]).inverse(ierr);
    
    if (ierr!=0) { 
      gMessMgr->Message("", "E", "OST") << "Can't invert FTPC ";
      if (i == 0) *gMessMgr << " east rotation matrix Y !" << endm;
      else *gMessMgr << " west rotation matrix Y !" << endm;
      gMessMgr->Message("", "I", "OST") << "FTPC rotation matrix Y :" << (*mFtpcRotationY[i]) << endm;
      gMessMgr->Message("", "I", "OST") << "Inverse FTPC rotation matrix Y :" << (*mFtpcRotationYInverse[i]) << endm;
    }

    // define rotation axis

    mObservedVertexOffsetX[0] = 0. * centimeter;
    mObservedVertexOffsetX[1] = -0.08 * centimeter;

    rx = 0.0 * centimeter;
    ry = 1.0 * centimeter;
    rz = 0.0 * centimeter;
    
    norm_r = TMath::Sqrt(rx*rx + ry*ry + rz*rz);

    rx = rx/norm_r;
    ry = ry/norm_r;
    rz = rz/norm_r;

    // calculate beta analog to alpha

    pq = TMath::Sqrt(TMath::Power(mObservedVertexOffsetX[i], 2.) 
			      - 2.*mObservedVertexOffsetX[i]*mInstallationPointX[i] 
			      + TMath::Power(mInstallationPointZ[i], 2.)); // p-q-formula

    Double_t beta=0;

    if (i == 0) { // east
      zShift = (mInstallationPointZ[i] + pq) * centimeter; // take correct solution of p-q-formula
      phi0 = TMath::ATan((mInstallationPointX[i] - mObservedVertexOffsetX[i]) / mInstallationPointZ[i]) * radian;
      phi1 = TMath::ATan(mInstallationPointX[i] / (mInstallationPointZ[i] - zShift)) * radian;
      beta = phi0 - phi1;
    }
    
    else if (i == 1) { //west
      zShift = (mInstallationPointZ[i] - pq) * centimeter; // take correct solution of p-q-formula
      phi0 = TMath::ATan((mObservedVertexOffsetX[i] - mInstallationPointX[i]) / mInstallationPointZ[i]) * radian;
      phi1 = TMath::ATan(mInstallationPointX[i] / (zShift - mInstallationPointZ[i]));
      beta = phi1 - phi0;
    }

    (*mFtpcRotationX[i])(1, 1) = rx * rx * (1 - TMath::Cos(beta)) +      TMath::Cos(beta);
    (*mFtpcRotationX[i])(1, 2) = ry * rx * (1 - TMath::Cos(beta)) - rz * TMath::Sin(beta);
    (*mFtpcRotationX[i])(1, 3) = rz * rx * (1 - TMath::Cos(beta)) + ry * TMath::Sin(beta);
    (*mFtpcRotationX[i])(2, 1) = rx * ry * (1 - TMath::Cos(beta)) + rz * TMath::Sin(beta);
    (*mFtpcRotationX[i])(2, 2) = ry * ry * (1 - TMath::Cos(beta)) +      TMath::Cos(beta);
    (*mFtpcRotationX[i])(2, 3) = rz * ry * (1 - TMath::Cos(beta)) - rx * TMath::Sin(beta);
    (*mFtpcRotationX[i])(3, 1) = rx * ry * (1 - TMath::Cos(beta)) - ry * TMath::Sin(beta);
    (*mFtpcRotationX[i])(3, 2) = ry * rz * (1 - TMath::Cos(beta)) + rx * TMath::Sin(beta);
    (*mFtpcRotationX[i])(3, 3) = rz * rz * (1 - TMath::Cos(beta)) +      TMath::Cos(beta);
    
    (*mFtpcRotationXInverse[i]) = (*mFtpcRotationX[i]).inverse(ierr);

    if (ierr!=0) { 
      gMessMgr->Message("", "E", "OST") << "Can't invert FTPC ";
      if (i == 0) *gMessMgr << " east rotation matrix X !" << endm;
      else *gMessMgr << " west rotation matrix X !" << endm;
      gMessMgr->Message("", "I", "OST") << "FTPC rotation matrix X :" << (*mFtpcRotationX[i]) << endm;
      gMessMgr->Message("", "I", "OST") << "Inverse FTPC rotation matrix X :" << (*mFtpcRotationXInverse[i]) << endm;
    }

    // combine Y and X rotation to rotation matrix

    (*mFtpcRotation[i]) = (*mFtpcRotationY[i])*(*mFtpcRotationX[i]);
    (*mFtpcRotationInverse[i])=(*mFtpcRotation[i]).inverse(ierr);


    if (ierr!=0) { 
      gMessMgr->Message("", "E", "OST") << "Can't invert FTPC ";
      if (i == 0) *gMessMgr << " east rotation matrix!" << endm;
      else *gMessMgr << " west rotation matrix!" << endm;
      gMessMgr->Message("", "I", "OST") << "FTPC rotation matrix:" << (*mFtpcRotation[i]) << endm;
      gMessMgr->Message("", "I", "OST") << "Inverse FTPC rotation matrix:" << (*mFtpcRotationInverse[i]) << endm;
    }
  }
}

StFtpcTrackingParams::~StFtpcTrackingParams() {
  // delete created pointers
  
  for (Int_t i = 0; i < 1; i++) {
    delete mFtpcRotation[i];
    delete mFtpcRotationInverse[i];
    delete mFtpcRotationX[i];
    delete mFtpcRotationXInverse[i];
    delete mFtpcRotationY[i];
    delete mFtpcRotationYInverse[i];
  }

  delete mPadRowPosZ;
  delete mMagField;
  
  mInstance = 0;
}


Int_t StFtpcTrackingParams::InitTrackingParams(ftpcTrackingPars_st *trackParsTable) {
  // Sets tracking parameters.
  
  if (trackParsTable) {
    
    // Vertex position
    mMaxVertexPosZWarning = trackParsTable->maxVertexPosZWarning * centimeter;
    mMaxVertexPosZError   = trackParsTable->maxVertexPosZError   * centimeter;
    
    // Vertex reconstruction
    mHistoBins    = trackParsTable->histoBins;
    mHistoMin     = trackParsTable->histoMin * centimeter;
    mHistoMax     = trackParsTable->histoMax * centimeter;
    mMaxDcaVertex = 100. * centimeter;
    mMinNumTracks = 1;  // must be >0 !


    // Tracker
    mRowSegments = trackParsTable->rowSegments;
    mPhiSegments = trackParsTable->phiSegments;
    mEtaSegments = trackParsTable->etaSegments;
    
    // Tracking
    // the 4 indizes represent: 0: main vertex tracking                        
    //                          1: non vertex tracking
    //                          2: no field tracking
    //                          3: laser tracking
    
    for (Int_t i = 0; i < 4; i++) {
      
      mLaser[i] = trackParsTable->laser[i] ? kTRUE : kFALSE;
      mVertexConstraint[i] = trackParsTable->vertexConstraint[i] ? kTRUE : kFALSE;
      
      mMaxTrackletLength[i] = trackParsTable->maxTrackletLength[i];
      mMinTrackLength[i]    = trackParsTable->minTrackLength[i];
      mRowScopeTracklet[i]  = trackParsTable->rowScopeTracklet[i];
      
      mRowScopeTrack[i] = trackParsTable->rowScopeTrack[i];
      mPhiScope[i]      = trackParsTable->phiScope[i];
      mEtaScope[i]      = trackParsTable->etaScope[i];
      
      mMaxDca[i] = trackParsTable->maxDca[i] * centimeter;
      
      // Tracklets
      mMaxAngleTracklet[i] = trackParsTable->maxAngleTracklet[i] * radian;
      
      // Tracks
      mMaxAngleTrack[i] = trackParsTable->maxAngleTrack[i] * radian;
      mMaxCircleDist[i] = trackParsTable->maxCircleDist[i] * 1./centimeter;
      mMaxLengthDist[i] = trackParsTable->maxLengthDist[i] * centimeter;
    }  
    
    // Split tracks
    mMaxDist       = trackParsTable->maxDist * centimeter;
    mMinPointRatio = trackParsTable->minPointRatio;
    mMaxPointRatio = trackParsTable->maxPointRatio;     
    
    return 1;
  }
  
  else {
    gMessMgr->Message("No data in table class St_TrackingPars.", "E", "OST");
    assert(trackParsTable);
    
    return 0;
  }
}


Int_t StFtpcTrackingParams::InitdEdx(FDE_FDEPAR_ST *dEdxParsTable) {
  // Sets dEdx parameters
  
  if (dEdxParsTable) {
    mDebugLevel   = dEdxParsTable[0].debug_level;
    mIdMethod     = dEdxParsTable[0].id_method;
    mNoAngle      = dEdxParsTable[0].no_angle;
    mMaxHit       = dEdxParsTable[0].max_hit; 
    mMinHit       = dEdxParsTable[0].min_hit;
    mMaxTrack     = dEdxParsTable[0].max_track;
    mPadLength    = dEdxParsTable[0].pad_length/100.; // from cm in um/keV (microsecond/keV = 100)
    mFracTrunc    = dEdxParsTable[0].frac_trun;
    mAip          = dEdxParsTable[0].a_i_p * 1.0e-9;  // in GeV 
    mALargeNumber = dEdxParsTable[0].a_large_number;
    
    return 1;
  }
  
  else {
    gMessMgr->Message("No data in table class St_fde_fdepar.", "E", "OST");
    assert(dEdxParsTable);
    
    return 0;
  }
}


Int_t StFtpcTrackingParams::InitDimensions(ftpcDimensions_st* dimensionsTable) {
  // Sets FTPC geometry.
  
  if (dimensionsTable) {
    mInnerRadius = dimensionsTable->innerRadiusSensitiveVolume * centimeter;
    mOuterRadius = dimensionsTable->outerRadiusSensitiveVolume * centimeter;
    mNumberOfPadRows        = dimensionsTable->totalNumberOfPadrows;
    mNumberOfPadRowsPerSide = dimensionsTable->numberOfPadrowsPerSide;
    mInstallationPointX[0] = dimensionsTable->installationPointX[0] * centimeter;
    mInstallationPointX[1] = dimensionsTable->installationPointX[1] * centimeter;
    mInstallationPointY[0] = dimensionsTable->installationPointY[0] * centimeter;
    mInstallationPointY[1] = dimensionsTable->installationPointY[1] * centimeter;
    mInstallationPointZ[0] = dimensionsTable->installationPointZ[0] * centimeter;
    mInstallationPointZ[1] = dimensionsTable->installationPointZ[1] * centimeter;

    return 1;
  }
  
  else {
    gMessMgr->Message("No data in table class St_ftpcDimensions.", "E", "OST");
    assert(dimensionsTable);
    
    return 0;
  }
}


Int_t StFtpcTrackingParams::InitPadRows(ftpcPadrowZ_st* padrowzTable) {
  // Set z-positione of pad rows.
  
  if (padrowzTable) {
    mPadRowPosZ = new Double_t[mNumberOfPadRows];
    
    for (Int_t i = 0; i < mNumberOfPadRows; i++) {
      mPadRowPosZ[i] = ((Float_t *)padrowzTable->z)[i];
    }
    
    return 1;
  } 
  
  else {
    gMessMgr->Message("No data in table class St_ftpcPadrowZ.", "E", "OST");
    assert(padrowzTable);
    
    return 0;
  }
}


Int_t StFtpcTrackingParams::InitCoordTransformation() {
  // Fill dummies.

  mObservedVertexOffsetY[0] = -9999.;
  mObservedVertexOffsetY[1] = -9999.;
  mObservedVertexOffsetX[0] = -9999.;
  mObservedVertexOffsetX[1] = -9999.;

  return 1;
}


Int_t StFtpcTrackingParams::InitCoordTransformation(ftpcCoordTrans_st* ftpcCoordTrans) {
  // Set rotation values.

  if (ftpcCoordTrans) {
    
    Double_t oldY[2] = {mObservedVertexOffsetY[0] * centimeter, mObservedVertexOffsetY[1] * centimeter};
    Double_t oldX[2] = {mObservedVertexOffsetX[0] * centimeter, mObservedVertexOffsetX[1] * centimeter};

    mObservedVertexOffsetY[0] = ftpcCoordTrans->observedVertexOffsetY[0] * centimeter;
    mObservedVertexOffsetY[1] = ftpcCoordTrans->observedVertexOffsetY[1] * centimeter;

    mObservedVertexOffsetX[0] = ftpcCoordTrans->observedVertexOffsetX[0] * centimeter;
    mObservedVertexOffsetX[1] = ftpcCoordTrans->observedVertexOffsetX[1] * centimeter;

    if ((mObservedVertexOffsetY[0] != oldY[0] || mObservedVertexOffsetY[1] != oldY[1]) && 
	(oldY[0] < -9990. || oldY[1] < -9990.)) {
      
      gMessMgr->Message("", "I", "OST") << "Observed vertex offset in y direction has changed. Changed from " 
					<< oldY[0] << " to " <<  mObservedVertexOffsetY[0] << " (east) and from " 
					<< oldY[1] << " to " <<  mObservedVertexOffsetY[1] << " (west)." << endm;
    }   
    

    if ((mObservedVertexOffsetX[0] != oldX[0] || mObservedVertexOffsetX[1] != oldX[1]) && 
	(oldX[0] < -9990. || oldX[1] < -9990.)) {
      
      gMessMgr->Message("", "I", "OST") << "Observed vertex offset in x direction has changed. Changed from " 
					<< oldX[0] << " to " <<  mObservedVertexOffsetX[0] << " (east) and from " 
					<< oldX[1] << " to " <<  mObservedVertexOffsetX[1] << " (west)." << endm;
    }

    return 1;
  }
  
  else {
    gMessMgr->Message("No data in table class St_ftpcCoordTrans.", "E", "OST");
    assert(ftpcCoordTrans);
    
    return 0;
  }
}


Int_t StFtpcTrackingParams::InitSpaceTransformation() {

  // transformation due to rotated and displaced TPC
  // lines commented out go in if TPC database is available

  if (gStTpcDb->GlobalPosition()) {

    Double_t phi   = 0.0 * radian;  //large uncertainty, so set to 0
    Double_t theta = gStTpcDb->GlobalPosition()->TpcRotationAroundGlobalAxisY() * radian;
    Double_t psi   = gStTpcDb->GlobalPosition()->TpcRotationAroundGlobalAxisX() * radian;

    mGlobalToTpcRotation(1, 1) =   TMath::Cos(theta) * TMath::Cos(phi);
    mGlobalToTpcRotation(1, 2) =   TMath::Cos(theta) * TMath::Sin(phi);
    mGlobalToTpcRotation(1, 3) = - TMath::Sin(theta);
    mGlobalToTpcRotation(2, 1) =   TMath::Sin(psi)   * TMath::Sin(theta) * TMath::Cos(phi) - TMath::Cos(psi) * TMath::Sin(phi);
    mGlobalToTpcRotation(2, 2) =   TMath::Sin(psi)   * TMath::Sin(theta) * TMath::Sin(phi) + TMath::Cos(psi) * TMath::Cos(phi);
    mGlobalToTpcRotation(2, 3) =   TMath::Cos(theta) * TMath::Sin(psi);
    mGlobalToTpcRotation(3, 1) =   TMath::Cos(psi)   * TMath::Sin(theta) * TMath::Cos(phi) + TMath::Sin(psi) * TMath::Sin(phi);
    mGlobalToTpcRotation(3, 2) =   TMath::Cos(psi)   * TMath::Sin(theta) * TMath::Sin(phi) - TMath::Sin(psi) * TMath::Cos(phi);
    mGlobalToTpcRotation(3, 3) =   TMath::Cos(theta) * TMath::Cos(psi);
    
    UInt_t ierr;
    mTpcToGlobalRotation = mGlobalToTpcRotation.inverse(ierr);
    
    if (ierr!=0) { 
      gMessMgr->Message("", "E", "OST") << "Cant invert rotation matrix!" << endm;
      gMessMgr->Message("", "E", "OST") << "Global to TPC rotation matrix:" << mGlobalToTpcRotation << endm;
      gMessMgr->Message("", "E", "OST") << "TPC to global rotation matrix:" << mTpcToGlobalRotation << endm;
    }
    
    mTpcPositionInGlobal.setX(gStTpcDb->GlobalPosition()->TpcCenterPositionX() * centimeter);
    mTpcPositionInGlobal.setY(gStTpcDb->GlobalPosition()->TpcCenterPositionY() * centimeter);
    mTpcPositionInGlobal.setZ(gStTpcDb->GlobalPosition()->TpcCenterPositionZ() * centimeter);    
  }
  
  else {
    gMessMgr->Message("", "E", "OST") << "StTpcDb IS INCOMPLETE! Cannot contstruct Coordinate transformation." << endm;
    assert(gStTpcDb->GlobalPosition());
  }
  
  // internal FTPC rotation [has do be done before local -> global]
  for (Int_t i = 0; i <= 1; i++) { // east and west rotation

    // define rotation angle
    Double_t zShift, phi0, phi1, alpha;
    Double_t pq = TMath::Sqrt(TMath::Power(mObservedVertexOffsetY[i], 2.) 
			      - 2.*mObservedVertexOffsetY[i]*mInstallationPointY[i] 
			      + TMath::Power(mInstallationPointZ[i], 2.)); // p-q-formula
    
     // Initialize variable to avoid compiler warning
     alpha = 0;
     
    if (i == 0) { // east
      zShift = (mInstallationPointZ[i] + pq) * centimeter; // take correct solution of p-q-formula
      phi0 = TMath::ATan((mInstallationPointY[i] - mObservedVertexOffsetY[i]) / mInstallationPointZ[i]) * radian;
      phi1 = TMath::ATan(mInstallationPointY[i] / (mInstallationPointZ[i] - zShift)) * radian;
      alpha = phi0 - phi1;
    }
    
    else if (i == 1) { //west
      zShift = (mInstallationPointZ[i] - pq) * centimeter; // take correct solution of p-q-formula
      phi0 = TMath::ATan((mObservedVertexOffsetY[i] - mInstallationPointY[i]) / mInstallationPointZ[i]) * radian;
      phi1 = TMath::ATan(mInstallationPointY[i] / (zShift - mInstallationPointZ[i]));
      alpha = phi1 - phi0;
    }

    // define rotation axis
    // simplify to rotation about x-axis because of very small y-z-offset
    Double_t rx = 1.0 * centimeter;
    Double_t ry = 0.0 * centimeter;
    Double_t rz = 0.0 * centimeter;
    
    // take the normal vector as rotation vector
    Double_t norm_r = TMath::Sqrt(rx*rx + ry*ry + rz*rz);
    rx = rx/norm_r;
    ry = ry/norm_r;
    rz = rz/norm_r;
    
    // rotation maxtrix : rotation about r(rx, ry, rz) with angle alpha
    // before that the coordinate system has to be transformed to x_installation, 
    // y_installation, z_installation as new coordinate system origin
    (*mFtpcRotationY[i])(1, 1) = rx * rx * (1 - TMath::Cos(alpha)) +      TMath::Cos(alpha);
    (*mFtpcRotationY[i])(1, 2) = ry * rx * (1 - TMath::Cos(alpha)) - rz * TMath::Sin(alpha);
    (*mFtpcRotationY[i])(1, 3) = rz * rx * (1 - TMath::Cos(alpha)) + ry * TMath::Sin(alpha);
    (*mFtpcRotationY[i])(2, 1) = rx * ry * (1 - TMath::Cos(alpha)) + rz * TMath::Sin(alpha);
    (*mFtpcRotationY[i])(2, 2) = ry * ry * (1 - TMath::Cos(alpha)) +      TMath::Cos(alpha);
    (*mFtpcRotationY[i])(2, 3) = rz * ry * (1 - TMath::Cos(alpha)) - rx * TMath::Sin(alpha);
    (*mFtpcRotationY[i])(3, 1) = rx * ry * (1 - TMath::Cos(alpha)) - ry * TMath::Sin(alpha);
    (*mFtpcRotationY[i])(3, 2) = ry * rz * (1 - TMath::Cos(alpha)) + rx * TMath::Sin(alpha);
    (*mFtpcRotationY[i])(3, 3) = rz * rz * (1 - TMath::Cos(alpha)) +      TMath::Cos(alpha);
    
    UInt_t ierr;
    
    (*mFtpcRotationYInverse[i]) = (*mFtpcRotationY[i]).inverse(ierr);
    
    if (ierr!=0) { 
      gMessMgr->Message("", "E", "OST") << "Can't invert FTPC ";
      if (i == 0) *gMessMgr << " east rotation matrix! Y " << endm;
      else *gMessMgr << " west rotation matrix! Y " << endm;
      gMessMgr->Message("", "I", "OST") << "FTPC rotation matrix Y :" << (*mFtpcRotationY[i]) << endm;
      gMessMgr->Message("", "I", "OST") << "Inverse FTPC rotation matrix Y :" << (*mFtpcRotationYInverse[i]) << endm;
    }

    // define rotation axis

    rx = 0.0 * centimeter;
    ry = 1.0 * centimeter;
    rz = 0.0 * centimeter;
    
    norm_r = TMath::Sqrt(rx*rx + ry*ry + rz*rz);

    rx = rx/norm_r;
    ry = ry/norm_r;
    rz = rz/norm_r;

    // calculate beta analog to alpha

    pq = TMath::Sqrt(TMath::Power(mObservedVertexOffsetX[i], 2.) 
			      - 2.*mObservedVertexOffsetX[i]*mInstallationPointX[i] 
			      + TMath::Power(mInstallationPointZ[i], 2.)); // p-q-formula

    Double_t beta=0;

    if (i == 0) { // east
      zShift = (mInstallationPointZ[i] + pq) * centimeter; // take correct solution of p-q-formula
      phi0 = TMath::ATan((mInstallationPointX[i] - mObservedVertexOffsetX[i]) / mInstallationPointZ[i]) * radian;
      phi1 = TMath::ATan(mInstallationPointX[i] / (mInstallationPointZ[i] - zShift)) * radian;
      beta = phi0 - phi1;
    }
    
    else if (i == 1) { //west
      zShift = (mInstallationPointZ[i] - pq) * centimeter; // take correct solution of p-q-formula
      phi0 = TMath::ATan((mObservedVertexOffsetX[i] - mInstallationPointX[i]) / mInstallationPointZ[i]) * radian;
      phi1 = TMath::ATan(mInstallationPointX[i] / (zShift - mInstallationPointZ[i]));
      beta = phi1 - phi0;
    }

    (*mFtpcRotationX[i])(1, 1) = rx * rx * (1 - TMath::Cos(beta)) +      TMath::Cos(beta);
    (*mFtpcRotationX[i])(1, 2) = ry * rx * (1 - TMath::Cos(beta)) - rz * TMath::Sin(beta);
    (*mFtpcRotationX[i])(1, 3) = rz * rx * (1 - TMath::Cos(beta)) + ry * TMath::Sin(beta);
    (*mFtpcRotationX[i])(2, 1) = rx * ry * (1 - TMath::Cos(beta)) + rz * TMath::Sin(beta);
    (*mFtpcRotationX[i])(2, 2) = ry * ry * (1 - TMath::Cos(beta)) +      TMath::Cos(beta);
    (*mFtpcRotationX[i])(2, 3) = rz * ry * (1 - TMath::Cos(beta)) - rx * TMath::Sin(beta);
    (*mFtpcRotationX[i])(3, 1) = rx * ry * (1 - TMath::Cos(beta)) - ry * TMath::Sin(beta);
    (*mFtpcRotationX[i])(3, 2) = ry * rz * (1 - TMath::Cos(beta)) + rx * TMath::Sin(beta);
    (*mFtpcRotationX[i])(3, 3) = rz * rz * (1 - TMath::Cos(beta)) +      TMath::Cos(beta);
    
    (*mFtpcRotationXInverse[i]) = (*mFtpcRotationX[i]).inverse(ierr);

    if (ierr!=0) { 
      gMessMgr->Message("", "E", "OST") << "Can't invert FTPC ";
      if (i == 0) *gMessMgr << " east rotation matrix X !" << endm;
      else *gMessMgr << " west rotation matrix X !" << endm;
      gMessMgr->Message("", "I", "OST") << "FTPC rotation matrix X :" << (*mFtpcRotationX[i]) << endm;
      gMessMgr->Message("", "I", "OST") << "Inverse FTPC rotation matrix X :" << (*mFtpcRotationXInverse[i]) << endm;
    }

    // combine Y and X rotation to rotation matrix

    (*mFtpcRotation[i]) = (*mFtpcRotationY[i])*(*mFtpcRotationX[i]);
    (*mFtpcRotationInverse[i])=(*mFtpcRotation[i]).inverse(ierr);


    if (ierr!=0) { 
      gMessMgr->Message("", "E", "OST") << "Can't invert FTPC ";
      if (i == 0) *gMessMgr << " east rotation matrix!" << endm;
      else *gMessMgr << " west rotation matrix!" << endm;
      gMessMgr->Message("", "I", "OST") << "FTPC rotation matrix:" << (*mFtpcRotation[i]) << endm;
      gMessMgr->Message("", "I", "OST") << "Inverse FTPC rotation matrix:" << (*mFtpcRotationInverse[i]) << endm;
    }

  }

  return 1;
}


Int_t StFtpcTrackingParams::ResetMagField(TDataSet *RunLog) {
  // Resets magnetic field if field configuration has changed.
  
  if (RunLog) {
    St_MagFactor *fMagFactor = (St_MagFactor *)RunLog->Find("MagFactor"); 
    
    Double_t newFactor = (*fMagFactor)[0].ScaleFactor;
    
    if (newFactor != mMagFieldFactor) { // field has changed
      
      if (mMagFieldFactor == -9999.) { // field will be set the first time
	mMagFieldFactor = newFactor;
	gMessMgr->Message("", "I", "OST") << "Initializing StMagUtilities for FTPC!" << endm;
	mMagField = new StMagUtilities((EBField)2, mMagFieldFactor, 0);  
	// I hope this is ok. Should be the same as the table in the database.
      }
      
      else { // field has been set before
	gMessMgr->Message("", "I", "OST") << "Magnetic field has changed. Reset magnetic field table and the field factor from " 
					  << mMagFieldFactor << " to " << newFactor << "." << endm;
	mMagFieldFactor = newFactor;
	delete mMagField;
	gMessMgr->Message("", "I", "OST") << "Initializing StMagUtilities for FTPC!" << endm;
	mMagField = new StMagUtilities((EBField)2, mMagFieldFactor, 0);
      }
    }
  }
  
  else { // fill dummy
    mMagFieldFactor = -9999.; // just some dumb number, will be set by ResetMagField() as soon as *RunLog is there
  }
  
  return 1;
}
  

void StFtpcTrackingParams::PrintParams() {
  // prints params to sreen

  gMessMgr->Message("", "I", "OST") << endm;

  gMessMgr->Message("", "I", "OST") << "Used parameters for FTPC tracking" << endm;
  gMessMgr->Message("", "I", "OST") << "---------------------------------" << endm;
  
  gMessMgr->Message("", "I", "OST") << endm;
  gMessMgr->Message("", "I", "OST") << "Magnetic field factor: " << mMagFieldFactor << endm;
  gMessMgr->Message("", "I", "OST") << endm;
  gMessMgr->Message("", "I", "OST") << "FTPC geometry" << endm;
  gMessMgr->Message("", "I", "OST") << "Inner radius (cm)...........: " << mInnerRadius << endm; 
  gMessMgr->Message("", "I", "OST") << "Outer radius (cm)...........: " << mOuterRadius << endm; 
  gMessMgr->Message("", "I", "OST") << "Total number of padows......: " << mNumberOfPadRows << endm; 
  gMessMgr->Message("", "I", "OST") << "Number of padows per FTPC...: " << mNumberOfPadRowsPerSide << endm; 
  
  for (Int_t i = 0; i < NumberOfPadRows(); i++) {
    gMessMgr->Message("", "I", "OST") << "z-position of padrow ";
    if (i<10) *gMessMgr << " ";
    *gMessMgr << i << " (cm): " << PadRowPosZ(i) << endm;
  }
  
  gMessMgr->Message("", "I", "OST") << endm;
  gMessMgr->Message("", "I", "OST") << "Vertex position" << endm;
  gMessMgr->Message("", "I", "OST") << "Max. vertex z-position to do tracking w/o warning (cm): " << mMaxVertexPosZWarning << endm; 
  gMessMgr->Message("", "I", "OST") << "Max. vertex z-position to do tracking at all (cm).....: " << mMaxVertexPosZError << endm; 
  
  gMessMgr->Message("", "I", "OST") << endm;
  gMessMgr->Message("", "I", "OST") << "Vertex reconstruction (with FTPC hits)" << endm;
  gMessMgr->Message("", "I", "OST") << "# of histogram bins.............: " << mHistoBins << endm; 
  gMessMgr->Message("", "I", "OST") << "lower boundary of histogram (cm): " << mHistoMin << endm; 
  gMessMgr->Message("", "I", "OST") << "upper boundary of histogram (cm): " << mHistoMax << endm; 
  gMessMgr->Message("", "I", "OST") << "Dca cut (cm)....................: " << mMaxDcaVertex << endm; 
  gMessMgr->Message("", "I", "OST") << "min. # of tracks required.......: " << mMinNumTracks << endm; 
  
  gMessMgr->Message("", "I", "OST") << endm;
  gMessMgr->Message("", "I", "OST") << "Tracker settings" << endm;
  gMessMgr->Message("", "I", "OST") << "# of row segments: " << mRowSegments << endm; 
  gMessMgr->Message("", "I", "OST") << "# of phi segments: " << mPhiSegments << endm; 
  gMessMgr->Message("", "I", "OST") << "# of eta segments: " << mEtaSegments << endm; 
  
  gMessMgr->Message("", "I", "OST") << endm;
  gMessMgr->Message("", "I", "OST") << "Settings for tracking" << endm;
  gMessMgr->Message("", "I", "OST") << "Tracking method    main vtx  non vtx  no fld  laser" << endm;
  gMessMgr->Message("", "I", "OST") << "Laser tracking switch: " << (Int_t)mLaser[0] << "        " 
				    << (Int_t)mLaser[1] << "        " << (Int_t)mLaser[2] << "      " 
				    << (Int_t)mLaser[3] << endm; 
  gMessMgr->Message("", "I", "OST") << "Vertex constraint....: " << (Int_t)mVertexConstraint[0] << "        " 
				    << (Int_t)mVertexConstraint[1] << "        " << (Int_t)mVertexConstraint[2] << "      " 
				    << (Int_t)mVertexConstraint[3] << endm;  
  gMessMgr->Message("", "I", "OST") << "Max. tracklet length.: " << mMaxTrackletLength[0] << "        " 
				    << mMaxTrackletLength[1] << "       " << mMaxTrackletLength[2] << "     " 
				    << mMaxTrackletLength[3] << endm; 
  gMessMgr->Message("", "I", "OST") << "Min. track length....: " << mMinTrackLength[0] << "        " 
				    << mMinTrackLength[1] << "        " << mMinTrackLength[2] << "      " 
				    << mMinTrackLength[3] << endm; 
  gMessMgr->Message("", "I", "OST") << "Tracklet row scope...: " << mRowScopeTracklet[0] << "        " 
				    << mRowScopeTracklet[1] << "        " << mRowScopeTracklet[2] << "      " 
				    << mRowScopeTracklet[3] << endm; 
  gMessMgr->Message("", "I", "OST") << "Track row scope......: " << mRowScopeTrack[0] << "        " 
				    << mRowScopeTrack[1] << "        " << mRowScopeTrack[2] << "      " 
				    << mRowScopeTrack[3] << endm; 
  gMessMgr->Message("", "I", "OST") << "Phi scope............: " << mPhiScope[0] << "        " 
				    << mPhiScope[1] << "        " << mPhiScope[2] << "      " 
				    << mPhiScope[3] << endm; 
  gMessMgr->Message("", "I", "OST") << "Eta scope............: " << mEtaScope[0] << "        " 
				    << mEtaScope[1] << "        " << mEtaScope[2] << "     " 
				    << mEtaScope[3] << endm; 
  gMessMgr->Message("", "I", "OST") << "Max. DCA (cm)........: " << mMaxDca[0] << "       " 
				    << mMaxDca[1] << "        " << mMaxDca[2] << "      " 
				    << mMaxDca[3] << endm; 
  
  gMessMgr->Message("", "I", "OST") << endm;
  gMessMgr->Message("", "I", "OST") << "Cuts for tracking" << endm;
  gMessMgr->Message("", "I", "OST") << "Tracking method                 main vtx  non vtx  no fld  laser" << endm;
  gMessMgr->Message("", "I", "OST") << "Max. tracklet angle (rad).......: " << mMaxAngleTracklet[0] << "     " 
				    << mMaxAngleTracklet[1] << "    " << mMaxAngleTracklet[2] << "   " 
				    << mMaxAngleTracklet[3] << endm; 
  gMessMgr->Message("", "I", "OST") << "Max. track angle (rad)..........: " << mMaxAngleTrack[0] << "     " 
				    << mMaxAngleTrack[1] << "    " << mMaxAngleTrack[2] << "  " 
				    << mMaxAngleTrack[3] << endm; 
  gMessMgr->Message("", "I", "OST") << "Max. dist. in circle fit (1/cm).: " << mMaxCircleDist[0] << "     " 
				    << mMaxCircleDist[1] << "     " << mMaxCircleDist[2] << "   " 
				    << mMaxCircleDist[3] << endm; 
  gMessMgr->Message("", "I", "OST") << "Max. dist. in length fit (cm)...: " << mMaxLengthDist[0] << "       " 
				    << mMaxLengthDist[1] << "       " << mMaxLengthDist[2] << "     " 
				    << mMaxLengthDist[3] << endm; 
  
  gMessMgr->Message("", "I", "OST") << endm;
  gMessMgr->Message("", "I", "OST") << "Settings for split track merging" << endm;
  gMessMgr->Message("", "I", "OST") << "Max. distance (cm): " << mMaxDist << endm; 
  gMessMgr->Message("", "I", "OST") << "Min. point ratio..: " << mMinPointRatio << endm; 
  gMessMgr->Message("", "I", "OST") << "Max. point ratio..: " << mMaxPointRatio << endm; 
  
  gMessMgr->Message("", "I", "OST") << endm;
  gMessMgr->Message("", "I", "OST") << "dE/dx" << endm;
  gMessMgr->Message("", "I", "OST") << "Debugging level..............: " << mDebugLevel << endm; 
  gMessMgr->Message("", "I", "OST") << "Method Id....................: " << mIdMethod << endm; 
  gMessMgr->Message("", "I", "OST") << "Switch for dip/cross angles..: " << mNoAngle << endm; 
  gMessMgr->Message("", "I", "OST") << "Max. allowable hits per track: " << mMaxHit << endm; 
  gMessMgr->Message("", "I", "OST") << "Min. no. of hits required....: " << mMinHit << endm; 
  gMessMgr->Message("", "I", "OST") << "Max. no. of tracks to be used: " << mMaxTrack << endm; 
  gMessMgr->Message("", "I", "OST") << "Length of pad (us/keV).......: " << mPadLength << endm; 
  gMessMgr->Message("", "I", "OST") << "Fraction for trunc. mean.....: " << mFracTrunc << endm; 
  gMessMgr->Message("", "I", "OST") << "a i p (GeV)..................: " << mAip << endm; 
  gMessMgr->Message("", "I", "OST") << "A large number...............: " << mALargeNumber << endm;

  gMessMgr->Message("", "I", "OST") << endm;
  gMessMgr->Message("", "I", "OST") << "FTPC to global transformation" << endm;
  gMessMgr->Message("", "I", "OST") << "Installation point x, y, z (east, cm).: " << InstallationPointX(0) << ", "<<InstallationPointY(0) << ", " << InstallationPointZ(0) << endm;
  gMessMgr->Message("", "I", "OST") << "Installation point x, y, z (west, cm).: " << InstallationPointX(1) << ", "<< InstallationPointY(1) << ", " << InstallationPointZ(1) << endm;
  gMessMgr->Message("", "I", "OST") << "Observed vertex offset y (east, cm): " << ObservedVertexOffsetY(0) << endm;
  gMessMgr->Message("", "I", "OST") << "Observed vertex offset y (west, cm): " << ObservedVertexOffsetY(1) << endm;
  gMessMgr->Message("", "I", "OST") << "Observed vertex offset x (east, cm): " << ObservedVertexOffsetX(0) << endm;
  gMessMgr->Message("", "I", "OST") << "Observed vertex offset x (west, cm): " << ObservedVertexOffsetX(1) << endm;
  gMessMgr->Message("", "I", "OST") << "FTPC east to global rotation Y : " << FtpcRotationY(0) << endm;
  gMessMgr->Message("", "I", "OST") << "Global to FTPC east rotation Y : " << FtpcRotationYInverse(0) << endm;
  gMessMgr->Message("", "I", "OST") << "FTPC west to global rotation Y: " << FtpcRotationY(1) << endm;
  gMessMgr->Message("", "I", "OST") << "Global to FTPC west rotation Y : " << FtpcRotationYInverse(1) << endm;
  gMessMgr->Message("", "I", "OST") << "FTPC east to global rotation X : " << FtpcRotationX(0) << endm;
  gMessMgr->Message("", "I", "OST") << "Global to FTPC east rotation X : " << FtpcRotationXInverse(0) << endm;
  gMessMgr->Message("", "I", "OST") << "FTPC west to global rotation X: " << FtpcRotationX(1) << endm;
  gMessMgr->Message("", "I", "OST") << "Global to FTPC west rotation X : " << FtpcRotationXInverse(1) << endm;
  gMessMgr->Message("", "I", "OST") << "FTPC east to global rotation : " << FtpcRotation(0) << endm;
  gMessMgr->Message("", "I", "OST") << "Global to FTPC east rotation : " << FtpcRotationInverse(0) << endm;
  gMessMgr->Message("", "I", "OST") << "FTPC west to global rotation : " << FtpcRotation(1) << endm;
  gMessMgr->Message("", "I", "OST") << "Global to FTPC west rotation : " << FtpcRotationInverse(1) << endm;
    
  gMessMgr->Message("", "I", "OST") << "TPC to global transformation" << endm;
  gMessMgr->Message("", "I", "OST") << "Position of TPC (cm)..: " << TpcPositionInGlobal() <<endm;
  gMessMgr->Message("", "I", "OST") << "TPC to global rotation: " << TpcToGlobalRotation() << endm;
  gMessMgr->Message("", "I", "OST") << "Global to TPC rotation: " << GlobalToTpcRotation()<< endm;

  return;
}
