// $Id: StFtpcTrackingParams.cc,v 1.8 2002/10/31 13:42:26 oldi Exp $
// $Log: StFtpcTrackingParams.cc,v $
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

#include "tables/St_MagFactor_Table.h"

#include "TMath.h"
#include "iostream.h"

////////////////////////////////////////////////////////////////////////
// StFtpcTrackingParams                                               //
//                                                                    //
// This class provides the necessary parameters for FTPC tracking.    //
////////////////////////////////////////////////////////////////////////

ClassImp(StFtpcTrackingParams)

class St_fde_fdepar;

// Initialization of instance
StFtpcTrackingParams* StFtpcTrackingParams::mInstance = 0;


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


inline StFtpcTrackingParams* StFtpcTrackingParams::Instance() {
  // return instance only
  
  return mInstance;
}


StFtpcTrackingParams::StFtpcTrackingParams(St_ftpcTrackingPars *trackPars,
					   St_fde_fdepar *dEdxPars,
					   St_ftpcDimensions *dimensions, 
					   St_ftpcPadrowZ *zrow) 
  : mTpcToGlobalRotation(3, 3, 1), mGlobalToTpcRotation(3, 3, 1), 
    mFtpcRotation(3, 3, 1), mFtpcRotationInverse(3, 3, 1)
{
  // default constructor

  InitTrackingParams(trackPars->GetTable());
  InitdEdx(dEdxPars->GetTable());
  InitDimensions(dimensions->GetTable());
  InitPadRows(zrow->GetTable());
  InitCoordTransformation();
  ResetMagField();
}


StFtpcTrackingParams::~StFtpcTrackingParams() {
  // delete created pointers
  
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
    mHistoBins = trackParsTable->histoBins;
    mHistoMin  = trackParsTable->histoMin * centimeter;
    mHistoMax  = trackParsTable->histoMax * centimeter;
    
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
      mMaxCircleDist[i] = trackParsTable->maxCircleDist[i] * 1/centimeter;
      mMaxLengthDist[i] = trackParsTable->maxLengthDist[i] * centimeter;
    }  
    
    // Split tracks
    mMaxDist       = trackParsTable->maxDist;
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
    mPadRowPosZ = new Float_t[mNumberOfPadRows];
    
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

  return 1;
}


Int_t StFtpcTrackingParams::InitCoordTransformation(ftpcCoordTrans_st* ftpcCoordTrans) {
  // Set rotation values.

  if (ftpcCoordTrans) {
    
    Float_t old[2] = {mObservedVertexOffsetY[0], mObservedVertexOffsetY[1]};
    
    mObservedVertexOffsetY[0] = ftpcCoordTrans->observedVertexOffsetY[0];
    mObservedVertexOffsetY[1] = ftpcCoordTrans->observedVertexOffsetY[1];  // not used

    if ((mObservedVertexOffsetY[0] != old[0] || mObservedVertexOffsetY[1] != old[1]) && 
	(old[0] < -9990. || old[1] < -9990.)) {
      
      gMessMgr->Message("", "I", "OST") << "Observed vertex offset in y direction has changed. Changed from " 
					<< old[0] << " to " <<  mObservedVertexOffsetY[0] << " (east) and from " 
					<< old[1] << " to " <<  mObservedVertexOffsetY[1] << " (west)." << endm;
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

    Double_t phi   = 0.0;  //large uncertainty, so set to 0
    Double_t theta = gStTpcDb->GlobalPosition()->TpcRotationAroundGlobalAxisY();
    Double_t psi   = gStTpcDb->GlobalPosition()->TpcRotationAroundGlobalAxisX();

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
    
    mTpcPositionInGlobal.setX(gStTpcDb->GlobalPosition()->TpcCenterPositionX());
    mTpcPositionInGlobal.setY(gStTpcDb->GlobalPosition()->TpcCenterPositionY());
    mTpcPositionInGlobal.setZ(gStTpcDb->GlobalPosition()->TpcCenterPositionZ());    
  }
  
  else {
    gMessMgr->Message("", "E", "OST") << "StTpcDb IS INCOMPLETE! Cannot contstruct Coordinate transformation." << endm;
    assert(gStTpcDb->GlobalPosition());
  }
  
  // internal FTPC rotation (East only) [has do be done before local -> global]

  // define rotation angle alpha=atan(y_vertex_offset cm/z_installation cm)
  Double_t alpha = TMath::ATan(mObservedVertexOffsetY[0] / TMath::Abs(mInstallationPointZ[0])); // radians and > 0
  
  // define rotation axis
  // simplify to rotation about x-axis because of very small y-z-offset
  Double_t rx = 1.0;
  Double_t ry = 0.0;
  Double_t rz = 0.0;
  
  // take the normal vector as rotation vector
  Double_t norm_r = TMath::Sqrt(rx*rx + ry*ry + rz*rz);
  rx = rx/norm_r;
  ry = ry/norm_r;
  rz = rz/norm_r;
  
  // rotation maxtrix : rotation about r(rx, ry, rz) with angle alpha [please check!]
  // before that the coordinate system has to be transformed to x_installation, 
  // y_installation, z_installation as new coordinate system origin
  mFtpcRotation(1, 1) = rx * rx * (1 - TMath::Cos(alpha)) +      TMath::Cos(alpha);
  mFtpcRotation(1, 2) = ry * rx * (1 - TMath::Cos(alpha)) - rz * TMath::Sin(alpha);
  mFtpcRotation(1, 3) = rz * rx * (1 - TMath::Cos(alpha)) + ry * TMath::Sin(alpha);
  mFtpcRotation(2, 1) = rx * ry * (1 - TMath::Cos(alpha)) + rz * TMath::Sin(alpha);
  mFtpcRotation(2, 2) = ry * ry * (1 - TMath::Cos(alpha)) +      TMath::Cos(alpha);
  mFtpcRotation(2, 3) = rz * ry * (1 - TMath::Cos(alpha)) - rx * TMath::Sin(alpha);
  mFtpcRotation(3, 1) = rx * ry * (1 - TMath::Cos(alpha)) - ry * TMath::Sin(alpha);
  mFtpcRotation(3, 2) = ry * rz * (1 - TMath::Cos(alpha)) + rx * TMath::Sin(alpha);
  mFtpcRotation(3, 3) = rz * rz * (1 - TMath::Cos(alpha)) + TMath::Cos(alpha);
  
  UInt_t ierr;
  mFtpcRotationInverse = mFtpcRotation.inverse(ierr);
  
  if (ierr!=0) { 
    gMessMgr->Message("", "E", "OST") << "Can't invert FTPC rotation matrix!" << endm;
    gMessMgr->Message("", "I", "OST") << "FTPC rotation matrix:" << mFtpcRotation << endm;
    gMessMgr->Message("", "I", "OST") << "Inverse FTPC rotation matrix:" << mFtpcRotationInverse << endm;
  }
  
  return 1;
}


Int_t StFtpcTrackingParams::ResetMagField(TDataSet *RunLog) {
  // Resets magnetic field if field configuration has changed.
  
  if (RunLog) {
    St_MagFactor *fMagFactor = (St_MagFactor *)RunLog->Find("MagFactor"); 
    
    Float_t newFactor = (*fMagFactor)[0].ScaleFactor;
    
    if (newFactor != mMagFieldFactor) { // field has changed
      
      if (mMagFieldFactor == -9999) { // field will be set the first time
	mMagFieldFactor = newFactor;
	mMagField = new StMagUtilities((EBField)2, mMagFieldFactor, 0);
      }
      
      else { // field has been set before
	gMessMgr->Message("", "I", "OST") << "Magnetic field has changed. Reset magnetic field table and the field factor from " 
					  << mMagFieldFactor << " to " << newFactor << "." << endm;
	mMagFieldFactor = newFactor;
	delete mMagField;
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
  gMessMgr->Message("", "I", "OST") << "Rotation point (east)..............: " << InstallationPointZ(0) << endm;
  gMessMgr->Message("", "I", "OST") << "Rotation point (west)..............: " << InstallationPointZ(1) << " (not used)" << endm;
  gMessMgr->Message("", "I", "OST") << "Observed vertex offset y (east, cm): " << ObservedVertexOffsetY(0) << endm;
  gMessMgr->Message("", "I", "OST") << "Observed vertex offset y (west, cm): " << ObservedVertexOffsetY(1) << " (not used)" << endm;
  gMessMgr->Message("", "I", "OST") << "FTPC to global rotation: " << FtpcRotation() << endm;
  gMessMgr->Message("", "I", "OST") << "Global to FTPC rotation: " << FtpcRotationInverse() << endm;
    
  gMessMgr->Message("", "I", "OST") << "TPC to global transformation" << endm;
  gMessMgr->Message("", "I", "OST") << "Position of TPC (cm)..: " << TpcPositionInGlobal() <<endm;
  gMessMgr->Message("", "I", "OST") << "TPC to global rotation: " << TpcToGlobalRotation() << endm;
  gMessMgr->Message("", "I", "OST") << "Global to TPC rotation: " << GlobalToTpcRotation()<< endm;

  return;
}

// FTPC geometry
inline Float_t StFtpcTrackingParams::InnerRadius()            { return mInnerRadius;            }
inline Float_t StFtpcTrackingParams::OuterRadius()            { return mOuterRadius;            }
inline   Int_t StFtpcTrackingParams::NumberOfPadRows()        { return mNumberOfPadRows;        }
inline   Int_t StFtpcTrackingParams::NumberOfPadRowsPerSide() { return mNumberOfPadRowsPerSide; }
inline Float_t StFtpcTrackingParams::PadRowPosZ(Int_t row)    { return mPadRowPosZ[row];        }

// Vertex position
inline Float_t StFtpcTrackingParams::MaxVertexPosZWarning() { return mMaxVertexPosZWarning; }
inline Float_t StFtpcTrackingParams::MaxVertexPosZError()   { return mMaxVertexPosZError;   }

// Vertex reconstruction
inline   Int_t StFtpcTrackingParams::HistoBins() { return mHistoBins; }
inline Float_t StFtpcTrackingParams::HistoMin()  { return mHistoMin;  }
inline Float_t StFtpcTrackingParams::HistoMax()  { return mHistoMax;  }

// Tracker
inline Int_t StFtpcTrackingParams::RowSegments() { return mRowSegments; }
inline Int_t StFtpcTrackingParams::PhiSegments() { return mPhiSegments; }
inline Int_t StFtpcTrackingParams::EtaSegments() { return mEtaSegments; }

// Tracking
inline  Bool_t StFtpcTrackingParams::Laser(Int_t tracking_method)             { return mLaser[tracking_method];             }
inline  Bool_t StFtpcTrackingParams::VertexConstraint(Int_t tracking_method)  { return mVertexConstraint[tracking_method];  }
inline   Int_t StFtpcTrackingParams::MaxTrackletLength(Int_t tracking_method) { return mMaxTrackletLength[tracking_method]; }
inline   Int_t StFtpcTrackingParams::MinTrackLength(Int_t tracking_method)    { return mMinTrackLength[tracking_method];    }
inline   Int_t StFtpcTrackingParams::RowScopeTracklet(Int_t tracking_method)  { return mRowScopeTracklet[tracking_method];  }
inline   Int_t StFtpcTrackingParams::RowScopeTrack(Int_t tracking_method)     { return mRowScopeTrack[tracking_method];     }
inline   Int_t StFtpcTrackingParams::PhiScope(Int_t tracking_method)          { return mPhiScope[tracking_method];          }
inline   Int_t StFtpcTrackingParams::EtaScope(Int_t tracking_method)          { return mEtaScope[tracking_method];          }
inline Float_t StFtpcTrackingParams::MaxDca(Int_t tracking_method)            { return mMaxDca[tracking_method];            }

// Tracklets
inline Float_t StFtpcTrackingParams::MaxAngleTracklet(Int_t tracking_method) { return mMaxAngleTracklet[tracking_method]; }

// Tracks
inline Float_t StFtpcTrackingParams::MaxAngleTrack(Int_t tracking_method) { return mMaxAngleTrack[tracking_method]; }
inline Float_t StFtpcTrackingParams::MaxCircleDist(Int_t tracking_method) { return mMaxCircleDist[tracking_method]; }
inline Float_t StFtpcTrackingParams::MaxLengthDist(Int_t tracking_method) { return mMaxLengthDist[tracking_method]; }

// Split tracks
inline Float_t StFtpcTrackingParams::MaxDist()       { return mMaxDist;       }
inline Float_t StFtpcTrackingParams::MinPointRatio() { return mMinPointRatio; }
inline Float_t StFtpcTrackingParams::MaxPointRatio() { return mMaxPointRatio; }

// dE/dx
inline Int_t StFtpcTrackingParams::DebugLevel() { return mDebugLevel;    }
inline Int_t StFtpcTrackingParams::IdMethod()   { return mIdMethod;      }
inline Int_t StFtpcTrackingParams::NoAngle()    { return mNoAngle;       }
inline Int_t StFtpcTrackingParams::MaxHit()     { return mMaxHit;        }
inline Int_t StFtpcTrackingParams::MinHit()     { return mMinHit;        }
inline Int_t StFtpcTrackingParams::MaxTrack()   { return mMaxTrack;      }

inline Double_t StFtpcTrackingParams::PadLength()    { return mPadLength;     }
inline Double_t StFtpcTrackingParams::FracTrunc()    { return mFracTrunc;     }
inline Double_t StFtpcTrackingParams::Aip()          { return mAip;           } 
inline Double_t StFtpcTrackingParams::ALargeNumber() { return mALargeNumber;  }

// transformation due to rotated and displaced TPC
inline      StMatrixD StFtpcTrackingParams::TpcToGlobalRotation() { return mTpcToGlobalRotation; }
inline      StMatrixD StFtpcTrackingParams::GlobalToTpcRotation() { return mGlobalToTpcRotation; }
inline StThreeVectorD StFtpcTrackingParams::TpcPositionInGlobal() { return mTpcPositionInGlobal; } 

inline StMatrixD StFtpcTrackingParams::FtpcRotation()                 { return mFtpcRotation;              }
inline StMatrixD StFtpcTrackingParams::FtpcRotationInverse()          { return mFtpcRotationInverse;       }
inline  Double_t StFtpcTrackingParams::InstallationPointZ(Int_t i)    { return (i>=0 && i<=1) ? mInstallationPointZ[i] : 0.; }
inline  Double_t StFtpcTrackingParams::ObservedVertexOffsetY(Int_t i) { return (i>=0 && i<=1) ? mObservedVertexOffsetY[i] : 0.; }

// magnetic field table
inline Float_t StFtpcTrackingParams::MagFieldFactor()   { return mMagFieldFactor; }
inline StMagUtilities *StFtpcTrackingParams::MagField() { return mMagField;       }
