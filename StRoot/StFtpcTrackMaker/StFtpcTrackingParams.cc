//----------Author:        Markus D. Oldenburg
//----------Last Modified: 25.04.2002
//----------Copyright:     &copy MDO Production 2002

#include "StFtpcTrackingParams.hh"
#include "SystemOfUnits.h"

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


StFtpcTrackingParams* StFtpcTrackingParams::Instance(Bool_t debug) {
  // makes new instance or returns old on if it exists already

  if (!mInstance) {
    mInstance = new StFtpcTrackingParams();

    if (debug) {
      mInstance->PrintParams();
    }
  }

  return mInstance;
}


StFtpcTrackingParams::StFtpcTrackingParams() : mTpcToGlobalRotation(3, 3, 1), mGlobalToTpcRotation(3, 3, 1), 
					       mFtpcRotation(3, 3, 1), mFtpcRotationInverse(3, 3, 1)
{
  // default constructor

  InitFromFile();
}

 
StFtpcTrackingParams::~StFtpcTrackingParams() {
  // nothing to be done
}


Int_t StFtpcTrackingParams::Init() {
  // Sets parameters from database

  return 0;
}
  

Int_t StFtpcTrackingParams::InitdEdx() {
  /* Does not work yet.
  St_fde_fdepar *m_fdepar = (St_fde_fdepar *)gime("fdepars/fdepar");
  FDE_FDEPAR_ST *fdepar = m_fdepar->GetTable();

  mDebugLevel = fdepar[0].debug_level;
  mNoAngle = fdepar[0].no_angle;
  mMaxHit = fdepar[0].max_hit; 
  mMinHit = fdepar[0].min_hit;
  mPadLength = fdepar[0].pad_length/100.;

  cout << mPadLength << " --- " << fdepar[0].pad_length * microsecond/keV;

  mFracTrunc = fdepar[0].frac_trun;
  mALargeNumber = fdepar[0].a_large_number;
  */
  return 0;
}


Int_t StFtpcTrackingParams::InitFromFile() {
  
  // Sets parameters as specified in this file.

  // Pion mass
  mM_pi = 0.13957 * GeV;

  // FTPC geometry
  mInnerRadius   =   7.73 * centimeter;
  mOuterRadius   =  30.05 * centimeter;

  mPadRowPosZ[0] = 162.75 * centimeter;
  mPadRowPosZ[1] = 171.25 * centimeter;
  mPadRowPosZ[2] = 184.05 * centimeter;
  mPadRowPosZ[3] = 192.55 * centimeter;
  mPadRowPosZ[4] = 205.35 * centimeter;
  mPadRowPosZ[5] = 213.85 * centimeter;
  mPadRowPosZ[6] = 226.65 * centimeter;
  mPadRowPosZ[7] = 235.15 * centimeter;
  mPadRowPosZ[8] = 247.95 * centimeter;
  mPadRowPosZ[9] = 256.45 * centimeter;

  // Vertex position
  mMaxVertexPosZWarning =  50. * centimeter;
  mMaxVertexPosZError   = 100. * centimeter;

  // Vertex reconstruction
  mHistoBins = 300;
  mHistoMin  = -75.0 * centimeter;
  mHistoMax  = +75.0 * centimeter;

  // Tracker
  mRowSegments =  20; // has to be fixed to the number of rows in both FTPCs
  mPhiSegments = 100;
  mEtaSegments = 200;
  
  // Tracking
  // the 4 indizes represent: 0: main vertex tracking                        
  //                          1: non vertex tracking
  //                          2: no field tracking
  //                          3: laser tracking

  mLaser[0] = kFALSE;
  mLaser[1] = kFALSE;
  mLaser[2] = kTRUE;
  mLaser[3] = kTRUE;

  mVertexConstraint[0] = kTRUE;
  mVertexConstraint[1] = kFALSE;
  mVertexConstraint[2] = kTRUE;
  mVertexConstraint[3] = kFALSE;

  mMaxTrackletLength[0] =  3;
  mMaxTrackletLength[1] =  3;
  mMaxTrackletLength[2] = 10;
  mMaxTrackletLength[3] = 10;

  mMinTrackLength[0] = 5;
  mMinTrackLength[1] = 5;
  mMinTrackLength[2] = 5;
  mMinTrackLength[3] = 5;

  mRowScopeTracklet[0] = 2; // was set to 1 for Markus' PhD thesis
  mRowScopeTracklet[1] = 2;
  mRowScopeTracklet[2] = 2;
  mRowScopeTracklet[3] = 3;

  mRowScopeTrack[0] = 3; // was set to 2 for Markus' PhD thesis
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

  mMaxDca[0] = 2.5 * centimeter;
  mMaxDca[1] = 2.5 * centimeter;
  mMaxDca[2] = 1.0 * centimeter;
  mMaxDca[3] = 1.0 * centimeter;

  // Tracklets
  mMaxAngleTracklet[0] = 0.015 * radian;
  mMaxAngleTracklet[1] = 0.030 * radian;
  mMaxAngleTracklet[2] = 0.030 * radian;
  mMaxAngleTracklet[3] = 0.050 * radian;
  // mMaxAngleTracklet[0] = 0.007 * radian;  // Markus' PhD thesis

  // Tracks
  mMaxAngleTrack[0] = 0.030 * radian; // was set to 0.007 * radian for Markus' PhD thesis
  mMaxAngleTrack[1] = 0.080 * radian;
  mMaxAngleTrack[2] = 0.007 * radian; // not used
  mMaxAngleTrack[3] = 0.007 * radian; // not used

  mMaxCircleDist[0] = 0.05 * 1/centimeter; // was set to 0.03 * 1/centimeter for Markus' PhD thesis
  mMaxCircleDist[1] = 0.05 * 1/centimeter;
  mMaxCircleDist[2] = 0.03 * 1/centimeter; // not used
  mMaxCircleDist[3] = 0.03 * 1/centimeter; // not used

  mMaxLengthDist[0] = 30. * centimeter;
  mMaxLengthDist[1] = 70. * centimeter;
  mMaxLengthDist[2] = 30. * centimeter; // not used
  mMaxLengthDist[3] = 30. * centimeter; // not used
  
  // Split tracks
  mMaxDist       = 0.11;
  mMinPointRatio = 0.50;
  mMaxPointRatio = 0.50; 

  // dE/dx (still missing!)

  // transformation due to rotated and displaced TPC
  // lines commented out go in if TPC database is available

  //gTpcDbPtr = globalDbPointer;  
  //if (gTpcDbPtr->GlobalPosition()) 
  {

    Double_t phi = 0.0;  //large uncertainty, so set to 0
    
    //double theta = gTpcDbPtr->GlobalPosition()->TpcRotationAroundGlobalAxisY();
    //double psi = gTpcDbPtr->GlobalPosition()->TpcRotationAroundGlobalAxisX();
    
    Double_t theta = -0.000381;
    Double_t psi   = -0.000156;
    
    // Debug :
    // cout << TMath::Cos(theta) << " " << TMath::Cos(phi) << endl;
        
    mGlobalToTpcRotation(1, 1) =  TMath::Cos(theta) * TMath::Cos(phi);
    mGlobalToTpcRotation(1, 2) =  TMath::Cos(theta) * TMath::Sin(phi);
    mGlobalToTpcRotation(1, 3) = -TMath::Sin(theta);
    mGlobalToTpcRotation(2, 1) =  TMath::Sin(psi) * TMath::Sin(theta) * TMath::Cos(phi) - TMath::Cos(psi) * TMath::Sin(phi);
    mGlobalToTpcRotation(2, 2) =  TMath::Sin(psi) * TMath::Sin(theta) * TMath::Sin(phi) + TMath::Cos(psi) * TMath::Cos(phi);
    mGlobalToTpcRotation(2, 3) =  TMath::Cos(theta) * TMath::Sin(psi);
    mGlobalToTpcRotation(3, 1) =  TMath::Cos(psi) * TMath::Sin(theta) * TMath::Cos(phi)+TMath::Sin(psi) * TMath::Sin(phi);
    mGlobalToTpcRotation(3, 2) =  TMath::Cos(psi) * TMath::Sin(theta) * TMath::Sin(phi)-TMath::Sin(psi) * TMath::Cos(phi);
    mGlobalToTpcRotation(3, 3) =  TMath::Cos(theta) * TMath::Cos(psi);
    
    UInt_t ierr;
    mTpcToGlobalRotation = mGlobalToTpcRotation.inverse(ierr);
    
    if (ierr!=0) { 
      cerr << "Cant invert rotation matrix" << endl;
      cout << "Global to TPC rotation matrix:" << mGlobalToTpcRotation << endl;
      cout << "TPC to global rotation matrix:" << mTpcToGlobalRotation << endl;
    }
    
    //mTpcPositionInGlobal.setX(gTpcDbPtr->GlobalPosition()->TpcCenterPositionX());
    //mTpcPositionInGlobal.setY(gTpcDbPtr->GlobalPosition()->TpcCenterPositionY());
    //mTpcPositionInGlobal.setZ(gTpcDbPtr->GlobalPosition()->TpcCenterPositionZ());
    mTpcPositionInGlobal.setX(-0.256);
    mTpcPositionInGlobal.setY(-0.082);
    mTpcPositionInGlobal.setZ(-0.192);
    
    // Debug :
    // cout<<"mTpcPositionInGlobal = " << mTpcPositionInGlobal << endl;
  }
  /* 
 else {
   cerr << "StTpcDb IS INCOMPLETE! Cannot contstruct Coordinate transformation." << endl;
   assert(gTpcDbPtr->GlobalPosition());
 }
  */ 
  
   // internal FTPC rotation (East only) [has do be done before local -> global]
   mInstallationPointZ    = -235.8855 * centimeter;
   mObservedVertexOffsetY =    0.258  * centimeter;
 
   // define rotation angle alpha=atan(y_vertex_offset cm/z_installation cm)
   Double_t alpha = TMath::ATan(mObservedVertexOffsetY / TMath::Abs(mInstallationPointZ)); // radians and > 0
   
   // define rotation axis
   Double_t rx = 1.0;
   Double_t ry = 0.0;
   Double_t rz = 0.0;
   // simplify to rotation about x-axis because of very small y-z-offset
 
   // take the normal vector as rotation vector
   Double_t norm_r = TMath::Sqrt(rx*rx + ry*ry + rz*rz);
   rx = rx/norm_r;
   ry = ry/norm_r;
   rz = rz/norm_r;
 
   // rotation maxtrix : rotation about r(rx, ry, rz) with angle alpha [please check!]
   // before that the coordinate system has to be transformed to x_installation, 
   // y_installation, z_installation as new coordinate system origin
   mFtpcRotation(1, 1) = rx * rx * (1 - TMath::Cos(alpha)) + TMath::Cos(alpha);
   mFtpcRotation(1, 2) = ry * rx * (1 - TMath::Cos(alpha)) - rz * TMath::Sin(alpha);
   mFtpcRotation(1, 3) = rz * rx * (1 - TMath::Cos(alpha)) + ry * TMath::Sin(alpha);
   mFtpcRotation(2, 1) = rx * ry * (1 - TMath::Cos(alpha)) + rz * TMath::Sin(alpha);
   mFtpcRotation(2, 2) = ry * ry * (1 - TMath::Cos(alpha)) + TMath::Cos(alpha);
   mFtpcRotation(2, 3) = rz * ry * (1 - TMath::Cos(alpha)) - rx * TMath::Sin(alpha);
   mFtpcRotation(3, 1) = rx * ry * (1 - TMath::Cos(alpha)) - ry * TMath::Sin(alpha);
   mFtpcRotation(3, 2) = ry * rz * (1 - TMath::Cos(alpha)) + rx * TMath::Sin(alpha);
   mFtpcRotation(3, 3) = rz * rz * (1 - TMath::Cos(alpha)) + TMath::Cos(alpha);

   UInt_t ierr;
   mFtpcRotationInverse = mFtpcRotation.inverse(ierr);
   
   if (ierr!=0) { 
     cerr << "Cant invert FTPC rotation matrix" << endl;
     cout << "FTPC rotation matrix:" << mFtpcRotation << endl;
     cout << "Inverse FTPC rotation matrix:" << mFtpcRotationInverse << endl;
   }

  return 1;
}


void StFtpcTrackingParams::PrintParams() {
  // prints params to sreen

  cout << "Used parameters for tracking" << endl;
  cout << "----------------------------" << endl;

  cout << endl;
  cout << "Pion mass (GeV): " << mM_pi << endl; 

  cout << endl;
  cout << "FTPC geometry" << endl;
  cout << "Inner radius (cm): " << mInnerRadius << endl; 
  cout << "Outer radius (cm): " << mOuterRadius << endl; 

  for (Int_t i = 0; i < 10; i++) {
    cout << "z-position of padrow " << i << ": " << mPadRowPosZ[i] << endl;
  }

  cout << endl;
  cout << "Vertex position" << endl;
  cout << "Max. vertex z-position to do tracking w/o warning (cm): " << mMaxVertexPosZWarning << endl; 
  cout << "Max. vertex z-position to do tracking at all (cm): " << mMaxVertexPosZWarning << endl; 

  cout << endl;
  cout << "Vertex reconstruction (with FTPC hits)" << endl;
  cout << "# of histogram bins: " << mHistoBins << endl; 
  cout << "lower boundary of histogram (cm): " << mHistoMin << endl; 
  cout << "upper boundary of histogram (cm): " << mHistoMax << endl; 

  cout << endl;
  cout << "Tracker settings" << endl;
  cout << "# of row segments: " << mRowSegments << endl; 
  cout << "# of phi segments: " << mPhiSegments << endl; 
  cout << "# of eta segments: " << mEtaSegments << endl; 

  cout << endl;
  cout << "Settings for tracking" << endl;
  cout << "Tracking method         main vtx  non vtx  no fld  laser" << endl;
  cout << "Laser tracking switch:      " << (Int_t)mLaser[0] << "        " 
       << (Int_t)mLaser[1] << "        " << (Int_t)mLaser[2] << "      " 
       << (Int_t)mLaser[3] << endl; 
  cout << "Vertex constraint:          " << (Int_t)mVertexConstraint[0] << "        " 
       << (Int_t)mVertexConstraint[1] << "        " << (Int_t)mVertexConstraint[2] << "      " 
       << (Int_t)mVertexConstraint[3] << endl;  
  cout << "Max. tracklet length:       " << mMaxTrackletLength[0] << "        " 
       << mMaxTrackletLength[1] << "       " << mMaxTrackletLength[2] << "     " 
       << mMaxTrackletLength[3] << endl; 
  cout << "Min. track length:          " << mMinTrackLength[0] << "        " 
       << mMinTrackLength[1] << "        " << mMinTrackLength[2] << "      " 
       << mMinTrackLength[3] << endl; 
  cout << "Tracklet row scope          " << mRowScopeTracklet[0] << "        " 
       << mRowScopeTracklet[1] << "        " << mRowScopeTracklet[2] << "      " 
       << mRowScopeTracklet[3] << endl; 
  cout << "Track row scope :           " << mRowScopeTrack[0] << "        " 
       << mRowScopeTrack[1] << "        " << mRowScopeTrack[2] << "      " 
       << mRowScopeTrack[3] << endl; 
  cout << "Phi scope:                  " << mPhiScope[0] << "        " 
       << mPhiScope[1] << "        " << mPhiScope[2] << "      " 
       << mPhiScope[3] << endl; 
  cout << "Eta scope:                  " << mEtaScope[0] << "        " 
       << mEtaScope[1] << "        " << mEtaScope[2] << "     " 
       << mEtaScope[3] << endl; 
  cout << "Max. DCA:                  " << mMaxDca[0] << "       " 
       << mMaxDca[1] << "        " << mMaxDca[2] << "      " 
       << mMaxDca[3] << endl; 

  cout << endl;
  cout << "Cuts for tracking" << endl;
  cout << "Tracking method           main vtx  non vtx  no fld  laser" << endl;
  cout << "Max. tracklet angle:        " << mMaxAngleTracklet[0] << "     " 
       << mMaxAngleTracklet[1] << "    " << mMaxAngleTracklet[2] << "   " 
       << mMaxAngleTracklet[3] << endl; 
  cout << "Max. track angle:           " << mMaxAngleTrack[0] << "     " 
       << mMaxAngleTrack[1] << "    " << mMaxAngleTrack[2] << "  " 
       << mMaxAngleTrack[3] << endl; 
  cout << "Max. dist. in circle fit:   " << mMaxCircleDist[0] << "     " 
       << mMaxCircleDist[1] << "     " << mMaxCircleDist[2] << "   " 
       << mMaxCircleDist[3] << endl; 
  cout << "Max. dist. in length fit:    " << mMaxLengthDist[0] << "       " 
       << mMaxLengthDist[1] << "       " << mMaxLengthDist[2] << "     " 
       << mMaxLengthDist[3] << endl; 
  
  cout << endl;
  cout << "Settings for split track merging" << endl;
  cout << "Max. distance (cm): " << mMaxDist << endl; 
  cout << "Min. point ratio: " << mMinPointRatio << endl; 
  cout << "Max. point ratio: " << mMaxPointRatio << endl; 

  return;
}


// Pion mass
inline Float_t StFtpcTrackingParams::m_pi() { return StFtpcTrackingParams::mM_pi; }

// FTPC geometry
inline Float_t StFtpcTrackingParams::InnerRadius()         { return StFtpcTrackingParams::mInnerRadius;     }
inline Float_t StFtpcTrackingParams::OuterRadius()         { return StFtpcTrackingParams::mOuterRadius;     }
inline Float_t StFtpcTrackingParams::PadRowPosZ(Int_t row) { return StFtpcTrackingParams::mPadRowPosZ[row]; }

// Vertex position
inline Float_t StFtpcTrackingParams::MaxVertexPosZWarning() { return StFtpcTrackingParams::mMaxVertexPosZWarning; }
inline Float_t StFtpcTrackingParams::MaxVertexPosZError()   { return StFtpcTrackingParams::mMaxVertexPosZError;   }

// Vertex reconstruction
inline   Int_t StFtpcTrackingParams::HistoBins()   { return StFtpcTrackingParams::mHistoBins; }
inline Float_t StFtpcTrackingParams::HistoMin()  { return StFtpcTrackingParams::mHistoMin;  }
inline Float_t StFtpcTrackingParams::HistoMax()  { return StFtpcTrackingParams::mHistoMax;  }

// Tracker
inline Int_t StFtpcTrackingParams::RowSegments() { return StFtpcTrackingParams::mRowSegments; }
inline Int_t StFtpcTrackingParams::PhiSegments() { return StFtpcTrackingParams::mPhiSegments; }
inline Int_t StFtpcTrackingParams::EtaSegments() { return StFtpcTrackingParams::mEtaSegments; }

// Tracking
inline  Bool_t StFtpcTrackingParams::Laser(Int_t tracking_method)             { return StFtpcTrackingParams::mLaser[tracking_method];             }
inline  Bool_t StFtpcTrackingParams::VertexConstraint(Int_t tracking_method)  { return StFtpcTrackingParams::mVertexConstraint[tracking_method];  }
inline   Int_t StFtpcTrackingParams::MaxTrackletLength(Int_t tracking_method) { return StFtpcTrackingParams::mMaxTrackletLength[tracking_method]; }
inline   Int_t StFtpcTrackingParams::MinTrackLength(Int_t tracking_method)    { return StFtpcTrackingParams::mMinTrackLength[tracking_method];    }
inline   Int_t StFtpcTrackingParams::RowScopeTracklet(Int_t tracking_method)  { return StFtpcTrackingParams::mRowScopeTracklet[tracking_method];  }
inline   Int_t StFtpcTrackingParams::RowScopeTrack(Int_t tracking_method)     { return StFtpcTrackingParams::mRowScopeTrack[tracking_method];     }
inline   Int_t StFtpcTrackingParams::PhiScope(Int_t tracking_method)          { return StFtpcTrackingParams::mPhiScope[tracking_method];          }
inline   Int_t StFtpcTrackingParams::EtaScope(Int_t tracking_method)          { return StFtpcTrackingParams::mEtaScope[tracking_method];          }
inline Float_t StFtpcTrackingParams::MaxDca(Int_t tracking_method)            { return StFtpcTrackingParams::mMaxDca[tracking_method];            }

// Tracklets
inline Float_t StFtpcTrackingParams::MaxAngleTracklet(Int_t tracking_method) { return StFtpcTrackingParams::mMaxAngleTracklet[tracking_method]; }

// Tracks
inline Float_t StFtpcTrackingParams::MaxAngleTrack(Int_t tracking_method) { return StFtpcTrackingParams::mMaxAngleTrack[tracking_method]; }
inline Float_t StFtpcTrackingParams::MaxCircleDist(Int_t tracking_method) { return StFtpcTrackingParams::mMaxCircleDist[tracking_method]; }
inline Float_t StFtpcTrackingParams::MaxLengthDist(Int_t tracking_method) { return StFtpcTrackingParams::mMaxLengthDist[tracking_method]; }

// Split tracks
inline Float_t StFtpcTrackingParams::MaxDist()       { return StFtpcTrackingParams::mMaxDist;       }
inline Float_t StFtpcTrackingParams::MinPointRatio() { return StFtpcTrackingParams::mMinPointRatio; }
inline Float_t StFtpcTrackingParams::MaxPointRatio() { return StFtpcTrackingParams::mMaxPointRatio; }

// dE/dx
inline Int_t StFtpcTrackingParams::DebugLevel() { return StFtpcTrackingParams::mDebugLevel;    }
inline Int_t StFtpcTrackingParams::NoAngle()    { return StFtpcTrackingParams::mNoAngle;       }
inline Int_t StFtpcTrackingParams::MaxHit()     { return StFtpcTrackingParams::mMaxHit;        }
inline Int_t StFtpcTrackingParams::MinHit()     { return StFtpcTrackingParams::mMinHit;        }

inline Double_t StFtpcTrackingParams::PadLength()    { return StFtpcTrackingParams::mPadLength;     }
inline Double_t StFtpcTrackingParams::FracTrunc()    { return StFtpcTrackingParams::mFracTrunc;     }
inline Double_t StFtpcTrackingParams::ALargeNumber() { return StFtpcTrackingParams::mALargeNumber;  }

// transformation due to rotated and displaced TPC
inline      StMatrixD StFtpcTrackingParams::TpcToGlobalRotation() { return mTpcToGlobalRotation; }
inline      StMatrixD StFtpcTrackingParams::GlobalToTpcRotation() { return mGlobalToTpcRotation; }
inline StThreeVectorD StFtpcTrackingParams::TpcPositionInGlobal() { return mTpcPositionInGlobal; } 

inline StMatrixD StFtpcTrackingParams::FtpcRotation()          { return mFtpcRotation;          }
inline StMatrixD StFtpcTrackingParams::FtpcRotationInverse()   { return mFtpcRotationInverse;   }
inline  Double_t StFtpcTrackingParams::InstallationPointZ()    { return mInstallationPointZ;    }
inline  Double_t StFtpcTrackingParams::ObservedVertexOffsetY() { return mObservedVertexOffsetY; }
