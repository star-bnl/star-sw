//----------Author:        Markus D. Oldenburg
//----------Last Modified: 25.04.2002
//----------Copyright:     &copy MDO Production 2002

#include "StFtpcTrackingParams.hh"
#include "SystemOfUnits.h"
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


StFtpcTrackingParams::StFtpcTrackingParams() {
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

  mRowScopeTracklet[0] = 1;
  mRowScopeTracklet[1] = 1;
  mRowScopeTracklet[2] = 2;
  mRowScopeTracklet[3] = 3;

  mRowScopeTrack[0] = 2;
  mRowScopeTrack[1] = 2;
  mRowScopeTrack[2] = 2;
  mRowScopeTrack[3] = 2;

  mPhiScope[0] = 1;
  mPhiScope[1] = 1;
  mPhiScope[2] = 1;
  mPhiScope[3] = 2;

  mEtaScope[0] =  1;
  //mEtaScope[0] =  3;  //loose cuts
  mEtaScope[1] =  1;
  mEtaScope[2] =  3;
  mEtaScope[3] = 15;

  mMaxDca[0] = 2.5 * centimeter;
  mMaxDca[1] = 1.0 * centimeter;
  mMaxDca[2] = 1.0 * centimeter;
  mMaxDca[3] = 1.0 * centimeter;

  // Tracklets
  mMaxAngleTracklet[0] = 0.020 * radian;
  mMaxAngleTracklet[1] = 0.007 * radian;
  mMaxAngleTracklet[2] = 0.030 * radian;
  mMaxAngleTracklet[3] = 0.050 * radian;
  // mMaxAngleTracklet[0] = 0.007 * radian;  //Markus
  // mMaxAngleTracklet[0] = 0.030 * radian;  //loose cuts

  // Tracks
  mMaxAngleTrack[0] = 0.060 * radian;
  mMaxAngleTrack[1] = 0.007 * radian;
  mMaxAngleTrack[2] = 0.007 * radian; // not used
  mMaxAngleTrack[3] = 0.007 * radian; // not used
  //mMaxAngleTrack[0] = 0.007 * radian;  //Markus
  //mMaxAngleTrack[0] = 0.070 * radian;  //loose cuts

  mMaxCircleDist[0] = 0.03 * 1/centimeter;
  mMaxCircleDist[1] = 0.03 * 1/centimeter;
  mMaxCircleDist[2] = 0.03 * 1/centimeter; // not used
  mMaxCircleDist[3] = 0.03 * 1/centimeter; // not used
  //mMaxCircleDist[0] = 0.03 * 1/centimeter;  //Markus
  //mMaxCircleDist[0] = 0.30 * 1/centimeter;  //loose cuts

  mMaxLengthDist[0] = 30. * centimeter;
  mMaxLengthDist[1] = 70. * centimeter;
  mMaxLengthDist[2] = 30. * centimeter; // not used
  mMaxLengthDist[3] = 30. * centimeter; // not used
  //mMaxLengthDist[0] = 30. * centimeter;  //Markus
  //mMaxLengthDist[0] = 50. * centimeter;  //loose cuts
  
  // Split tracks
  mMaxDist       = 0.11;
  mMinPointRatio = 0.50;
  mMaxPointRatio = 0.50; 

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
