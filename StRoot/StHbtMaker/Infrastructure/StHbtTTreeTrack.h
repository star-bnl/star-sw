/***************************************************************************
 *
 * $Id: StHbtTTreeTrack.h,v 1.1 2001/06/21 19:15:47 laue Exp $
 *
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/

#ifndef StHbtTTreeTrack_h
#define StHbtTTreeTrack_h

#include "TObject.h"
#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StarClassLibrary/StHelixD.hh"

// a StPhysicalHelix is 104 byes, so we make our own helix

/* class smallHelix { */
/*  public: */
/*   smallHelix() {}; */
/*   smallHelix(const StHelixD& hh) { */
/*     c     = hh.curvature(); */
/*     dip   = hh.dipAngle(); */
/*     phase = hh.phase(); */
/*     x     = hh.origin().x(); */
/*     y     = hh.origin().y(); */
/*     z     = hh.origin().z(); */
/*     h     = hh.h(); */
/*   } */
/*   StPhysicalHelixD physicalHelix() { return StPhysicalHelixD(c,dip,phase,StThreeVectorD(x,y,z),h); } */
/*   StHelixD helix() { return StHelixD(c,dip,phase,StThreeVectorD(x,y,z),h); } */
/*   double c; */
/*   double dip; */
/*   double phase; */
/*   double x; */
/*   double y; */
/*   double z; */
/*   int h; */
/* }; */

class StHbtEvent;
class StHbtTrack;

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
class StHbtTTreeTrack : public TObject {
public:
  StHbtTTreeTrack(){/* no-op*/};//!
  StHbtTTreeTrack(const StHbtEvent*, const StHbtTrack*);//!
  //  virtual ~StHbtTTreeTrack(){/* no-op*/};//!
private:
  Short_t mTrackType;
  Short_t mNHits;
  Short_t mNHitsPoss; 
  Short_t mNHitsDedx;
  Short_t mPidProbElectron;
  Short_t mPidProbPion;
  Short_t mPidProbKaon;
  Short_t mPidProbProton;
  Short_t mHelixH;
  Short_t mHelixGlobalH;
  Short_t mTrackId;
  Float_t mNSigmaElectron;
  Float_t mNSigmaPion;
  Float_t mNSigmaKaon;
  Float_t mNSigmaProton;
  Float_t mdEdx;
  Float_t mChiSqXY;
  Float_t mChiSqZ;
  UInt_t  mMap[2];
  Float_t mHelixC;
  Float_t mHelixDip;
  Float_t mHelixPhase;
  Float_t mHelixX;
  Float_t mHelixY;
  Float_t mHelixZ;
  Float_t mHelixGlobalC;
  Float_t mHelixGlobalDip;
  Float_t mHelixGlobalPhase;
  Float_t mHelixGlobalX;
  Float_t mHelixGlobalY;
  Float_t mHelixGlobalZ;

  friend class StHbtTTreeReader; //!
  friend class StHbtEvent; //!
  friend class StHbtTrack; //!
  ClassDef(StHbtTTreeTrack,1)
};

#endif

/***************************************************************************
 *
 * $Log: StHbtTTreeTrack.h,v $
 * Revision 1.1  2001/06/21 19:15:47  laue
 * Modified fiels:
 *   CTH.hh : new constructor added
 *   StHbtEvent, StHbtKink, StHbtTrack : constructors from the persistent
 *                                   (TTree) classes added
 *   StHbtLikeSignAnalysis : minor changes, for debugging
 *   StHbtTypes: split into different files
 * Added files: for the new TTree muDst's
 *   StExceptions.cxx StExceptions.hh StHbtEnumeration.hh
 *   StHbtHelix.hh StHbtHisto.hh StHbtString.hh StHbtTFile.hh
 *   StHbtTTreeEvent.cxx StHbtTTreeEvent.h StHbtTTreeKink.cxx
 *   StHbtTTreeKink.h StHbtTTreeTrack.cxx StHbtTTreeTrack.h
 *   StHbtTTreeV0.cxx StHbtTTreeV0.h StHbtVector.hh
 *
 *
 **************************************************************************/
