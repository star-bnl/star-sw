// $Id: StFtpcV0.hh,v 1.5 2003/09/02 17:58:19 perev Exp $
//
// $Log: StFtpcV0.hh,v $
// Revision 1.5  2003/09/02 17:58:19  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.4  2000/11/16 12:48:16  jcs
// Save FTPC vzero inforamtion in correct banks
// Use correct FTPC track class
//
// Revision 1.3  2000/01/03 13:16:12  jcs
// Add CVS Id strings
//
///////////////////////////////////////////////////////////
//  This is a class for defining V0s.
//
// Written by Mike Heffner 10/30/98
///////////////////////////////////////////////////////////

#ifndef ST_FTPC_V0
#define ST_FTPC_V0

#include <Stiostream.h>
 
#include "StPhysicalHelix.hh"
#include "StThreeVector.hh"
#include "StLorentzVector.hh"
#include "StGlobals.hh"
#include "V0PhysicalConstants.hh"
#include "PhysicalConstants.h"

#include "StMatrix.hh"
  
//#include "StFtpcTrackMaker/StFtpcTrack.hh"
#include "StFtpcV0Track.hh"

class StFtpcV0: public StFtpcV0Track
{
public:
  StFtpcV0(StFtpcV0Track track1,StFtpcV0Track track2);
  ~StFtpcV0();

  //DATA MEMBERS
  // all data members refer to the v0 unless
  // marked otherwise. i.e. mTrack1Mass doesn't refer to the V0
 protected:
  StDouble mTrack1Mass;
  StDouble mTrack2Mass;
  StDouble mMass;
  StThreeVector<double> mDecayVertex;
  StThreeVector<double> mMomentum;
  StDouble mDca;  // distance of closest approch of the two daughters

  StFtpcV0Track mTrack1;
  StFtpcV0Track mTrack2;

  //MEMBER FUNCTIONS
 public:
  StDouble GetMass() const {return mMass;}
  StDouble GetDca() const {return mDca;}

  StThreeVector<double> GetMomentum() const {return mMomentum;}
  StThreeVector<double> GetDecayVertex() const {return mDecayVertex;}

  StDouble GetTrack1Mass() const {return mTrack1Mass;}
  StDouble GetTrack2Mass() const {return mTrack2Mass;}

  StDouble GetImpactParameter(StThreeVector<double> origin)const;
  StDouble GetTrack1Dca(StThreeVector<double> origin) const;
  StDouble GetTrack2Dca(StThreeVector<double> origin) const;
  StThreeVector<double> GetTrack1Momentum() const;
  StThreeVector<double> GetTrack2Momentum() const;

  StDouble GetKaonMass();
  StDouble GetLambdaMass();
  StDouble GetAntiLambdaMass();

  void SetTrack1Mass(const StDouble mass) {mTrack1Mass=mass;}
  void SetTrack2Mass(const StDouble mass) {mTrack2Mass=mass;}

   virtual double Refit();
  //  virtual StBool IsValid() const;
  virtual int helixIntersect(double zLimit1,double zLimit2);
  void ComputeMassAndMomentum();
 protected:
  int HelixInter(double xcH1,double ycH1,double pitchH1,double radiusH1,double zH1,double phiH1,double xcH2,double ycH2,double pitchH2,double radiusH2,double zH2,double phiH2,double &x,double &y,double &z,double &deltaZ);

  
  virtual void ComputeDecayVertex();

};


#endif //ST_FTPC_V0 
