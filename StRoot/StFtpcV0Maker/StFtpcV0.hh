// $Id: StFtpcV0.hh,v 1.3 2000/01/03 13:16:12 jcs Exp $
//
// $Log: StFtpcV0.hh,v $
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

#include <iostream.h>
 
#include "StPhysicalHelix.hh"
#include "StThreeVector.hh"
#include "StLorentzVector.hh"
#include "StGlobals.hh"
#include "V0PhysicalConstants.hh"
#include "PhysicalConstants.h"

#include "StMatrix.hh"
  
#include "StFtpcTrack.hh"

class StFtpcV0: public StFtpcTrack
{
public:
  StFtpcV0(StFtpcTrack track1,StFtpcTrack track2);
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

  StFtpcTrack mTrack1;
  StFtpcTrack mTrack2;

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
