/***************************************************************************
 *
 * $Id: helensV0Cut.h,v 1.2 1999/10/05 11:37:40 lisa Exp $
 *
 * Authors: Helen Caines, Tom Humanic, Ohio State, humanic@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *    a V0 particle cut that selects on phasespace, particle type,etc..
 *
 ***************************************************************************
 *
 * $Log: helensV0Cut.h,v $
 * Revision 1.2  1999/10/05 11:37:40  lisa
 * Helens realistic V0Cut and Franks memory-sealed McReader
 *
 * Revision 1.1  1999/09/23 23:28:03  lisa
 * add helensV0Cut  AND  rename mikes and franks ParticleCuts to TrackCuts  AND  update documentation
 *
 *
 **************************************************************************/

#ifndef helensV0Cut_hh
#define helensV0Cut_hh

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StHbtMaker/Base/StHbtV0Cut.hh"
#include "StHbtMaker/Infrastructure/StHbtV0.hh"	//9-17-99 seems like good idea

class helensV0Cut : public StHbtV0Cut {

public:

  helensV0Cut();
  //~helensV0Cut();

  virtual bool Pass(const StHbtV0*);

  virtual StHbtString Report();

  void SetV0Type(const char* type);
  void SetV0MassRange(const float& lo, const float& hi);
  void SetdcaV0daughters(const float& lo, const float& hi);
  void SetdcaV0ToPrimVertex(const float& lo, const float& hi);
  void SetdecayLengthV0(const float& lo, const float& hi);
  void SettpcHitsPos(const int& lo, const int& hi);
  void SettpcHitsNeg(const int& lo, const int& hi);
  void SetdcaPosToPrimVertex(const float& lo, const float& hi);
  void SetdcaNegToPrimVertex(const float& lo, const float& hi);
  void SetptArmV0(const float& lo, const float& hi);
  void SetalphaV0(const float& lo, const float& hi);
  void SetPt(const float& lo, const float& hi);
  void SetRapidity(const float& lo, const float& hi);

  virtual void SetMass(const double& mass);  // need this in derived class so 
						//Cint can handle it

private:   // here are the quantities we want to cut on...

  float             mV0MassRange[2];        //Invariant mass limits
  float             mdcaV0daughters[2];     //DCA between 2 tracks
  float             mdcaV0ToPrimVertex[2];  //DCA between V0 and event vertex
  float             mdecayLengthV0[2];      //decay length from prim. vertex
  int               mtpcHitsPos[2];         //no. of tpc hits on pos track
  int               mtpcHitsNeg[2];         //no. of tpc hits on neg track
  float             mdcaPosToPrimVertex[2];  //min. value + track at intersect
  float             mdcaNegToPrimVertex[2];  //min. value - track at intersect
  float             mptArmV0[2];             //pt Armenteros
  float             malphaV0[2];             //alpha Armenteros
  float             mPt[2];                 //pt of V0
  float             mRapidity[2];           //rapidity of V0

  long              mNV0sPassed;
  long              mNV0sFailed;

  char*             V0Type;                // String selecting v0 (la,antil,k0)

  ClassDef(helensV0Cut, 1)

};

inline void helensV0Cut::SetMass(const double& mass) {mMass = mass;}

inline void helensV0Cut::SetV0MassRange(const float& lo, const float& hi) {
mV0MassRange[0] =lo; mV0MassRange[1]=hi;}
inline void helensV0Cut::SetdcaV0daughters(const float& lo, const float& hi)
{mdcaV0daughters[0]=lo; mdcaV0daughters[1]=hi;}
inline void helensV0Cut::SetdcaV0ToPrimVertex(const float& lo, const float& hi)
{mdcaV0ToPrimVertex[0]=lo; mdcaV0ToPrimVertex[1]=hi;}
inline void helensV0Cut::SetdecayLengthV0(const float& lo, const float& hi)
{mdecayLengthV0[0]=lo; mdecayLengthV0[1]=hi;}

inline void helensV0Cut::SettpcHitsPos(const int& lo, const int& hi)
{mtpcHitsPos[0]=lo;mtpcHitsPos[1]=hi;}
inline void helensV0Cut::SettpcHitsNeg(const int& lo, const int& hi)
{mtpcHitsNeg[0]=lo;mtpcHitsNeg[1]=hi;}

inline void helensV0Cut::SetdcaPosToPrimVertex(const float& lo, const float& hi)
{mdcaPosToPrimVertex[0]=lo; mdcaPosToPrimVertex[1]=hi;}
inline void helensV0Cut::SetdcaNegToPrimVertex(const float& lo, const float& hi)
{mdcaNegToPrimVertex[0]=lo; mdcaNegToPrimVertex[1]=hi;}
inline void helensV0Cut::SetptArmV0(const float& lo, const float& hi)
{mptArmV0[0]=lo; mptArmV0[1]=hi;}
inline void helensV0Cut::SetalphaV0(const float& lo, const float& hi)
{malphaV0[0]=lo; malphaV0[1]=hi;}

inline void helensV0Cut::SetPt(const float& lo, const float& hi)
{mPt[0]=lo; mPt[1]=hi;}
inline void helensV0Cut::SetRapidity(const float& lo,const float& hi)
{mRapidity[0]=lo; mRapidity[1]=hi;}

inline void helensV0Cut::SetV0Type(const char* type)
{V0Type = type;}

#endif



