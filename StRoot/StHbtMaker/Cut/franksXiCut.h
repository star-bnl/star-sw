#ifndef franksXiCut_hh
#define franksXiCut_hh

//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "StHbtMaker/Base/StHbtXiCut.h"
#include "StHbtMaker/Infrastructure/StHbtXi.hh"

class franksXiCut : public StHbtXiCut {

public:

  franksXiCut();
  //~franksXiCut();

  virtual bool Pass(const StHbtXi*);

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
  void SetdEdx(const float& charge, const float& m1, const float& c1, const float& m2, const float& c2);


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
  float             mdEdx[4];        // dEdx lines for daughter track
  float             mChargedEdx;            // Charge of track to use in dedx

  long              mNPassed;
  long              mNFailed;

  char*             V0Type;                // String selecting v0 (la,antil,k0)

#ifdef __ROOT__ 
  ClassDef(franksXiCut, 1)
#endif
};


inline void franksXiCut::SetV0MassRange(const float& lo, const float& hi) {
mV0MassRange[0] =lo; mV0MassRange[1]=hi;}
inline void franksXiCut::SetdcaV0daughters(const float& lo, const float& hi)
{mdcaV0daughters[0]=lo; mdcaV0daughters[1]=hi;}
inline void franksXiCut::SetdcaV0ToPrimVertex(const float& lo, const float& hi)
{mdcaV0ToPrimVertex[0]=lo; mdcaV0ToPrimVertex[1]=hi;}
inline void franksXiCut::SetdecayLengthV0(const float& lo, const float& hi)
{mdecayLengthV0[0]=lo; mdecayLengthV0[1]=hi;}

inline void franksXiCut::SettpcHitsPos(const int& lo, const int& hi)
{mtpcHitsPos[0]=lo;mtpcHitsPos[1]=hi;}
inline void franksXiCut::SettpcHitsNeg(const int& lo, const int& hi)
{mtpcHitsNeg[0]=lo;mtpcHitsNeg[1]=hi;}

inline void franksXiCut::SetdcaPosToPrimVertex(const float& lo, const float& hi)
{mdcaPosToPrimVertex[0]=lo; mdcaPosToPrimVertex[1]=hi;}
inline void franksXiCut::SetdcaNegToPrimVertex(const float& lo, const float& hi)
{mdcaNegToPrimVertex[0]=lo; mdcaNegToPrimVertex[1]=hi;}
inline void franksXiCut::SetptArmV0(const float& lo, const float& hi)
{mptArmV0[0]=lo; mptArmV0[1]=hi;}
inline void franksXiCut::SetalphaV0(const float& lo, const float& hi)
{malphaV0[0]=lo; malphaV0[1]=hi;}

inline void franksXiCut::SetdEdx(const float& charge,
				 const float& m1,  const float& c1,
				 const float& m2, const float& c2)
{mChargedEdx=charge;mdEdx[0]=m1; mdEdx[1]=c1; mdEdx[2]=m2; mdEdx[3]=c2;}

inline void franksXiCut::SetPt(const float& lo, const float& hi)
{mPt[0]=lo; mPt[1]=hi;}
inline void franksXiCut::SetRapidity(const float& lo,const float& hi)
{mRapidity[0]=lo; mRapidity[1]=hi;}

inline void franksXiCut::SetV0Type(const char* type)
{V0Type = (char*)type;}

#endif



/***************************************************************************
 *
 * $Id: franksXiCut.h,v 1.1 2001/09/05 20:41:25 laue Exp $
 *
 * Authors: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *
 ***************************************************************************
 *
 * $Log: franksXiCut.h,v $
 * Revision 1.1  2001/09/05 20:41:25  laue
 * Updates of the hbtMuDstTree microDSTs
 *
 *
 **************************************************************************/
