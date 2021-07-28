#ifndef franksXiCut_hh
#define franksXiCut_hh

//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "StHbtMaker/Base/StHbtXiCut.h"
#include "StHbtMaker/Infrastructure/StHbtXi.hh"	//9-17-99 seems like good idea

class franksXiCut : public StHbtXiCut {

public:

  franksXiCut();
  //~franksXiCut();

  virtual bool Pass(const StHbtXi*);

  virtual StHbtString Report();

  void SetXiMassRange(const float& lo, const float& hi);
  void SetOmegaMassRange(const float& lo, const float& hi);
  void SetdcaXidaughters(const float& lo, const float& hi);
  void SetdcaXiToPrimVertex(const float& lo, const float& hi);
  void SetdecayLengthXi(const float& lo, const float& hi);
  void SettpcHitsBac(const int& lo, const int& hi);
  void SetdcaBacToPrimVertex(const float& lo, const float& hi);
  void SetptArmXi(const float& lo, const float& hi);
  void SetalphaXi(const float& lo, const float& hi);
  void SetPt(const float& lo, const float& hi);
  void SetRapidity(const float& lo, const float& hi);
  void SetdEdx(const float& charge, const float& m1, const float& c1, const float& m2, const float& c2);


private:   // here are the quantities we want to cut on...

  float             mXiMassRange[2];        //Invariant mass limits
  float             mOmegaMassRange[2];        //Invariant mass limits
  float             mdcaXidaughters[2];     //DCA between 2 tracks
  float             mdcaXiToPrimVertex[2];  //DCA between V0 and event vertex
  float             mdecayLengthXi[2];      //decay length from prim. vertex
  int               mtpcHitsBac[2];         //no. of tpc hits on pos track
  float             mdcaBacToPrimVertex[2]; //min. value + track at intersect
  float             mptArmXi[2];            //pt Armenteros
  float             malphaXi[2];            //alpha Armenteros
  float             mPt[2];                 //pt of V0
  float             mRapidity[2];           //rapidity of V0
  float             mdEdx[4];               // dEdx lines for daughter track
  float             mChargedEdx;            // Charge of track to use in dedx

  long              mNXisPassed;
  long              mNXisFailed;


#ifdef __ROOT__ 
  ClassDef(franksXiCut, 1)
#endif
};


inline void franksXiCut::SetXiMassRange(const float& lo, const float& hi) {
mXiMassRange[0] =lo; mXiMassRange[1]=hi;}
inline void franksXiCut::SetOmegaMassRange(const float& lo, const float& hi) {
mOmegaMassRange[0] =lo; mOmegaMassRange[1]=hi;}
inline void franksXiCut::SetdcaXidaughters(const float& lo, const float& hi)
{mdcaXidaughters[0]=lo; mdcaXidaughters[1]=hi;}
inline void franksXiCut::SetdcaXiToPrimVertex(const float& lo, const float& hi)
{mdcaXiToPrimVertex[0]=lo; mdcaXiToPrimVertex[1]=hi;}
inline void franksXiCut::SetdecayLengthXi(const float& lo, const float& hi)
{mdecayLengthXi[0]=lo; mdecayLengthXi[1]=hi;}

inline void franksXiCut::SettpcHitsBac(const int& lo, const int& hi)
{mtpcHitsBac[0]=lo;mtpcHitsBac[1]=hi;}

inline void franksXiCut::SetdcaBacToPrimVertex(const float& lo, const float& hi)
{mdcaBacToPrimVertex[0]=lo; mdcaBacToPrimVertex[1]=hi;}
inline void franksXiCut::SetptArmXi(const float& lo, const float& hi)
{mptArmXi[0]=lo; mptArmXi[1]=hi;}
inline void franksXiCut::SetalphaXi(const float& lo, const float& hi)
{malphaXi[0]=lo; malphaXi[1]=hi;}

inline void franksXiCut::SetdEdx(const float& charge,
				 const float& m1,  const float& c1,
				 const float& m2, const float& c2)
{mChargedEdx=charge;mdEdx[0]=m1; mdEdx[1]=c1; mdEdx[2]=m2; mdEdx[3]=c2;}

inline void franksXiCut::SetPt(const float& lo, const float& hi)
{mPt[0]=lo; mPt[1]=hi;}
inline void franksXiCut::SetRapidity(const float& lo,const float& hi)
{mRapidity[0]=lo; mRapidity[1]=hi;}


#endif

