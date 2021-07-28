/***************************************************************************
 *
 * $Id: helensV0Cut.h,v 1.5 2000/10/09 21:56:16 laue Exp $
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
 * Revision 1.5  2000/10/09 21:56:16  laue
 * Helens new cuts
 *
 * Revision 1.4  2000/01/25 17:35:02  laue
 * I. In order to run the stand alone version of the StHbtMaker the following
 * changes have been done:
 * a) all ClassDefs and ClassImps have been put into #ifdef __ROOT__ statements
 * b) unnecessary includes of StMaker.h have been removed
 * c) the subdirectory StHbtMaker/doc/Make has been created including everything
 * needed for the stand alone version
 *
 * II. To reduce the amount of compiler warning
 * a) some variables have been type casted
 * b) some destructors have been declared as virtual
 *
 * Revision 1.3  1999/10/15 01:57:02  lisa
 * Important enhancement of StHbtMaker - implement Franks CutMonitors
 * ----------------------------------------------------------
 * This means 3 new files in Infrastructure area (CutMonitor),
 * several specific CutMonitor classes in the Cut area
 * and a new base class in the Base area (StHbtCutMonitor).
 * This means also changing all Cut Base class header files from .hh to .h
 * so we have access to CutMonitor methods from Cint command line.
 * This last means
 * 1) files which include these header files are slightly modified
 * 2) a side benefit: the TrackCuts and V0Cuts no longer need
 * a SetMass() implementation in each Cut class, which was stupid.
 * Also:
 * -----
 * Include Franks StHbtAssociationReader
 * ** None of these changes should affect any user **
 *
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

//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "StHbtMaker/Base/StHbtV0Cut.h"
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

  long              mNV0sPassed;
  long              mNV0sFailed;

  char*             V0Type;                // String selecting v0 (la,antil,k0)

#ifdef __ROOT__ 
  ClassDef(helensV0Cut, 1)
#endif
};


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

inline void helensV0Cut::SetdEdx(const float& charge,
				 const float& m1,  const float& c1,
				 const float& m2, const float& c2)
{mChargedEdx=charge;mdEdx[0]=m1; mdEdx[1]=c1; mdEdx[2]=m2; mdEdx[3]=c2;}

inline void helensV0Cut::SetPt(const float& lo, const float& hi)
{mPt[0]=lo; mPt[1]=hi;}
inline void helensV0Cut::SetRapidity(const float& lo,const float& hi)
{mRapidity[0]=lo; mRapidity[1]=hi;}

inline void helensV0Cut::SetV0Type(const char* type)
{V0Type = (char*)type;}

#endif



