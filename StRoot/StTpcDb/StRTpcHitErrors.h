/***************************************************************************
 *
 * $Id: StRTpcHitErrors.h,v 1.1 2002/04/02 00:16:30 hardtke Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: TPC Hit Errors Interface 
 *
 ***************************************************************************
 *
 * $Log: StRTpcHitErrors.h,v $
 * Revision 1.1  2002/04/02 00:16:30  hardtke
 * New class that gets hit errors from database
 *
 * Revision 1.1  2002/02/06 18:39:13  hardtke
  *
 *
 **************************************************************************/
#ifndef __STRTPCHitErrors__
#define __STRTPCHitErrors__
//#include <TObject.h>
#include "StMessMgr.h"
#include "StTpcHitErrorsI.h"
#include "tables/St_tpcHitErrors_Table.h"

class StRTpcHitErrors : public StTpcHitErrorsI {

private:

  St_tpcHitErrors* mHitErrors;

public:

  StRTpcHitErrors(St_tpcHitErrors* In=0){AddData(In);}
  ~StRTpcHitErrors(){}
  void AddData(St_tpcHitErrors* In) {
   mHitErrors = In;
  }   

  //Implements Abstract Interface 


   float Sig2IntrinsicOuterX()    const; //sigma^2, intrinsic
   float Sig2DriftOuterX()        const; //sigma^2, drift dependent
   float Sig2TanOuterX()          const; //sigma^2, angular wire

   float Sig2IntrinsicOuterZ()    const; //sigma^2, intrinsic
   float Sig2DriftOuterZ()        const; //sigma^2, drift dependent
   float Sig2TanOuterZ()          const; //sigma^2, angular wire

   float Sig2IntrinsicInnerX()    const; //sigma^2, intrinsic
   float Sig2DriftInnerX()        const; //sigma^2, drift dependent
   float Sig2TanInnerX()          const; //sigma^2, angular wire

   float Sig2IntrinsicInnerZ()    const; //sigma^2, intrinsic
   float Sig2DriftInnerZ()        const; //sigma^2, drift dependent
   float Sig2TanInnerZ()          const; //sigma^2, angular wire


 


 ClassDef(StRTpcHitErrors,0)

};

inline float  StRTpcHitErrors::Sig2IntrinsicOuterX() const { return (*mHitErrors)[0].sig2_intrinsic_outer_x;}
inline float  StRTpcHitErrors::Sig2DriftOuterX() const { return (*mHitErrors)[0].sig2_drift_outer_x;}
inline float  StRTpcHitErrors::Sig2TanOuterX() const { return (*mHitErrors)[0].sig2_tan_outer_x;}

inline float  StRTpcHitErrors::Sig2IntrinsicOuterZ() const { return (*mHitErrors)[0].sig2_intrinsic_outer_z;}
inline float  StRTpcHitErrors::Sig2DriftOuterZ() const { return (*mHitErrors)[0].sig2_drift_outer_z;}
inline float  StRTpcHitErrors::Sig2TanOuterZ() const { return (*mHitErrors)[0].sig2_tan_outer_z;}


inline float  StRTpcHitErrors::Sig2IntrinsicInnerX() const { return (*mHitErrors)[0].sig2_intrinsic_inner_x;}
inline float  StRTpcHitErrors::Sig2DriftInnerX() const { return (*mHitErrors)[0].sig2_drift_inner_x;}
inline float  StRTpcHitErrors::Sig2TanInnerX() const { return (*mHitErrors)[0].sig2_tan_inner_x;}

inline float  StRTpcHitErrors::Sig2IntrinsicInnerZ() const { return (*mHitErrors)[0].sig2_intrinsic_inner_z;}
inline float  StRTpcHitErrors::Sig2DriftInnerZ() const { return (*mHitErrors)[0].sig2_drift_inner_z;}
inline float  StRTpcHitErrors::Sig2TanInnerZ() const { return (*mHitErrors)[0].sig2_tan_inner_z;}


#endif









