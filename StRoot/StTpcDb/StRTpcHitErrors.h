/***************************************************************************
 *
 * $Id: StRTpcHitErrors.h,v 1.1.4.1 2007/08/12 23:27:42 jeromel Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: TPC Hit Errors Interface 
 *
 ***************************************************************************
 *
 * $Log: StRTpcHitErrors.h,v $
 * Revision 1.1.4.1  2007/08/12 23:27:42  jeromel
 * Further fixes for SL06g built for SL44
 *
 * Revision 1.2  2007/08/04 00:38:04  jeromel
 * SL4 issue: Removal of the inline func, moved to class implementation.
 *     Symbols may otherwise be hidden.
 *
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

#endif









