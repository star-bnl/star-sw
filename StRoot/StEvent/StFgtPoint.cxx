/***************************************************************************
 *
 * $Id: StFgtPoint.cxx,v 2.5 2017/11/20 20:01:49 smirnovd Exp $
 * Author: S. Gliske, Oct 2011
 *
 ***************************************************************************
 *
 * Description: see header file.
 *
 ***************************************************************************
 *
 * $Log: StFgtPoint.cxx,v $
 * Revision 2.5  2017/11/20 20:01:49  smirnovd
 * Remove StRoot/ from included header prefix
 *
 * Revision 2.4  2013/02/19 21:55:57  ullrich
 * Modified charge asymmetry calculation (Anselm).
 *
 * Revision 2.2  2013/01/08 19:53:15  ullrich
 * Added comparison operators.
 *
 * Revision 1.4  2012/12/11 00:13:10  avossen
 * update of StFgtPoint
 *
 * Revision 1.3  2012/03/06 22:28:01  sgliske
 * asserts removed in StFgtPoint constructor.
 * Now must check key after constructing
 *
 * Revision 1.2  2011/11/01 18:42:57  sgliske
 * Added ::Clear(Option_t*) and few other missing things to FGT containers
 *
 * Revision 1.1  2011/10/31 21:51:30  sgliske
 * creation: StEvent containers, take 2
 *
 *
 **************************************************************************/
#include "StFgtHit.h"
#include "StFgtPoint.h"
#include "St_base/StMessMgr.h"

// constructor
StFgtPoint::StFgtPoint( StFgtHit* hit1, StFgtHit* hit2, int key, int rank ) : StHit(), mKey( key ), mRank( rank ) {

   if( !hit1 || !hit2 ){
      LOG_ERROR << "Passed null pointer into StFgtPoint::StFgtPoint( StFgtHit* hit1, StFgtHit* hit2, int key )" << endm;
      mKey = -999;
      return;
   };

   mHitR = mHitPhi = 0;
   if( hit1->getLayer() == 'R' && hit2->getLayer() == 'P' ){
      mHitR = hit1;
      mHitPhi = hit2;
   } else if ( hit2->getLayer() == 'R' && hit1->getLayer() == 'P' ){
      mHitR = hit2;
      mHitPhi = hit1;
   };

   if( !mHitR || !mHitPhi ){
      LOG_ERROR << "Constructor not provided a r/phi pair." << endm;
      mKey = -999;
      return;
   };

   if( mHitR->getDisc() != mHitPhi->getDisc() ){
      LOG_ERROR << "Cluster pair are not on the same disc." << endm;
      mKey = -999;
      return;
   };

   if( mHitR->getQuad() != mHitPhi->getQuad() ){
      LOG_ERROR << "Cluster pair are not on the same quadrant." << endm;
      mKey = -999;
      return;
   };

   mHardwarePosition = mHitR->hardwarePosition();
   mCharge = hit1->charge() + hit2->charge();
   mChargeAsymmetry=(mHitPhi->charge()-mHitR->charge())/mCharge;
   mRank=1;
};

// deconstructor
StFgtPoint::~StFgtPoint() {
   // nothing to do
};

ClassImp(StFgtPoint);
