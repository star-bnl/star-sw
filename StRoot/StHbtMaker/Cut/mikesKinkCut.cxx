/***************************************************************************
 *
 * $Id: mikesKinkCut.cxx,v 1.1 2001/06/01 19:23:54 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: This is an example of a StHbtKinkCut, which cuts on StHbtKinks
 *     in the StHbtEvent.  If a StHbtKink passes the cut, then it becomes a
 *     StHbtParticle to be used in the StHbtAnalysis
 *
 ***************************************************************************
 *
 * $Log: mikesKinkCut.cxx,v $
 * Revision 1.1  2001/06/01 19:23:54  lisa
 * new example class Cut/mikesKinkCut
 *
 *
 **************************************************************************/

#include "StHbtMaker/Cut/mikesKinkCut.h"
#include <cstdio>

#ifdef __ROOT__
ClassImp(mikesKinkCut)
#endif

mikesKinkCut::mikesKinkCut(){
  mNKinksPassed =  mNKinksFailed = 0;
} 
//------------------------------
//mikesKinkCut::~mikesKinkCut(){
//  /* noop */
//}
//------------------------------
bool mikesKinkCut::Pass(const StHbtKink* k){
  bool goodKink =
    ((k->DcaParentDaughter()          > mDcaParentDaughter[0]) &&
     (k->DcaParentDaughter()          < mDcaParentDaughter[1]) &&
     (k->DcaDaughterPrimaryVertex()   > mDcaDaughterPrimaryVertex[0]) &&
     (k->DcaDaughterPrimaryVertex()   < mDcaDaughterPrimaryVertex[1]) &&
     (k->DcaParentPrimaryVertex()     > mDcaParentPrimaryVertex[0]) &&
     (k->DcaParentPrimaryVertex()     < mDcaParentPrimaryVertex[1]) &&
     (k->HitDistanceParentDaughter()  > mHitDistanceParentDaughter[0]) &&
     (k->HitDistanceParentDaughter()  < mHitDistanceParentDaughter[1]) &&
     (k->HitDistanceParentVertex()    > mHitDistanceParentVertex[0]) &&
     (k->HitDistanceParentVertex()    < mHitDistanceParentVertex[1]) &&
     (k->DecayAngle()                 > mDecayAngle[0]) &&
     (k->DecayAngle()                 < mDecayAngle[1]) &&
     (k->DecayAngleCM()               > mDecayAngleCM[0]) &&
     (k->DecayAngleCM()               < mDecayAngleCM[1]));

  goodKink ? mNKinksPassed++ : mNKinksFailed++ ;
  return (goodKink);
}
//------------------------------
StHbtString mikesKinkCut::Report(){
  string Stemp;
  char Ctemp[100];
  sprintf(Ctemp,"\nThis is mikesKinkCut");
  Stemp = Ctemp;
  sprintf(Ctemp,"\nDcaParentDaughter:\t %E-%E",mDcaParentDaughter[0],mDcaParentDaughter[1]);
  Stemp += Ctemp;

  sprintf(Ctemp,"\nDcaDaughterPrimaryVertex:\t %E-%E",mDcaDaughterPrimaryVertex[0],mDcaDaughterPrimaryVertex[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"\nDcaParentPrimaryVertex:\t %E-%E",mDcaParentPrimaryVertex[0],mDcaParentPrimaryVertex[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"\nHitDistanceParentDaughter:\t %E-%E",mHitDistanceParentDaughter[0],mHitDistanceParentDaughter[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"\nHitDistanceParentVertex:\t %E-%E",mHitDistanceParentVertex[0],mHitDistanceParentVertex[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"\nDecayAngle:\t %E-%E",mDecayAngle[0],mDecayAngle[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"\nDecayAngleCM:\t %E-%E",mDecayAngleCM[0],mDecayAngleCM[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"\nNumber of kinks which passed:\t%ld  Number which failed:\t%ld",mNKinksPassed,mNKinksFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
