/**********************************************************
 * $Id: StRichMCTrack.h,v 2.1 2000/09/29 01:35:36 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichMCTrack.h,v $
 *  Revision 2.1  2000/09/29 01:35:36  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  Revision 1.1  2000/06/16 02:37:11  horsley
 *  many additions, added features to pad plane display (MIPS, rings, etc)
 *  along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 *  Revision 1.1  2000/04/03 19:36:09  horsley
 *  initial revision
 **********************************************************/
#ifndef STRICHMCTRACK_H
#define STRICHMCTRACK_H

#include "StRichTrackingControl.h"
#include "StParticleDefinition.hh"
#include "StRichPIDMaker/StRichTrack.h"
#include "StThreeVectorF.hh"
#include "StMcTrack.hh"
#include "StRichMCHit.h"
#include <vector>
#include "StMcEvent.hh"

class StRichMCHit;
class StSPtrVecRichHit;
class St_g2t_track;

class StRichMCTrack : public StRichTrack {

public:
  
  StRichMCTrack();
  StRichMCTrack(StTrack*, double);
#ifdef RICH_WITH_L3_TRACKS
  StRichMCTrack(globalTrack*,double);
#endif
  ~StRichMCTrack();
  
  // monte carlo info
  void       setStMcTrack(StMcTrack* );
  StMcTrack* getStMcTrack();
  StMcTrack* getGeantTrack(StRichMCHit*, StMcEvent*, St_g2t_track*);

  void  setGeantRecoMIP(const StSPtrVecRichHit*, StMcEvent*,St_g2t_track * );
  StRichMCHit* getGeantRecoMIP();

  void  setGeantMomentumAtRadiator(StThreeVectorF& mcMomentum);
  void  setGeantImpactPointAtRadiator(StThreeVectorF& mcImpactPoint);

  StThreeVectorF&  getGeantMomentumAtRadiator();
  StThreeVectorF&  getGeantImpactPointAtRadiator();
  StThreeVectorF&  getGeantMIP();

  double  getGeantThetaAtRadiator();
  double  getGeantPhiAtRadiator();
  int     getCommonTpcHits();
  int     getNumberOfPartners();
  int     getNumberOfGeantHitsInRadiator();
  int     getNumberOfGeantHitsInGap();

  vector<StMcRichHit* > getGeantPhotons();
  vector<StRichMCHit* > getRecoGeantPhotons();

  void    setGeantPhoton(StRichMCHit*);
  void    setGeantThetaAtRadiator(double);
  void    setGeantPhiAtRadiator(double);
  void    setGeantMIP(StThreeVectorF );
  void    setCommonTpcHits(int);
  void    setNumberOfPartners(int);
  void    setGeantPhotons(StMcEvent*);
  void    setRecoGeantPhotons(const StSPtrVecRichHit*, StMcEvent*, St_g2t_track*);

  void    useGeantInfo();
  void    useTPCInfo();
  
protected:

  // monte carlo
  StMcTrack* mStMcTrack;

  StThreeVectorF  mMCImpactPoint;
  StThreeVectorF  mMCMomentum;
  StThreeVectorF  mMCMIP;
  StThreeVectorF  mImpactPoint_TPC;
  StThreeVectorF  mMomentum_TPC;
  StThreeVectorF  mMIP_TPC;

  double mMCTheta;
  double mMCPhi;
  int     mCommonTpcHits;
  int     mNumberOfPartners;
  int     mRadiatorHits;
  int     mGapHits;
  StRichMCHit*     mMCRecoMIP;

  vector<StMcRichHit* > mGeantPhotons;
  vector<StRichMCHit* > mRecoGeantPhotons;
};

#endif











