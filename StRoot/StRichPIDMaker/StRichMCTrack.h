/**********************************************************
 * $Id: StRichMCTrack.h,v 2.0 2000/08/09 16:26:18 gans Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichMCTrack.h,v $
 *  Revision 2.0  2000/08/09 16:26:18  gans
 *  Naming Convention for TDrawable Ojects. All drawable objects now in StRichDisplayMaker
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
#include "StThreeVector.hh"
#include "StMcTrack.hh"
#include <vector>
#include "StMcEvent.hh"


class StRichMCTrack : public StRichTrack {

public:

  StRichMCTrack();
    StRichMCTrack(StTrack*, double);
#ifdef RICH_WITH_L3_TRACKS
    StRichMCTrack(globalTrack*,double);
#endif
    ~StRichMCTrack();
 
  // monte carlo info
  void        setStMcTrack(StMcTrack* );
  StMcTrack*  getStMcTrack();

  void  setGeantMomentumAtRadiator(StThreeVector<double>& mcMomentum);
  void  setGeantImpactPointAtRadiator(StThreeVector<double>& mcImpactPoint);

  StThreeVector<double>&  getGeantMomentumAtRadiator();
  StThreeVector<double>&  getGeantImpactPointAtRadiator();
  StThreeVector<double>&  getGeantMIP();
  StThreeVector<double>&  getGeantStopVertex();

  double  getGeantThetaAtRadiator();
  double  getGeantPhiAtRadiator();
  int     getCommonTpcHits();
  int     getNumberOfPartners();
  int     getNumberOfGeantHitsInRadiator();
  int     getNumberOfGeantHitsInGap();
  vector<StThreeVector<double> > getGeantPhotons();


  void    setGeantThetaAtRadiator(double);
  void    setGeantPhiAtRadiator(double);
  void    setGeantMIP(StThreeVector<double> );
  void    setCommonTpcHits(int);
  void    setNumberOfPartners(int);
  void    setGeantStopVertex(StThreeVector<double>& );
  void    setGeantPhotons(StMcEvent* mEvent);

  void    useGeantInfo();
  void    useTPCInfo();
  
protected:

  // monte carlo
  StMcTrack* mStMcTrack;

  StThreeVector<double>  mMCImpactPoint;
  StThreeVector<double>  mMCMomentum;
  StThreeVector<double>  mMCMIP;
  StThreeVector<double>  mMCStopVertex;
  
  StThreeVector<double>  mImpactPoint_TPC;
  StThreeVector<double>  mMomentum_TPC;
  StThreeVector<double>  mMIP_TPC;

  double mMCTheta;
  double mMCPhi;
  int     mCommonTpcHits;
  int     mNumberOfPartners;
  int     mRadiatorHits;
  int     mGapHits;

  vector<StThreeVector<double> > mGeantPhotons;
};

#endif











