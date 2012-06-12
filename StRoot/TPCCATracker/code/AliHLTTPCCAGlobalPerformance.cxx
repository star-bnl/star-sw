// $Id: AliHLTTPCCAGlobalPerformance.cxx,v 1.13 2012/06/12 18:05:51 fisyak Exp $
// **************************************************************************
// This file is property of and copyright by the ALICE HLT Project          *
// ALICE Experiment at CERN, All rights reserved.                           *
//                                                                          *
// Primary Authors: Sergey Gorbunov <sergey.gorbunov@kip.uni-heidelberg.de> *
//                  Ivan Kisel <kisel@kip.uni-heidelberg.de>                *
//                  for The ALICE HLT Project.                              *
//                                                                          *
// Permission to use, copy, modify and distribute this software and its     *
// documentation strictly for non-commercial purposes is hereby granted     *
// without fee, provided that the above copyright notice appears in all     *
// copies and that both the copyright notice and this permission notice     *
// appear in the supporting documentation. The authors make no claims       *
// about the suitability of this software for any purpose. It is            *
// provided "as is" without express or implied warranty.                    *
//                                                                          *
//***************************************************************************

#include "AliHLTTPCCounters.h"

#include "AliHLTTPCCAPerformanceBase.h"
#include "AliHLTTPCCAGlobalPerformance.h"


#include "AliHLTTPCCAGBHit.h"
#include "AliHLTTPCCAMCTrack.h"
#ifndef HLTCA_STANDALONE
#include "AliHLTTPCCAMCPoint.h"
#endif
#include "AliHLTTPCCAOutTrack.h"
#include "AliHLTTPCCAGBTrack.h"
#include "AliHLTTPCCAGBTracker.h"

#include "AliHLTTPCCATracker.h"

#include "AliHLTTPCCADisplay.h"


#include "TMath.h"
#include "TROOT.h"
#include "Riostream.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TStyle.h"
using namespace std;
void AliHLTTPCCAGlobalPerformance::SetNewEvent(const AliHLTTPCCAGBTracker * const Tracker,
                            AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
                            AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
                            AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints)
{
  AliHLTTPCCAPerformanceBase::SetNewEvent(Tracker, hitLabels, mcTracks, localMCPoints);


  nRecoTracks = fTracker->NTracks();


} // void AliHLTTPCCAGlobalPerformance::SetNewEvent

void AliHLTTPCCAGlobalPerformance::CheckMCTracks()
{

  for ( int imc = 0; imc < nMCTracks; imc++ ) (*fMCTracks)[imc].SetNHits( 0 );

  // for ( int ih = 0; ih < (*fHitLabels).Size(); ih++ ) { // TODO: do we need to calculate consequtive hits??
  //   const AliHLTTPCCAHitLabel &l = (*fHitLabels)[ih];
  //   if ( l.fLab[0] >= 0 ) (*fMCTracks)[l.fLab[0]].SetNHits( (*fMCTracks)[l.fLab[0]].NHits() + 1 );
  //   if ( l.fLab[1] >= 0 ) (*fMCTracks)[l.fLab[1]].SetNHits( (*fMCTracks)[l.fLab[1]].NHits() + 1 );
  //   if ( l.fLab[2] >= 0 ) (*fMCTracks)[l.fLab[2]].SetNHits( (*fMCTracks)[l.fLab[2]].NHits() + 1 );
  // }

  mcData.resize(nMCTracks);
  for ( int imc = 0; imc < nMCTracks; imc++ ) {
    AliHLTTPCCAMCTrack &mc = (*fMCTracks)[imc];
    AliHLTTPCCAPerformanceMCTrackData &mcTrackData = mcData[imc];
    mc.SetSet( 0 );
    mc.SetNReconstructed( 0 );
    mc.SetNTurns( 1 );
//    if ( mc.NHits() >= PParameters::MinimumHitsForMCTrack ){
//      mcTrackData.SetAsReconstructable();
//    }
    if ( mc.NMCPoints() >= PParameters::MinimumMCPointsForMCTrack ){
      mcTrackData.SetAsReconstructable();
    }
    if ( mc.P() >= AliHLTTPCCAParameters::ExtraThreshold ) {
      if ( mc.P() >= AliHLTTPCCAParameters::RefThreshold ) {
        mc.SetSet( 2 );
        mcTrackData.SetSet( 2 );
      }
      else{
        mc.SetSet( 1 );
        mcTrackData.SetSet( 1);
      }
    }
  } // for iMC

} // void AliHLTTPCCAGlobalPerformance::CheckMCTracks()


void AliHLTTPCCAGlobalPerformance::MatchTracks()
{
  recoData.resize(nRecoTracks);
  for ( int itr = 0; itr < nRecoTracks; itr++ ) {
    int traLabels = -1;
    double traPurity = 0;
    const AliHLTTPCCAGBTrack &tCA = fTracker->Track( itr );
    const int nhits = tCA.NHits();
    int *lb = new int[nhits*3];
    int nla = 0;
    for ( int ihit = 0; ihit < nhits; ihit++ ) {
      const int index = fTracker->TrackHit( tCA.FirstHitRef() + ihit );
      const AliHLTTPCCAHitLabel &l = (*fHitLabels)[fTracker->Hit( index ).ID()];
      if ( l.fLab[0] >= 0 ) lb[nla++] = l.fLab[0];
      if ( l.fLab[1] >= 0 ) lb[nla++] = l.fLab[1];
      if ( l.fLab[2] >= 0 ) lb[nla++] = l.fLab[2];
    }
    sort( lb, lb + nla );
    int labmax = -1, labcur = -1, lmax = 0, lcurr = 0;
    for ( int i = 0; i < nla; i++ ) {
      if ( lb[i] != labcur ) {
        if ( labcur >= 0 && lmax < lcurr ) {
          lmax = lcurr;
          labmax = labcur;
        }
        labcur = lb[i];
        lcurr = 0;
      }
      lcurr++;
    }
    if ( labcur >= 0 && lmax < lcurr ) {
      lmax = lcurr;
      labmax = labcur;
    }
    lmax = 0;
    for ( int ihit = 0; ihit < nhits; ihit++ ) {
      const int index = fTracker->TrackHit( tCA.FirstHitRef() + ihit );
      const AliHLTTPCCAHitLabel &l = (*fHitLabels)[fTracker->Hit( index ).ID()];
      if ( l.fLab[0] == labmax || l.fLab[1] == labmax || l.fLab[2] == labmax
         ) lmax++;
    }
    traLabels = labmax;
    traPurity = ( ( nhits > 0 ) ? double( lmax ) / double( nhits ) : 0 );
    if ( lb ) delete[] lb;

    recoData[itr].SetMCTrack(traLabels, traPurity, nhits);
    if ( recoData[itr].IsReco(PParameters::MinTrackPurity, PParameters::MinimumHitsForRecoTrack) ) mcData[traLabels].AddReconstructed();
  } // for iReco
} // void AliHLTTPCCAGlobalPerformance::MatchTracks()


void AliHLTTPCCAGlobalPerformance::EfficiencyPerformance( )
{
  for ( int iRTr = 0; iRTr < nRecoTracks; iRTr++ ) {
    if (  recoData[iRTr].IsGhost(PParameters::MinTrackPurity) )
      fEff.ghosts++;
  }

  for ( int iMCTr = 0; iMCTr < nMCTracks; iMCTr++ ) {
    AliHLTTPCCAPerformanceMCTrackData &mc = mcData[iMCTr];
    if ( !mc.IsReconstructable() ) continue;
    const bool reco = mc.IsReconstructed();
    const int clones = mc.GetNClones();

    if ( mc.GetSet() == 0){ // rest, out track
      fEff.Inc(reco,clones,"rest");
    }
    else{ // good
      fEff.Inc(reco,clones,"total");
      if ( mc.GetSet() == 1){
        fEff.Inc(reco,clones,"extra");
      }
      else{
        fEff.Inc(reco,clones,"ref");
      }
    }
  } // for iMCTr

  AliHLTTPCCAPerformanceBase::EfficiencyPerformance();
} // void AliHLTTPCCAGlobalPerformance::EfficiencyPerformance( )

void AliHLTTPCCAGlobalPerformance::FillHistos()
{
  AliHLTTPCCAPerformanceBase::FillHistos();
  
  for(int iRTr=0; iRTr < nRecoTracks; iRTr++){  // TODO: make common
    AliHLTTPCCAPerformanceRecoTrackData &recoD = recoData[iRTr];

    const AliHLTTPCCAGBTrack &recoTr = fTracker->Track( iRTr );  // TODO: make common
    AliHLTTPCCAMCTrack &mcTr = (*fMCTracks)[ recoD.GetMCTrackId() ];
    
      //    AliHLTTPCCATrackParam param = t.EndPoint();
      //    double p = 1. / param.QPt() * sqrt(1. + param.DzDs()*param.DzDs());
      //     fNVsMom->Fill( param.GetY());
      //     fLengthVsMom->Fill( param.GetY(), t.NHits());
    if (  recoD.IsGhost(SPParameters::MinTrackPurity) ) {
      GetHisto("ghostsLength")->Fill( recoTr.NHits() );
      GetHisto("ghostsRMom")->Fill( mcTr.P() );
    }
    else {
      GetHisto("recosLength")->Fill( recoTr.NHits() );
      GetHisto("recosRMom")->Fill( mcTr.P() );
    }
  }
  
  // global tracker performance
  {
    for ( int itr = 0; itr < nRecoTracks; itr++ ) {
      const int iMC = recoData[itr].GetMCTrackId();

      if ( recoData[itr].IsGhost(PParameters::MinTrackPurity) ) continue;

      const AliHLTTPCCAGBTrack &tCA = fTracker->Track( itr );
      AliHLTTPCCAMCTrack &mc = (*fMCTracks)[iMC];

      int nFirstMC = mc.FirstMCPointID();
      int nMCPoints = mc.NMCPoints();

      AliHLTTPCCALocalMCPoint *points = &((*fLocalMCPoints).Data()[nFirstMC]);

      const AliHLTTPCCAGBTrack &t = fTracker->Track( itr );
      AliHLTTPCCATrackParam p = t.Param();
      int MCindex=-1;
      for(int iMCPoint=0; iMCPoint<nMCPoints; iMCPoint++)
      {
        if(fabs(points[iMCPoint].X() - p.X())< 2.f)
        {
          if(fabs(p.Y() - points[iMCPoint].Y())<2 && fabs(p.Z() - points[iMCPoint].Z())<2)
            MCindex = iMCPoint;
        }
      }
      if(MCindex == -1)
      {
        continue;
      }

      // track resolutions
      while ( 1/*mc.Set() == 2 && TMath::Abs( mc.TPCPar()[0] ) + TMath::Abs( mc.TPCPar()[1] ) > 1*/ ) {
/*        double cosA = TMath::Cos( t.Alpha() );
        double sinA = TMath::Sin( t.Alpha() );
        double mcX =  mc.TPCPar()[0] * cosA + mc.TPCPar()[1] * sinA;
        double mcY = -mc.TPCPar()[0] * sinA + mc.TPCPar()[1] * cosA;
        double mcZ =  mc.TPCPar()[2];
        double mcEx =  mc.TPCPar()[3] * cosA + mc.TPCPar()[4] * sinA;
        double mcEy = -mc.TPCPar()[3] * sinA + mc.TPCPar()[4] * cosA;
        double mcEz =  mc.TPCPar()[5];
        double mcEt = TMath::Sqrt( mcEx * mcEx + mcEy * mcEy );
        if ( TMath::Abs( mcEt ) < 1.e-4 ) break;
        double mcSinPhi = mcEy / mcEt;
        double mcDzDs   = mcEz / mcEt;
        double mcQPt = mc.TPCPar()[6] / mcEt;
        if ( TMath::Abs( mcQPt ) < 1.e-4 ) break;
        double mcPt = 1. / TMath::Abs( mcQPt );
        if ( mcPt < Parameters::RefThreshold ) break;
        if ( t.NHits() <  PParameters::MinimumHitsForMCTrack ) break;
        double bz = fTracker->Slice( 0 ).Param().Bz();*/
	
	
        double mcX =  points[MCindex].X();
        double mcY =  points[MCindex].Y();
        double mcZ =  points[MCindex].Z();
        double mcQP = points[MCindex].QP();
        double mcEx = points[MCindex].Px()*mcQP;
        double mcEy = points[MCindex].Py()*mcQP;
        double mcEz = points[MCindex].Pz()*mcQP;
        double mcEt = TMath::Sqrt( mcEx * mcEx + mcEy * mcEy );
        if ( TMath::Abs( mcEt ) < 1.e-4 ) break;
        double mcSinPhi = mcEy / mcEt;
        double mcDzDs   = mcEz / mcEt;
        double mcQPt =  mcQP / mcEt;
        if ( TMath::Abs( mcQPt ) < 1.e-6 ) break;
        double mcPt = 1. / TMath::Abs( mcQPt );
//        if ( mcPt < Parameters::RefThreshold ) break;
//	if ( t.NHits() <  PParameters::MinimumHitsForMCTrack ) break;
        double bz = fTracker->Slice( 0 ).Param().cBz();
	
        if ( !p.TransportToXWithMaterial( mcX, bz ) ) break;
        if ( p.GetCosPhi()*mcEx < 0 ) { // change direction
          mcSinPhi = -mcSinPhi;
          mcDzDs = -mcDzDs;
          mcQPt = -mcQPt;
        }
        const double kCLight = 0.000299792458;
        double k2QPt = 100;
        if ( TMath::Abs( bz ) > 1.e-4 ) k2QPt = 1. / ( bz * kCLight );
/*        double qPt = p.GetKappa( bz ) * k2QPt;
        double pt = 100;*/
	double qPt = p.GetQPt();
        double pt = 1. / TMath::Abs( qPt );
	
        if ( mcPt < 0.010 ) break;
//        if ( TMath::Abs( qPt ) > 1.e-4 ) pt = 1. / TMath::Abs( qPt );

        GetHisto("resY")->Fill( p.GetY() - mcY );
        GetHisto("resZ")->Fill( p.GetZ() - mcZ );
        GetHisto("resSinPhi")->Fill( p.GetSinPhi() - mcSinPhi );
        GetHisto("resDzDs")->Fill( p.GetDzDs() - mcDzDs );
        if(CAMath::Abs(qPt) > 1.e-8){
          GetHisto("resPt")->Fill( (pt - mcPt)/mcPt );
        }
        if ( p.GetErr2Y() > 0 ) GetHisto("pullY")->Fill( ( p.GetY() - mcY ) / TMath::Sqrt( p.GetErr2Y() ) );
        if ( p.GetErr2Z() > 0 ) GetHisto("pullZ")->Fill( ( p.GetZ() - mcZ ) / TMath::Sqrt( p.GetErr2Z() ) );

        if ( p.GetErr2SinPhi() > 0 ) GetHisto("pullSinPhi")->Fill( ( p.GetSinPhi() - mcSinPhi ) / TMath::Sqrt( p.GetErr2SinPhi() ) );
        if ( p.GetErr2DzDs() > 0 ) GetHisto("pullDzDs")->Fill( ( p.DzDs() - mcDzDs ) / TMath::Sqrt( p.GetErr2DzDs() ) );
        if(CAMath::Abs(qPt) > 1.e-7 && p.GetErr2QPt()>0 ) GetHisto("pullQPt")->Fill( (qPt - mcQPt)/TMath::Sqrt(p.GetErr2QPt()) );

        break;
      }
    }

  }


  // distribution of cluster errors

  {
    int nHits = fTracker->NHits();
    for ( int ih = 0; ih < nHits; ih++ ) {
      const AliHLTTPCCAGBHit &hit = fTracker->Hit( ih );
      const AliHLTTPCCAHitLabel &l = (*fHitLabels)[hit.ID()];
//       fhHitErrY->Fill( hit.ErrY() );
//       fhHitErrZ->Fill( hit.ErrZ() );
      int nmc = 0;
      for ( int il = 0; il < 3; il++ ) if ( l.fLab[il] >= 0 ) nmc++;
//       if ( nmc == 1 ) fhHitShared->Fill( hit.IRow(), 0 );
//       else if ( nmc > 1 ) fhHitShared->Fill( hit.IRow(), 1 );
    }
  }


} // void AliHLTTPCCAGlobalPerformance::FillHistos()
