// $Id: AliHLTTPCCAStiPerformance.cxx,v 1.14 2012/08/13 19:35:05 fisyak Exp $
// **************************************************************************
// This file is property of and copyright by the ALICE HLT Project          *
// ALICE Experiment at CERN, All rights reserved.                           *
//                                                                          *
// Primary Authors: Sergey Gorbunov <sergey.gorbunov@kip.uni-heidelberg.de> *
//                  Ivan Kisel <kisel@kip.uni-heidelberg.de>                *
//                  for The ALICE HLT Project.                              *
//                                                                          *
// Developed by:   Igor Kulakov <I.Kulakov@gsi.de>                          *
//                 Maksym Zyzak <M.Zyzak@gsi.de>                            *
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
#if 1//def DO_TPCCATRACKER_EFF_PERFORMANCE

#include "AliHLTTPCCounters.h"

#include "AliHLTTPCCATrackPerformanceBase.h"
#include "AliHLTTPCCAStiPerformance.h"


#include "AliHLTTPCCAGBHit.h"
#include "AliHLTTPCCAMCTrack.h"
#include "AliHLTTPCCAMCPoint.h"
#include "AliHLTTPCCAOutTrack.h"
#include "AliHLTTPCCAGBTrack.h"
#include "AliHLTTPCCAGBTracker.h"

#include "AliHLTTPCCATracker.h"

#ifndef HLTCA_STANDALONE
#include "AliHLTTPCCADisplay.h"

#include "TMath.h"
#include "TROOT.h"
#include "Riostream.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TStyle.h"
//#define DRAW_STI_DIFF
#endif

#ifdef DRAW_STI_DIFF
#include "AliHLTTPCCADisplay.h"
#include "AliHLTTPCCAPerformance.h"
#endif // DRAW_STI_DIFF

void AliHLTTPCCAStiPerformance::SetNewEvent(const AliHLTTPCCAGBTracker * const tracker,
                            AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
                            AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
                            AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints)
{
  AliHLTTPCCATrackPerformanceBase::SetNewEvent(tracker, hitLabels, mcTracks, localMCPoints);
  Tracks.clear();
  HitIndex.clear();
} // void AliHLTTPCCAStiPerformance::SetNewEvent

void AliHLTTPCCAStiPerformance::CheckMCTracks()
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
        if ( mc.NMCRows() >= fTracker->Slice(0).Param().NRows() ) {
          mc.SetSet( 3 );
          mcTrackData.SetSet( 3 );
        }
      }
      else{
        mc.SetSet( 1 );
        mcTrackData.SetSet( 1);
      }
    }
  } // for iMC
} // void AliHLTTPCCAStiPerformance::CheckMCTracks()


void AliHLTTPCCAStiPerformance::MatchTracks()
{
  recoData.resize(NTracks());
  for ( int itr = 0; itr < NTracks(); itr++ ) {
    int traLabels = -1;
    double traPurity = 0;
    const AliHLTTPCCAGBTrack &tCA = Tracks[itr];
    const int nhits = tCA.NHits();
    int *lb = new int[nhits*3];
    int nla = 0;
    for ( int ihit = 0; ihit < nhits; ihit++ ) {
//      const int index = HitIndex[tCA.FirstHitRef() + ihit];
      const AliHLTTPCCAHitLabel &l = (*fHitLabels)[fTracker->Hit( tCA.FirstHitRef() + ihit ).ID()];
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
 //     const int index = HitIndex[tCA.FirstHitRef() + ihit];
      const AliHLTTPCCAHitLabel &l = (*fHitLabels)[fTracker->Hit( tCA.FirstHitRef() + ihit ).ID()];
      if ( l.fLab[0] == labmax || l.fLab[1] == labmax || l.fLab[2] == labmax
         ) lmax++;
    }
    traLabels = labmax;
    traPurity = ( ( nhits > 0 ) ? double( lmax ) / double( nhits ) : 0 );
    if ( lb ) delete[] lb;
    recoData[itr].SetMCTrack(traLabels, traPurity, nhits);
    if ( recoData[itr].IsReco(PParameters::MinTrackPurity, PParameters::MinimumHitsForRecoTrack) ) mcData[traLabels].AddReconstructed();
  } // for iReco
} // void AliHLTTPCCAStiPerformance::MatchTracks()


void AliHLTTPCCAStiPerformance::EfficiencyPerformance( )
{
  for ( int iRTr = 0; iRTr < NTracks(); iRTr++ ) {
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
      else if ( mc.GetSet() == 2 ) {
        fEff.Inc(reco,clones,"ref");
      }
      else {
        fEff.Inc(reco,clones,"ref");
        fEff.Inc(reco,clones,"long_ref");
      }
    }
  } // for iMCTr

  AliHLTTPCCATrackPerformanceBase::EfficiencyPerformance();
} // void AliHLTTPCCAStiPerformance::EfficiencyPerformance( )

#ifndef HLTCA_STANDALONE
void AliHLTTPCCAStiPerformance::FillHistos()
{
  AliHLTTPCCATrackPerformanceBase::FillHistos();

  for(int iRTr=0; iRTr < nRecoTracks; iRTr++){  // TODO: make common
    AliHLTTPCCAPerformanceRecoTrackData &recoD = recoData[iRTr];

    const AliHLTTPCCAGBTrack &recoTr = fTracker->Tracks()[ iRTr ];  // TODO: make common
    AliHLTTPCCAMCTrack &mcTr = (*fMCTracks)[ recoD.GetMCTrackId() ];

    //AliHLTTPCCATrackParam param = t.EndPoint();
    double RecoPt  = 1. / fabs(recoTr.InnerParam().QPt());
    double RecoMom = RecoPt * sqrt(1. + recoTr.InnerParam().DzDs()*recoTr.InnerParam().DzDs());
      //     fNVsMom->Fill( param.GetY());
      //     fLengthVsMom->Fill( param.GetY(), t.NHits());
    if (  recoD.IsGhost(SPParameters::MinTrackPurity) ) {
      GetHisto(kghostsLength)->Fill( recoTr.NHits() );
      GetHisto(kghostsRMom)->Fill( RecoMom );
      GetHisto(kghostsRPt)->Fill( RecoPt );
      GetHisto(kghostsLengthAndRMom)->Fill( recoTr.NHits() , RecoMom );
      GetHisto(kghostsChi2)->Fill( recoTr.InnerParam().GetChi2() );
      GetHisto(kghostsProb)->Fill( TMath::Prob(recoTr.InnerParam().GetChi2(),recoTr.InnerParam().GetNDF()));
    }
    else {
      GetHisto(krecosLength)->Fill( recoTr.NHits() );
      GetHisto(krecosRMom)->Fill( RecoMom );
      GetHisto(krecosMCMom)->Fill( mcTr.P() );
      GetHisto(krecosRPt)->Fill( RecoPt );
      GetHisto(krecosMCPt)->Fill( mcTr.Pt() );
      GetHisto(krecosLengthAndMCMom)->Fill( recoTr.NHits() , mcTr.P() );
      GetHisto(krecosLengthAndRMom)->Fill( recoTr.NHits() , RecoMom );
      GetHisto(krecosChi2)->Fill( recoTr.InnerParam().GetChi2() );
      GetHisto(krecosProb)->Fill( TMath::Prob(recoTr.InnerParam().GetChi2(),recoTr.InnerParam().GetNDF()));
    }
  }

  // global tracker performance
  {
    for ( int itr = 0; itr < NTracks(); itr++ ) {
      const int iMC = recoData[itr].GetMCTrackId();

      if ( recoData[itr].IsGhost(PParameters::MinTrackPurity) ) continue;

      AliHLTTPCCAMCTrack &mc = (*fMCTracks)[iMC];

      int nFirstMC = mc.FirstMCPointID();
      int nMCPoints = mc.NMCPoints();

      AliHLTTPCCALocalMCPoint *points = &((*fLocalMCPoints).Data()[nFirstMC]);

      const AliHLTTPCCAGBTrack &t = Tracks[itr];
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
//         const double kCLight = 0.000299792458;
        //double k2QPt = 100;
        //if ( TMath::Abs( bz ) > 1.e-4 ) k2QPt = 1. / ( bz * kCLight );
/*        double qPt = p.GetKappa( bz ) * k2QPt;
        double pt = 100;*/
	double qPt = p.GetQPt();
        double pt = 1. / TMath::Abs( qPt );
	
        if ( mcPt < 0.010 ) break;
//        if ( TMath::Abs( qPt ) > 1.e-4 ) pt = 1. / TMath::Abs( qPt );

        GetHisto(kresY)->Fill( p.GetY() - mcY );
        GetHisto(kresZ)->Fill( p.GetZ() - mcZ );
        GetHisto(kresSinPhi)->Fill( p.GetSinPhi() - mcSinPhi );
        GetHisto(kresDzDs)->Fill( p.GetDzDs() - mcDzDs );
        if(CAMath::Abs(qPt) > 1.e-8){
          GetHisto(kresPt)->Fill( (pt - mcPt)/mcPt );
        }
        if ( p.GetErr2Y() > 0 ) GetHisto(kpullY)->Fill( ( p.GetY() - mcY ) / TMath::Sqrt( p.GetErr2Y() ) );
        if ( p.GetErr2Z() > 0 ) GetHisto(kpullZ)->Fill( ( p.GetZ() - mcZ ) / TMath::Sqrt( p.GetErr2Z() ) );

        if ( p.GetErr2SinPhi() > 0 ) GetHisto(kpullSinPhi)->Fill( ( p.GetSinPhi() - mcSinPhi ) / TMath::Sqrt( p.GetErr2SinPhi() ) );
        if ( p.GetErr2DzDs() > 0 ) GetHisto(kpullDzDs)->Fill( ( p.DzDs() - mcDzDs ) / TMath::Sqrt( p.GetErr2DzDs() ) );
        if(CAMath::Abs(qPt) > 1.e-7 && p.GetErr2QPt()>0 ) GetHisto(kpullQPt)->Fill( (qPt - mcQPt)/TMath::Sqrt(p.GetErr2QPt()) );

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


} // void AliHLTTPCCAStiPerformance::FillHistos()

void AliHLTTPCCAStiPerformance::Draw()
{
#ifdef DRAW_STI_DIFF
  AliHLTTPCCAPerformance::Instance().Init();
  AliHLTTPCCAPerformance& gbPerfo = AliHLTTPCCAPerformance::Instance();
  AliHLTTPCCADisplay::Instance().SetGB( gbPerfo.GetTracker() );
  AliHLTTPCCADisplay::Instance().SetTPC( fTracker->Slices()[0].Param() );
  AliHLTTPCCADisplay::Instance().SetTPCView();
  AliHLTTPCCADisplay::Instance().DrawTPC();
  AliHLTTPCCADisplay::Instance().DrawGBHits( *gbPerfo.GetTracker(), -1, 0.2, 1  );

  for ( int imc = 0; imc < nMCTracks; imc++ ) {
    AliHLTTPCCAPerformanceMCTrackData &mc = mcData[imc];
    bool doDraw = true;
    doDraw &= (mc.GetSet() >= 2);
    doDraw &= mc.IsReconstructable();
    doDraw &= mc.IsReconstructed();
    doDraw &= !gbPerfo.GetSubPerformance("Global Performance")->GetMCData()[imc].IsReconstructed();
    if ( doDraw )
      AliHLTTPCCADisplay::Instance().SpecDrawMCTrack( (*fMCTracks)[imc], fLocalMCPoints );
  }

  AliHLTTPCCADisplay::Instance().SaveCanvasToFile( "MCTracksDiff.pdf" );
  AliHLTTPCCADisplay::Instance().Ask();
  
#endif // DRAW_STI_DIFF
} // void AliHLTTPCCAStiPerformance::Draw()
#endif // HLTCA_STANDALONE
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
