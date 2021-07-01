// $Id: AliHLTTPCCAGlobalPerformance.cxx,v 1.14 2012/08/13 19:35:05 fisyak Exp $
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
#include "AliHLTTPCCAGlobalPerformance.h"

#include "AliHLTTPCCADef.h"

#include "AliHLTTPCCAGBHit.h"
#include "AliHLTTPCCAMCTrack.h"
#include "AliHLTTPCCAMCPoint.h"
#include "AliHLTTPCCAOutTrack.h"
#include "AliHLTTPCCAGBTrack.h"
#include "AliHLTTPCCAGBTracker.h"

#include "AliHLTTPCCATracker.h"

#ifndef HLTCA_STANDALONE
#ifdef MAIN_DRAW
#define DRAW_GLOBALPERF
#endif

#include "TMath.h"
#include "TROOT.h"
#include "Riostream.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TStyle.h"
#endif // HLTCA_STANDALONE

#ifdef DRAW_GLOBALPERF
#include "AliHLTTPCCADisplay.h"
#include "AliHLTTPCCAPerformance.h"
#endif


// #include "TParticlePDG.h"
// #include "TDatabasePDG.h"

void AliHLTTPCCAGlobalPerformance::SetNewEvent(const AliHLTTPCCAGBTracker * const tracker,
                            AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
                            AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
                            AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints)
{
  AliHLTTPCCATrackPerformanceBase::SetNewEvent(tracker, hitLabels, mcTracks, localMCPoints);

  if(fTracker)
    nRecoTracks = fTracker->NTracks();
  else
    nRecoTracks = 0;


} // void AliHLTTPCCAGlobalPerformance::SetNewEvent

void AliHLTTPCCAGlobalPerformance::CheckMCTracks()
{
//  std::cout<<" - CheckMCTracks\n";

  for ( int imc = 0; imc < nMCTracks; imc++ ) (*fMCTracks)[imc].SetNHits( 0 );

  // for ( int ih = 0; ih < (*fHitLabels).Size(); ih++ ) { // TODO: do we need to calculate consequtive hits??
  //   const AliHLTTPCCAHitLabel &l = (*fHitLabels)[ih];
  //   if ( l.fLab[0] >= 0 ) (*fMCTracks)[l.fLab[0]].SetNHits( (*fMCTracks)[l.fLab[0]].NHits() + 1 );
  //   if ( l.fLab[1] >= 0 ) (*fMCTracks)[l.fLab[1]].SetNHits( (*fMCTracks)[l.fLab[1]].NHits() + 1 );
  //   if ( l.fLab[2] >= 0 ) (*fMCTracks)[l.fLab[2]].SetNHits( (*fMCTracks)[l.fLab[2]].NHits() + 1 );
  // }

  mcData.resize(nMCTracks);
//  std::cout<<" _ nMCTracks: "<<nMCTracks<<"\n";
  for ( int imc = 0; imc < nMCTracks; imc++ ) {
    AliHLTTPCCAMCTrack &mc = (*fMCTracks)[imc];
//    std::cout<<" -> imc: "<<imc<<";   p: "<<mc.P()<<"\n";
    AliHLTTPCCAPerformanceMCTrackData &mcTrackData = mcData[imc];
    // ---
    std::map<int,int> rowCounter;
    rowCounter.clear();
    int nHits = fTracker->NHits();
    int nTrHits = 0;
    for ( int ih = 0; ih < nHits; ih++ ) {
      const AliHLTTPCCAGBHit &hit = fTracker->Hit( ih );
      const AliHLTTPCCAHitLabel &l = (*fHitLabels)[hit.ID()];
      const int iMC = (*fLocalMCPoints)[mc.FirstMCPointID()].TrackI();
      if ( l.fLab[0] != iMC && l.fLab[1] != iMC && l.fLab[2] != iMC )
	continue;

      nTrHits++;
      rowCounter.insert( std::pair<int,int>( hit.IRow(), ih ) );
    }
//    std::cout<<"___mc track: "<<imc<<";   nPoints: "<<mc.NMCPoints()<<";   nRows: "<<mc.NMCRows()<<";   nTrHits: "<<nTrHits<<"\n";
    mc.SetNHits( nTrHits );
//    std::cout<<"___mc track: "<<imc<<";   nPoints: "<<mc.NMCPoints()<<";   nRows: "<<mc.NMCRows()<<";   nRowsCalc: "<<rowCounter.size()<<";   nMCHits: "<<mc.NHits()<<";   nTrHits: "<<nTrHits<<"\n";
    int tmpnr = mc.NMCRows();
    mc.SetNMCRows( rowCounter.size() );
    // ---
    mc.SetSet( 0 );
    mc.SetNReconstructed( 0 );
    mc.SetNTurns( 1 );
    if( mc.NMCPoints() > 72 || mc.NHits() > 72 ) continue;	// Loopers cut
    // ---
    if ( mc.NHits() >= 4 && mc.NHits() < 15 ) {
	mcTrackData.SetAsReconstructable_mc4_15_r4();
    }
    else {
	mcTrackData.SetAsNonReconstructable_mc4_15_r4();
    }

    if ( mc.NHits() >= 4 ) {
	mcTrackData.SetAsReconstructable_mc4_r4();
    }
    else {
	mcTrackData.SetAsNonReconstructable_mc4_r4();
    }

    mcTrackData.SetSet_mc4_15_r4( 0 );
    if ( mc.NHits() >= 4 && mc.NHits() < 15 ) {
      if ( mc.P() >= AliHLTTPCCAParameters::ExtraThreshold ) {
        if ( mc.P() >= AliHLTTPCCAParameters::RefThreshold ) {
          mcTrackData.SetSet_mc4_15_r4( 2 );
//            if ( mc.NMCRows() >= fTracker->Slice(0).Param().NRows() ) {
          if ( tmpnr >= fTracker->Slice(0).Param().NRows() ) {
            mcTrackData.SetSet_mc4_15_r4( 3 );
          }
        }
        else{
          mcTrackData.SetSet_mc4_15_r4( 1 );
        }
      }
    }

    mcTrackData.SetSet_mc4_r4( 0 );
    if ( mc.NHits() >= 4 ) {
      if ( mc.P() >= AliHLTTPCCAParameters::ExtraThreshold ) {
        if ( mc.P() >= AliHLTTPCCAParameters::RefThreshold ) {
          mcTrackData.SetSet_mc4_r4( 2 );
//            if ( mc.NMCRows() >= fTracker->Slice(0).Param().NRows() ) {
          if ( tmpnr >= fTracker->Slice(0).Param().NRows() ) {
            mcTrackData.SetSet_mc4_r4( 3 );
          }
        }
        else{
          mcTrackData.SetSet_mc4_r4( 1 );
        }
      }
    }
    // ---
    if ( mc.NHits() >= /*PParameters::MinimumHitsForMCTrack*/10 ){
//    if ( mc.NMCPoints() >= /*PParameters::MinimumMCPointsForMCTrack*/15 ) {
//    if ( mc.NMCRows() >= PParameters::MinimumMCPointsForMCTrack ) {
//    if ( mc.NMCRows() >= /*PParameters::MinimumMCPointsForMCTrack*/15 ) {
      mcTrackData.SetAsReconstructable();
//      std::cout<<" ___ imc: "<<imc<<";   nRows: "<<mc.NMCRows()<<";   nHits: "<<mc.NHits()<<";   nPoints: "<<mc.NMCPoints()<<" - set us reconstructable\n";
//    }

      if ( mc.NMCRows() >= 30 && mc.NHits() <= 72 ) {
        mcTrackData.SetAsReconstructable30();
      }
      else {
        mcTrackData.SetAsNonReconstructable30();
      }

      if ( mc.NMCRows() < 30 && mc.NHits() <= 72 ) {
        mcTrackData.SetAsReconstructable_l30();
        mcTrackData.SetAsReconstructable_l30_r10();
      }
      else {
        mcTrackData.SetAsNonReconstructable_l30();
        mcTrackData.SetAsNonReconstructable_l30_r10();
      }
//    else std::cout<<" ___ imc: "<<imc<<";   nRows: "<<mc.NMCRows()<<";   nHits: "<<mc.NHits()<<";   nPoints: "<<mc.NMCPoints()<<" - skip\n";
    // ---
      mc.SetSet( 1 );
      mcTrackData.SetSet( 1 );
    // ---
//      if( mc.NMCPoints() <= 20 ) {
      if( mc.NHits() <= 20 ) {
//      if( mc.NMCRows() >= 15 && mc.NMCRows() <= 20 ) {
	  mc.SetSet1( 1 );
	  mcTrackData.SetSet1( 1 );
      }
//      if( mc.NMCPoints() > 20 ) {
      if( mc.NHits() > 20 ) {
//    if( mc.NMCRows() > 20 ) {
	  mc.SetSet1( 2 );
	  mcTrackData.SetSet1( 2 );
      }
    // ---

      if ( mc.P() >= AliHLTTPCCAParameters::ExtraThreshold ) {
        if ( mc.P() >= AliHLTTPCCAParameters::RefThreshold ) {
          mc.SetSet( 2 );
          mcTrackData.SetSet( 2 );
//        if ( mc.NMCRows() >= fTracker->Slice(0).Param().NRows() ) {
          if ( tmpnr >= fTracker->Slice(0).Param().NRows() ) {
            mc.SetSet( 3 );
            mcTrackData.SetSet( 3 );
          }
        }
        else{
          mc.SetSet( 1 );
          mcTrackData.SetSet( 1);
        }
      }

      mc.SetSet30( 0 );
      mcTrackData.SetSet30( 0 );
      if ( mc.NMCRows() >= 30 && mc.NHits() <= 72 ) {
        if ( mc.P() >= AliHLTTPCCAParameters::ExtraThreshold ) {
          if ( mc.P() >= AliHLTTPCCAParameters::RefThreshold ) {
            mc.SetSet30( 2 );
            mcTrackData.SetSet30( 2 );
            if ( mc.NMCRows() >= fTracker->Slice(0).Param().NRows() ) {
//        if ( tmpnr >= fTracker->Slice(0).Param().NRows() ) {
              mc.SetSet30( 3 );
              mcTrackData.SetSet30( 3 );
            }
          }
          else{
            mc.SetSet30( 1 );
            mcTrackData.SetSet30( 1 );
          }
        }
      }

      mcTrackData.SetSet_l30( 0 );
      mcTrackData.SetSet_l30_r10( 0 );
      if ( mc.NMCRows() < 30 && mc.NHits() <= 72 ) {
        if ( mc.P() >= AliHLTTPCCAParameters::ExtraThreshold ) {
          if ( mc.P() >= AliHLTTPCCAParameters::RefThreshold ) {
//          mc.SetSet_l30( 2 );
            mcTrackData.SetSet_l30( 2 );
            mcTrackData.SetSet_l30_r10( 2 );
            if ( mc.NMCRows() >= fTracker->Slice(0).Param().NRows() ) {
//        if ( tmpnr >= fTracker->Slice(0).Param().NRows() ) {
//            mc.SetSet30( 3 );
              mcTrackData.SetSet_l30( 3 );
              mcTrackData.SetSet_l30_r10( 3 );
            }
          }
          else{
//          mc.SetSet30( 1 );
            mcTrackData.SetSet_l30( 1 );
            mcTrackData.SetSet_l30_r10( 1 );
          }
        }
      }
    }
  } // for iMC

} // void AliHLTTPCCAGlobalPerformance::CheckMCTracks()

// --- For merging debug and tuning only ---
#include "AliHLTTPCCASliceTrack.h"
#include "AliHLTTPCCASliceOutput.h"
// ---

static float purCut = 0.75;

void AliHLTTPCCAGlobalPerformance::MatchTracks()
{
  std::cout<<" --- MatchTracks - nRecoTracks: "<<nRecoTracks<<"\n";
  std::map<int, float> dzds_map, qpt_map;
  std::map<int, int> loopers_map;
  int mergedMT(0), allMT(0);
  int rst(0);
  recoData.resize(nRecoTracks);
  for ( int itr = 0; itr < nRecoTracks; itr++ ) {
    int traLabels = -1;
    double traPurity = 0;
    const AliHLTTPCCAGBTrack &tCA = fTracker->Tracks()[itr];
    AliHLTTPCCAGBTrack &tCA1 = fTracker->Tracks()[itr];
    tCA1.SetFirstMC(traLabels);
    tCA1.SetReco( 0 );
    tCA1.SetClone( false );
// --- For merging debug and tuning only ---
//    const AliHLTTPCCATracker& slice = fTracker->Slice(0);
//    std::cout<<" > itr: "<<itr<<"Merged: "<<tCA.IsMerged()<<"\n";
//    std::cout<<"-itr: "<<itr<<";   DzDs0: "<<tCA.DzDs0()<<";   QPt0: "<<tCA.QPt0()<<"\n";
// ---
    const int nhits = tCA.NHits();
//    if( nhits < 10 ) continue;
    int *lb = new int[nhits*3];
    int nla = 0;
//    std::cout<<"_NHits: "<<nhits<<";   FirstHitRef: "<<tCA.FirstHitRef()<<"\n";
//if( itr != 146 ) continue;

    if ( fHitLabels->Size() > 0 )
    for ( int ihit = 0; ihit < nhits; ihit++ ) {
//      std::cout<<"___ihit: "<<ihit;
      const int index = fTracker->TrackHit( tCA.FirstHitRef() + ihit );
//      std::cout<<";   index: "<<index;
//if( fTracker->Hit( index ).IRow() < 13 ) continue;
      const AliHLTTPCCAHitLabel &l = (*fHitLabels)[fTracker->Hit( index ).ID()];
//      std::cout<<";   label: "<<l.fLab[0]<<"\n";
//      std::cout<<" ------- ihit: "<<ihit<<";   label: "<<l.fLab[0]<<"\n";
      /*if ( l.fLab[0] >= 0 )*/ lb[nla++] = l.fLab[0];
      if ( l.fLab[1] >= 0 ) lb[nla++] = l.fLab[1];
      if ( l.fLab[2] >= 0 ) lb[nla++] = l.fLab[2];
    }
    sort( lb, lb + nla );
    int labmax = -1, labcur = -1, lmax = 0, lcurr = 0;
    for ( int i = 0; i < nla; i++ ) {
      if ( lb[i] != labcur ) {
        if ( /*labcur >= 0 &&*/ lmax < lcurr ) {
          lmax = lcurr;
          labmax = labcur;
        }
        labcur = lb[i];
        lcurr = 0;
      }
      lcurr++;
    }
    if ( /*labcur >= 0 &&*/ lmax < lcurr ) {
      lmax = lcurr;
      labmax = labcur;
    }
    lmax = 0;
    if ( fHitLabels->Size() > 0 )
    for ( int ihit = 0; ihit < nhits; ihit++ ) {
      const int index = fTracker->TrackHit( tCA.FirstHitRef() + ihit );
//if( fTracker->Hit( index ).IRow() < 13 ) continue;
      const AliHLTTPCCAHitLabel &l = (*fHitLabels)[fTracker->Hit( index ).ID()];
      if ( l.fLab[0] == labmax || l.fLab[1] == labmax || l.fLab[2] == labmax
         ) lmax++;
//      std::cout<<"---ihit: "<<ihit<<";   label: "<<l.fLab[0]<<"   "<<l.fLab[1]<<"   "<<l.fLab[2]<<"\n";
    }
    traLabels = labmax;
    traPurity = ( ( nhits > 0 ) ? double( lmax ) / double( nhits ) : 0 );
    // ---
//    std::cout<<"> itr: "<<itr<<";   nhits: "<<nhits<<";   lmax: "<<lmax<<";   traLabels: "<<traLabels<<";   traPurity: "<<traPurity<<"\n";
    if( traLabels < 0 ) { traPurity = 0; continue; }
    // ---
//    std::cout<<" ---> itr: "<<itr<<";   nhits: "<<nhits<<";   lmax: "<<lmax<<";   traPurity: "<<traPurity<<";   traLabels: "<<traLabels<<"\n";
//    const AliHLTTPCCAMCTrack &mct = (*fMCTracks)[traLabels];
//    std::cout<<" -----> MC Track: PDG: "<<mct.PDG()<<";   NMCRows: "<<mct.NMCRows()<<";   NMCPoints: "<<mct.NMCPoints()<<"\n";
    if ( lb ) delete[] lb;

    recoData[itr].SetMCTrack(traLabels, traPurity, nhits);
//    std::cout<<" ----- itr: "<<itr<<";   traLabels: "<<traLabels<<";   traPurity: "<<traPurity<<";   nhits: "<<nhits<<";   tCA.IsMerged: "<<tCA.IsMerged()<<"\n";
    tCA1.SetFirstMC(traLabels);

    if( (loopers_map.find(traLabels)->second == 1) && !tCA.IsMerged() && mcData[traLabels].IsReconstructed() ) {
      allMT++;
    }
//    if( loopers_map.find(traLabels)->second == 2 ) {
//	allMT--;
//	loopers_map.find(traLabels)->second++;
//    }

//    if( (loopers_map.find(traLabels)->second == 0) && tCA.IsMerged() ) {
//	allMT++;
//	mergedMT++;
//    }
//    std::cout<<" - itr: "<<itr<<" - set MC track: "<<traLabels<<";   with purity: "<<traPurity<<"\n";
//     TParticlePDG* particlePDG = TDatabasePDG::Instance()->GetParticle((*fMCTracks)[traLabels].PDG());
//     Double_t qMC = (particlePDG) ? particlePDG->Charge() :1;
//     int qMCi = qMC>=0?1:-1;
//     int qTr = tCA.Param().QPt()>=0?1:-1;
//
//     if(qMCi==-qTr)
//    if ( recoData[itr].IsReco(PParameters::MinTrackPurity, PParameters::MinimumHitsForRecoTrack) ) {
    if ( mcData[traLabels].IsReconstructable30() && recoData[itr].IsReco(purCut,15) ) {
      mcData[traLabels].AddReconstructed30();
      recoData[itr].SetReconstructed();
      if( mcData[traLabels].GetNClones30() ) recoData[itr].SetClone();
    }

    if ( mcData[traLabels].IsReconstructable_l30() && recoData[itr].IsReco(purCut,15) ) {
      mcData[traLabels].AddReconstructed_l30();
//      recoData[itr].SetReconstructed();
//      if( mcData[traLabels].GetNClones_l30() ) recoData[itr].SetClone();
    }

    if ( mcData[traLabels].IsReconstructable_l30_r10() && recoData[itr].IsReco(purCut,10) ) {
      mcData[traLabels].AddReconstructed_l30_r10();
//      recoData[itr].SetReconstructed();
//      if( mcData[traLabels].GetNClones_l30() ) recoData[itr].SetClone();
    }

    if ( mcData[traLabels].IsReconstructable_mc4_15_r4() && recoData[itr].IsReco(purCut,4) ) {
      mcData[traLabels].AddReconstructed_mc4_15_r4();
    }

    if ( mcData[traLabels].IsReconstructable_mc4_r4() && recoData[itr].IsReco(purCut,4) ) {
      mcData[traLabels].AddReconstructed_mc4_r4();
    }

    if ( recoData[itr].IsReco(0.75,10) ) {
	mcData[traLabels].AddReconstructed1();
    }
    if ( recoData[itr].IsReco(/*PParameters::MinTrackPurity, PParameters::MinimumHitsForRecoTrack*/purCut,10) ) {
//	if( !mcData[traLabels].IsReconstructed() && !tCA.IsMerged() ){
//	    dzds_map.insert( pair<int, float>(traLabels, tCA.DzDs0()) );
//	    qpt_map.insert( pair<int, float>(traLabels, tCA.QPt0()) );
//	    loopers_map.insert( pair<int, int>(traLabels, 1) );
//	}
	if( !mcData[traLabels].IsReconstructed() && tCA.IsMerged() ){
	    allMT++;
	    mergedMT++;
	}
	mcData[traLabels].AddReconstructed();
	tCA1.SetReco( 1 );
if( mcData[traLabels].NReconstructed() == 1 ) {
//    AliHLTTPCCAGBTrack &tCA1 = fTracker->Tracks()[itr];
//    tCA1.SetFirstMC(traLabels);
    mcData[traLabels].SetFirstTrackID(itr);
}
if( (mcData[traLabels].NReconstructed() > 1) && !(recoData[itr].IsGhost(PParameters::MinTrackPurity)) ) {
    AliHLTTPCCAGBTrack &tCA1 = fTracker->Tracks()[itr];
    tCA1.SetClone();
//#ifdef DRAW_GLOBALPERF
//  if ( AliHLTTPCCADisplay::Instance().DrawType() != 3 ) return;
//
//  // AliHLTTPCCAPerformance::Instance().Init();
//  AliHLTTPCCAPerformance& gbPerfo = AliHLTTPCCAPerformance::Instance();
//  AliHLTTPCCADisplay &disp = AliHLTTPCCADisplay::Instance();
//  disp.ClearView();
//  disp.SetGB( gbPerfo.GetTracker() );
//  disp.SetTPC( fTracker->Slices()[0].Param() );
//  disp.SetTPCView();
//  disp.DrawTPC();
//  disp.SpecDrawRecoTrackGlobal( mcData[traLabels].GetFirstTrackID(), kBlack, 0.3 );
//  disp.SpecDrawRecoTrackGlobal( itr, kRed, 0.1 );
//  disp.SaveCanvasToFile( "DrawClones.pdf" );
//  disp.Ask();
//#endif
}
//	std::cout<<"... itr: "<<itr<<";   mc: "<<traLabels<<";   p: "<<(*fMCTracks)[traLabels].P()<<"\n";
    }
//    std::cout<<"_____ map size: "<<dzds_map.size()<<"\n";
//    std::cout<<">itr: "<<itr<<";   nhits: "<<nhits<<";   traLabels: "<<traLabels<<";   traPurity: "<<traPurity<<"\n";
    if ( recoData[itr].IsReco(PParameters::MinTrackPurity, PParameters::MinimumHitsForRecoTrack) ) {
//	std::cout<<">>> Reconstructed\n";
//	std::cout<<" ___ imc: "<<traLabels<<";   nRows: "<<(*fMCTracks)[traLabels].NMCRows()<<";   nHits: "<<(*fMCTracks)[traLabels].NHits()<<";   nPoints: "<<(*fMCTracks)[traLabels].NMCPoints()<<"\n";
	rst++;
    }
  } // for iReco
//  std::cout<<" >>> mergedMT: "<<mergedMT<<";   allMT: "<<allMT<<"\n";
//  std::cout<<" --- global - nRecoT: "<<rst<<"\n";
  if( allMT == 0 ) allMT = 1;
//  std::cout<<" | Merging efficiency:   | "<<100 * mergedMT / allMT<<"% | "<<mergedMT<<" / "<<allMT<<"\n";
//  std::cout<<"\n <><><> reconstructed total: "<<rst<<"\n\n";
} // void AliHLTTPCCAGlobalPerformance::MatchTracks()


void AliHLTTPCCAGlobalPerformance::EfficiencyPerformance( )
{
//  std::cout<<" - EfficiencyPerformance\n";
  for ( int iRTr = 0; iRTr < nRecoTracks; iRTr++ ) {
#if 0
//    if (  recoData[iRTr].IsGhost(PParameters::MinTrackPurity) )
    if (  !recoData[iRTr].IsReco(0,15) )
      {
	const AliHLTTPCCAGBTrack &tCA = fTracker->Tracks()[iRTr];
	int mct = tCA.GetFirstMC();
//	if( mct < 0 ) continue;
//	if( mcData[mct].IsReconstructable() )
	if( mct < 0 )
	  fEff.ghosts++;
      }
    else
//      if (  !recoData[iRTr].IsGhost(PParameters::MinTrackPurity) ) {
      /*if( recoData[iRTr].GetNHits() > 30 )*/ fEff.IncA( recoData[iRTr].GetNHits(), recoData[iRTr].GetPurity() );
//	  std::cout<<" --- iRTr: "<<iRTr<<";   Purity: "<<recoData[iRTr].GetPurity()<<"\n";
//      }
#else
  const AliHLTTPCCAGBTrack &tCA = fTracker->Tracks()[iRTr];
  int mct = tCA.GetFirstMC();
  if( mct < 0 || recoData[iRTr].IsGhost(purCut) ) {
    if( tCA.NHits() >= 10 )
      fEff.ghosts++;
  }
  else {
    fEff.IncA( recoData[iRTr].GetNHits(), recoData[iRTr].GetPurity() );
  }
#endif
  }

//  std::cout<<" --- set nRecoTracks: "<<nRecoTracks<<"\n";
  fEff.SetNRecoTracks( nRecoTracks );
//  std::cout<<" --- set fEff.ghosts: "<<fEff.ghosts<<"\n";
//  std::cout<<" --- nMCTracks: "<<nMCTracks<<"\n";
  int rbl(0), rtd(0), rr(0);

  int notFound(0), wrongFound(0), found(0);
  for ( int iMCTr = 0; iMCTr < nMCTracks; iMCTr++ ) {
    AliHLTTPCCAPerformanceMCTrackData &mc = mcData[iMCTr];
//    if( mc.IsReconstructable() || mc.IsReconstructed() )
//      std::cout<<" ----- iMCTr: "<<iMCTr<<";   reconstractable: "<<mc.IsReconstructable()<<";   reconstructed: "<<mc.IsReconstructed()<<"\n";
    if( mc.IsReconstructable() && !mc.IsReconstructed() ) notFound++;
    if( !mc.IsReconstructable() && mc.IsReconstructed() ) wrongFound++;
    if( mc.IsReconstructable() && mc.IsReconstructed() ) found++;
    if( mc.IsReconstructable() ) rbl++;
    if( mc.IsReconstructed() ) rtd++;
//    if( mc.IsReconstructed() ) std::cout<<" ----- iMCTr: "<<iMCTr<<" - reconstructed\n";
    if( mc.IsReconstructable() && mc.IsReconstructed() ) rr++;
    if ( !mc.IsReconstructable() ) continue;
    const bool reco = mc.IsReconstructed();
    const int clones = mc.GetNClones();
    // ---
    //    const int clones1 = mc.GetNClones1();
    const bool reco11 = mc.IsReconstructed1();
    // ---
    //    const int clones4 = mc.GetNClones_mc4_15_r4();
    //    const bool reco4 = mc.IsReconstructed_mc4_15_r4();

//     const bool reco_all4 = mc.IsReconstructed_mc4_15_r4() || mc.IsReconstructed();
//     const int clones_all4 = mc.GetNClones_all4();
    // ---
//    std::cout<<" --- iMCTr: "<<iMCTr<<";   reco: "<<reco<<";   clones: "<<clones<<";   clones1: "<<clones1<<";   reco11: "<<reco11<<"\n";
//    int clones = 0;
//    if( mc.GetNClones() ) clones = 1;
//    std::cout<<" >>> iMCTr: "<<iMCTr<<";   IsReconstructed: "<<mc.IsReconstructed()<<";   NClones: "<<mc.GetNClones()<<"\n";

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
    if ( mc.GetSet() == 0){
	//
    }
    else {
    if( mc.GetSet1() == 1 ) {
	fEff.Inc(reco,clones,"all_15_20");
    }
    if( mc.GetSet1() == 2 ) {
	fEff.Inc(reco,clones,"all_20");
    }
    }

    if ( mc.GetSet() == 0){
	//
    }
    else {
      fEff.Inc(reco11,clones,"all_mc15_r10");
    }
  } // for iMCTr
  // ---
  for ( int iMCTr = 0; iMCTr < nMCTracks; iMCTr++ ) {
    AliHLTTPCCAPerformanceMCTrackData &mc = mcData[iMCTr];
    if ( !mc.IsReconstructable30() ) continue;
    const bool reco = mc.IsReconstructed30();
    const int clones = mc.GetNClones30();
    if ( mc.GetSet30() == 0){ // rest, out track
      //
    }
    else {
      fEff.Inc(reco,clones,"total30");
      if ( mc.GetSet30() == 1){
        fEff.Inc(reco,clones,"extra30");
      }
      else if ( mc.GetSet30() == 2 ) {
        fEff.Inc(reco,clones,"ref30");
      }
      else {
//	  std::cout<<" --- !!! --- iMCTr: "<<iMCTr<<";   GetSet: "<<mc.GetSet30()<<";   IsReconstructable30: "<<mc.IsReconstructable30()<<";   IsReconstructed30: "<<mc.IsReconstructed30()<<"\n";
        fEff.Inc(reco,clones,"ref30");
        fEff.Inc(reco,clones,"long_ref30");
      }
    }
  }

  for ( int iMCTr = 0; iMCTr < nMCTracks; iMCTr++ ) {
    AliHLTTPCCAPerformanceMCTrackData &mc = mcData[iMCTr];
    if ( !mc.IsReconstructable_l30() ) continue;
    const bool reco = mc.IsReconstructed_l30();
    const int clones = mc.GetNClones_l30();
    if ( mc.GetSet_l30() == 0){ // rest, out track
      //
    }
    else {
      fEff.Inc(reco,clones,"total_l30");
      if ( mc.GetSet_l30() == 1){
        fEff.Inc(reco,clones,"extra_l30");
      }
      else if ( mc.GetSet_l30() == 2 ) {
        fEff.Inc(reco,clones,"ref_l30");
      }
      else {
        fEff.Inc(reco,clones,"ref_l30");
        fEff.Inc(reco,clones,"long_ref_l30");
      }
    }
  }

  for ( int iMCTr = 0; iMCTr < nMCTracks; iMCTr++ ) {
    AliHLTTPCCAPerformanceMCTrackData &mc = mcData[iMCTr];
    if ( !mc.IsReconstructable_l30_r10() ) continue;
    const bool reco = mc.IsReconstructed_l30_r10();
    const int clones = mc.GetNClones_l30_r10();
    if ( mc.GetSet_l30() == 0){ // rest, out track
      //
    }
    else {
      fEff.Inc(reco,clones,"total_l30_r10");
      if ( mc.GetSet_l30_r10() == 1){
        fEff.Inc(reco,clones,"extra_l30_r10");
      }
      else if ( mc.GetSet_l30_r10() == 2 ) {
        fEff.Inc(reco,clones,"ref_l30_r10");
      }
      else {
        fEff.Inc(reco,clones,"ref_l30_r10");
        fEff.Inc(reco,clones,"long_ref_l30_r10");
      }
    }
  }
  // ---
  for ( int iMCTr = 0; iMCTr < nMCTracks; iMCTr++ ) {
    AliHLTTPCCAPerformanceMCTrackData &mc = mcData[iMCTr];
    if ( !mc.IsReconstructable_mc4_15_r4() ) continue;
    const int clones4 = mc.GetNClones_mc4_15_r4();
    const bool reco4 = mc.IsReconstructed_mc4_15_r4();
    if( mc.GetSet_mc4_15_r4() > 0 ) {
      fEff.Inc(reco4,clones4,"total_4_15");
    }
  }

  for ( int iMCTr = 0; iMCTr < nMCTracks; iMCTr++ ) {
    AliHLTTPCCAPerformanceMCTrackData &mc = mcData[iMCTr];
    if ( !mc.IsReconstructable_mc4_r4() ) continue;
    const int clones4 = mc.GetNClones_mc4_r4();
    const bool reco4 = mc.IsReconstructed_mc4_r4();
    if( mc.GetSet_mc4_r4() > 0 ) {
      fEff.Inc(reco4,clones4,"total_4");
    }
  }
  // ---
//  std::cout<<" ------- notFound: "<<notFound<<";   wrongFound: "<<wrongFound<<";   found: "<<found<<"\n";

//  std::cout<<" --- reconstractable: "<<rbl<<";   reconstructed: "<<rtd<<";   recrec: "<<rr<<"\n";
  // ---
//  for ( int iRTr = 0; iRTr < nRecoTracks; iRTr++ ) {
//      std::cout<<" > Track: "<<iRTr<<";   isReco: "<<recoData[iRTr].IsReco()<<";   isGhost: "<<recoData[iRTr].IsGhost(PParameters::MinTrackPurity)<<";   Purity: "<<recoData[iRTr].GetPurity()<<"\n";
//      std::cout<<" _ nHits: "<<recoData[iRTr].GetNHits()<<"\n";
//      recoData[iRTr].Print();
//  }
  // ---

  AliHLTTPCCATrackPerformanceBase::EfficiencyPerformance();
} // void AliHLTTPCCAGlobalPerformance::EfficiencyPerformance( )

#ifndef HLTCA_STANDALONE
void AliHLTTPCCAGlobalPerformance::FillHistos()
{
  AliHLTTPCCATrackPerformanceBase::FillHistos();


    //

  const int NMCTracks = (*fMCTracks).Size();
  vector<int> mcTrackNRecoHits;
  vector<int> nHitsVsRow;
  vector<int> nMCPointsVsRow;
  const int Multiplicity = (*fMCTracks).Size();
  
  mcTrackNRecoHits.resize(NMCTracks, 0);
  nHitsVsRow.resize(AliHLTTPCCAParameters::MaxNumberOfRows8);
  nMCPointsVsRow.resize(AliHLTTPCCAParameters::MaxNumberOfRows8);
  for(int iH=0; iH < fTracker->NHits(); iH++){
    const AliHLTTPCCAGBHit &hit = fTracker->Hit( iH );
    
    nHitsVsRow[hit.IRow()]++;

    if ( fHitLabels->Size() <= 0 ) continue;
    const AliHLTTPCCAHitLabel &l = (*fHitLabels)[hit.ID()];
if( l.fLab[0] < 0 ) continue;
    if ( l.fLab[0] >= 0 ) mcTrackNRecoHits[l.fLab[0]]++;
    if ( l.fLab[1] >= 0 ) mcTrackNRecoHits[l.fLab[1]]++;
    if ( l.fLab[2] >= 0 ) mcTrackNRecoHits[l.fLab[2]]++;
  }
  for(int i=0; i < NMCTracks; i++){
    AliHLTTPCCAMCTrack &mcT = (*fMCTracks)[i];
    // ---
    if( mcT.Set30() <= 0 ) continue;
if( mcTrackNRecoHits[i] < 30 ) continue;
    // ---
        
    GetHisto(kmcTrackNRecoHits)->Fill( mcTrackNRecoHits[i] );
    GetHisto(knMCPointsVsMCMom)->Fill( mcT.P(), mcT.NMCPoints() );
    
    if ( mcT.NMCPoints() > 0 ) {
      double mcEx = mcT.Px();
      double mcEy = mcT.Py();
      double mcEz = mcT.Pz();
      double mcEt = TMath::Sqrt( mcEx * mcEx + mcEy * mcEy );

      const double dZdS = mcEz/mcEt;

      GetHisto(knHitsOverNMCPointsVsMCMom)->Fill( mcT.P(), float(mcTrackNRecoHits[i])/float(mcT.NMCPoints()) );
      GetHisto(knHitsOverNMCPointsVsMCDzDs)->Fill( dZdS, float(mcTrackNRecoHits[i])/float(mcT.NMCPoints()) );
    }
  }

  for(int i=0; i < (*fLocalMCPoints).Size(); i++){
    nMCPointsVsRow[(*fLocalMCPoints)[i].IRow()]++;
  }
  for(int i=0; i < const_cast<AliHLTTPCCAGBTracker *>(fTracker)->Slice(0).Param().NRows(); i++){
    if ( nMCPointsVsRow[i] > 0 ) {
      GetHisto(knHitsOverNMCPointsVsRow)->Fill( i, float(nHitsVsRow[i])/float(nMCPointsVsRow[i]) );
      GetHisto(knHitsOverNMCPointsVsNMCTracks)->Fill( Multiplicity, float(nHitsVsRow[i])/float(nMCPointsVsRow[i]) );
    }
  }
  
//  std::cout<<"___ nRecoTracks: "<<nRecoTracks<<"\n";
  int recoTrC1(0), recoTrC2(0), ghostTr(0);
  for(int iRTr=0; iRTr < nRecoTracks; iRTr++){  // TODO: make common
    AliHLTTPCCAPerformanceRecoTrackData &recoD = recoData[iRTr];
//std::cout<<"   iRTr: "<<iRTr<<";   reco: "<<recoD.IsReconstructed()<<";   clone: "<<recoD.IsClone()<<"\n";
    // ---
 //    const AliHLTTPCCAGBTrack &tCA = fTracker->Tracks()[iRTr];
//if( tCA.NHits() < 4 ) {
//    std::cout<<" ---!!!--- bad tracklet with nHits: "<<tCA.NHits()<<"\n";
//    int aaa;
//    std::cin>>aaa;
//}
//    int mct = tCA.GetFirstMC();
//    if( mct < 0 ) std::cout<<" --- iRTr: "<<iRTr<<";   is ghost;   nHits: "<<tCA.NHits()<<"\n";
//    if (  recoD.IsGhost(0) ) std::cout<<" ----- is ghost 0\n";
    // ---
    //    AliHLTTPCCAMCTrack &mcTr1 = (*fMCTracks)[ recoD.GetMCTrackId() ];
//    if( mcTr1.Set30() <= 0 ) continue;
//    if (  recoD.IsGhost(0) ) std::cout<<" ----- is ghost 1\n";
    if( recoD.IsClone() ) continue;
//    if (  recoD.IsGhost(0) ) std::cout<<" ----- is ghost 2\n";
//if( mcTrackNRecoHits[recoD.GetMCTrackId()] < 30 ) continue;
    recoTrC1++;
    // ---
//    if (  recoD.IsGhost(0) ) std::cout<<"> ghost...1\n";

    const AliHLTTPCCAGBTrack &recoTr = fTracker->Tracks()[iRTr];  // TODO: make common

//    if( recoTr.NHits() < 15 ) continue;
//    std::cout<<"   iRTr: "<<iRTr<<";   reco: "<<recoD.IsReconstructed()<<";   clone: "<<recoD.IsClone()<<"\n";

    //AliHLTTPCCATrackParam param = t.EndPoint();
    double RecoPt  = 1. / fabs(recoTr.InnerParam().QPt());
    double RecoMom = RecoPt * sqrt(1. + recoTr.InnerParam().DzDs()*recoTr.InnerParam().DzDs());
      //     fNVsMom->Fill( param.GetY());
      //     fLengthVsMom->Fill( param.GetY(), t.NHits());

    GetHisto(kpurity)->Fill( recoData[iRTr].GetPurity() );
//    if (  recoD.IsGhost(SPParameters::MinTrackPurity) ) {
    if (  recoD.IsGhost(0) ) {
//	std::cout<<" -----> is ghost 0 - fill histos\n";
      GetHisto(kghostsLength)->Fill( recoTr.NHits() );
      GetHisto(kghostsRMom)->Fill( RecoMom );
      GetHisto(kghostsRPt)->Fill( RecoPt );
      GetHisto(kghostsLengthAndRMom)->Fill( recoTr.NHits(), RecoMom );
      GetHisto(kghostsChi2)->Fill( recoTr.InnerParam().GetChi2() );
      GetHisto(kghostsProb)->Fill( TMath::Prob(recoTr.InnerParam().GetChi2(),recoTr.InnerParam().GetNDF()));

      ghostTr++;
    }
    else {
      AliHLTTPCCAMCTrack &mcTr = (*fMCTracks)[ recoD.GetMCTrackId() ];
      recoTrC2++;
//std::cout<<"     iRTr: "<<iRTr<<";   recoTr.NHits(): "<<recoTr.NHits()<<"\n";
      GetHisto(krecosLength)->Fill( recoTr.NHits() );
      GetHisto(krecosRMom)->Fill( RecoMom );
      GetHisto(krecosMCMom)->Fill( mcTr.P() );
      GetHisto(krecosRPt)->Fill( RecoPt );
      GetHisto(krecosMCPt)->Fill( mcTr.Pt() );
      GetHisto(krecosLengthAndMCMom)->Fill( recoTr.NHits() , mcTr.P() );
      GetHisto(krecosLengthAndRMom)->Fill( recoTr.NHits() , RecoMom );
      GetHisto(krecosChi2)->Fill( recoTr.InnerParam().GetChi2() );
      GetHisto(krecosProb)->Fill( TMath::Prob(recoTr.InnerParam().GetChi2(),recoTr.InnerParam().GetNDF()));
      /*if( mcTr.P() > 0.5 ) */GetHisto(knHitsRecoTOverNHitsMCT)->Fill( float(recoTr.NHits()) / mcTrackNRecoHits[recoD.GetMCTrackId()] );

//      GetHisto(krecoMCToHits2D)->Fill( recoTr.NHits(), mcTrackNRecoHits[recoD.GetMCTrackId()] );
      GetHisto(krecoMCToHits2D)->Fill( mcTrackNRecoHits[recoD.GetMCTrackId()], recoTr.NHits() );
    }
  }
//  std::cout<<"___ recoTrC1: "<<recoTrC1<<";   recoTrC2: "<<recoTrC2<<";   ghostTr: "<<ghostTr<<"\n";
  
  // global tracker performance
  {
    int nWrittenTracks = 0;
    for ( int itr = 0; itr < nRecoTracks; itr++ ) {
      const int iMC = recoData[itr].GetMCTrackId();
//      if ( recoData[itr].IsGhost(PParameters::MinTrackPurity) ) continue;
      AliHLTTPCCAMCTrack &mc = (*fMCTracks)[iMC];
      // ---
      if( mc.Set30() <= 0 ) continue;
      nWrittenTracks++;
//      std::cout<<" --- itr: "<<itr<<";   iMC: "<<iMC<<";   mc.P: "<<mc.P()<<";   motherId: "<<mc.MotherId()<<";   nWrittenTracks: "<<nWrittenTracks<<"\n";
      // ---
      const AliHLTTPCCAGBTrack &tCA = fTracker->Tracks()[itr];

      AliHLTTPCCAPerformanceRecoTrackData &recoD = recoData[itr];
      if( recoD.IsClone() ) continue;
      if( tCA.NHits() < 15 ) continue;

      //      const int nhits = tCA.NHits();
//       for ( int ihit = 0; ihit < nhits; ihit++ ) {
//         const int index = fTracker->TrackHit( tCA.FirstHitRef() + ihit );
	//        const AliHLTTPCCAGBHit &hit = fTracker->Hit( index );
//        std::cout<<" ------->>> hit: "<<ihit<<";   x: "<<hit.X()<<";   y: "<<hit.Y()<<";   z: "<<hit.Z()<<"\n";
//        int iSlice = hit.ISlice();
//	  float alpha = fTracker->Slice( iSlice ).Param().Alpha();
//	  float x = hit.X();
//	  float y = hit.Y();
//	  float z = hit.Z();
//	  float gx = x * cos(alpha) - y * sin(alpha);
//	  float gy = y * cos(alpha) + x * sin(alpha);
////	  float gx = x * cos(alpha) + y * sin(alpha);
////	  float gy = y * cos(alpha) - x * sin(alpha);
//	  float gx1 = gy;
//	  float gy1 = -gx;
//	  float gz = z;
//	  std::cout<<" -----. gx: "<<gx<<";   gy: "<<gy<<";   gx1: "<<gx1<<";   gy1: "<<gy1<<"\n";
//      }
      // ---
//      if ( mc.P() < 1 ) continue;
//      if ( mc.MotherId() != -1 ) continue;
      
      int nFirstMC = mc.FirstMCPointID();
      int nMCPoints = mc.NMCPoints();

      AliHLTTPCCALocalMCPoint *points = &((*fLocalMCPoints).Data()[nFirstMC]);

      const AliHLTTPCCAGBTrack &t = fTracker->Tracks()[itr];
      AliHLTTPCCATrackParam p = t.Param();
      // ---
      int iSlice = fTracker->Hit( fTracker->TrackHit( tCA.FirstHitRef() ) ).ISlice();
      float alpha = fTracker->Slice( iSlice ).Param().Alpha();
      float x = p.X();
      float y = p.Y();
      //      float z = p.Z();
      float gx = x * cos(alpha) - y * sin(alpha);
      float gy = y * cos(alpha) + x * sin(alpha);
      float gx1 = gy;
      float gy1 = -gx;
      //      float gz = z;
//      std::cout<<" -----. gx: "<<gx<<";   gy: "<<gy<<";   gx1: "<<gx1<<";   gy1: "<<gy1<<"\n";
      // ---
      int MCindex=-1;
//      std::cout<<" ---> nMCPoints: "<<nMCPoints<<"\n";
      for(int iMCPoint=0; iMCPoint<nMCPoints; iMCPoint++)
      {
//	  std::cout<<" ----- iMCPoint: "<<iMCPoint<<";   point.x: "<<points[iMCPoint].X()<<";   param.x: "<<p.X()<<";   pouint.z: "<<points[iMCPoint].Z()<<";   param.z: "<<p.Z()<<"\n";
//	  std::cout<<"        point.y: "<<points[iMCPoint].Y()<<";   param.y: "<<p.Y()<<"\n";
	  // ---
//	  int iSlice = points[iMCPoint].ISlice();
//	  float alpha = fTracker->Slice( iSlice ).Param().Alpha();
//	  float x = points[iMCPoint].X();
//	  float y = points[iMCPoint].Y();
//	  float z = points[iMCPoint].Z();
//	  float gx = x * cos(alpha) - y * sin(alpha);
//	  float gy = y * cos(alpha) + x * sin(alpha);
//	  float gz = z;
//	  std::cout<<" -----. gx: "<<gx<<";   gy: "<<gy<<"\n";
	  // ---
//std::cout<<"     mcX: "<<points[iMCPoint].X()<<";   hX: "<<gx1<<";   dX: "<<fabs(points[iMCPoint].X() - gx1)<<"\n";
//        if(fabs(points[iMCPoint].X() - p.X())< 2.f)
	if(fabs(points[iMCPoint].X() - gx1)< 2.f)
        {
//            std::cout<<" -----> point.y: "<<points[iMCPoint].Y()<<";   param.y: "<<p.Y()<<"\n";
//            std::cout<<" -----> point.z: "<<points[iMCPoint].Z()<<";   param.z: "<<p.Z()<<"\n";
//          if(fabs(p.Y() - points[iMCPoint].Y())<2 && fabs(p.Z() - points[iMCPoint].Z())<2)
//std::cout<<"     mcY: "<<points[iMCPoint].Y()<<";   hY: "<<gy1<<";   dY: "<<fabs(points[iMCPoint].Y() - gy1)<<"\n";
//std::cout<<"     mcZ: "<<points[iMCPoint].Z()<<";   hZ: "<<gz<<";   dZ: "<<fabs(points[iMCPoint].Z() - gz)<<"\n";
          if(fabs(gy1 - points[iMCPoint].Y())<2 && fabs(p.Z() - points[iMCPoint].Z())<2) {
            MCindex = iMCPoint;
//std::cout<<"          ACCEPTED\n";
//            std::cout<<" <<< set mc. point.z: "<<points[iMCPoint].Z()<<";   param.z: "<<p.Z()<<"\n";
//            std::cout<<" <<< p.X(): "<<p.X()<<";   p.Y(): "<<p.Y()<<"\n";
          }
        }
      }
//      std::cout<<" ---> MCindex: "<<MCindex<<"\n";
      if(MCindex == -1)
      {
        continue;
      }
      // track resolutions
      while ( 1/*mc.Set() == 2 && TMath::Abs( mc.TPCPar()[0] ) + TMath::Abs( mc.TPCPar()[1] ) > 1*/ ) {
//        double cosA = TMath::Cos( t.Alpha() );
//        double sinA = TMath::Sin( t.Alpha() );
	  double cosA = TMath::Cos( alpha );
	  double sinA = TMath::Sin( alpha );
//        double mcX0 =  mc.TPCPar()[0] * cosA + mc.TPCPar()[1] * sinA;
//        double mcY0 = -mc.TPCPar()[0] * sinA + mc.TPCPar()[1] * cosA;
//        double mcZ0 =  mc.TPCPar()[2];

        double mcY0 =  points[MCindex].X() * cosA + points[MCindex].Y() * sinA;
        double mcX0 = -(-points[MCindex].X() * sinA + points[MCindex].Y() * cosA);
	//        double mcZ0 =  points[MCindex].Z();
//	  double mcY0 =  -points[MCindex].X() * cosA + points[MCindex].Y() * sinA;
//	  double mcX0 = points[MCindex].X() * sinA + points[MCindex].Y() * cosA;
//	  double mcZ0 =  points[MCindex].Z();
        double mcEy0 =  mc.Par(3) * cosA + mc.Par(4) * sinA;
        double mcEx0 = -(-mc.Par(3) * sinA + mc.Par(4) * cosA);
	//        double mcEz0 =  mc.Par(5);
	//        double mcEt0 = TMath::Sqrt( mcEx0 * mcEx0 + mcEy0 * mcEy0 );
//        if ( TMath::Abs( mcEt ) < 1.e-4 ) break;
//        double mcSinPhi0 = mcEy0 / mcEt0;
//        double mcDzDs0   = mcEz0 / mcEt0;
//        double mcQPt0 = mc.Par(6) / mcEt0;
//        if ( TMath::Abs( mcQPt ) < 1.e-4 ) break;
//        double mcPt0 = 1. / TMath::Abs( mcQPt0 );
//        if ( mcPt < Parameters::RefThreshold ) break;
//        if ( t.NHits() <  PParameters::MinimumHitsForMCTrack ) break;
//        double bz = fTracker->Slice( 0 ).Param().Bz();
        // ---
        double mcQP_ = points[MCindex].QP();
        double px0_ = -(-points[MCindex].Px() * sinA + points[MCindex].Py() * cosA);
        double py0_ = points[MCindex].Px() * cosA + points[MCindex].Py() * sinA;
        double mcEx_ = px0_*mcQP_;
        double mcEy_ = py0_*mcQP_;
	//        double mcEz_ = points[MCindex].Pz()*mcQP_;
        double mcEt_ = TMath::Sqrt( mcEx_ * mcEx_ + mcEy_ * mcEy_ );
        double mcSinPhi_ = mcEy_ / mcEt_;
        // ---
	
	
//         double mcX =  points[MCindex].X();
//         double mcY =  points[MCindex].Y();
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
	
        if ( !p.TransportToXWithMaterial( mcX0, bz ) ) break;
        // ---
//              int iSlice = fTracker->Hit( fTracker->TrackHit( tCA.FirstHitRef() ) ).ISlice();
//              float alpha = fTracker->Slice( iSlice ).Param().Alpha();
              float xc = p.X();
              float yc = p.Y();
	      //              float zc = p.Z();
	      //              float gxc = xc * cos(alpha) - yc * sin(alpha);
	      //              float gyc = yc * cos(alpha) + xc * sin(alpha);
	      //              float gx1c = gyc;
	      //              float gy1c = -gx;
	      //              float gzc = zc;
//              std::cout<<" -----. gxc: "<<gxc<<";   gy: "<<gyc<<";   gx1c: "<<gx1c<<";   gy1c: "<<gy1c<<"\n";
              // ---
        if ( p.GetCosPhi()*mcEx < 0 ) { // change direction
          mcSinPhi = -mcSinPhi;
          mcDzDs = -mcDzDs;
          mcQPt = -mcQPt;
        }

        if ( p.GetCosPhi()*mcEx_ < 0 ) {
          mcSinPhi_ = -mcSinPhi_;
        }
        // ---
//        if ( p.GetCosPhi()*mcEx0 < 0 ) { // change direction
//          mcSinPhi0 = -mcSinPhi0;
//          mcDzDs0 = -mcDzDs0;
//          mcQPt0 = -mcQPt0;
//        }

        // ---
//         const double kCLight = 0.000299792458;
//         double k2QPt = 100;
//         if ( TMath::Abs( bz ) > 1.e-4 ) k2QPt = 1. / ( bz * kCLight );
/*        double qPt = p.GetKappa( bz ) * k2QPt;
        double pt = 100;*/
	double qPt = p.GetQPt();
        double pt = 1. / TMath::Abs( qPt );
	
        if ( mcPt < 0.010 ) break;
//        if ( TMath::Abs( qPt ) > 1.e-4 ) pt = 1. / TMath::Abs( qPt );
        // ---
        if( (mcDzDs>0 && p.GetDzDs()<0) || (mcDzDs<0 && p.GetDzDs()>0) ) {
          mcDzDs = -mcDzDs;

//          mcSinPhi = -mcSinPhi;
//          mcQPt = -mcQPt;
        }

        if ( mcQPt * p.GetQPt() < 0) {
            mcQPt = -mcQPt;
        }
        // ---
//std::cout<<"> Fill histos\n";
//std::cout<<"___ sinPhi: "<<p.GetSinPhi()<<";   mcSinPhi: "<<mcSinPhi<<";   mcSinPhi0: "<<mcSinPhi0<<";   mcSinPhi_: "<<mcSinPhi_<<";   dSinPhi: "<<( p.GetSinPhi() - mcSinPhi )<<"\n";
//std::cout<<" --- \n";
//std::cout<<" >>> mcX: "<<mcX<<";   mcY: "<<mcY<<" |||   mcX0: "<<mcX0<<";   mcY0: "<<mcY0<<"\n";
//std::cout<<" >>> p.GetSinPhi(): "<<p.GetSinPhi()<<";   mcSinPhi: "<<mcSinPhi<<";   mcSinPhi0: "<<mcSinPhi0<<"\n";
//std::cout<<" >>>>>>> fill dy: "<<gy1c - mcY<<";   mc.y: "<<mcY<<";   param.y: "<<gy1c<<"\n";
//std::cout<<" >>>>>>> fill dz: "<<p.GetZ() - mcZ<<";   mc.z: "<<mcZ<<";   param.z: "<<p.GetZ()<<"\n";
//std::cout<<" >>>>>>> fill sinPhi: "<<p.GetSinPhi() - mcSinPhi<<";   mc.sinPhi: "<<mcSinPhi<<";   param.sinPhi: "<<p.GetSinPhi()<<"\n";
//std::cout<<" >>>>>>> fill DzDs: "<<p.GetDzDs() - mcDzDs<<";   mc.DzDs: "<<mcDzDs<<";   param.DzDs: "<<p.GetDzDs()<<";   mcDzDs0: "<<mcDzDs0<<"\n";
        GetHisto(kresY)->Fill( p.GetY() - mcY0 );
//        GetHisto(kresY)->Fill( gy1c - mcY );
        GetHisto(kresZ)->Fill( p.GetZ() - mcZ );
        GetHisto(kresSinPhi)->Fill( p.GetSinPhi() - mcSinPhi_ );
        GetHisto(kresDzDs)->Fill( p.GetDzDs() - mcDzDs );
        if(CAMath::Abs(qPt) > 1.e-8){
          GetHisto(kresPt)->Fill( (pt - mcPt)/mcPt );
        }
        if ( p.GetErr2Y() > 0 ) GetHisto(kpullY)->Fill( ( p.GetY() - mcY0 ) / TMath::Sqrt( p.GetErr2Y() ) );
//        if ( p.GetErr2Y() > 0 ) GetHisto(kpullY)->Fill( ( gy1c - mcY ) / TMath::Sqrt( p.GetErr2Y() ) );
        if ( p.GetErr2Z() > 0 ) GetHisto(kpullZ)->Fill( ( p.GetZ() - mcZ ) / TMath::Sqrt( p.GetErr2Z() ) );

        if ( p.GetErr2SinPhi() > 0 ) GetHisto(kpullSinPhi)->Fill( ( p.GetSinPhi() - mcSinPhi_ ) / TMath::Sqrt( p.GetErr2SinPhi() ) );
        if ( p.GetErr2DzDs() > 0 ) GetHisto(kpullDzDs)->Fill( ( p.DzDs() - mcDzDs ) / TMath::Sqrt( p.GetErr2DzDs() ) );
        if(CAMath::Abs(qPt) > 1.e-7 && p.GetErr2QPt()>0 ) GetHisto(kpullQPt)->Fill( (qPt - mcQPt)/TMath::Sqrt(p.GetErr2QPt()) );

        break;
      }
    }

  }

  // distribution of cluster errors

  {
    int nHits = fTracker->NHits();

    if ( fHitLabels->Size() > 0 )
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

void AliHLTTPCCAGlobalPerformance::Draw()
{
#ifndef KFPARTICLE
#ifdef DRAW_GLOBALPERF
  if ( AliHLTTPCCADisplay::Instance().DrawType() != 3 ) return;
  
  // AliHLTTPCCAPerformance::Instance().Init();
  AliHLTTPCCAPerformance& gbPerfo = AliHLTTPCCAPerformance::Instance();
  AliHLTTPCCADisplay &disp = AliHLTTPCCADisplay::Instance();
  disp.SetGB( gbPerfo.GetTracker() );
  disp.SetTPC( fTracker->Slices()[0].Param() );
  disp.SetTPCView();
  disp.DrawTPC();
//  disp.DrawGBHits( *gbPerfo.GetTracker(), kGreen, 0.03, 1  );

  // ---
//  disp.ClearViewPT();
//  disp.DrawSliceHitsPT(1, 0.5);
//  disp.DrawSliceLinksPT(-1,-1,1);
  // ---

#if 0 // MC info
  /*for ( int imc = 0; imc < nMCTracks; imc++ ) {
    AliHLTTPCCAPerformanceMCTrackData &mc = mcData[imc];
    bool doDraw = true;
    // doDraw &= (mc.GetSet() >= 2);
    doDraw &= (*fMCTracks)[imc].P() > 2;
    doDraw &= mc.IsReconstructable();
    // doDraw &= mc.IsReconstructed();
    if ( doDraw ) {
      disp.SpecDrawMCTrackPointsGlobal( (*fMCTracks)[imc], fLocalMCPoints, kRed, 0.3 );
      disp.SpecDrawMCTrackHitsGlobal( (*fMCTracks)[imc], fLocalMCPoints, fHitLabels, kGreen, 0.2 );


      for ( int irt = 0; irt < nRecoTracks; irt++ ) {
        AliHLTTPCCAPerformanceRecoTrackData &rt = recoData[irt];
        if ( rt.GetMCTrackId() != imc ) continue;

        disp.SpecDrawRecoTrackGlobal( irt, kBlue, 0.1 );
      }
    }
  }*/
  for ( int imc = 0; imc < nMCTracks; imc++ ) {
      bool doDraw = true;
      doDraw &= (*fMCTracks)[imc].NMCPoints() < 47;
      doDraw &= mcData[imc].IsReconstructable();
      if(doDraw&&(*fMCTracks)[imc].P()<0.5) disp.SpecDrawMCTrackGlobalPT( (*fMCTracks)[imc], fLocalMCPoints, kBlue, 0.3 );
      doDraw &= (*fMCTracks)[imc].P() > 0.5;
      if ( !doDraw ) continue;
      disp.SpecDrawMCTrackGlobalPT( (*fMCTracks)[imc], fLocalMCPoints, kRed, 0.3 );
  }
#else // reco info
//  for ( int irt = 0; irt < nRecoTracks; irt++ ) {
//      disp.SpecDrawRecoTrackGlobalPT( irt, kBlue, 0.03 );
//  }
//  for(int i = 0; i < nMCTracks; i++) {
//      if(mcData[i].IsReconstructable())
////      disp.SpecDrawMCTrackGlobal( (*fMCTracks)[i], fLocalMCPoints, kRed, 0.3 );
//	disp.SpecDrawMCTrackPointsGlobal( (*fMCTracks)[i], fLocalMCPoints, kRed, 0.3 );
//	      disp.SpecDrawMCTrackHitsGlobal( (*fMCTracks)[i], fLocalMCPoints, fHitLabels, kGreen, 0.2 );
//  }

  for ( int imc = 0; imc < nMCTracks; imc++ ) {
//      std::cout<<"       imc: "<<imc<<";   nreconstructed: "<<(*fMCTracks)[imc].NReconstructed()<<";   "<<mcData[imc].NReconstructed()<<";   nRows: "<<(*fMCTracks)[imc].NMCRows()<<";   nPoints: "<<(*fMCTracks)[imc].NMCPoints()<<";   nHits: "<<(*fMCTracks)[imc].NHits()<<"\n";
//      if((*fMCTracks)[imc].NReconstructed()) continue;
//      if(mcData[imc].NReconstructed()) continue;
//      if( imc != 39 ) continue;

//      if( mcData[imc].GetNClones() != 1 ) continue;
      if( !mcData[imc].GetNClones() ) continue;

//      if(mcData[imc].NReconstructed() || !mcData[imc].IsReconstructable()) continue;
//      if( mcData[imc].GetSet() == 2 || mcData[imc].GetSet() == 3 )
//      if( mcData[imc].GetSet30() == 2 || mcData[imc].GetSet30() == 3 )
//      if( mcData[imc].GetSet30() <=0 ) continue;
      if( !mcData[imc].IsReconstructable() ) continue;

//      if( mcData[imc].GetSet_l30() != 2 ) continue;

//      if( mcData[imc].GetSet30() != 3 ) continue;
//      if( mcData[imc].GetSet() != 3 ) continue;
      // ---
//      int iSlice = fLocalMCPoints[(*fMCTracks)[imc].FirstMCPointID()];
//      std::cout<<"_(*fMCTracks)[imc].FirstMCPointID(): "<<(*fMCTracks)[imc].FirstMCPointID()<<"\n";
//      std::cout<<"___iSlice: "<<iSlice<<"\n";
//      if( iSlice != 10 && iSlice != 12 ) continue;
      // ---
//	  std::cout<<"___ imc: "<<imc<<"\n";
//#if 0
//      disp.SpecDrawMCTrackHitsGlobal( (*fMCTracks)[imc], fLocalMCPoints, fHitLabels, kRed, 1. );
//#endif

//      disp.SaveCanvasToFile( "DrawGlobalPerformance_tmp.pdf" );
//      disp.Ask();
//      }
  }
//  disp.DrawBadMCHits( fHitLabels );

  int badTrack = 0;
  for ( int irt = 0; irt < nRecoTracks; irt++ ) {
      const AliHLTTPCCAGBTrack &tCA = fTracker->Tracks()[irt];
      int mct = tCA.GetFirstMC();
      if( mct < 0 ) {
	  badTrack++;
	  continue;
      }
      if( mcData[mct].IsReconstructed() ) continue;
      if( !mcData[mct].IsReconstructable() ) continue;
//      if( irt != 95 ) continue;
//      std::cout<<" . Draw mct: "<<mct<<"\n";
//      std::cout<<"0 0\n"<<"0 0 0 0 0 0 0\n"<<"0 0 0 0 0 0 0\n"<<"0 0\n"<<tCA.NHits()<<" "<<tCA.NHits()<<" 0\n"<<"0 0 1\n";
//      if( !tCA.IsMerged() ) continue;
//      if( irt != 98 ) continue;
//      if( mct != 324 ) continue;
//      if( !mcData[mct].IsReconstructable() ) continue;
//      if ( recoData[irt].IsReco(0,15) ) continue;
      disp.SpecDrawMCTrackHitsGlobal( (*fMCTracks)[mct], fLocalMCPoints, fHitLabels, kRed, 1. );
  }

  for ( int irt = 0; irt < nRecoTracks; irt++ ) {
//    AliHLTTPCCAPerformanceRecoTrackData &rt = recoData[irt];
      const AliHLTTPCCAGBTrack &track = fTracker->Track( irt );
//      if( !track.IsMerged() ) continue;
//      if( irt != 17 ) continue;

      const AliHLTTPCCAGBTrack &tCA = fTracker->Tracks()[irt];
//    disp.SpecDrawRecoTrackGlobal( irt, kBlue, 0.01 );
//      std::cout<<" . itr: "<<irt<<";   isReco: "<<track.IsReco()<<";   nHits: "<<track.NHits()<<"\n";
//      if( track.NHits() < 15 ) continue;
      int mct = tCA.GetFirstMC();
//      std::cout<<"___mct: "<<mct<<";   GetSet: "<<mcData[mct].GetSet()<<";   isClone: "<<int(tCA.IsClone())<<"\n";
//      if( mct < 0 ) continue;
//      if( mcData[mct].GetSet() != 2 && mcData[mct].GetSet() != 3 ) continue;
//      if( mcData[mct].GetSet30() != 2 && mcData[mct].GetSet30() != 3 ) continue;
//      if( mcData[mct].GetSet30() != 3 ) continue;
//      if( mcData[mct].GetSet30() <=0 ) continue;

//      if( mcData[mct].GetSet_l30() != 2 ) continue;
//      if( !mcData[mct].IsReconstructed_l30() ) {
//	disp.SpecDrawRecoTrackGlobal( irt, kOrange, 0.3 );
//	continue;
//      }
//      if( irt != 146 ) continue;
//      if( irt != 98 ) continue;
//      if( mct != 324 ) continue;
//      if( mcData[mct].GetSet() != 3 ) continue;
      if( mcData[mct].IsReconstructed() ) continue;
      if( !mcData[mct].IsReconstructable() ) continue;
//      if( mcData[mct].GetNClones() != 1 ) continue;
//      if( !mcData[mct].GetNClones() ) continue;
//      if ( recoData[irt].IsReco(0,15) ) continue;
//      if ( track.NHits() >= 15 ) continue;
      std::cout<<" . itr: "<<irt<<";   mct: "<<mct<<";   isReco: "<<track.IsReco()<<";   mcReco(0,15): "<<recoData[irt].IsReco(0,15)<<";   nMCHits: "<<(*fMCTracks)[mct].NHits()<<";   nMCPoints: "<<(*fMCTracks)[mct].NMCPoints()<<"\n";

//      if( track.IsReco() ) continue;

//      disp.SpecDrawMCTrackHitsGlobal( (*fMCTracks)[mct], fLocalMCPoints, fHitLabels, kRed, 1. );

//      if( track.NHits() < 15 ) disp.Ask();
//      std::cout<<" - irt: "<<irt<<";   nHits: "<<track.NHits()<<";   mct: "<<mct<<";   nPoints: "<<(*fMCTracks)[mct].NMCPoints()<<";   nMCHits: "<<(*fMCTracks)[mct].NHits()<<";   nMCRows: "<<(*fMCTracks)[mct].NMCRows()<<";   purity: "<<recoData[irt].GetPurity()<<"\n";
//      for ( int ih = 0; ih < track.NHits(); ih++ ) {
//        const AliHLTTPCCAGBHit &hit = fTracker->Hit( track.FirstHitRef() + ih );
//        const AliHLTTPCCAHitLabel &l = (*fHitLabels)[hit.ID()];
//        std::cout<<" --- ih: "<<ih<<";   l0: "<<l.fLab[0]<<";   l1: "<<l.fLab[1]<<";   l2: "<<l.fLab[2]<<"\n";
//        if( l.fLab[0] >= 0 ) disp.SpecDrawMCTrackHitsGlobal( (*fMCTracks)[l.fLab[0]], fLocalMCPoints, fHitLabels, ih, 1. );
//      }

//      const int nhits = tCA.NHits();
//      std::cout<<" _ nhits: "<<nhits<<"\n";
//      for ( int ihit = 0; ihit < nhits; ihit++ ) {
////        std::cout<<"___ihit: "<<ihit;
//        const int index = fTracker->TrackHit( tCA.FirstHitRef() + ihit );
////        std::cout<<";   index: "<<index;
//        const AliHLTTPCCAHitLabel &l = (*fHitLabels)[fTracker->Hit( index ).ID()];
////        std::cout<<";   label: "<<l.fLab[0]<<"\n";
//        const AliHLTTPCCAGBHit &hit = fTracker->Hit( index );
//        std::cout<<hit.X()<<" "<<hit.Y()<<" "<<hit.Z()<<"\n";
//        std::cout<<"0 0.12 0.16\n";
//        std::cout<<"0 "<<hit.ISlice()<<" "<<hit.IRow()<<" "<<ihit<<" 0\n";
//      }
//      if( !tCA.IsMerged() ) continue;

    disp.SpecDrawRecoTrackGlobal( irt, kBlue, 0.5 );

    if( tCA.IsMerged() ) disp.SpecDrawRecoTrackGlobal( irt, kGreen, 0.5 );
//    disp.SaveCanvasToFile( "DrawGlobalPerformance_tmp.pdf" );
//    disp.Ask();

//    if( tCA.IsClone() ) disp.SpecDrawRecoTrackGlobal( irt, kRed, 0.3 );
//    if(track.OuterParam().QPt()<0.5)disp.SpecDrawRecoTrackGlobalPT( irt, kBlue, 0.03 );
//    if(track.OuterParam().QPt()>0.5)disp.SpecDrawRecoTrackGlobalPT( irt, kRed, 0.03 );
    // ---
//    for ( int imc = 0; imc < nMCTracks; imc++ ) {
//	if(!mcData[imc].IsReconstructable()) continue;
//	std::cout<<"<>imc: "<<imc<<"\n";
//	disp.SpecDrawMCTrackGlobalPT1( (*fMCTracks)[imc], fLocalMCPoints, kRed, 0.3 );
//	disp.SpecDrawMCTrackGlobalDzDsQPt( (*fMCTracks)[imc], fLocalMCPoints, kRed, 0.3 );
//    }
  }
  std::cout<<" --- ghost is not labeld: "<<badTrack<<"\n";
#endif
  
  disp.SaveCanvasToFile( "DrawGlobalPerformance.pdf" );
//  disp.SaveCanvasToFilePT( "DrawGlobalPerformancePT.pdf" );
  disp.Ask();
  
#endif // DRAW_GLOBALPERF
#endif //KFPARTICLE
} // void AliHLTTPCCAGlobalPerformance::Draw()
#endif // HLTCA_STANDALONE
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE

