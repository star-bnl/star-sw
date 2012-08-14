// $Id: AliHLTTPCCASlicePerformance.cxx,v 1.14 2012/08/14 16:30:42 fisyak Exp $
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

#include "AliHLTTPCCounters.h"

#include "AliHLTTPCCAPerformanceBase.h"
#include "AliHLTTPCCASlicePerformance.h"


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
#define IsOutTrack1 // define to use reffited with materials track parameters
using namespace std;
void AliHLTTPCCASlicePerformance::SetNewEvent(const AliHLTTPCCAGBTracker * const Tracker,
                            AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
                            AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
                            AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints)
{
  AliHLTTPCCAPerformanceBase::SetNewEvent(Tracker, hitLabels, mcTracks, localMCPoints);
    
  sliceTracker = &fTracker->Slice( fISlice );
  
  firstSliceHit = 0; endSliceHit = 0;
  for ( ; firstSliceHit < fTracker->NHits(); firstSliceHit++ ) {
    if ( fTracker->Hit( firstSliceHit ).ISlice() == fISlice ) break;
  }
  endSliceHit = firstSliceHit;
  for ( ; endSliceHit < fTracker->NHits(); endSliceHit++ ) {
    if ( fTracker->Hit( endSliceHit ).ISlice() != fISlice ) break;
  };

#ifdef IsOutTrack1
  nRecoTracks = sliceTracker->NOutTracks1();
#else
  nRecoTracks = sliceTracker->NOutTracks();
#endif

} // void AliHLTTPCCASlicePerformance::SetNewEvent

void AliHLTTPCCASlicePerformance::CheckMCTracks()
{

  for ( int imc = 0; imc < nMCTracks; imc++ )
    (*fMCTracks)[imc].SetNHits( 0 );

  std::vector<int> iLastRow; // last row with hit for iMCTrack
  std::vector<int> nLastRows; // last row with hit for iMCTrack
  for ( int imc = 0; imc < nMCTracks; imc++ ){
    iLastRow.push_back(-1);
    nLastRows.push_back(0);
  };

  for ( int ih = firstSliceHit; ih < endSliceHit; ih++ ) {
    int id = fTracker->Hit( ih ).ID();
    int iRow = fTracker->Hit( ih ).IRow();
    if ( (id < 0) || (id >= (*fHitLabels).Size()) ) break;
    const AliHLTTPCCAHitLabel &l = (*fHitLabels)[id];
    for (int j = 0; j < 3; j++){
      int iMCTrack = l.fLab[j];
      if ( iMCTrack >= nMCTracks ){
        cout << "ERROR: Memory corruption or Incorrect MC Data! " << endl;
        cout << "HitLabels[" << id << "].fLab[" << j << "] = " << iMCTrack << endl;
        exit (1);
      }
      if ( iMCTrack >= 0 ){
        if ( iLastRow[iMCTrack] != iRow ){
          if ((iLastRow[iMCTrack] == iRow - 1) && (nLastRows[iMCTrack] != -10)){
            nLastRows[iMCTrack] += 1;
            if (nLastRows[iMCTrack] >= SPParameters::MinimumConsHitsForMCTrack) nLastRows[iMCTrack] = -10;
          }
          (*fMCTracks)[iMCTrack].SetNHits( (*fMCTracks)[iMCTrack].NHits() + 1 );
          iLastRow[iMCTrack] = iRow;
        }
      }
    } // for j
  } // for ih

  mcData.resize(nMCTracks);
  for ( int imc = 0; imc < nMCTracks; imc++ ) {
    AliHLTTPCCAMCTrack &mc = (*fMCTracks)[imc];
    AliHLTTPCCAPerformanceMCTrackData &mcTrackData = mcData[imc];

    mc.SetSet( 0 );
    mc.SetNReconstructed( 0 );
    mc.SetNTurns( 1 );
    if ((nLastRows[imc] == -10) && (mc.NHits() >=  SPParameters::MinimumHitsForMCTrack)){
      mcTrackData.SetAsReconstructable();
    } // recoable
    
    if ( mc.P() >= AliHLTTPCCAParameters::ExtraThreshold ) {
      if ( mc.P() >= AliHLTTPCCAParameters::RefThreshold ) {
        mc.SetSet( 2 );
        mcTrackData.SetSet( 2 );
      } // ref
      else{
        mc.SetSet( 1 );
        mcTrackData.SetSet( 1 );
      } // extra
    } // recoable

  } // for iMC
} // void AliHLTTPCCASlicePerformance::CheckMCTracks()


void AliHLTTPCCASlicePerformance::MatchTracks()
{
  for(int i=0; i<sliceTracker->NOutTracks1(); i++)
  {
    int sliceTracker_tr_id = sliceTracker->OutTrack1(i).OrigTrackID();
    sliceTracker->fOutTracks1[i].SetFirstHitRef(-1);
    sliceTracker->fOutTracks1[i].SetNHits(-1);
    for(int j=0; j<sliceTracker->NOutTracks(); j++)
    {
      if(sliceTracker_tr_id == sliceTracker->OutTrack(j).OrigTrackID())
      {
        sliceTracker->fOutTracks1[i].SetFirstHitRef(sliceTracker->OutTrack(j).FirstHitRef());
        sliceTracker->fOutTracks1[i].SetNHits(sliceTracker->OutTrack(j).NHits());
      }
    }
  }

  recoData.resize(nRecoTracks);
  for ( int itr = 0; itr < nRecoTracks; itr++ ) {

#ifdef IsOutTrack1
    if(    sliceTracker->fOutTracks1[itr].NHits() == -1) continue;
#endif
    int traLabels = -1;
    double traPurity = 0;
#ifdef IsOutTrack1
    const AliHLTTPCCAOutTrack &tCA = sliceTracker->OutTrack1( itr );
#else
    const AliHLTTPCCAOutTrack &tCA = sliceTracker->OutTrack( itr );
#endif
    int nhits = tCA.NHits();
    int *lb = new int[nhits*3];
    int nla = 0;
      //cout<<"\nHit labels:"<<endl;
        // collect all labels

    for ( int ihit = 0; ihit < nhits; ihit++ ) {
      const int outTrackHitIndex = tCA.FirstHitRef() + ihit;
      assert( outTrackHitIndex < sliceTracker->NOutTrackHits() );
      const int index = firstSliceHit + sliceTracker->OutTrackHit( outTrackHitIndex );
//if(fISlice == 2 && ihit==0)cout  <<"G4.1.3  "<<index<< fTracker->NHits( ) <<endl;
      const AliHLTTPCCAHitLabel &l = (*fHitLabels)[fTracker->Hit( index ).ID()];
        //cout<<l.fLab[0]<<" "<<l.fLab[1]<<" "<<l.fLab[2]<<endl;
      if ( l.fLab[0] >= 0 ) lb[nla++] = l.fLab[0];
      if ( l.fLab[1] >= 0 ) lb[nla++] = l.fLab[1];
      if ( l.fLab[2] >= 0 ) lb[nla++] = l.fLab[2];
    }
        // find one with maximum entries.
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
        // count n hits with max label
    lmax = 0;
    for ( int ihit = 0; ihit < nhits; ihit++ ) {
      const int index = firstSliceHit + sliceTracker->OutTrackHit( tCA.FirstHitRef() + ihit );
      const AliHLTTPCCAHitLabel &l = (*fHitLabels)[fTracker->Hit( index ).ID()];
      if ( l.fLab[0] == labmax || l.fLab[1] == labmax || l.fLab[2] == labmax
         ) lmax++;
      // cout << index << " ";
    }
    traLabels = labmax;
    traPurity = ( ( nhits > 0 ) ? double( lmax ) / double( nhits ) : 0 );
      // cout<<"perf track "<<itr<<": "<<nhits<<" "<<labmax<<" "<<traPurity<<endl;
    if ( lb ) delete[] lb;

    recoData[itr].SetMCTrack(traLabels, traPurity, nhits);
    if ( recoData[itr].IsReco(SPParameters::MinTrackPurity, SPParameters::MinimumHitsForRecoTrack) ) mcData[traLabels].AddReconstructed();
  } // for itr

} // void AliHLTTPCCASlicePerformance::MatchTracks()


void AliHLTTPCCASlicePerformance::EfficiencyPerformance( )
{
  for ( int iRTr = 0; iRTr < nRecoTracks; iRTr++ ) {
    if (  recoData[iRTr].IsGhost(SPParameters::MinTrackPurity) )
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

  fEffStat += fEff;
  
//     // ---PDG performance  ---
//   bool collectPDG = 0;
//   static int nElectrons = 0;
//   static int nProtons = 0;
//   static int nPions = 0;
//   static int nMuons = 0;
//   for ( int ipart = 0; ipart < nMCTracks; ipart++ ) {
//     AliHLTTPCCAMCTrack &mc = (*fMCTracks)[ipart];
//     if (collectPDG && mc.NReconstructed() > 0){
// //     std::cout << mc.PDG() << " ";
//       switch (abs(mc.PDG())){
//         case 11:
//             nElectrons++;
//             break;
//         case 13:
//             nMuons++;
//             break;
//         case 211:
//             nPions++;
//             break;
//         case 2212:
//             nProtons++;
//             break;
//       }
//     }
//   }
//   if (collectPDG && fISlice == 23){
//     std::cout << " nElectrons "  << nElectrons << std::endl;
//     std::cout << " nMuons "  << nMuons << std::endl;
//     std::cout << " nPions "  << nPions << std::endl;
//     std::cout << " nProtons "  << nProtons << std::endl;
//   }

} // SlicePerformance

void AliHLTTPCCASlicePerformance::FillHistos()
{
  AliHLTTPCCAPerformanceBase::FillHistos();
    
  for(int iRTr=0; iRTr < nRecoTracks; iRTr++){  // TODO: make common
    AliHLTTPCCAPerformanceRecoTrackData &recoD = recoData[iRTr];
#ifdef IsOutTrack1
    const  AliHLTTPCCAOutTrack &recoTr = sliceTracker->OutTrack1( iRTr );  // TODO: make common
#else
    const AliHLTTPCCAOutTrack &recoTr = sliceTracker->OutTrack( iRTr );
#endif
    AliHLTTPCCAMCTrack &mcTr = (*fMCTracks)[ recoD.GetMCTrackId() ];
    
    AliHLTTPCCATrackParam param = recoTr.EndPoint();

    const int nHits = recoTr.NHits();
    const double p = 1. / param.QPt() * sqrt(1. + param.DzDs()*param.DzDs());
    const double pMC = mcTr.P();    
    const double chi2 = param.Chi2() / nHits;
    if (  recoD.IsGhost(SPParameters::MinTrackPurity) ) {
      GetHisto("ghostsLength")->Fill( nHits );
      GetHisto("ghostsRMom")->Fill( p );
      GetHisto("ghostsMCMom")->Fill( pMC );
      GetHisto("ghostsLengthAndRMom")->Fill( nHits , p ); // TODO add same for other perfs
      GetHisto("ghostsLengthAndMCMom")->Fill( nHits , pMC );
      GetHisto("ghostsLengthAndChi2")->Fill( nHits , chi2 );

      GetHisto("ghostsChi2")->Fill( chi2 );
    }
    else {
      GetHisto("recosLength")->Fill( nHits );
      GetHisto("recosRMom")->Fill( p );
      GetHisto("recosMCMom")->Fill( pMC );
      GetHisto("recosLengthAndRMom")->Fill( nHits , p );
      GetHisto("recosLengthAndMCMom")->Fill( nHits , pMC );
      GetHisto("recosLengthAndChi2")->Fill( nHits , chi2 );
      
      GetHisto("recosChi2")->Fill( chi2 );
    }
  }
  
  ///mvz start 27.01.2010
  const int nMCTr = nMCTracks;
  map<int,int> NHitsRecoTrack;
  for(int iMCTr=0; iMCTr<nMCTr; iMCTr++)
    NHitsRecoTrack[iMCTr]=-1;
  ///mvz end 27.01.2010

  for ( int itr = 0; itr < nRecoTracks; itr++ ) {
#ifdef IsOutTrack1
    if(    sliceTracker->fOutTracks1[itr].NHits() == -1) continue;
#endif
    if ( recoData[itr].IsGhost() ) continue;

    const int iMC = recoData[itr].GetMCTrackId();
    AliHLTTPCCAMCTrack &mc = (*fMCTracks)[iMC];

    ///  mvz begin
//      while ( mc.Set() == 2 && TMath::Abs( mc.TPCPar()[0] ) + TMath::Abs( mc.TPCPar()[1] ) > 1 ) {
    while ( 1 ) {
      if ( recoData[itr].GetPurity() < .90 ) break;
#ifdef IsOutTrack1
      const AliHLTTPCCAOutTrack &t = sliceTracker->OutTrack1( itr );
#else
      const AliHLTTPCCAOutTrack &t = sliceTracker->OutTrack( itr );
#endif
      AliHLTTPCCATrackParam p = t.StartPoint();

///mvz start 27.01.2010
      if( mc.NReconstructed() == 1 ) NHitsRecoTrack[iMC] = t.NHits();
      if( mc.NReconstructed() > 1 && NHitsRecoTrack[iMC] > t.NHits()) NHitsRecoTrack[iMC] = t.NHits();
///mvz end 27.01.2010
//cout <<"Start  "<< p.X() << "  "<<p.Y()<<"  "<<p.Z()<<endl;
//cout <<"Start  "<< p.QPt() <<endl;

      int nFirstMC = mc.FirstMCPointID();
      int nMCPoints = mc.NMCPoints();

      AliHLTTPCCALocalMCPoint *points = &((*fLocalMCPoints).Data()[nFirstMC]);

      int iTrHit = firstSliceHit + sliceTracker->OutTrackHit(t.FirstHitRef());
      AliHLTTPCCAGBHit hit = fTracker->Hits()[iTrHit];

      int MCindex=-1;
      for(int iMCPoint=0; iMCPoint<nMCPoints; iMCPoint++)
      {
        if(points[iMCPoint].ISlice() != fISlice) continue;
/*          if(points[iMCPoint].IRow() <iRowMin) 
        {
        iRowMin = points[iMCPoint].IRow();
        MCindex = iMCPoint;
      }*/
        if(points[iMCPoint].IRow() == hit.IRow())
        {
          if(fabs(hit.Y() - points[iMCPoint].Y())<2 && fabs(hit.Z() - points[iMCPoint].Z())<2)
            MCindex = iMCPoint;
        }
      }
      if(MCindex == -1)
      {
        break;
      }
//        if(iRowMin == 10000) continue;

      double mcX =  points[MCindex].X();
      double mcY =  points[MCindex].Y();
      double mcZ =  points[MCindex].Z();
      double mcQP = points[MCindex].QP();
      double mcEx = points[MCindex].Px()*mcQP;
      double mcEy = points[MCindex].Py()*mcQP;
      double mcEz = points[MCindex].Pz()*mcQP;
      double mcEt = TMath::Sqrt( mcEx * mcEx + mcEy * mcEy );
//cout << fISlice <<" "<<mc.FirstMCPointID() << endl;

      if ( TMath::Abs( mcEt ) < 1.e-4 ) break;
      double mcSinPhi = mcEy / mcEt;
      double mcDzDs   = mcEz / mcEt;
      double mcQPt =  mcQP / mcEt;
//cout << mcX <<"  "<<mcY<<"  "<<mcZ<<"  "<<mcQPt<< endl;
      if ( TMath::Abs( mcQPt ) < 1.e-6 ) break;
      double mcPt = 1. / TMath::Abs( mcQPt );
//        if ( t.NHits() <  50 ) break;
      double bz = fTracker->Slice( 0 ).Param().cBz();
#ifdef DRAW2
      AliHLTTPCCATracker &sliceM = const_cast<AliHLTTPCCATracker&>(*sliceTracker);
      AliHLTTPCCADisplay::Instance().ClearView();
      AliHLTTPCCADisplay::Instance().SetSliceView();
      AliHLTTPCCADisplay::Instance().SetCurrentSlice( &sliceM);
      AliHLTTPCCADisplay::Instance().DrawSlice( &sliceM, 1 );
      AliHLTTPCCADisplay::Instance().DrawTrackParam( t.StartPoint() );
      for(int i=0; i<nMCPoints; i++)
        if(points[i].ISlice() == fISlice)
      {
        AliHLTTPCCADisplay::Instance().DrawPoint(points[i].X(), points[i].Y(), points[i].Z(), 13 );
            //cout << points[i].TrackID() << endl;
      }
//        AliHLTTPCCADisplay::Instance().DrawPoint(fISlice, &sliceTracker, hit.X(), hit.Y(), hit.Z(), -1 );
      AliHLTTPCCADisplay::Instance().DrawSliceHits(-1, 0.5);
      AliHLTTPCCADisplay::Instance().DrawPoint(mcX, mcY, mcZ, -1 );
      AliHLTTPCCADisplay::Instance().DrawPoint(p.X(), p.Y(), p.Z(), 0 );
#endif
      if ( !p.TransportToXWithMaterial( mcX, bz ) ) break;
#ifdef DRAW2
      AliHLTTPCCADisplay::Instance().DrawPoint(p.X(), p.Y(), p.Z(), 1 );
      AliHLTTPCCADisplay::Instance().Ask();
#endif


      if ( p.GetCosPhi()*mcEx < 0 ) { // change direction
        mcSinPhi = -mcSinPhi;
        mcDzDs = -mcDzDs;
        mcQPt = -mcQPt;
      }
      double qPt = p.GetQPt();
      double pt = 1. / TMath::Abs( qPt );
//        std::cout << "op!"<<std::endl;
      if ( mcPt < 0.010 ) break;

      AliHLTTPCCATrackParam p1 = t.EndPoint();

        
      GetHisto("resY")->Fill( p.GetY() - mcY );
      GetHisto("resZ")->Fill( p.GetZ() - mcZ );
      GetHisto("resSinPhi")->Fill( p.GetSinPhi() - mcSinPhi );
      GetHisto("resDzDs")->Fill( p.GetDzDs() - mcDzDs );
      if(CAMath::Abs(qPt) > 1.e-8){
        GetHisto("resPt")->Fill( (pt - mcPt)/mcPt );
      }
//        cout << qPt << "  "<< mcQPt << endl;
      if ( p.GetErr2Y() > 0 ) GetHisto("pullY")->Fill( ( p.GetY() - mcY ) / TMath::Sqrt( p.GetErr2Y() ) );
      if ( p.GetErr2Z() > 0 ) GetHisto("pullZ")->Fill( ( p.GetZ() - mcZ ) / TMath::Sqrt( p.GetErr2Z() ) );
      if ( p.GetErr2SinPhi() > 0 ) GetHisto("pullSinPhi")->Fill( ( p.GetSinPhi() - mcSinPhi ) / TMath::Sqrt( p.GetErr2SinPhi() ) );
      if ( p.GetErr2DzDs() > 0 ) GetHisto("pullDzDs")->Fill( ( p.DzDs() - mcDzDs ) / TMath::Sqrt( p.GetErr2DzDs() ) );
      if(CAMath::Abs(qPt) > 1.e-7 && p.GetErr2QPt()>0 ) GetHisto("pullQPt")->Fill( (qPt - mcQPt)/TMath::Sqrt(p.GetErr2QPt()) );

      break;
    }
    ///  mvz end
  }

  { // TODO
    int nHits = fTracker->NHits();
    for ( int ih = 0; ih < nHits; ih++ ) {
      const AliHLTTPCCAGBHit &hit = fTracker->Hit( ih );
      const AliHLTTPCCAHitLabel &l = (*fHitLabels)[hit.ID()];
//       fhHitErrY->Fill( hit.ErrY() );
//       fhHitErrZ->Fill( hit.ErrZ() );

      const int iMC = l.fLab[0];
      AliHLTTPCCAMCTrack &mc = (*fMCTracks)[iMC];

      int MCindex = -1;
      int nFirstMC = mc.FirstMCPointID();
      int nMCPoints = mc.NMCPoints();

      AliHLTTPCCALocalMCPoint *points = &((*fLocalMCPoints).Data()[nFirstMC]);

      for(int iMCPoint=0; iMCPoint<nMCPoints; iMCPoint++)
      {
        if(points[iMCPoint].ISlice() != fISlice) continue;
        if(points[iMCPoint].IRow() == hit.IRow())
        {
          if(fabs(hit.Y() - points[iMCPoint].Y())<2 && fabs(hit.Z() - points[iMCPoint].Z())<2)
            MCindex = iMCPoint;
        }
      }
      if(MCindex == -1)
      {
        continue;
      }

      GetHisto("resYHit")->Fill( hit.Y() - points[MCindex].Y() );
      GetHisto("resZHit")->Fill( hit.Z() - points[MCindex].Z() );
    }
  }


} // void AliHLTTPCCASlicePerformance::FillHistos()
