// $Id: AliHLTTPCCAMerger.cxx,v 1.13 2012/08/13 19:35:05 fisyak Exp $
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


#include "AliHLTTPCCASliceTrack.h"
#include "AliHLTTPCCATracker.h"
#include "AliHLTTPCCAGBTrack.h"
#include "AliHLTTPCCATrackParam.h"

#include "AliHLTTPCCAMerger.h"

#include "AliHLTTPCCAMath.h"
#include "Stopwatch.h"

#include "AliHLTTPCCATrackParam.h"
#include "AliHLTTPCCASliceTrack.h"
#include "AliHLTTPCCASliceOutput.h"
#include "AliHLTTPCCAMergedTrack.h"
#include "AliHLTTPCCAMergerOutput.h"
#include "AliHLTTPCCADataCompressor.h"
#include "AliHLTTPCCAParam.h"
#include "AliHLTTPCCATrackLinearisation.h"

#include <iostream>
using std::cout;
using std::endl;

#ifdef MAIN_DRAW
#include "AliHLTTPCCADisplay.h"
#endif // MAIN_DRAW

#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
//#define DO_MERGER_PERF // TODO don't work for Standalone version
#else
// #include "AliHLTTPCCAMergerPerformance.h"
// #include "AliHLTTPCCAPerformance.h"
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE

#include "AliHLTTPCCATrackParamVector.h"
#include "AliHLTTPCCATrackLinearisationVector.h"

/*struct AliHLTTPCCAMerger::AliHLTTPCCATrackMemory
{
  int_v fHitIndex; // index of the current hit
  uint_v fNHits; // n track hits
  TrackParamVector fInnerParam;
  TrackParamVector fOuterParam;
};

struct AliHLTTPCCAMerger::AliHLTTPCCAHitMemory
{
  float_v *XData;         // packed x coordinate of the given (global) hit index
  float_v *YData;         // packed y coordinate of the given (global) hit index
  float_v *ZData;         // packed z coordinate of the given (global) hit index

  uint_v *ISector;            // sector number
  uint_v *IRow;              // row number

  uint_v *IClu;

  unsigned int FirstSectorHit[fgkNSlices];
};*/

int AliHLTTPCCAMerger::fgDoNotMergeBorders = 0;

AliHLTTPCCAMerger::AliHLTTPCCAMerger()
    :
    fSliceParam(),

    fMaxClusterInfos( 0 ),
    fClusterInfos( 0 ),

    fMaxTrackInfos( 0 ),
    fTrackInfos( 0 ),
    fOutput( 0 )

{
  //* constructor
  Clear();
  
  for(int iSlice=0; iSlice<fgkNSlices; iSlice++)
    slices[iSlice] = 0;
}

/*
AliHLTTPCCAMerger::AliHLTTPCCAMerger(const AliHLTTPCCAMerger&)
  :
  fSliceParam(),
  fkSlices(0),
  fOutput(0),
  fTrackInfos(0),
  fMaxTrackInfos(0),
  fClusterInfos(0),
  fMaxClusterInfos(0)
{
}

const AliHLTTPCCAMerger &AliHLTTPCCAMerger::operator=(const AliHLTTPCCAMerger&) const
{
  return *this;
}
*/

AliHLTTPCCAMerger::~AliHLTTPCCAMerger()
{
  //* destructor
    //yf?  if ( fTrackInfos ) delete[] fTrackInfos;
  if ( fClusterInfos ) delete[] fClusterInfos;
  if ( fOutput ) delete[] ( ( char* )( fOutput ) );
}

void AliHLTTPCCAMerger::Clear()
{
  for ( int i = 0; i < fgkNSlices; ++i ) {
    fkSlices[i] = 0;
  }
  for ( int i = 0; i < fNTimers; i++) fTimers[i] = 0;
}

void AliHLTTPCCAMerger::SetSlices (int i, AliHLTTPCCATracker *sl )
{
  //copy sector parameters information
  slices[i] = sl;
}

void AliHLTTPCCAMerger::SetSliceData( int index, const AliHLTTPCCASliceOutput *sliceData )
{
  //copy sector output information: hits, reconstructed tracks...
  fkSlices[index] = sliceData;
}

void AliHLTTPCCAMerger::Reconstruct()
{
  //* Main merging routine. Consist of 3 steps:
#ifdef USE_TIMERS
  Stopwatch timer;
  timer.Start();
#endif // USE_TIMERS

// 1) copy information from the sector tracker
  UnpackSlices();
#ifdef USE_TIMERS
  timer.Stop();
  fTimers[0] = timer.RealTime();
#endif // USE_TIMERS
  
#ifdef DO_MERGER_PERF  
  AliHLTTPCCAPerformance::Instance().CreateHistos("Merger");
  ((AliHLTTPCCAMergerPerformance*)(AliHLTTPCCAPerformance::Instance().GetSubPerformance("Merger")))->SetNewEvent(
                                   AliHLTTPCCAPerformance::Instance().GetTracker(),
				   AliHLTTPCCAPerformance::Instance().GetHitLabels(),
				   AliHLTTPCCAPerformance::Instance().GetMCTracks(),
				   AliHLTTPCCAPerformance::Instance().GetMCPoints());
  ((AliHLTTPCCAMergerPerformance*)(AliHLTTPCCAPerformance::Instance().GetSubPerformance("Merger")))->FillMC();
#endif //DO_MERGER_PERF

#ifndef DO_NOT_MERGE
#ifdef USE_TIMERS
  timer.Start();
#endif // USE_TIMERS

// 2) merge nonoverlaping tracks
  FindNeighbourTracks(1);
  Merging(1);

#ifdef USE_TIMERS
  timer.Stop();
  fTimers[1] = timer.RealTime();

// 3) merge overlaping tracks, store the tracks to the global tracker
  timer.Start();
#endif // USE_TIMERS

  FindNeighbourTracks(0);
  Merging(0);
  
#ifdef USE_TIMERS
  timer.Stop();
  fTimers[2] = timer.RealTime();
#endif // USE_TIMERS
#else // DO_NOT_MERGE  // just to save tracks in output format
#ifdef USE_TIMERS
  timer.Start();
#endif // USE_TIMERS
  Merging(0);
#ifdef USE_TIMERS
  timer.Stop();
  fTimers[1] = timer.RealTime();
#endif // USE_TIMERS
#endif // DO_NOT_MERGE
}

void AliHLTTPCCAMerger::UnpackSlices()
{
  //* unpack the cluster information from the slice tracks and initialize track info array

  // get N tracks and N clusters in event
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
  int NTracksPrev=0;
#endif // DO_TPCCATRACKER_EFF_PERFORMANCE
  int nTracksTotal = 0;
  int nTrackClustersTotal = 0;
  for ( int iSlice = 0; iSlice < fgkNSlices; iSlice++ ) {
    if ( !fkSlices[iSlice] ) continue;
    nTracksTotal += fkSlices[iSlice]->NTracks();
    nTrackClustersTotal += fkSlices[iSlice]->NTrackClusters();
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
    slices[iSlice]->fNOutTracks1 = 0;
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
  }

  // book/clean memory if necessary
  {
 //   if ( nTracksTotal > fMaxTrackInfos || ( fMaxTrackInfos > 100 && nTracksTotal < 0.5*fMaxTrackInfos ) )
    {
      fMaxTrackInfos = ( int ) ( nTracksTotal );
      fTrackInfos.resize(fMaxTrackInfos);
    }

//    if ( nTrackClustersTotal > fMaxClusterInfos || ( fMaxClusterInfos > 1000 && nTrackClustersTotal < 0.5*fMaxClusterInfos ) ) 
    {
      if ( fClusterInfos ) delete[] fClusterInfos;
      fMaxClusterInfos = ( int ) ( nTrackClustersTotal );
      fClusterInfos = new AliHLTTPCCAClusterInfo [fMaxClusterInfos];
    }

    if ( fOutput ) delete[] ( ( char* )( fOutput ) );
    int size = fOutput->EstimateSize( nTracksTotal, nTrackClustersTotal );
    fOutput = ( AliHLTTPCCAMergerOutput* )( new float2[size/sizeof( float2 )+1] );
  }

  // unpack track and cluster information

  int nTracksCurrent = 0;
  int nClustersCurrent = 0;
  for ( int iSlice = 0; iSlice < fgkNSlices; iSlice++ ) {

    fSliceTrackInfoStart[ iSlice ] = nTracksCurrent;
    fSliceNTrackInfos[ iSlice ] = 0;

    if ( !fkSlices[iSlice] ) continue;

    const AliHLTTPCCASliceOutput &slice = *( fkSlices[iSlice] );

    for ( int itr = 0; itr < slice.NTracks(); itr += uint_v::Size ) {

      int nTracksVector = uint_v::Size;
      if(slice.NTracks() - itr < uint_v::Size )
        nTracksVector = slice.NTracks() - itr;

      float_v startAlpha;
      float_v endAlpha;

      int hits[1000][uint_v::Size];
      uint_v nHits;
      const AliHLTTPCCATrackParam *pStartPoint[uint_v::Size] = {0};
      unsigned int nCluNew = 0;

      for(int iV=0; iV < nTracksVector; iV++)
      {
        const AliHLTTPCCASliceTrack &sTrack = slice.Track( itr + iV );

        nHits[iV] = 0;
        for ( int iTrClu = 0; iTrClu < sTrack.NClusters(); iTrClu++ ) {
// unpack cluster information
          AliHLTTPCCAClusterInfo &clu = fClusterInfos[nClustersCurrent + nCluNew + nHits[iV]];
          int ic = sTrack.FirstClusterRef() + iTrClu;

          clu.SetISlice( iSlice );
          clu.SetIRow( slice.ClusterIDrc( ic ).Row() );
          clu.SetIClu( slice.ClusterIDrc( ic ).Cluster() );
          float2 yz = slice.ClusterUnpackedYZ( ic );
          clu.SetX( slice.ClusterUnpackedX( ic ) );
          clu.SetY( yz.x );
          clu.SetZ( yz.y );
          hits[iTrClu][iV] = nClustersCurrent + nCluNew + iTrClu;
          nHits[iV]++;
        }
        nCluNew += nHits[iV];

        pStartPoint[iV] = &sTrack.Param();
        startAlpha[iV] = slices[iSlice]->Param().Alpha();
        endAlpha[iV]   = startAlpha[iV];
      }

        //when we turn off the extrapolation step in the tracklet constructor, we have parameters in the last point, not in the first!
        //that's why the fitting direction should be changed
      float_m fitted = float_m(true);
      fitted &= static_cast<float_m>(static_cast<uint_v>(nHits) >= 3);
      const uint_v NAllHits(nHits);
//#define DISABLE_HIT_SEARCH
#ifdef DISABLE_HIT_SEARCH
//TODO correct code
      fitted &= FitTrack( startPoint, startAlpha, endPoint, endAlpha, hits, nHits, nTracksVector, 1 );
      for(int iV=0; iV < nTracksVector; iV++)
      {
        endPoint[iV] = startPoint[iV];
        endAlpha[iV] = startAlpha[iV];
      }
      fitted &= FitTrack( endPoint, endAlpha, startPoint, startAlpha, hits, nHits, nTracksVector, 0 );
#else
      fitted &= static_cast<float_m>( uint_v( Vc::IndexesFromZero ) < nTracksVector );
// refit the track

        // start from startPoint
      AliHLTTPCCATrackParamVector vEndPoint;
      ConvertPTrackParamToVector(pStartPoint,vEndPoint,nTracksVector); // save as end because it will be fitted
      float_v vEndAlpha(Vc::Zero);
      vEndAlpha = startAlpha;

      uint_v firstHits(Vc::Zero);
        // refit in the forward direction: going from the first hit to the last, mask "fitted" marks with 0 tracks, which are not fitted correctly
      fitted &= FitTrack( vEndPoint,   vEndAlpha,   hits, firstHits, nHits, nTracksVector, fitted, 0 );
        // if chi2 per degree of freedom > 3. sigma - mark track with 0
      fitted &= vEndPoint.Chi2()  < 9.f*static_cast<float_v>(vEndPoint.NDF());

      AliHLTTPCCATrackParamVector vStartPoint(vEndPoint);
      float_v vStartAlpha(vEndAlpha);
        // refit in the backward direction: going from the last hit to the first
      fitted &= FitTrack( vStartPoint, vStartAlpha, hits, firstHits, nHits, nTracksVector, fitted, 1 );
        // if chi2 per degree of freedom > 3. sigma - mark track with 0
      fitted &= vStartPoint.Chi2() < 9.f*static_cast<float_v>(vStartPoint.NDF());
#endif
//      fitted &= static_cast<uint_v>(nHits) >= NAllHits * AliHLTTPCCAParameters::MinTrackPurity; // bad cat. dependence on MC definitions

      for(int iV=0; iV<float_v::Size; iV++)
      {
        if(!fitted[iV]) continue;
//       foreach_bit(int iV, fitted)
//       {
          // if the track fitted correctly store the track
        AliHLTTPCCASliceTrackInfo &track = fTrackInfos[nTracksCurrent];
        
        track.SetInnerParam( AliHLTTPCCATrackParam( vStartPoint, iV ) );
        track.SetInnerAlpha( vStartAlpha[iV] );
        track.SetOuterParam( AliHLTTPCCATrackParam( vEndPoint,   iV ) );
        track.SetOuterAlpha( vEndAlpha[iV] );
        track.SetFirstClusterRef( nClustersCurrent );
        track.SetNClusters( nHits[iV] );
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
        track.orig_track_id = itr+iV;
        track.fSlice = iSlice;
        track.number = nTracksCurrent - fSliceTrackInfoStart[iSlice];
#endif // DO_TPCCATRACKER_EFF_PERFORMANCE
        track.fInnerRow = (fClusterInfos[hits[0][iV]]).IRow();
        track.fOuterRow = (fClusterInfos[hits[nHits[iV]-1][iV]]).IRow();

        for ( unsigned int i = 0; i < nHits[iV]; i++ )
          fClusterInfos[nClustersCurrent + i] = fClusterInfos[hits[i][iV]];
        nTracksCurrent++;
        fSliceNTrackInfos[ iSlice ]++;
        nClustersCurrent += nHits[iV];
      }
    }

#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
    if (slices[iSlice]->fOutTracks1) delete[] slices[iSlice]->fOutTracks1;
    slices[iSlice]->fOutTracks1 = new AliHLTTPCCAOutTrack [nTracksCurrent-NTracksPrev];
    for (int i=0; i<nTracksCurrent-NTracksPrev; i++)
    {
      slices[iSlice]->fOutTracks1[i].SetStartPoint(fTrackInfos[i+NTracksPrev].InnerParam());
      slices[iSlice]->fOutTracks1[i].SetEndPoint(fTrackInfos[i+NTracksPrev].OuterParam());
      slices[iSlice]->fOutTracks1[i].SetOrigTrackID(fTrackInfos[i+NTracksPrev].orig_track_id);
      slices[iSlice]->fNOutTracks1++;
    }
    NTracksPrev = nTracksCurrent;
#endif // DO_TPCCATRACKER_EFF_PERFORMANCE
  }
}

float_m AliHLTTPCCAMerger::FitTrack( AliHLTTPCCATrackParamVector &t, float_v &Alpha0V,
                                      int hits[2000][uint_v::Size], uint_v &firstHits,uint_v &NTrackHits,
                                      int &nTracksV, float_m active0, bool dir )
{
  // Fit the track
/*#ifdef MAIN_DRAW
      AliHLTTPCCADisplay::Instance().ClearView();
      AliHLTTPCCADisplay::Instance().SetTPCView();
      AliHLTTPCCADisplay::Instance().DrawTPC();
#endif*/
  AliHLTTPCCATrackParamVector::AliHLTTPCCATrackFitParam fitPar;

  AliHLTTPCCATrackLinearisationVector linearization( t );

  t.CalculateFitParameters( fitPar );

  const int MaxNHits = 4*AliHLTTPCCAParameters::MaxNumberOfRows8; // koeff 4 reserves place for several turn
  int hitsNew[MaxNHits][uint_v::Size];
  uint_v nHitsNew(Vc::Zero);

  uint_v nHits(NTrackHits);
  nHits.setZero(static_cast<uint_m>(!active0));

  int nHitsMax = nHits.max();

    // pack hits
  float_v xVs[MaxNHits];
  float_v yVs[MaxNHits];
  float_v zVs[MaxNHits];
  float_v sliceAlphaVs[MaxNHits];
  uint_v RowVs[MaxNHits];
  
  for ( int ihit = 0; ihit < nHitsMax; ihit++ ) {
    const float_m &active = static_cast<float_m>( ihit < nHits ) && active0;

    for(int iV=0; iV < nTracksV; iV++)
    {
      if( !active[iV] ) continue;

      const unsigned int& jhit = HitIndex( firstHits, uint_v(NTrackHits), dir, iV, ihit );
      const AliHLTTPCCAClusterInfo &h = fClusterInfos[hits[jhit][iV]];
      sliceAlphaVs[ihit][iV] =  slices[h.ISlice()]->Param().Alpha();
      xVs[ihit][iV] = h.X();
      yVs[ihit][iV] = h.Y();
      zVs[ihit][iV] = h.Z();
      RowVs[ihit][iV] = h.IRow();
    }
  }

    // fit
  bool first = true;
  float_m active = active0; // stop on a hit if this hit can't be added
  for ( int ihit = 0; ihit < nHitsMax; ihit++ ) {
    active &= static_cast<float_m>( ihit < nHits );
    
    if(active.isEmpty()) continue;

    const float_v& xV = xVs[ihit];
    const float_v& yV = yVs[ihit];
    const float_v& zV = zVs[ihit];
    const float_v& sliceAlphaV = sliceAlphaVs[ihit];
    const uint_v& RowV = RowVs[ihit];

    const float_m savedActive = active;
    const float_v rotateA = sliceAlphaV - Alpha0V;
    float_m rotated(active);
    if( ISUNLIKELY( !(!CAMath::IsZero(rotateA) && active).isEmpty() ) ) { // track crosses a sector border very rarely
      rotated = t.Rotate( rotateA, linearization, .999f, active);
      active &= rotated;
    }
    const float_v xLast = t.X();

    Alpha0V(active) = sliceAlphaV;
    
    const float_m &transported = t.TransportToXWithMaterial( xV, linearization, fitPar, fSliceParam.cBz( ), 0.999f, active);
    active &= transported;

    if ( first ) {
      t.InitCovMatrixAndChi2AndNDF( active );
      t.CalculateFitParameters( fitPar );
      first = false;
    }

    float_v err2Y, err2Z;
    fSliceParam.GetClusterErrors2( RowV, t, &err2Y, &err2Z );
    const float_m &filtered = t.FilterWithMaterial(yV, zV, err2Y, err2Z, 0.999f, active);

    const float_m broken = savedActive && (!rotated || !transported || !filtered);
    if ( !broken.isEmpty() ) {
      t.TransportToXWithMaterial( xLast, linearization, fitPar, fSliceParam.cBz( ), 0.999f, transported && !filtered ); // transport back if hit can't be added. TODO with out material
      t.Rotate( -rotateA, linearization, .999f, rotated && (!transported || !filtered) );
    }

    active &= filtered;

    for(int iV=0; iV < nTracksV; iV++) {
      if( !(active[iV]) ) continue;

      const unsigned int& jhit = HitIndex( firstHits, uint_v(NTrackHits), dir, iV, ihit );
      hitsNew[nHitsNew[iV]][iV] = hits[jhit][iV];
    }
    nHitsNew(uint_m(active))++;
  }
  
  float_m ok = active0 && float_m(uint_v(nHitsNew) >= 3) && t.IsNotDiverged();

  t.SetQPt( float_v(1.e-8f), CAMath::Abs( t.QPt() ) < 1.e-8f );
  t.NormilizeSignCosPhi( linearization, ok );

  NTrackHits = nHitsNew;
  for(int iV=0; iV < nTracksV; iV++) {
    if ( !ok[iV] ) continue;
    
    for ( unsigned char i = 0; i < NTrackHits[iV]; i++ ) {
      const unsigned int &jhit = HitIndex( firstHits, uint_v(NTrackHits), dir, iV, i );
      hits[jhit][iV] = hitsNew[i][iV];
    }
  }

  return ok;
}

void AliHLTTPCCAMerger::MakeBorderTracks( AliHLTTPCCABorderTrack B[], unsigned int &nB, unsigned char &iSlice)
{
  //* prepare slice tracks for merging with next/previous/same sector

  for ( int itr = 0; itr < fSliceNTrackInfos[iSlice]; itr++ ) 
  {
    const AliHLTTPCCASliceTrackInfo &track = fTrackInfos[ fSliceTrackInfoStart[iSlice] + itr ];

//    if(track.NClusters() > 41) continue;

    AliHLTTPCCABorderTrack &bTr = B[nB];

    bTr.SetTrackID( itr );
    bTr.Setb(track.InnerParam().DzDs());
    bTr.SetbErr2(track.InnerParam().Err2DzDs());
    bTr.Setp(track.InnerParam().QPt());
    bTr.SetpErr2(track.InnerParam().Err2QPt());

    bTr.SetInnerRow( track.fInnerRow );
    bTr.SetOuterRow( track.fOuterRow );

    nB++;
  }
}

void AliHLTTPCCAMerger::CheckTracksMatch( int number,
const AliHLTTPCCATrackParamVector &InParT1, const AliHLTTPCCATrackParamVector &OutParT1, const float_v &InAlphaT1, const float_v &OutAlphaT1,
const AliHLTTPCCATrackParamVector &InParT2, const AliHLTTPCCATrackParamVector &OutParT2, const float_v &InAlphaT2, const float_v &OutAlphaT2,
const float_v dxArr[4], const float_v dyArr[4], const float_v &sinS1_E2v, const float_v &sinS2_E1v,
float_v &minL2v, const float &bestChi2, float_v& min_chi2, float_m& active )
{
  const float_v k1   = InParT1.QPt() * fSliceParam.cBz();
  const float_v dx_1 = -dxArr[2];
  const float_v t_sin1 = k1 * dx_1 + sinS1_E2v;
    // check, whether inner parameters of both tracks in the pair could be transported to the outer parameters of the other track in the pair
  const float_v k2   = InParT2.QPt() * fSliceParam.cBz();
  const float_v dx_2 = -dxArr[3];
  const float_v t_sin2 = k2 * dx_2 + sinS2_E1v;
  active &= CAMath::Abs(t_sin1) <= 0.999f && CAMath::Abs(t_sin2) <= 0.999f;
  if(active.isEmpty()) return;
  
  const float_v dzArr[4] = { OutParT1.Z() - OutParT2.Z(),
                             InParT1.Z()  - InParT2.Z() ,
                             InParT1.Z()  - OutParT2.Z(),
                             OutParT1.Z() - InParT2.Z()  };
  
  const float_v dr2Arr[4] = { dxArr[0]*dxArr[0] + dyArr[0]*dyArr[0] + dzArr[0]*dzArr[0],   // OO
                              dxArr[1]*dxArr[1] + dyArr[1]*dyArr[1] + dzArr[1]*dzArr[1],   // II
                              dxArr[2]*dxArr[2] + dyArr[2]*dyArr[2] + dzArr[2]*dzArr[2],   // IO
                              dxArr[3]*dxArr[3] + dyArr[3]*dyArr[3] + dzArr[3]*dzArr[3] }; // OI

  int_v idMin01(Vc::Zero), idMin23(2);
  float_v min01 = dr2Arr[0], min23 = dr2Arr[2];
  const float_m &mask1lt0 = dr2Arr[1] < min01;
  const float_m &mask3lt2 = dr2Arr[3] < min23;
  min01  (                    mask1lt0  ) = dr2Arr[1]; 
  idMin01( static_cast<int_m>(mask1lt0) ) = 1;
  min23  (                    mask3lt2  ) = dr2Arr[3];
  idMin23( static_cast<int_m>(mask3lt2) ) = 3;

  float_v dr2(min23);
  int_v idMin(idMin23);
  
  if(number == 0) {
    const int_m &mask01lt23 = static_cast<int_m>(min23 > min01);
    idMin(mask01lt23) = idMin01;
    dr2(float_m(mask01lt23)) = min01;
  }

  active &= float_m(idMin != 2) || float_m(number != 1); // distanse in rows between inner1 and outer2 is maximum, it can't beminimum in dr unless tracks have completely different slopes
  
    // check distance between tracks
  minL2v = dr2;
  active &= (minL2v < bestChi2) || float_m(number != 1); // chi2 cut == dr2 only for number == 1

    // check dx
  float_v dx(Vc::Zero);
  const float_m &IsIdMin0 = active && static_cast<float_m>(idMin == 0);
  const float_m &IsIdMin1 = active && static_cast<float_m>(idMin == 1);
  const float_m &IsIdMin2 = active && static_cast<float_m>(idMin == 2);
  const float_m &IsIdMin3 = active && static_cast<float_m>(idMin == 3);
  dx( IsIdMin0 ) = dxArr[0];
  dx( IsIdMin1 ) = dxArr[1];
  dx( IsIdMin2 ) = dxArr[2];
  dx( IsIdMin3 ) = dxArr[3];

    // on the first iteration we process only overlaped tracks on second - only nonoverlaped TODO: understand how it works for already merged tracks and for tracks from different sectors
  active &= (CAMath::Abs(dx) < 0.1f) ||
    ( (dx > 0.f) && float_m(number == 1) || 
      (dx < 0.f) && float_m(number == 0)    );
    // active &= 
    //   ( (dx >= -0.1f) && float_m(number == 1) || 
    //     (dx <  -0.1f) && float_m(number == 0)    );

  if(active.isEmpty()) return;
    
    // collect parameters for min distance
  AliHLTTPCCATrackParamVector Thelp, Thelp1;
  float_v a1(Vc::Zero);
  float_v a2(Vc::Zero);

  if(number == 0) {
    if ( !IsIdMin0.isEmpty() ) {
      Thelp.SetTrackParam(OutParT1, IsIdMin0 );
      Thelp1.SetTrackParam(OutParT2, IsIdMin0 );
      a1( IsIdMin0 ) = OutAlphaT1;
      a2( IsIdMin0 ) = OutAlphaT2;
    }
    if ( !IsIdMin1.isEmpty() ) {
      Thelp.SetTrackParam(InParT1, IsIdMin1 );
      Thelp1.SetTrackParam(InParT2, IsIdMin1 );
      a1( IsIdMin1 ) = InAlphaT1;
      a2( IsIdMin1 ) = InAlphaT2;
    }
    if ( !IsIdMin2.isEmpty() ) {
      Thelp.SetTrackParam(InParT1, IsIdMin2 );
      Thelp1.SetTrackParam(OutParT2, IsIdMin2 );
      a1( IsIdMin2 ) = InAlphaT1;
      a2( IsIdMin2 ) = OutAlphaT2;
    }
  }
  if ( !IsIdMin3.isEmpty() ) {
    Thelp.SetTrackParam(OutParT1, IsIdMin3 );
    Thelp1.SetTrackParam(InParT2, IsIdMin3 );
    a1( IsIdMin3 ) = OutAlphaT1;
    a2( IsIdMin3 ) = InAlphaT2;
  }

    // rotate track parameters to the same coordinate system
  if ( !( CAMath::Abs( a2 - a1 ) > 1e-7f && active ).isEmpty() )
    active &= Thelp.Rotate( a2 - a1 , 0.999f, active);
  
    // transport tracks to the middle point by X coordinate
  const float_v &ToX = 0.5f * (Thelp.X() + Thelp1.X());
  active &= Thelp.TransportToX( ToX, float_v(fSliceParam.cBz()), 0.999f, active);
  if(active.isEmpty()) return;
  active &= Thelp1.TransportToX( ToX, float_v(fSliceParam.cBz()), 0.999f, active);
          
    // if tracks are too far one from each other after propagation - they could not be neighbours
  active &= CAMath::Abs(Thelp1.Y() - Thelp.Y()) <= 10.f;
  active &= CAMath::Abs(Thelp1.Z() - Thelp.Z()) <= 10.f;
  active &= CAMath::Abs(Thelp1.SinPhi() - Thelp.SinPhi()) <= 0.15f;
  if(active.isEmpty()) return;
          
    // merge parameters and calculate chi2
  float_v C[15], r[5], chi2(Vc::Zero);
  FilterTracks(Thelp.GetPar(), Thelp.GetCov(), Thelp1.GetPar(), Thelp1.GetCov(), r, C, chi2, active);
          
    // if after merging parameters are infinite, or diagonal elements of cov. matrix are negative - tracks could not be neighbours
  for ( int i = 0; i < 15; i++ ) active &= CAMath::Finite( C[i] );
  for ( int i = 0; i < 5; i++ ) active &= CAMath::Finite( r[i] );
  active &= ( C[0] > 0 ) && ( C[2] > 0 ) && ( C[5] > 0 ) && ( C[9] > 0 ) && ( C[14] > 0 );
        
    // if obtained chi2 value is too high - tracks could not be neighbours
  active &= (chi2 <= 100.f) && float_m(number == 1) || (chi2 <= 300.f) && float_m(number == 0);
  active &= (chi2 < bestChi2) || float_m(number != 0); // for number == 0
        
  min_chi2(active) = minL2v; // use minL2v with number == 1
  min_chi2(active && float_m(number == 0) ) = chi2;
}

void AliHLTTPCCAMerger::FindMinMaxIndex( int N2, const unsigned int FirstTrIR[], const unsigned int LastTrIR[], int minIRow, int maxIRow, int &min, int &max )
{
    // find min index
  min = -1;
  for(; minIRow < fSliceParam.NRows(); minIRow++)
    if (LastTrIR[minIRow] != 50000) {
      min = LastTrIR[minIRow];
      break;
    }
    // find max index
  max = N2;
  for(; maxIRow >= minIRow; maxIRow--)
    if(FirstTrIR[maxIRow] != 50000) {
      max = FirstTrIR[maxIRow];
      break;
    }
}

//#define BACK_ORDER_FOR_0 // little bit faster without it. (but why?)
void AliHLTTPCCAMerger::MergeBorderTracks( AliHLTTPCCABorderTrack B1[], int N1, unsigned int iSlice1, AliHLTTPCCABorderTrack B2[], int N2, unsigned int iSlice2, int number, const unsigned int FirstTrIR[], const unsigned int LastTrIR[] )
{
// The function creates links to the inner and outer neighbours

  const float factor2k = 64.f;
  for ( int i1 = 0; i1 < N1; i1++ ) {
  
    AliHLTTPCCABorderTrack &b1 = B1[i1];
    
      // Find firts and last row for the nighbours finding. All tracks are sorted by the inner row.

    int lastIRow = fSliceParam.NRows()-1; // row to end finding of a clone
    int firstIRow = 0;
    int dir = -1; // how track2 index is changed from a start to an end row.
    int ifirst2 = -1;
    int ilast2 = N2;
    
    if(number == 1) { // find tracks with <= 1 common rows.
      dir = -1; // Tracks2 sorted in order of decrease innerRow. For number==1 we'll go in the upper dir, so track indices will decrease
      
      firstIRow = b1.OuterRow() + 0; // row to begin finding of clone
      lastIRow = b1.OuterRow() + AliHLTTPCCAParameters::MaximumRowGapBetweenClones;
      lastIRow = (lastIRow < fSliceParam.NRows()) ? lastIRow : fSliceParam.NRows()-1;

      FindMinMaxIndex( N2, FirstTrIR, LastTrIR, firstIRow, lastIRow, ifirst2, ilast2 );
    }
    else if(number == 0) { // find tracks with >= 2 common rows.
#ifdef BACK_ORDER_FOR_0
      dir = 1; // Tracks2 sorted in order of decrease innerRow. For number==0 we'll go in the down dir, so indices will increase
            
      firstIRow = b1.OuterRow() - 1; // row to begin finding of a clone
      lastIRow = b1.OuterRow() - AliHLTTPCCAParameters::MaximumRowGapBetweenOverlapingClones;
      if ( firstIRow < 0 ) {
        firstIRow = 0;
        lastIRow = 0;
      }
      else if ( lastIRow < 0 ){
        lastIRow = 0;
      }

      FindMinMaxIndex( N2, FirstTrIR, LastTrIR, lastIRow, firstIRow, ilast2, ifirst2 );
#else // BACK_ORDER_FOR_0
      dir = -1;
      
      firstIRow = b1.OuterRow() - AliHLTTPCCAParameters::MaximumRowGapBetweenOverlapingClones; // row to begin finding of a clone
      lastIRow = b1.OuterRow() - 1;
      if ( lastIRow < 0 ) {
        firstIRow = 0;
        lastIRow = 0;
      }
      else if ( firstIRow < 0 ){
        firstIRow = 0;
      }

      FindMinMaxIndex( N2, FirstTrIR, LastTrIR, firstIRow, lastIRow, ifirst2, ilast2 );
#endif // BACK_ORDER_FOR_0
    }

    if (dir*ifirst2 > dir*ilast2) continue;
    
      // rarely changed parameters
    const AliHLTTPCCASliceTrackInfo *Tt1 = &fTrackInfos[fSliceTrackInfoStart[iSlice1] + b1.TrackID() ];

    int bestI2 = -1; // index of second tracksegment, which corresponds to minMinLv and bestChi2
    float bestChi2( std::max( Tt1->ChiNext, Tt1->ChiPrev ) );
    bool bestIsNext(1e10f);
    float_v minL2v(1e10f);

    float Tt2OuterAlpha=0;
    float Tt2InnerAlpha=0;
    float xE1_E2=0, xS1_S2=0, xS1_E2=0;
    float yE1_E2=0, yS1_S2=0, yS1_E2=0;
    float sinS1_E2=0;

    if( !((number == 1) && (iSlice1 == iSlice2)) ) {       // rotate coordinates to the same coordinate system
      Tt2OuterAlpha = fTrackInfos[fSliceTrackInfoStart[iSlice2] + B2[ifirst2].TrackID() ].OuterAlpha();
      Tt2InnerAlpha = fTrackInfos[fSliceTrackInfoStart[iSlice2] + B2[ifirst2].TrackID() ].InnerAlpha();

      if (number == 0) {
        float tmp;
        Tt1->OuterParam().RotateXY(Tt2OuterAlpha - Tt1->OuterAlpha(),xE1_E2,yE1_E2,tmp);
        Tt1->InnerParam().RotateXY(Tt2InnerAlpha - Tt1->InnerAlpha(),xS1_S2,yS1_S2,tmp);
      }
      Tt1->InnerParam().RotateXY(Tt2OuterAlpha - Tt1->InnerAlpha(),xS1_E2,yS1_E2,sinS1_E2);
    }
    else {
      xE1_E2 = Tt1->OuterParam().X(); yE1_E2 = Tt1->OuterParam().Y();
      xS1_S2 = Tt1->InnerParam().X(); yS1_S2 = Tt1->InnerParam().Y();
      xS1_E2 = Tt1->InnerParam().X(); yS1_E2 = Tt1->InnerParam().Y(); sinS1_E2 = Tt1->InnerParam().SinPhi();
    }
    
      // Convert data of the first track to the SIMD vectors. 
    float_v xE1_E2v(xE1_E2), xS1_S2v(xS1_S2), xS1_E2v(xS1_E2);
    float_v yE1_E2v(yE1_E2), yS1_S2v(yS1_S2), yS1_E2v(yS1_E2);
    float_v sinS1_E2v(sinS1_E2);
    float_v vTt2OuterAlpha(Tt2OuterAlpha), vTt2InnerAlpha(Tt2InnerAlpha);

    uint_v b2index;

    AliHLTTPCCATrackParamVector OutParT1( Tt1->OuterParam() ), OutParT2,
                                InParT1(  Tt1->InnerParam() ), InParT2;
    float_v OutAlphaT1( Tt1->OuterAlpha() ), OutAlphaT2,
            InAlphaT1( Tt1->InnerAlpha() ),  InAlphaT2;
    const AliHLTTPCCATrackParam *T2OuterParamMemory[uint_v::Size] = {0},
                                *T2InnerParamMemory[uint_v::Size] = {0};
    float_v T2InnerAlphaMemory,T2OuterAlphaMemory;

    for ( int i2 = ifirst2; ; ) {
      int nVecElements = 0;
      for( ; nVecElements < uint_v::Size && dir*i2 <= dir*ilast2; i2 += dir ) {

        const AliHLTTPCCABorderTrack &b2 = B2[i2];
      
          // if dz/ds or q/pt of the tracks differs more, than by several sigmas (now 8 is used) - they are not neighbours
        float db2 = b1.b() - b2.b();
        db2 *= db2;
        const float ddb2 = b1.bErr2() + b2.bErr2();
        float dp2 = b1.p() - b2.p();
        dp2 *= dp2;
        const float ddp2 = b1.pErr2() + b2.pErr2();
        if( db2 > factor2k * ddb2 || dp2 > factor2k * ddp2 ||
            ( iSlice1 == iSlice2 && b1.TrackID() == b2.TrackID() ) )  // the track could not be itselfs neighbour
          continue;

        const AliHLTTPCCASliceTrackInfo *Tt2 = &fTrackInfos[ fSliceTrackInfoStart[iSlice2] + b2.TrackID() ];

        if( (Tt1->NextNeighbour() == b2.TrackID() && Tt1->SliceNextNeighbour() == iSlice2) ||
            (Tt1->PrevNeighbour() == b2.TrackID() && Tt1->SlicePrevNeighbour() == iSlice2) )
          continue;  // the tracks are already matched
        
        if ( number == 0 ) { // reconstruct only parallel tracks, created because of clusters spliting
          const float &z1I = Tt1->InnerParam().Z();
          const float &z1O = Tt1->OuterParam().Z();
          const float &z2I = Tt2->InnerParam().Z();
          const float &z2O = Tt2->OuterParam().Z();

          float dzArr[4] = { z1O - z2O,
                                   z1I - z2I,
                                   z1I - z2O,
                                   z1O - z2I };

          for( int k = 0; k < 4; k++ )
            if( CAMath::Abs(dzArr[k]) < 1.f ) dzArr[k] = 0.f; // indistinguishable

            // tracks has to be overlaped in z. I.e. at least one edge of one track is inside of other track
          if( !(dzArr[0]*dzArr[3] <= 0 || dzArr[1]*dzArr[2] <= 0 || dzArr[1]*dzArr[3] <= 0) ) continue;

          const float &r1I = b1.InnerRow();
          const float &r1O = b1.OuterRow();
          const float &r2I = b2.InnerRow();
          const float &r2O = b2.OuterRow();
          const float drArr[4] = { r1O - r2O,
                                   r1I - r2I,
                                   r1I - r2O,
                                   r1O - r2I };
            // tracks has to be parallel
          if( !(drArr[0]*dzArr[0] >= 0 &&
                drArr[1]*dzArr[1] >= 0 &&
                drArr[2]*dzArr[2] >= 0 &&
                drArr[3]*dzArr[3] >= 0) ) continue;
        }

          // store tracks, which passed previous cuts
        T2InnerParamMemory[nVecElements] = &Tt2->InnerParam();
        T2OuterParamMemory[nVecElements] = &Tt2->OuterParam();
        T2InnerAlphaMemory[nVecElements] = Tt2->InnerAlpha();
        T2OuterAlphaMemory[nVecElements] = Tt2->OuterAlpha();
        b2index[nVecElements] = i2;
        nVecElements++;
      }
      if (nVecElements == 0) break;

        // convert parameters to SIMD vectors for the array of second tracks
      ConvertPTrackParamToVector(T2InnerParamMemory,InParT2,nVecElements);
      ConvertPTrackParamToVector(T2OuterParamMemory,OutParT2,nVecElements);

      OutAlphaT2 = (T2OuterAlphaMemory);
      InAlphaT2 = (T2InnerAlphaMemory);

      float_m active = static_cast<float_m>( uint_v( Vc::IndexesFromZero ) < nVecElements );

      const float_v &yE1v = OutParT1.Y();
      const float_v &xE1v = OutParT1.X();

      const float_v &yE2v = OutParT2.Y();
      const float_v &yS2v = InParT2.Y();

      const float_v &xE2v = OutParT2.X();
      const float_v &xS2v = InParT2.X();
      
      float_v xS2_E1v(Vc::Zero);
      float_v yS2_E1v(Vc::Zero);
      float_v sinS2_E1v(Vc::Zero);

      if( !((number == 1) && (iSlice1 == iSlice2)) ) {
        InParT2.RotateXY(OutAlphaT1 - InAlphaT2,xS2_E1v,yS2_E1v,sinS2_E1v,active);

        const float_m &rotate = active && (CAMath::Abs(vTt2OuterAlpha - OutAlphaT2) > 0.01f || CAMath::Abs(vTt2InnerAlpha - InAlphaT2) > 0.01f );

        if( ISUNLIKELY(!(rotate.isEmpty())) ) // ISUNLIKELY is crussial, 3 times speed up!
        {// recalculate values if they could change
          vTt2OuterAlpha(rotate) = OutAlphaT2;
          vTt2InnerAlpha(rotate) = InAlphaT2;

          if (number == 0) {
            float_v tmp;
            OutParT1.RotateXY( vTt2OuterAlpha - OutAlphaT1, xE1_E2v, yE1_E2v, tmp,       rotate);
            InParT1.RotateXY(  vTt2InnerAlpha - InAlphaT1,  xS1_S2v, yS1_S2v, tmp,       rotate);
          }
          InParT1.RotateXY(  vTt2OuterAlpha - InAlphaT1,  xS1_E2v, yS1_E2v, sinS1_E2v, rotate);
        }
      }
      else {
        xS2_E1v = InParT2.X(); yS2_E1v = InParT2.Y(); sinS2_E1v = InParT2.SinPhi();
      }

      const float_v dxArr[4] = { (xE1_E2v - xE2v),
                                 (xS1_S2v - xS2v), 
                                 (xS1_E2v - xE2v), 
                                 (xS2_E1v - xE1v) };
      const float_v dyArr[4] = { (yE1_E2v - yE2v), 
                                 (yS1_S2v - yS2v), 
                                 (yS1_E2v - yE2v), 
                                 (yS2_E1v - yE1v) };

      float_v min_chi2(1e10f);
      CheckTracksMatch( number,
      InParT1, OutParT1, InAlphaT1, OutAlphaT1,
      InParT2, OutParT2, InAlphaT2, OutAlphaT2,
      dxArr, dyArr, sinS1_E2v, sinS2_E1v,
      minL2v, bestChi2, min_chi2, active );

      if(active.isEmpty()) continue;

      for(int iV=0; iV < nVecElements; iV++) { // foreach_bit( int iV, active )  is slower since mostly nVecElements < 4
        if(!active[iV]) continue;
          // determine, whether neighbour is inner or outer
          
        const AliHLTTPCCABorderTrack &b2iV = B2[b2index[iV]];
          
        bool IsNext =
          ( b1.InnerRow() < b2iV.InnerRow()     ) ||
          ( b1.InnerRow() == b2iV.InnerRow() && 
            b1.OuterRow() < b2iV.OuterRow()     ) ||
          ( b1.InnerRow() == b2iV.InnerRow() && 
            b1.OuterRow() == b2iV.OuterRow() && 
            b1.TrackID() < b2iV.TrackID()       );

        AliHLTTPCCASliceTrackInfo *T1, *T2;
        if(IsNext) {
          T1 = &fTrackInfos[fSliceTrackInfoStart[iSlice1] + b1.TrackID() ];
          T2 = &fTrackInfos[fSliceTrackInfoStart[iSlice2] + b2iV.TrackID() ];
        }
        else {
          T1 = &fTrackInfos[fSliceTrackInfoStart[iSlice2] + b2iV.TrackID()  ];
          T2 = &fTrackInfos[fSliceTrackInfoStart[iSlice1] + b1.TrackID()  ];
        }

          // if current neighbour is better than previus one - save it
        if(T1->ChiNext > min_chi2[iV] && T2->ChiPrev > min_chi2[iV]) // clone was found
        {
            // reestimate end row and index
          if (number == 1) {
              // find end row
            const float x0 = fSliceParam.RowX(b1.OuterRow());
            for (; fSliceParam.RowX(lastIRow) - x0 > sqrt(minL2v[iV]); lastIRow--);
              // find end index
            for(; lastIRow >= firstIRow; lastIRow--)
              if(FirstTrIR[lastIRow] != 50000) {
                ilast2 = FirstTrIR[lastIRow];
                break;
              }
          }
#ifdef BACK_ORDER_FOR_0
          else if (number == 0) {
              // find end row
            const float x0 = fSliceParam.RowX(b1.OuterRow());
            for (; x0 - fSliceParam.RowX(lastIRow) > sqrt(minL2v[iV]); lastIRow++);
              // find end index
            for(; lastIRow <= firstIRow; lastIRow++)
              if(LastTrIR[lastIRow] != 50000) {
                  //           ilast2 = LastTrIR[lastIRow];
                break;
              }
          }
#endif

          bestI2 = b2index[iV];
          bestChi2 = min_chi2[iV];
          bestIsNext = IsNext;
        }
      } // for iV
    } // for i2

    if (bestI2 >= 0) {
      const AliHLTTPCCABorderTrack &b2iV = B2[bestI2];

      AliHLTTPCCASliceTrackInfo *T1, *T2;
      int NextNew, SliceNextNew, PrevNew, SlicePrevNew;

      if(bestIsNext) {
        T1 = &fTrackInfos[fSliceTrackInfoStart[iSlice1] + b1.TrackID() ];
        T2 = &fTrackInfos[fSliceTrackInfoStart[iSlice2] + b2iV.TrackID() ];

        NextNew = b2iV.TrackID();
        SliceNextNew = iSlice2;
        PrevNew = b1.TrackID();
        SlicePrevNew = iSlice1;
      }
      else {
        T1 = &fTrackInfos[fSliceTrackInfoStart[iSlice2] + b2iV.TrackID()  ];
        T2 = &fTrackInfos[fSliceTrackInfoStart[iSlice1] + b1.TrackID()  ];
        
        NextNew = b1.TrackID();
        SliceNextNew = iSlice1;
        PrevNew = b2iV.TrackID();
        SlicePrevNew = iSlice2;
      }

      { // mark neighbours
        if(T1->NextNeighbour() > -1) {
          AliHLTTPCCASliceTrackInfo *T3 = &fTrackInfos[fSliceTrackInfoStart[T1->SliceNextNeighbour()] + T1->NextNeighbour() ];
          T3->SetPrevNeighbour(-2); // mark that T3 lose best neighbour (so there can be other neighbour for T3)
          T3->ChiPrev = 1e10f;
        }
        if(T2->PrevNeighbour() > -1) {
          AliHLTTPCCASliceTrackInfo *T3 = &fTrackInfos[fSliceTrackInfoStart[T2->SlicePrevNeighbour()] + T2->PrevNeighbour() ];
          T3->SetNextNeighbour(-2);
          T3->ChiNext = 1e10f;
        }
        T1->SetNextNeighbour( NextNew );
        T1->SetSliceNextNeighbour( SliceNextNew );
        T1->ChiNext = bestChi2;

        T2->SetPrevNeighbour( PrevNew );
        T2->SetSlicePrevNeighbour( SlicePrevNew );
        T2->ChiPrev = bestChi2;
      }
    } // if bestI2
  } // for i1
}

void AliHLTTPCCAMerger::FindNeighbourTracks(int number)
{
#ifdef USE_TIMERS
  Stopwatch timer;
  timer.Start();
#endif // USE_TIMERS
  
  //* track merging between slices

  fOutput->SetNTracks( 0 );
  fOutput->SetNTrackClusters( 0 );
  fOutput->SetPointers();

  // for each slice set number of the next neighbouring slice
  int nextSlice[fgkNSlices], oppSlice[fgkNSlices/2];

  const unsigned int mid = fgkNSlices / 2 - 1 ;
  const unsigned int last = fgkNSlices - 1 ;

  for ( int iSlice = 0; iSlice < fgkNSlices/2; iSlice++ ) {
    nextSlice[iSlice] = iSlice + 1;
    oppSlice [iSlice] = 22 - iSlice;
    if(iSlice == 11) oppSlice[iSlice]=23;
  }

  for ( int iSlice = fgkNSlices/2; iSlice < fgkNSlices; iSlice++ ) {
    nextSlice[iSlice] = iSlice - 1;
  }

  nextSlice[ mid ] = 0;
  nextSlice[ fgkNSlices/2 ] = last;

  int maxNSliceTracks = 0;
  for ( int iSlice = 0; iSlice < fgkNSlices; iSlice++ ) {
    if ( maxNSliceTracks < fSliceNTrackInfos[iSlice] ) maxNSliceTracks = fSliceNTrackInfos[iSlice];
  }

  AliHLTTPCCABorderTrack *bCurrIR = new AliHLTTPCCABorderTrack[maxNSliceTracks*fgkNSlices];
  AliHLTTPCCABorderTrack *bCurrOR = new AliHLTTPCCABorderTrack[maxNSliceTracks*fgkNSlices];
  unsigned int FirstTrIR[fgkNSlices][AliHLTTPCCAParameters::MaxNumberOfRows8]; // index of the first track on row
  unsigned int LastTrIR[fgkNSlices][AliHLTTPCCAParameters::MaxNumberOfRows8];

    // init arrays with out of range number - 50000
  std::fill(&(FirstTrIR[0][0]), &(FirstTrIR[0][0]) + fgkNSlices*AliHLTTPCCAParameters::MaxNumberOfRows8, 50000);
  std::fill(&(LastTrIR[0][0]),  &(LastTrIR[0][0])  + fgkNSlices*AliHLTTPCCAParameters::MaxNumberOfRows8, 50000);
  
  unsigned int nCurr[fgkNSlices];
  for(unsigned char iSl=0; iSl<fgkNSlices; iSl++)
  {

    nCurr[iSl] = 0;
      // make border tracks for each sector, sort them by inner row
    AliHLTTPCCABorderTrack * const bCurrSliceIR = bCurrIR + maxNSliceTracks*iSl;
    AliHLTTPCCABorderTrack * const bCurrSliceOR = bCurrOR + maxNSliceTracks*iSl;
    MakeBorderTracks(bCurrSliceIR, nCurr[iSl], iSl);
    std::sort(bCurrSliceIR, bCurrSliceIR + nCurr[iSl], CompareInnerRow); // sort such that innerRow decrease

    for(unsigned int itr=0; itr < nCurr[iSl]; itr++)
      bCurrSliceOR[itr] = bCurrSliceIR[itr];
      // std::sort(bCurrOR+maxNSliceTracks*iSl, bCurrOR+maxNSliceTracks*iSl+nCurr[iSl], CompareOuterRow);

      // save track indices range for each row
    if(nCurr[iSl] > 0)
    {
      unsigned char curRow = bCurrSliceIR[0].InnerRow();
      FirstTrIR[iSl][curRow] = 0;
      for(unsigned int itr = 1; itr < nCurr[iSl]; itr++)
      {
        if( bCurrSliceIR[itr].InnerRow() < curRow )
        {
            // for (int iR = bCurrIR[maxNSliceTracks*iSl+itr].InnerRow() + 1; iR < curRow; iR++) // check for a jump
            //   FirstTrIR[iSl][iR] = FirstTrIR[iSl][curRow];
          LastTrIR[iSl][curRow] = itr - 1;
          curRow = bCurrSliceIR[itr].InnerRow();
          FirstTrIR[iSl][curRow] = itr;
        }
      }
      LastTrIR[iSl][curRow] = nCurr[iSl] - 1;
    }

      // create links to neighbour tracks clones in the same sector
    MergeBorderTracks( bCurrSliceOR, nCurr[iSl], iSl,
                       bCurrSliceIR, nCurr[iSl], iSl,
                       number, FirstTrIR[iSl], LastTrIR[iSl] );
  }

  if (number == 1) // with number == 0 only parallel tracks are merged, they should be at the same sector
  if (! fgDoNotMergeBorders )
    for(int iSl=0; iSl<fgkNSlices; iSl++)
    {
        //  create links to neighbour tracks in the next sector in the same xy-plane
      MergeBorderTracks( bCurrOR+maxNSliceTracks*iSl,            nCurr[iSl],            iSl,
                         bCurrIR+maxNSliceTracks*nextSlice[iSl], nCurr[nextSlice[iSl]], nextSlice[iSl],
                         number, FirstTrIR[nextSlice[iSl]], LastTrIR[nextSlice[iSl]] ); // merge upper edges
      MergeBorderTracks( bCurrOR+maxNSliceTracks*nextSlice[iSl], nCurr[nextSlice[iSl]], nextSlice[iSl],
                         bCurrIR+maxNSliceTracks*iSl,            nCurr[iSl],            iSl,
                         number, FirstTrIR[iSl],            LastTrIR[iSl] );            // merge lower edges

      if(iSl < fgkNSlices / 2)
      {
          //  create links to neighbour tracks with the oposit sector (in z direction)
        MergeBorderTracks( bCurrOR+maxNSliceTracks*iSl, nCurr[iSl], iSl, 
                           bCurrIR+maxNSliceTracks*oppSlice[iSl], nCurr[oppSlice[iSl]], oppSlice[iSl],
                           number, FirstTrIR[oppSlice[iSl]], LastTrIR[oppSlice[iSl]] );
        MergeBorderTracks( bCurrOR+maxNSliceTracks*oppSlice[iSl], nCurr[oppSlice[iSl]], oppSlice[iSl], 
                           bCurrIR+maxNSliceTracks*iSl, nCurr[iSl], iSl,
                           number, FirstTrIR[iSl],           LastTrIR[iSl] );
      }
    }

  if ( bCurrIR ) delete[] bCurrIR;
  if ( bCurrOR ) delete[] bCurrOR;

#ifdef USE_TIMERS
  timer.Stop();
  fTimers[3+(1-number)] = timer.RealTime();
#endif // USE_TIMERS
}

void AliHLTTPCCAMerger::Merging(int number)
{
#ifdef USE_TIMERS
  Stopwatch timer;
  timer.Start();
#endif // USE_TIMERS
  
  int nOutTracks = 0;
  int nOutTrackClusters = 0;

  AliHLTTPCCAMergedTrack *outTracks = 0;
  DataCompressor::SliceRowCluster *outClusterIDsrc = 0;
  UChar_t  *outClusterPackedAmp = 0;

  if(number == 0)
  {
    outTracks = new AliHLTTPCCAMergedTrack[fMaxTrackInfos];
    outClusterIDsrc = new DataCompressor::SliceRowCluster[fMaxClusterInfos];
    outClusterPackedAmp = new UChar_t [fMaxClusterInfos];
  }

  AliHLTTPCCAClusterInfo *tmpH = new AliHLTTPCCAClusterInfo[fMaxClusterInfos];
  Vc::vector<AliHLTTPCCASliceTrackInfo> tmpT(fMaxTrackInfos);
  int nEndTracks = 0; // n tracks after merging.
  int tmpSliceTrackInfoStart[fgkNSlices];

  int nTrNew[fgkNSlices] = {0};

  int nH = 0;

///Scalar version at revision 10273

// merge tracks, using obtained links to neighbours
  for ( int iSlice = 0; iSlice < fgkNSlices; iSlice++ ) {

    tmpSliceTrackInfoStart[iSlice] = nEndTracks;
    assert( iSlice == 0 || nEndTracks == tmpSliceTrackInfoStart[iSlice-1] + nTrNew[iSlice-1] );

    const AliHLTTPCCATrackParam *pStartPoint[int_v::Size] = {0};
    const AliHLTTPCCATrackParam *pEndPoint[int_v::Size] = {0};
    float_v StartAlpha, EndAlpha;

    float_v vStartAlpha(Vc::Zero);
    float_v vEndAlpha(Vc::Zero);
    AliHLTTPCCATrackParamVector vStartPoint;
    AliHLTTPCCATrackParamVector vEndPoint;


     // -- Resort tracks to proceed faster
    vector<unsigned int> firstInChainIndex(fSliceNTrackInfos[iSlice]);
    int nChains = 0;

      // store tracks, which are not merged. And save indexes of the most previous(inner) merged tracks
   for(int iT=0; iT< fSliceNTrackInfos[iSlice]; iT++) {
     const int index = fSliceTrackInfoStart[iSlice] + iT;
     const AliHLTTPCCASliceTrackInfo& tr = fTrackInfos[index];

     if(tr.PrevNeighbour() < 0 && tr.NextNeighbour() >= 0) {
       firstInChainIndex[nChains++] = index;
       continue;
     }
     
     if(tr.PrevNeighbour() >= 0 || tr.NextNeighbour() >= 0) continue;
      
      const unsigned int NHits = tr.NClusters();

        // on the final stage stor data to the global tracker
      if(number == 0) {
        outTracks[nOutTracks].AssignTrack(tr, nOutTrackClusters);

        for ( unsigned int i = 0; i < NHits; i++ ) {
          AliHLTTPCCAClusterInfo &clu = fClusterInfos[tr.FirstClusterRef()+i];
          outClusterIDsrc[nOutTrackClusters+i] =
            DataCompressor::SliceRowCluster( clu.ISlice(), clu.IRow(), clu.IClu() );
        }
        nOutTracks++;
        nOutTrackClusters += NHits;
      }
        // else restore tracks, obtained after merging
      if(number == 1)
      {
        AliHLTTPCCASliceTrackInfo &track = tmpT[nEndTracks];

        track = tr;

        track.SetFirstClusterRef( nH );
        track.ChiPrev = 1e10f;
        track.ChiNext = 1e10f;

        for( unsigned int iClu=0; iClu < NHits; iClu++) tmpH[nH + iClu] = fClusterInfos[tr.FirstClusterRef()+iClu];
        nH += NHits;
      }

      nTrNew[iSlice]++;
      nEndTracks++;
    } // if no merged
    
    for ( int itr = 0; ; ) {

        // pack data
      int nVecElements = 0;
      uint_v iIndexes(Vc::Zero);
      for ( ; nVecElements < uint_v::Size && itr < nChains; itr++ ) {
        const unsigned int sI = firstInChainIndex[itr];
        AliHLTTPCCASliceTrackInfo& trackOld = fTrackInfos[sI];
        
          // If track is already used or has previous neighbours - do not use it
        if ( trackOld.Used() ) continue;

          // Store selected track
        pStartPoint[nVecElements] = &trackOld.InnerParam();
        pEndPoint[nVecElements] = &trackOld.OuterParam();
        StartAlpha[nVecElements] = trackOld.InnerAlpha();
        EndAlpha[nVecElements] = trackOld.OuterAlpha();
        iIndexes[nVecElements] = sI;
        nVecElements++;
      }
      if (nVecElements == 0) break;
      
        // Convert their parameters to SIMD vectors
      float_m active = static_cast<float_m>( int_v( Vc::IndexesFromZero ) < nVecElements );

      int hits[2000][uint_v::Size];
      uint_v firstHit(1000u);

      ConvertPTrackParamToVector(pStartPoint,vStartPoint,nVecElements);
      ConvertPTrackParamToVector(pEndPoint,vEndPoint,nVecElements);
      vStartAlpha = (StartAlpha);
      vEndAlpha = (EndAlpha);

      const float_m &invert1 = active && (vEndPoint.X() < vStartPoint.X());
      if( ISUNLIKELY(!(invert1.isEmpty())) )
      {
        AliHLTTPCCATrackParamVector helpPoint = vEndPoint;
        vEndPoint.SetTrackParam(vStartPoint, invert1);
        vStartPoint.SetTrackParam(helpPoint, invert1);
        float_v helpAlpha = vEndAlpha;
        vEndAlpha(invert1) = vStartAlpha;
        vStartAlpha(invert1) = helpAlpha;
      }

      const int_m &activeI = static_cast<int_m>(active);
      const uint_m &activeU = static_cast<uint_m>(active);
      //int_v(Vc::One).scatter( fTrackInfos, &AliHLTTPCCASliceTrackInfo::fUsed, iIndexes, activeI ); // set track as used
      for(int iV=0; iV<int_v::Size; iV++)
      {
        if(!activeI[iV]) continue;
        fTrackInfos[int(iIndexes[iV])].fUsed = 1;
      }
//       uint_v vNHits(fTrackInfos, &AliHLTTPCCASliceTrackInfo::fNClusters         , iIndexes, activeU );
//       vNHits(!active) = Vc::Zero;
      uint_v vNHits(Vc::Zero);
      vNHits(uint_m(activeU)) = fTrackInfos[iIndexes][&AliHLTTPCCASliceTrackInfo::fNClusters];

      //const int_v vFirstClusterRef(fTrackInfos, &AliHLTTPCCASliceTrackInfo::fFirstClusterRef   , iIndexes, activeI );
      int_v vFirstClusterRef(Vc::Zero);
      vFirstClusterRef(activeI) = fTrackInfos[iIndexes][&AliHLTTPCCASliceTrackInfo::fFirstClusterRef];
      
      for ( unsigned int jhit = 0; jhit < vNHits.max(); jhit++ ) {
        const int_m mask = int_m(active) && int_m(jhit < vNHits);
        int_v id = vFirstClusterRef + static_cast<int>(jhit);
        for(int iV=0; iV<int_v::Size; iV++)
        {
          if(!mask[iV]) continue;
//         foreach_bit( int iV, mask ) {
          hits[static_cast <unsigned int>(firstHit[iV])+jhit][iV] = id[iV];
        }
//        id.scatter( hits, static_cast<uint_v>(firstHit + static_cast<unsigned int>(jhit)), mask );
      }
      
      uint_v jIndexes = iIndexes;
      float_m isNeighbour = active;
      while (1) // while there are still outer neighbours
      {
        const int_m &isNeighbourI = static_cast<int_m>(isNeighbour);
        const uint_m &isNeighbourU = static_cast<uint_m>(isNeighbour);
//         int_v vNextNeighbour(       fTrackInfos, &AliHLTTPCCASliceTrackInfo::fNextNeighbour     , jIndexes, isNeighbourI );
//         uint_v vSliceNextNeighbour( fTrackInfos, &AliHLTTPCCASliceTrackInfo::fSliceNextNeighbour, jIndexes, isNeighbourU );
        int_v vNextNeighbour(Vc::Zero);
        vNextNeighbour(isNeighbourI) = fTrackInfos[jIndexes][&AliHLTTPCCASliceTrackInfo::fNextNeighbour];
        uint_v vSliceNextNeighbour(Vc::Zero);
        vSliceNextNeighbour(isNeighbourU) = fTrackInfos[jIndexes][&AliHLTTPCCASliceTrackInfo::fSliceNextNeighbour];
        
        isNeighbour &= (static_cast<float_v>(vNextNeighbour) > -1);
        if (isNeighbour.isEmpty()) break;
        
          // take the next neighbour
        const int_m &isNeighbourI0 = static_cast<int_m>(isNeighbour);
        jIndexes = int_v(fSliceTrackInfoStart, vSliceNextNeighbour, isNeighbourI0) + vNextNeighbour;
//         const int_v vUsed( fTrackInfos, &AliHLTTPCCASliceTrackInfo::fUsed, jIndexes, isNeighbourI0 );
        int_v vUsed(Vc::Zero);
        vUsed(isNeighbourI0) = fTrackInfos[jIndexes][&AliHLTTPCCASliceTrackInfo::fUsed];
        isNeighbour &= !static_cast<float_m>(vUsed > 0);
        
        if ( isNeighbour.isEmpty() ) break;
        
        isNeighbour &= AddNeighbour( jIndexes, nVecElements, isNeighbour,
                                     hits, firstHit, vStartPoint, vEndPoint, vStartAlpha, vEndAlpha, vNHits );
      } // while isNeighbour
  
      const float_m &swap = active && (vEndPoint.X() < vStartPoint.X());
      if( !(swap.isEmpty())) {
        for(int iV=0; iV<float_v::Size; iV++)
        {
          if(!swap[iV]) continue;
//         foreach_bit(int iV, swap) {
          for ( unsigned int i = 0; i < vNHits[iV]; i++ ) hits[i][iV] = hits[firstHit[iV]+vNHits[iV]-1-i][iV];
        }
        firstHit(uint_m(swap)) = uint_v(Vc::Zero);
 
        AliHLTTPCCATrackParamVector helpPoint = vEndPoint;
        vEndPoint.SetTrackParam(vStartPoint, swap);
        vStartPoint.SetTrackParam(helpPoint, swap);
        float_v helpAlpha = vEndAlpha;
        vEndAlpha(swap) = vStartAlpha;
        vStartAlpha(swap) = helpAlpha;
      }

        // Refit tracks, which have been merged.
      uint_v nHits = vNHits;
      AliHLTTPCCATrackParamVector vHelpEndPoint = vStartPoint;
      float_v vHelpEndAlpha = vStartAlpha;
      active &= FitTrack( vHelpEndPoint, vHelpEndAlpha, hits, firstHit, nHits, nVecElements,active, 0 );
      AliHLTTPCCATrackParamVector vHelpStartPoint = vHelpEndPoint;
      float_v vHelpStartAlpha = vHelpEndAlpha;
      active &= FitTrack( vHelpStartPoint, vHelpStartAlpha, hits, firstHit, nHits, nVecElements,active, 1 );
      vNHits = nHits;

        // store tracks
      for(int iV=0; iV<float_v::Size; iV++)
      {
        if(!active[iV]) continue;
//       foreach_bit(int iV, active) {
 
        int h[1000];
        for( unsigned int iClu = 0; iClu < vNHits[iV]; iClu++)
          h[iClu] = hits[iClu + firstHit[iV]][iV];
        
        int *usedHits = h; // get begin of array
          // If track has been merged, resort hits, rid of double hits.
          //std::sort( usedHits, usedHits + vNHits[iV], TrackHitsCompare(fClusterInfos) ); // sort hits by X (iRow) // TODO normal sort
        
          // rid of double hits
        unsigned int ihit2 = 0; // ihit in the output array
        for( unsigned int ihit = 1; ihit < vNHits[iV]; ihit++)
        {
          if( ISUNLIKELY(usedHits[ihit2] == usedHits[ihit]) ) continue;
          ++ihit2;
          if( ISUNLIKELY(     ihit2  != ihit      ) )
            usedHits[ihit2] = usedHits[ihit];
        }
        nHits[iV] = ihit2+1;

          // on the final stage stor data to the global tracker
        if(number == 0)
        {
          AliHLTTPCCAMergedTrack &mergedTrack = outTracks[nOutTracks];
          mergedTrack.SetNClusters( nHits[iV] );
          mergedTrack.SetFirstClusterRef( nOutTrackClusters );
          mergedTrack.SetInnerParam( AliHLTTPCCATrackParam( vHelpStartPoint, iV ) );
          mergedTrack.SetInnerAlpha( vHelpStartAlpha[iV] );
          mergedTrack.SetOuterParam( AliHLTTPCCATrackParam( vHelpEndPoint, iV ) );
          mergedTrack.SetOuterAlpha( vHelpEndAlpha[iV] );

          for ( unsigned int i = 0; i < nHits[iV]; i++ ) {
            AliHLTTPCCAClusterInfo &clu = fClusterInfos[usedHits[i]];
            outClusterIDsrc[nOutTrackClusters+i] =
              DataCompressor::SliceRowCluster( clu.ISlice(), clu.IRow(), clu.IClu() );
          }
          nOutTracks++;
          nOutTrackClusters += nHits[iV];
        }
          // else restore tracks, obtained after merging
        if(number == 1)
        {
          AliHLTTPCCASliceTrackInfo &track = tmpT[nEndTracks];

          track = fTrackInfos[int(iIndexes[iV])];//*vTrackOld[iV];

          track.SetFirstClusterRef( nH );
          track.SetNClusters( nHits[iV] );
          track.SetUsed(0);
          track.SetPrevNeighbour(-1);
          track.SetNextNeighbour(-1);
          track.SetSlicePrevNeighbour(-1);
          track.SetSliceNextNeighbour(-1);
          track.ChiPrev = 10000000;
          track.ChiNext = 10000000;

          track.SetInnerParam( AliHLTTPCCATrackParam( vHelpStartPoint, iV ) );
          track.SetInnerAlpha( vHelpStartAlpha[iV] );
          track.SetOuterParam( AliHLTTPCCATrackParam( vHelpEndPoint, iV ) );
          track.SetOuterAlpha( vHelpEndAlpha[iV] );

          track.fInnerRow = (fClusterInfos[usedHits[0]]).IRow();
          track.fOuterRow = (fClusterInfos[usedHits[nHits[iV]-1]]).IRow();

          for( unsigned int iClu=0; iClu < nHits[iV]; iClu++)
            tmpH[nH + iClu] = fClusterInfos[usedHits[iClu]];
          
          nH += nHits[iV];
        }

        nTrNew[iSlice]++;
        nEndTracks++;
      } // for iV

    } // for itr
  } // for iSlice

  if (fClusterInfos) delete[] fClusterInfos;
  fClusterInfos = tmpH;

  fTrackInfos = tmpT;
  for(int iSlice=0; iSlice < fgkNSlices; iSlice++ )
  {
    fSliceNTrackInfos[iSlice] = nTrNew[iSlice];
    fSliceTrackInfoStart[iSlice] = tmpSliceTrackInfoStart[iSlice];
  }

  if(number == 0)
  {
    fOutput->SetNTracks( nOutTracks );
    fOutput->SetNTrackClusters( nOutTrackClusters );
    fOutput->SetPointers();

    for ( int itr = 0; itr < nOutTracks; itr++ ) fOutput->SetTrack( itr, outTracks[itr] );

    for ( int ic = 0; ic < nOutTrackClusters; ic++ ) {
      fOutput->SetClusterIDsrc( ic, outClusterIDsrc[ic] );
      fOutput->SetClusterPackedAmp( ic, outClusterPackedAmp[ic] );
    }

    if (outTracks) delete[] outTracks;
    if (outClusterIDsrc) delete[] outClusterIDsrc;
    if (outClusterPackedAmp) delete[] outClusterPackedAmp;
  }

#ifdef USE_TIMERS
  timer.Stop();
  fTimers[5+(1-number)] = timer.RealTime();
#endif // USE_TIMERS 
}

float_m AliHLTTPCCAMerger::AddNeighbour( const uint_v& jIndexes, const int& nVecElements, const float_m& isNeighbour,
int hits[2000][uint_v::Size], uint_v& firstHit, AliHLTTPCCATrackParamVector& vStartPoint, AliHLTTPCCATrackParamVector& vEndPoint, float_v& vStartAlpha, float_v& vEndAlpha, uint_v& vNHits )
{
  float_m mask = isNeighbour;

  const int_m &maskI = static_cast<int_m>(mask);
  const uint_m &maskU = static_cast<uint_m>(mask);
//   uint_v jNHits(
//     fTrackInfos, &AliHLTTPCCASliceTrackInfo::fNClusters         , jIndexes, maskU );
//   jNHits(!mask) = Vc::Zero;
//   const float_v vInnerAlpha(
//     fTrackInfos, &AliHLTTPCCASliceTrackInfo::fInnerAlpha        , jIndexes, mask );
//   const float_v vOuterAlpha(
//     fTrackInfos, &AliHLTTPCCASliceTrackInfo::fOuterAlpha        , jIndexes, mask );
  uint_v jNHits(Vc::Zero);
  jNHits(maskU) = fTrackInfos[jIndexes][&AliHLTTPCCASliceTrackInfo::fNClusters];
  float_v vInnerAlpha(Vc::Zero);
  vInnerAlpha(mask) = fTrackInfos[jIndexes][&AliHLTTPCCASliceTrackInfo::fInnerAlpha];
  float_v vOuterAlpha(Vc::Zero);
  vOuterAlpha(mask) = fTrackInfos[jIndexes][&AliHLTTPCCASliceTrackInfo::fOuterAlpha];
  
  AliHLTTPCCATrackParamVector vInnerParam;
  AliHLTTPCCATrackParamVector vOuterParam;

  const AliHLTTPCCATrackParam *pInnerParam[int_v::Size] = {0};
  const AliHLTTPCCATrackParam *pOuterParam[int_v::Size] = {0};
  for(int iV=0; iV<float_v::Size; iV++)
  {
    if(!mask[iV]) continue;
//   foreach_bit(int iV, mask) {
    AliHLTTPCCASliceTrackInfo &segment = fTrackInfos[jIndexes[iV]];
    pInnerParam[iV] = &segment.InnerParam();
    pOuterParam[iV] = &segment.OuterParam();
    
    fTrackInfos[jIndexes[iV]].fUsed = 1;
  }
  ConvertPTrackParamToVector(pInnerParam,vInnerParam,nVecElements);
  ConvertPTrackParamToVector(pOuterParam,vOuterParam,nVecElements);

//   int_v(Vc::One).scatter(fTrackInfos, &AliHLTTPCCASliceTrackInfo::fUsed, jIndexes, maskI );
  float_m dir(false);
  uint_v startHit = firstHit + vNHits;
  const float_v d00 = vStartPoint.GetDistXZ2( vInnerParam );
  const float_v d01 = vStartPoint.GetDistXZ2( vOuterParam );
  const float_v d10 = vEndPoint.GetDistXZ2( vInnerParam );
  const float_v d11 = vEndPoint.GetDistXZ2( vOuterParam );
  const float_v dz00 = CAMath::Abs( vStartPoint.Z() - vInnerParam.Z() );
  const float_v dz01 = CAMath::Abs( vStartPoint.Z() - vOuterParam.Z() );
  const float_v dz10 = CAMath::Abs( vEndPoint.Z() - vInnerParam.Z() );
  const float_v dz11 = CAMath::Abs( vEndPoint.Z() - vOuterParam.Z() );

  const float_m &case1 = mask &&
    (d00 <= d01 && d00 <= d10 && d00 <= d11) &&
    (dz11 >= dz00 && dz11 >= dz01 && dz11 >= dz10 );
    /*                  0----1
     * connection like : \
     *                    0------1
     */
  const float_m &case2 = mask &&
    (d01 < d00 && d01 <= d10 && d01 <= d11) &&
    (dz10 > dz00 && dz10 >= dz01 && dz10 >= dz11 );
    /*                       0----1
     * connection like :      \
     *                    0----1
     */
  const float_m &case3 = mask &&
    (d10 < d00 && d10 <= d01 && d10 <= d11) &&
    (dz01 > dz00 && dz01 >= dz10 && dz01 >= dz11 );
    /*                   0---1
     * connection like :    /
     *                     0-------1
     */
  const float_m &case4 = mask &&
    (d11 < d00 && d11 <= d10 && d10 <= d01) &&
    (dz00 > dz01 && dz00 >= dz10 && dz00 > dz11);
    /*                      0--1
     * connection like :        \
     *                    0------1
     */
  mask &= case1 || case2 || case3 || case4;
          
  if(!(case1.isEmpty()))
  {
    vStartPoint.SetTrackParam( vOuterParam, case1);
    vStartAlpha(case1) = vOuterAlpha;
    firstHit(static_cast<uint_m>(case1)) -= jNHits;
    startHit(static_cast<uint_m>(case1)) = firstHit;
  }
  if(!(case2.isEmpty()))
  {
    vStartPoint.SetTrackParam( vInnerParam, case2);
    vStartAlpha(case2) = vInnerAlpha;
    firstHit(static_cast<uint_m>(case2)) -= jNHits;
    startHit(static_cast<uint_m>(case2)) = firstHit;
  }
  if(!(case3.isEmpty()))
  {
    vEndPoint.SetTrackParam( vOuterParam, case3);
    vEndAlpha(case3) = vOuterAlpha;
  }
  if(!(case4.isEmpty()))
  {
    vEndPoint.SetTrackParam( vInnerParam, case4);
    vEndAlpha(case4) = vInnerAlpha;
  }

  const float_m &m1 = mask && (vEndPoint.X() < vOuterParam.X());
  vEndPoint.SetTrackParam( vOuterParam, m1);
  vEndAlpha(m1) = vOuterAlpha;

  const float_m &m2 = mask && (vEndPoint.X() < vInnerParam.X());
  vEndPoint.SetTrackParam( vInnerParam, m2);
  vEndAlpha(m2) = vInnerAlpha;

  const float_m &m3 = mask && (vStartPoint.X() > vInnerParam.X());
  vStartPoint.SetTrackParam( vInnerParam, m3 );
  vStartAlpha(m3) = vInnerAlpha;

  const float_m &m4 = mask && (vStartPoint.X() > vOuterParam.X());
  vStartPoint.SetTrackParam( vOuterParam, m4 );
  vStartAlpha(m4) = vOuterAlpha;

  const float_m dir_float = (case1 || case4);
  dir = dir_float;
    // add hits of the neighbour
//   const int_v jFirstClusterRef(fTrackInfos, &AliHLTTPCCASliceTrackInfo::fFirstClusterRef   , jIndexes, mask );
  int_v jFirstClusterRef(Vc::Zero);
  jFirstClusterRef(int_m(mask)) = fTrackInfos[jIndexes][&AliHLTTPCCASliceTrackInfo::fFirstClusterRef];
  
  for(int iV=0; iV<float_v::Size; iV++)
  {
    if(!mask[iV]) continue;
//   foreach_bit(int iV, mask) { // faster outside
    for ( unsigned int jhit = 0; jhit < jNHits[iV]; jhit++ ) {
      const int& id = jFirstClusterRef[iV] + jhit;
      const unsigned int& index = HitIndex( startHit, jNHits, dir[iV], iV, jhit );
      hits[index][iV] = id;
        //hits[static_cast <unsigned int>(startHit[iV])+( dir[iV] ?( static_cast <unsigned int>(jNHits[iV])-1-jhit ) :jhit )][iV] = id;
    }
  }
    // for ( unsigned int jhit = 0; jhit < jNHits.max(); jhit++ ) {
    //   const int_m mask = mask && jhit < jNHits;
    //   int_v id = jFirstClusterRef + static_cast<int>(jhit);
    //   foreach_bit( int iV, mask ) {
    //     hits[static_cast <unsigned int>(startHit[iV])+( dir[iV] ?( static_cast <unsigned int>(jNHits[iV])-1-jhit ) :jhit )][iV] = id[iV]; }}
  vNHits(static_cast<uint_m>(mask)) += jNHits;
  return mask;
}
