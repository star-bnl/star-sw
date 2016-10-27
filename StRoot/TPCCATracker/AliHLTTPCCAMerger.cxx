// $Id: AliHLTTPCCAMerger.cxx,v 1.3 2016/06/21 03:39:54 smirnovd Exp $
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
  short_v fHitIndex; // index of the current hit
  ushort_v fNHits; // n track hits
  TrackParamVector fInnerParam;
  TrackParamVector fOuterParam;
};

struct AliHLTTPCCAMerger::AliHLTTPCCAHitMemory
{
  sfloat_v *XData;         // packed x coordinate of the given (global) hit index
  sfloat_v *YData;         // packed y coordinate of the given (global) hit index
  sfloat_v *ZData;         // packed z coordinate of the given (global) hit index

  ushort_v *ISector;            // sector number
  ushort_v *IRow;              // row number

  ushort_v *IClu;

  unsigned short FirstSectorHit[fgkNSlices];
};*/

int AliHLTTPCCAMerger::fgDoNotMergeBorders = 0;

class AliHLTTPCCAMerger::AliHLTTPCCASliceTrackInfo
{
   public:
    const AliHLTTPCCATrackParam &InnerParam() const { return fInnerParam;      }
    const AliHLTTPCCATrackParam &OuterParam() const { return fOuterParam;      }
    float InnerAlpha() const { return fInnerAlpha;      }
    float OuterAlpha() const { return fOuterAlpha;      }
    unsigned short   NClusters() const { return fNClusters;       } 
    int   FirstClusterRef() const { return fFirstClusterRef; }
    int   PrevNeighbour()  const { return fPrevNeighbour;   }
    int   NextNeighbour()  const { return fNextNeighbour;   }
    unsigned char   SlicePrevNeighbour() const { return fSlicePrevNeighbour;   }
    unsigned char   SliceNextNeighbour() const { return fSliceNextNeighbour;   }
    bool  Used()                            const { return fUsed;            }

    void SetInnerParam( const AliHLTTPCCATrackParam &v ) { fInnerParam = v;      }
    void SetOuterParam( const AliHLTTPCCATrackParam &v ) { fOuterParam = v;      }
    void SetInnerAlpha( float v )                      { fInnerAlpha = v;      }
    void SetOuterAlpha( float v )                      { fOuterAlpha = v;      }
    void SetNClusters ( unsigned short v )                        { fNClusters = v;       }
    void SetFirstClusterRef( int v )                   { fFirstClusterRef = v; }
    void SetPrevNeighbour( int v )                     { fPrevNeighbour = v;   }
    void SetNextNeighbour( int v )                     { fNextNeighbour = v;   }
    void SetSlicePrevNeighbour( unsigned char v )                     { fSlicePrevNeighbour = v;   }
    void SetSliceNextNeighbour( unsigned char v )                     { fSliceNextNeighbour = v;   }
    void SetUsed( bool v )                             { fUsed = v;            }
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
    int orig_track_id;
    unsigned char fSlice;
    int number;
#endif // DO_TPCCATRACKER_EFF_PERFORMANCE

   public:

    float ChiPrev; //characteristic of the link quality to the inner neighbour (for overlaped tracks it is chi2, for not overlaped - distance between tracks)
    float ChiNext; //characteristic of the link quality to the outer neighbour
    unsigned char fInnerRow; // number of the inner row of the track
    unsigned char fOuterRow; // number of the outer row of the track

   private:

    AliHLTTPCCATrackParam fInnerParam; // Parameters of the track at the inner point
    AliHLTTPCCATrackParam fOuterParam; // Parameters of the track at the outer point
    float fInnerAlpha;               // The angle of the sector, where inner parameters are
    float fOuterAlpha;               // The angle of the sector, where outers parameters are
    unsigned short fNClusters;  //Clusters number of the track
    int fFirstClusterRef;  // index of the first track cluster in the global cluster array (AliHLTTPCCAMerger::fClusterInfos)
    int fPrevNeighbour; // The number of the inner (previous) neighbour in the tracks array (AliHLTTPCCAMerger::fTrackInfos)
    int fNextNeighbour; // The number of the outer (previous) neighbour in the tracks array (AliHLTTPCCAMerger::fTrackInfos)
    unsigned char fSlicePrevNeighbour; // The number of the sector, which contains inner neighbour
    unsigned char fSliceNextNeighbour; // The number of the sector, which contains outer neighbour
    bool fUsed;            // is the slice track already merged (=1 -> merged, 0 -> not merged)
};


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
  if ( fTrackInfos ) delete[] fTrackInfos;
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
  
#ifdef USE_TIMERS
  timer.Start();
#endif // USE_TIMERS

// 2) merge nonoverlaping tracks
  Merging(1);

#ifdef USE_TIMERS
  timer.Stop();
  fTimers[1] = timer.RealTime();

// 3) merge overlaping tracks, store the tracks to the global tracker
  timer.Start();
#endif // USE_TIMERS
  
  Merging(0);
  
#ifdef USE_TIMERS
  timer.Stop();
  fTimers[2] = timer.RealTime();
#endif // USE_TIMERS
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
      if ( fTrackInfos ) delete[] fTrackInfos;
      fMaxTrackInfos = ( int ) ( nTracksTotal );
      fTrackInfos = new AliHLTTPCCASliceTrackInfo[fMaxTrackInfos];
    }

//    if ( nTrackClustersTotal > fMaxClusterInfos || ( fMaxClusterInfos > 1000 && nTrackClustersTotal < 0.5*fMaxClusterInfos ) ) 
    {
      if ( fClusterInfos ) delete[] fClusterInfos;
      fMaxClusterInfos = ( int ) ( nTrackClustersTotal );
      fClusterInfos = new AliHLTTPCCAClusterInfo [fMaxClusterInfos];
    }

    if ( fOutput ) delete[] ( ( char* )( fOutput ) );
    int size = AliHLTTPCCAMergerOutput::EstimateSize( nTracksTotal, nTrackClustersTotal );
    fOutput = ( AliHLTTPCCAMergerOutput* )( new float2[size/sizeof( float2 )+1] );
  }

///FOR SIMD start
/*  if ( fHitMemory.XData ) delete[] fHitMemory.XData;
  fHitMemory.XData = new float[nTracksTotal];
  if ( fHitMemory.YData ) delete[] fHitMemory.YData;
  fHitMemory.YData = new float[nTracksTotal];
  if ( fHitMemory.ZData ) delete[] fHitMemory.ZData;
  fHitMemory.ZData = new float[nTracksTotal];

  if ( fHitMemory.ISector ) delete[] fHitMemory.ISector;
  fHitMemory.ISector = new unsigned char[nTracksTotal];
  if ( fHitMemory.IRow ) delete[] fHitMemory.IRow;
  fHitMemory.IRow = new unsigned char[nTracksTotal];
  if ( fHitMemory.IClu ) delete[] fHitMemory.IClu;
  fHitMemory.IClu = new short[nTracksTotal];

  for ( int iSec = 0; iSec < fgkNSlices; iSec++ ) {
    if(iSec == 0) fHitMemory.FirstSectorHit[iSec] = 0;
    if(iSec >  0) fHitMemory.FirstSectorHit[iSec] = fkSlices[iSec-1]->NTrackClusters();
    const AliHLTTPCCASliceOutput &slice = *( fkSlices[iSlice] );

    for(int iHit=0; iHit < fkSlices[iSec-1]->NTrackClusters(); iHit++)
    {
      fHitMemory.ISector = iSec;
      fHitMemory.IRow    = slice.ClusterIDrc( iHit ).Row();
      fHitMemory.IClu    = slice.ClusterIDrc( iHit ).Cluster();
      const float2 &yz = slice.ClusterUnpackedYZ(iHit);
      fHitMemory.XData = slice.ClusterUnpackedX(iHit);
      fHitMemory.YData = yz.x;
      fHitMemory.ZData = yz.y;
    }
  }*/
///FOR SIMD end

  // unpack track and cluster information

  int nTracksCurrent = 0;
  int nClustersCurrent = 0;
    // TODO !t0.NDF()
  for ( int iSlice = 0; iSlice < fgkNSlices; iSlice++ ) {

    fSliceTrackInfoStart[ iSlice ] = nTracksCurrent;
    fSliceNTrackInfos[ iSlice ] = 0;

    if ( !fkSlices[iSlice] ) continue;

    const AliHLTTPCCASliceOutput &slice = *( fkSlices[iSlice] );

    for ( int itr = 0; itr < slice.NTracks(); itr += ushort_v::Size ) {

      int nTracksVector = ushort_v::Size;
      if(slice.NTracks() - itr < ushort_v::Size )
        nTracksVector = slice.NTracks() - itr;

      sfloat_v::Memory startAlpha;
      sfloat_v::Memory endAlpha;

      int hits[1000][ushort_v::Size];
      ushort_v::Memory nHits;
      AliHLTTPCCATrackParam startPoint[ushort_v::Size];
      AliHLTTPCCATrackParam endPoint[ushort_v::Size];
      unsigned short nCluNew = 0;

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

        startPoint[iV] = sTrack.Param();
        endPoint[iV]   = startPoint[iV];
        startAlpha[iV] = slices[iSlice]->Param().Alpha();
        endAlpha[iV]   = startAlpha[iV];
      }

//when we turn off the extrapolation step in the tracklet constructor, we have parameters in the last point, not in the first!
//that's why the fitting direction should be changed
      sfloat_m fitted = sfloat_m(true);
      fitted &= static_cast<sfloat_m>(static_cast<ushort_v>(nHits) >= 3);
      //?      assert( fitted );
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
// refit the track
      AliHLTTPCCATrackParamVector vStartPoint;
      AliHLTTPCCATrackParamVector vEndPoint;
      ConvertTrackParamToVector(startPoint,vStartPoint,nTracksVector);
      sfloat_v vStartAlpha(Vc::Zero);
      vStartAlpha.load(startAlpha);
      sfloat_v vEndAlpha(Vc::Zero);

      fitted &= static_cast<sfloat_m>( ushort_v( Vc::IndexesFromZero ) < nTracksVector );

      ushort_v firstHits(Vc::Zero);

      vEndPoint = vStartPoint;
      vEndAlpha = vStartAlpha;
//refit in the forward direction: going from the first hit to the last, mask "fitted" marks with 0 tracks, which are not fitted correctly
      fitted &= FitTrack( vEndPoint, vEndAlpha, hits, firstHits, nHits, nTracksVector,fitted, 0 );
// if chi2 per degree of freedom > 3. sigma - mark track with 0
      fitted &= vEndPoint.Chi2()/static_cast<sfloat_v>(vEndPoint.NDF()) < 9.f;

      vStartPoint = vEndPoint;
      vStartAlpha = vEndAlpha;
//refit in the backward direction: going from the last hit to the first
      fitted &= FitTrack( vStartPoint, vStartAlpha, hits, firstHits, nHits, nTracksVector,fitted, 1 );
// if chi2 per degree of freedom > 3. sigma - mark track with 0
      fitted &= vStartPoint.Chi2()/static_cast<sfloat_v>(vStartPoint.NDF()) < 9.f;

      for(int iV=0; iV < nTracksVector; iV++)
      {
        if ( !fitted[iV] ) continue;
        startPoint[iV] = AliHLTTPCCATrackParam( vStartPoint, iV );
        endPoint[iV] = AliHLTTPCCATrackParam( vEndPoint, iV );
      }
      vStartAlpha.store(&(startAlpha[0])); // work around .store(startAlpha)
      vEndAlpha.store(&(endAlpha[0]));
#endif
      for(int iV=0; iV < nTracksVector; iV++)
      {
        const AliHLTTPCCASliceTrack &sTrack = slice.Track( itr + iV );
        if ( !(fitted[iV]) ) continue;
        if ( nHits[iV] < AliHLTTPCCAParameters::MinTrackPurity*sTrack.NClusters() ) continue;
// if the track fitted correctly store the track
        AliHLTTPCCASliceTrackInfo &track = fTrackInfos[nTracksCurrent];
        
        track.SetInnerParam( startPoint[iV] );
        track.SetInnerAlpha( startAlpha[iV] );
        track.SetOuterParam( endPoint[iV] );
        track.SetOuterAlpha( endAlpha[iV] );
        track.SetFirstClusterRef( nClustersCurrent );
        track.SetNClusters( nHits[iV] );
        track.SetPrevNeighbour( -1 );
        track.SetNextNeighbour( -1 );
        track.SetSlicePrevNeighbour( -1 );
        track.SetSliceNextNeighbour( -1 );
        track.ChiPrev = 10000000;
        track.ChiNext = 10000000;
        track.SetUsed( 0 );
///mvz start
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
        track.orig_track_id = itr+iV;
        track.fSlice = iSlice;
        track.number = nTracksCurrent - fSliceTrackInfoStart[iSlice];
#endif // DO_TPCCATRACKER_EFF_PERFORMANCE
        track.fInnerRow = (fClusterInfos[hits[0][iV]]).IRow();
        track.fOuterRow = (fClusterInfos[hits[nHits[iV]-1][iV]]).IRow();
///mvz end
        for ( int i = 0; i < nHits[iV]; i++ )
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
    //std::cout<<"Unpack slice "<<iSlice<<": ntracks "<<slice.NTracks()<<"/"<<fSliceNTrackInfos[iSlice]<<std::endl;
  }
}

sfloat_m AliHLTTPCCAMerger::FitTrack( AliHLTTPCCATrackParamVector &t, sfloat_v &Alpha0V,
                                      int hits[2000][ushort_v::Size], ushort_v &firstHits,ushort_v::Memory &NTrackHits,
                                      int &nTracksV, sfloat_m active0, bool dir )
{
  // Fit the track
/*#ifdef MAIN_DRAW
      AliHLTTPCCADisplay::Instance().ClearView();
      AliHLTTPCCADisplay::Instance().SetTPCView();
      AliHLTTPCCADisplay::Instance().DrawTPC();
#endif*/
  AliHLTTPCCATrackParamVector::AliHLTTPCCATrackFitParam fitPar;

  AliHLTTPCCATrackLinearisationVector l( t );

  bool first = 1;

  t.CalculateFitParameters( fitPar );

  int hitsNew[AliHLTTPCCAParameters::MaxNumberOfRows8*4][ushort_v::Size]; // 4 - reserve for several turn
  ushort_v::Memory nHitsNew;
  for(int iV=0; iV<ushort_v::Size; iV++)
    nHitsNew[iV] = 0;

  ushort_v nHits(NTrackHits);
  nHits.setZero(static_cast<ushort_m>(!active0));

  int nHitsMax = nHits.max();

  sfloat_m active = active0; // stop on a hit if this hit can't be added
  for ( unsigned short ihit = 0; ihit < nHitsMax; ihit++ ) {

    active &= static_cast<sfloat_m>( ihit < nHits );
    if(active.isEmpty()) continue;
    sfloat_v::Memory xH, yH, zH, sliceAlpha;
    ushort_v::Memory Row;

    for(int iV=0; iV < nTracksV; iV++)
    {
      if( !(active[iV]) ) continue;
      if( ihit > NTrackHits[iV] - 1) continue;
      const int jhit = dir ? ( NTrackHits[iV] - 1 - ihit ) : ihit;
      AliHLTTPCCAClusterInfo &h = fClusterInfos[hits[static_cast <unsigned short>(firstHits[iV]) + jhit][iV]];
      sliceAlpha[iV] =  slices[h.ISlice()]->Param().Alpha();
      xH[iV] = h.X();
      yH[iV] = h.Y();
      zH[iV] = h.Z();
      Row[iV] = h.IRow();
    }

    const sfloat_v xV(xH);
    const sfloat_v yV(yH);
    const sfloat_v zV(zH);
    const sfloat_v sliceAlphaV(sliceAlpha);
    const ushort_v RowV(Row);

    const sfloat_m savedActive = active;
    const sfloat_v rotateA = sliceAlphaV - Alpha0V;
    const sfloat_m &rotated = t.Rotate( rotateA, l, .999f, active);
    active &= rotated;
    const sfloat_v xLast = t.X();

    Alpha0V(active) = sliceAlphaV;
    
    const sfloat_m &transported = t.TransportToXWithMaterial( xV, l, fitPar, fSliceParam.cBz( ), 0.999f, active);
    active &= transported;

    if ( first ) {
      t.SetCov( 0, 10.f, active );
      t.SetCov( 1,  0.f, active );
      t.SetCov( 2, 10.f, active );
      t.SetCov( 3,  0.f, active );
      t.SetCov( 4,  0.f, active );
      t.SetCov( 5,  1.f, active );
      t.SetCov( 6,  0.f, active );
      t.SetCov( 7,  0.f, active );
      t.SetCov( 8,  0.f, active );
      t.SetCov( 9,  1.f, active );
      t.SetCov( 10,  0.f, active );
      t.SetCov( 11,  0.f, active );
      t.SetCov( 12,  0.f, active );
      t.SetCov( 13,  0.f, active );
      t.SetCov( 14,  10.f, active );
      t.SetChi2( 0.f, active);
      t.SetNDF( short_v(-5), static_cast<short_m>(active) );
      t.CalculateFitParameters( fitPar );
    }

    sfloat_v err2Y, err2Z;

    fSliceParam.GetClusterErrors2( RowV, t, &err2Y, &err2Z );
    const sfloat_m &filtered = t.FilterWithMaterial(yV, zV, err2Y, err2Z, 0.999f,active);

    const sfloat_m broken = savedActive && (!rotated || !transported || !filtered);
    if ( !broken.isEmpty() ) {
      t.TransportToXWithMaterial( xLast, l, fitPar, fSliceParam.cBz( ), 0.999f, transported && !filtered ); // transport back if hit can't be added. TODO with out material
      t.Rotate( -rotateA, l, .999f, rotated && !transported );
  }

    active &= filtered;
    if(active.isEmpty()) continue;

    first = 0;
    for(int iV=0; iV < nTracksV; iV++)
    {
      if( !(active[iV]) ) continue;
      const int jhit = dir ? ( NTrackHits[iV] - 1 - ihit ) : ihit;
      hitsNew[nHitsNew[iV]][iV] = hits[static_cast <unsigned short>(firstHits[iV]) + jhit][iV];
      nHitsNew[iV]++;
    }
  }
  t.SetQPt( sfloat_v(1.e-8f), CAMath::Abs( t.QPt() ) < 1.e-8f );

  sfloat_m ok = active0 && ushort_v(nHitsNew) >= 3;

// if track has infinite partameters or covariances, or negative diagonal elements of Cov. matrix, mark it as fitted uncorrectly
  const sfloat_v *c = t.Cov();
  for ( unsigned char i = 0; i < 15; i++ ) ok &= CAMath::Finite( c[i] );
  for ( unsigned char i = 0; i <  5; i++ ) ok &= CAMath::Finite( t.Par()[i] );
///  ok = ok && ( t.GetX() > 50 ); //this check is wrong! X could be < 50, when a sector is rotated!
  ok &= (c[0] > Vc::Zero) && (c[2] > Vc::Zero) && (c[5] > Vc::Zero) && (c[9] > Vc::Zero) && (c[14] > Vc::Zero);
//  ok &= (c[0] < 5.) && (c[2] < 5.) && (c[5] < 2.) && (c[9] < 2.) && (c[14] < 2.);
  ok &= CAMath::Abs( t.SinPhi() ) < .999f;

  t.SetSignCosPhi( sfloat_v(1.f),  ok && static_cast<sfloat_m>(l.CosPhi() >= Vc::Zero) );
  t.SetSignCosPhi( sfloat_v(-1.f), ok && static_cast<sfloat_m>(l.CosPhi() < Vc::Zero) );

  for(int iV=0; iV < nTracksV; iV++)
  {
    if ( !ok[iV] ) continue;
    NTrackHits[iV] = nHitsNew[iV];
    for ( unsigned char i = 0; i < NTrackHits[iV]; i++ ) {
      hits[static_cast <unsigned short>(firstHits[iV]) + (dir ?( NTrackHits[iV]-1-i ) :i)][iV] = hitsNew[i][iV];
    }
  }

  return ok;
}

void AliHLTTPCCAMerger::InvertCholetsky(float a[15])
{
  float d[5], uud, u[5][5];
  for(int i=0; i<5; i++) 
  {
    d[i]=0.f;
    for(int j=0; j<5; j++) 
      u[i][j]=0.;
  }

  for(int i=0; i<5; i++)
  {
    uud=0.;
    for(int j=0; j<i; j++) 
      uud += u[j][i]*u[j][i]*d[j];
    uud = a[i*(i+3)/2] - uud;

    if(fabs(uud)<1.e-12) uud = 1.e-12;
    d[i] = uud/fabs(uud);
    u[i][i] = sqrt(fabs(uud));

    for(int j=i+1; j<5; j++) 
    {
      uud = 0.;
      for(int k=0; k<i; k++)
        uud += u[k][i]*u[k][j]*d[k];
      uud = a[j*(j+1)/2+i] - uud;
      u[i][j] = d[i]/u[i][i]*uud;
    }
  }

  float u1[5];

  for(int i=0; i<5; i++)
  {
    u1[i] = u[i][i];
    u[i][i] = 1.f/u[i][i];
  }
  for(int i=0; i<4; i++)
  {
    u[i][i+1] = - u[i][i+1]*u[i][i]*u[i+1][i+1];
  }
  for(int i=0; i<3; i++)
  {
    u[i][i+2] = u[i][i+1]*u1[i+1]*u[i+1][i+2]-u[i][i+2]*u[i][i]*u[i+2][i+2];
  }
  for(int i=0; i<2; i++)
  {
    u[i][i+3] = u[i][i+2]*u1[i+2]*u[i+2][i+3] - u[i][i+3]*u[i][i]*u[i+3][i+3];
    u[i][i+3] -= u[i][i+1]*u1[i+1]*(u[i+1][i+2]*u1[i+2]*u[i+2][i+3] - u[i+1][i+3]);
  }
  u[0][4] = u[0][2]*u1[2]*u[2][4] - u[0][4]*u[0][0]*u[4][4];
  u[0][4] += u[0][1]*u1[1]*(u[1][4] - u[1][3]*u1[3]*u[3][4] - u[1][2]*u1[2]*u[2][4]);
  u[0][4] += u[3][4]*u1[3]*(u[0][3] - u1[2]*u[2][3]*(u[0][2] - u[0][1]*u1[1]*u[1][2]));

  for(int i=0; i<5; i++)
    a[i+10] = u[i][4]*d[4]*u[4][4];
  for(int i=0; i<4; i++)
    a[i+6] = u[i][3]*u[3][3]*d[3] + u[i][4]*u[3][4]*d[4];
  for(int i=0; i<3; i++)
    a[i+3] = u[i][2]*u[2][2]*d[2] + u[i][3]*u[2][3]*d[3] + u[i][4]*u[2][4]*d[4];
  for(int i=0; i<2; i++)
    a[i+1] = u[i][1]*u[1][1]*d[1] + u[i][2]*u[1][2]*d[2] + u[i][3]*u[1][3]*d[3] + u[i][4]*u[1][4]*d[4];
  a[0] = u[0][0]*u[0][0]*d[0] + u[0][1]*u[0][1]*d[1] + u[0][2]*u[0][2]*d[2] + u[0][3]*u[0][3]*d[3] + u[0][4]*u[0][4]*d[4];
}

void AliHLTTPCCAMerger::MultiplySS(float const C[15], float const V[15], float K[5][5])
{
//multiply 2 symmetric matricies
  K[0][0] = C[0]*V[ 0] + C[1]*V[ 1] + C[3]*V[ 3] + C[6]*V[ 6] + C[10]*V[10];
  K[0][1] = C[0]*V[ 1] + C[1]*V[ 2] + C[3]*V[ 4] + C[6]*V[ 7] + C[10]*V[11];
  K[0][2] = C[0]*V[ 3] + C[1]*V[ 4] + C[3]*V[ 5] + C[6]*V[ 8] + C[10]*V[12];
  K[0][3] = C[0]*V[ 6] + C[1]*V[ 7] + C[3]*V[ 8] + C[6]*V[ 9] + C[10]*V[13];
  K[0][4] = C[0]*V[10] + C[1]*V[11] + C[3]*V[12] + C[6]*V[13] + C[10]*V[14];

  K[1][0] = C[1]*V[ 0] + C[2]*V[ 1] + C[4]*V[ 3] + C[7]*V[ 6] + C[11]*V[10];
  K[1][1] = C[1]*V[ 1] + C[2]*V[ 2] + C[4]*V[ 4] + C[7]*V[ 7] + C[11]*V[11];
  K[1][2] = C[1]*V[ 3] + C[2]*V[ 4] + C[4]*V[ 5] + C[7]*V[ 8] + C[11]*V[12];
  K[1][3] = C[1]*V[ 6] + C[2]*V[ 7] + C[4]*V[ 8] + C[7]*V[ 9] + C[11]*V[13];
  K[1][4] = C[1]*V[10] + C[2]*V[11] + C[4]*V[12] + C[7]*V[13] + C[11]*V[14];

  K[2][0] = C[3]*V[ 0] + C[4]*V[ 1] + C[5]*V[ 3] + C[8]*V[ 6] + C[12]*V[10];
  K[2][1] = C[3]*V[ 1] + C[4]*V[ 2] + C[5]*V[ 4] + C[8]*V[ 7] + C[12]*V[11];
  K[2][2] = C[3]*V[ 3] + C[4]*V[ 4] + C[5]*V[ 5] + C[8]*V[ 8] + C[12]*V[12];
  K[2][3] = C[3]*V[ 6] + C[4]*V[ 7] + C[5]*V[ 8] + C[8]*V[ 9] + C[12]*V[13];
  K[2][4] = C[3]*V[10] + C[4]*V[11] + C[5]*V[12] + C[8]*V[13] + C[12]*V[14];

  K[3][0] = C[6]*V[ 0] + C[7]*V[ 1] + C[8]*V[ 3] + C[9]*V[ 6] + C[13]*V[10];
  K[3][1] = C[6]*V[ 1] + C[7]*V[ 2] + C[8]*V[ 4] + C[9]*V[ 7] + C[13]*V[11];
  K[3][2] = C[6]*V[ 3] + C[7]*V[ 4] + C[8]*V[ 5] + C[9]*V[ 8] + C[13]*V[12];
  K[3][3] = C[6]*V[ 6] + C[7]*V[ 7] + C[8]*V[ 8] + C[9]*V[ 9] + C[13]*V[13];
  K[3][4] = C[6]*V[10] + C[7]*V[11] + C[8]*V[12] + C[9]*V[13] + C[13]*V[14];

  K[4][0] = C[10]*V[ 0] + C[11]*V[ 1] + C[12]*V[ 3] + C[13]*V[ 6] + C[14]*V[10];
  K[4][1] = C[10]*V[ 1] + C[11]*V[ 2] + C[12]*V[ 4] + C[13]*V[ 7] + C[14]*V[11];
  K[4][2] = C[10]*V[ 3] + C[11]*V[ 4] + C[12]*V[ 5] + C[13]*V[ 8] + C[14]*V[12];
  K[4][3] = C[10]*V[ 6] + C[11]*V[ 7] + C[12]*V[ 8] + C[13]*V[ 9] + C[14]*V[13];
  K[4][4] = C[10]*V[10] + C[11]*V[11] + C[12]*V[12] + C[13]*V[13] + C[14]*V[14];
}

void AliHLTTPCCAMerger::MultiplyMS(float const C[5][5], float const V[15], float K[15])
{
//multiply symmetric and nonsymmetric matricies
  K[0] = C[0][0]*V[0] + C[0][1]*V[1] + C[0][2]*V[3] + C[0][3]*V[6] + C[0][4]*V[10];

  K[1] = C[1][0]*V[0] + C[1][1]*V[1] + C[1][2]*V[3] + C[1][3]*V[6] + C[1][4]*V[10];
  K[2] = C[1][0]*V[1] + C[1][1]*V[2] + C[1][2]*V[4] + C[1][3]*V[7] + C[1][4]*V[11];

  K[3] = C[2][0]*V[0] + C[2][1]*V[1] + C[2][2]*V[3] + C[2][3]*V[6] + C[2][4]*V[10];
  K[4] = C[2][0]*V[1] + C[2][1]*V[2] + C[2][2]*V[4] + C[2][3]*V[7] + C[2][4]*V[11];
  K[5] = C[2][0]*V[3] + C[2][1]*V[4] + C[2][2]*V[5] + C[2][3]*V[8] + C[2][4]*V[12];

  K[6] = C[3][0]*V[0] + C[3][1]*V[1] + C[3][2]*V[3] + C[3][3]*V[6] + C[3][4]*V[10];
  K[7] = C[3][0]*V[1] + C[3][1]*V[2] + C[3][2]*V[4] + C[3][3]*V[7] + C[3][4]*V[11];
  K[8] = C[3][0]*V[3] + C[3][1]*V[4] + C[3][2]*V[5] + C[3][3]*V[8] + C[3][4]*V[12];
  K[9] = C[3][0]*V[6] + C[3][1]*V[7] + C[3][2]*V[8] + C[3][3]*V[9] + C[3][4]*V[13];

  K[10] = C[4][0]*V[ 0] + C[4][1]*V[ 1] + C[4][2]*V[ 3] + C[4][3]*V[ 6] + C[4][4]*V[10];
  K[11] = C[4][0]*V[ 1] + C[4][1]*V[ 2] + C[4][2]*V[ 4] + C[4][3]*V[ 7] + C[4][4]*V[11];
  K[12] = C[4][0]*V[ 3] + C[4][1]*V[ 4] + C[4][2]*V[ 5] + C[4][3]*V[ 8] + C[4][4]*V[12];
  K[13] = C[4][0]*V[ 6] + C[4][1]*V[ 7] + C[4][2]*V[ 8] + C[4][3]*V[ 9] + C[4][4]*V[13];
  K[14] = C[4][0]*V[10] + C[4][1]*V[11] + C[4][2]*V[12] + C[4][3]*V[13] + C[4][4]*V[14];
}

void AliHLTTPCCAMerger::MultiplySR(float const C[15], float const r_in[5], float r_out[5])
{
//multiply vector and symmetric matrix
  r_out[0] = r_in[0]*C[ 0] + r_in[1]*C[ 1] + r_in[2]*C[ 3] +r_in[3]*C[ 6] + r_in[4]*C[10];
  r_out[1] = r_in[0]*C[ 1] + r_in[1]*C[ 2] + r_in[2]*C[ 4] +r_in[3]*C[ 7] + r_in[4]*C[11];
  r_out[2] = r_in[0]*C[ 3] + r_in[1]*C[ 4] + r_in[2]*C[ 5] +r_in[3]*C[ 8] + r_in[4]*C[12];
  r_out[3] = r_in[0]*C[ 6] + r_in[1]*C[ 7] + r_in[2]*C[ 8] +r_in[3]*C[ 9] + r_in[4]*C[13];
  r_out[4] = r_in[0]*C[10] + r_in[1]*C[11] + r_in[2]*C[12] +r_in[3]*C[13] + r_in[4]*C[14];
}

void AliHLTTPCCAMerger::FilterTracks(float const r[5], float const C[15], float const m[5], float const V[15], float R[5], float W[15], float &chi2)
{
  float S[15];
  for(int i=0; i<15; i++)
  {
    W[i] = C[i];
    S[i] = C[i] + V[i];
  }
  for(int i=0; i<5; i++)
    R[i] = r[i];

  InvertCholetsky(S);
  
  float K[5][5];
  MultiplySS(C,S,K);
  float dzeta[5];
  for(int i=0; i<5; i++) dzeta[i] = m[i] - r[i];
  float KC[15];
  MultiplyMS(K,C,KC);
  for(int i=0; i< 15; i++)
    W[i] -= KC[i];

  float kd;
  for(int i=0; i<5; i++)
  {
    kd = 0.f;
    for(int j=0; j<5; j++)
      kd += K[i][j]*dzeta[j];
    R[i] += kd;
  }
  float S_dzeta[5];
  MultiplySR(S, dzeta, S_dzeta);
  chi2 = dzeta[0]*S_dzeta[0] + dzeta[1]*S_dzeta[1] + dzeta[2]*S_dzeta[2] + dzeta[3]*S_dzeta[3] + dzeta[4]*S_dzeta[4];
}

void AliHLTTPCCAMerger::MakeBorderTracks(AliHLTTPCCABorderTrack B[], unsigned short &nB, unsigned char &iSlice)
{
  //* prepare slice tracks for merging with next/previous/same sector

  for ( unsigned short itr = 0; itr < fSliceNTrackInfos[iSlice]; itr++ ) 
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
///      SCALAR CODE at revision 10272!
// #define BACK_ORDER_FOR_0 // little bit faster without it. (but why?)
void AliHLTTPCCAMerger::MergeBorderTracks( AliHLTTPCCABorderTrack B1[], int N1, int iSlice1, AliHLTTPCCABorderTrack B2[], int N2, int iSlice2, int number,unsigned short FirstTrIR[],unsigned short LastTrIR[])
{
// The function creates links to the inner and outer neighbours

//if(number == 0) return;
  const float factor2k = 64.f;

  sfloat_v dr_min2_localv(10000000.f);
// Find firts and last row for the nighbours finding. All tracks are sorted by the inner row.
  for ( int i1 = 0; i1 < N1; i1++ ) {

    AliHLTTPCCABorderTrack &b1 = B1[i1];

    int lastIRow = fSliceParam.NRows()-1; // row to end finding of a clone
    int firstIRow = 0;
    int dir = -1; // direction from a start to an end row.
    int ifirst2 = -1;
    int ilast2 = N2;
    
    if(number == 1) { // find tracks with <= 1 common rows.
      dir = -1; // Tracks2 sorted in order of decrease innerRow. For number==1 we'll go in the upper dir, so indices will decrease
      
      firstIRow = b1.OuterRow() + 0; // row to begin finding of clone
      lastIRow = b1.OuterRow() + 11; // TODO choose parameter // todo parameters
      lastIRow = (lastIRow < fSliceParam.NRows()) ? lastIRow : fSliceParam.NRows()-1;
      
        // find start index
      ifirst2 = -1;
      for(; firstIRow < fSliceParam.NRows(); firstIRow++)
        if (LastTrIR[firstIRow] != 50000) {
          ifirst2 = LastTrIR[firstIRow];
          break;
        }
        // find end index
      ilast2 = N2;
      for(; lastIRow >= firstIRow; lastIRow--)
        if(FirstTrIR[lastIRow] != 50000) {
          ilast2 = FirstTrIR[lastIRow];
          break;
        }
      
    // std::cout << b1.OuterRow() << " " << firstIRow <<  " " << lastIRow <<  " " << ifirst2 << " " << ilast2 << std::endl; // dbg
    }
    else if(number == 0) { // find tracks with >= 2 common rows.
#ifdef BACK_ORDER_FOR_0
      dir = 1; // Tracks2 sorted in order of decrease innerRow. For number==0 we'll go in the down dir, so indices will increase
            
      firstIRow = b1.OuterRow() - 1; // row to begin finding of a clone
      lastIRow = b1.OuterRow() - 7; // TODO choose parameter // todo parameters
      if ( firstIRow < 0 ) {
        firstIRow = 0;
        lastIRow = 0;
      }
      else if ( lastIRow < 0 ){
        lastIRow = 0;
      }

        // find start index
      ifirst2 = N2;
      for(; firstIRow >= 0; firstIRow--)
        if (FirstTrIR[firstIRow] != 50000) {
          ifirst2 = FirstTrIR[firstIRow];
          break;
        }
        // find end index
      ilast2 = -1;
      for(; lastIRow <= firstIRow; lastIRow++)
        if(LastTrIR[lastIRow] != 50000) {
          ilast2 = LastTrIR[lastIRow];
          break;
        }
#else // BACK_ORDER_FOR_0
      dir = -1;
      
      firstIRow = b1.OuterRow() - 7; // row to begin finding of a clone
      lastIRow = b1.OuterRow() - 1; // TODO choose parameter // todo parameters
      if ( lastIRow < 0 ) {
        firstIRow = 0;
        lastIRow = 0;
      }
      else if ( firstIRow < 0 ){
        firstIRow = 0;
      }
      
        // find start index
      ifirst2 = -1;
      for(; firstIRow < fSliceParam.NRows(); firstIRow++)
        if (LastTrIR[firstIRow] != 50000) {
          ifirst2 = LastTrIR[firstIRow];
          break;
        }
        // find end index
      ilast2 = N2;
      for(; lastIRow >= firstIRow; lastIRow--)
        if(FirstTrIR[lastIRow] != 50000) {
          ilast2 = FirstTrIR[lastIRow];
          break;
        }
#endif // BACK_ORDER_FOR_0
    }

//std::cout << ifirst2 << "  " << N2 << "  " << N1 << std::endl;
//    ifirst2 = (iSlice1 != iSlice2) ? 0 : i1+1;

    // rarely changed parameters
    const AliHLTTPCCASliceTrackInfo *Tt1 = &fTrackInfos[fSliceTrackInfoStart[iSlice1] + b1.TrackID() ];
    const float k1   = Tt1->InnerParam().QPt() * fSliceParam.cBz();

    sfloat_v maxLv(1e10f);
    sfloat_v maxL2v(1e10f);

    float Tt2OuterAlpha=0;
    float Tt2InnerAlpha=0;
    float xE1_E2=0, xS1_S2=0, xS1_E2=0;
    float yE1_E2=0, yS1_S2=0, yS1_E2=0;
    float sinE1_E2=0, sinS1_S2=0, sinS1_E2=0;

    if (dir*ifirst2 <= dir*ilast2) {
      Tt2OuterAlpha = fTrackInfos[fSliceTrackInfoStart[iSlice2] + B2[ifirst2].TrackID() ].OuterAlpha();
      Tt2InnerAlpha = fTrackInfos[fSliceTrackInfoStart[iSlice2] + B2[ifirst2].TrackID() ].InnerAlpha();
// rotate coordinates to the same coordinate system
      Tt1->OuterParam().RotateXY(Tt2OuterAlpha - Tt1->OuterAlpha(),xE1_E2,yE1_E2,sinE1_E2);
      Tt1->InnerParam().RotateXY(Tt2InnerAlpha - Tt1->InnerAlpha(),xS1_S2,yS1_S2,sinS1_S2);
      Tt1->InnerParam().RotateXY(Tt2OuterAlpha - Tt1->InnerAlpha(),xS1_E2,yS1_E2,sinS1_E2);
    }
// Convert data of the first track to the SIMD vectors. 
    sfloat_v xE1_E2v(xE1_E2), xS1_S2v(xS1_S2), xS1_E2v(xS1_E2);
    sfloat_v yE1_E2v(yE1_E2), yS1_S2v(yS1_S2), yS1_E2v(yS1_E2);
    sfloat_v sinE1_E2v(sinE1_E2), sinS1_S2v(sinS1_S2), sinS1_E2v(sinS1_E2);
    sfloat_v vTt2OuterAlpha(Tt2OuterAlpha), vTt2InnerAlpha(Tt2InnerAlpha);
    int nVecElements = 0;

    ushort_v::Memory b2index;

    AliHLTTPCCATrackParamVector OutParT1,OutParT2,InParT1,InParT2;
    sfloat_v OutAlphaT1( Tt1->OuterAlpha() ), OutAlphaT2, InAlphaT1( Tt1->InnerAlpha() ),InAlphaT2;
    AliHLTTPCCATrackParam T2OuterParamMemory[ushort_v::Size],T2InnerParamMemory[ushort_v::Size];
    sfloat_v::Memory T2InnerAlphaMemory,T2OuterAlphaMemory;

    OutParT1.SetX( sfloat_v( Tt1->OuterParam().X() ) );
    OutParT1.SetSignCosPhi(sfloat_v( Tt1->OuterParam().SignCosPhi() ));
    for(int iP=0; iP<5; iP++) { OutParT1.SetPar(iP,sfloat_v( Tt1->OuterParam().Par()[iP] )); }
    for(int iC=0; iC<15; iC++){ OutParT1.SetCov(iC,sfloat_v( Tt1->OuterParam().Cov()[iC] )); }
    OutParT1.SetChi2(sfloat_v( Tt1->OuterParam().Chi2() ));
    OutParT1.SetNDF( Tt1->OuterParam().NDF() );

    InParT1.SetX(sfloat_v( Tt1->InnerParam().X() ));
    InParT1.SetSignCosPhi(sfloat_v( Tt1->InnerParam().SignCosPhi() ));
    for(int iP=0; iP<5; iP++) { InParT1.SetPar(iP,sfloat_v( Tt1->InnerParam().Par()[iP] )); }
    for(int iC=0; iC<15; iC++){ InParT1.SetCov(iC,sfloat_v( Tt1->InnerParam().Cov()[iC] )); }
    InParT1.SetChi2(sfloat_v( Tt1->InnerParam().Chi2() ));
    InParT1.SetNDF(Tt1->InnerParam().NDF());

    const sfloat_v &yE1v = OutParT1.Y();
    const sfloat_v &xE1v = OutParT1.X();

    for ( int i2 = ifirst2;  dir*i2 <= dir*ilast2; i2 += dir ) {

      AliHLTTPCCABorderTrack &b2 = B2[i2];
      bool Continue = 0;
// if dz/ds of the tracks differs more, than by several sigmas (now 8 is used) - they are not neighbours
      float db2 = (b1.b()) - (b2.b());
      db2 *= db2;
      const float ddb2 = b1.bErr2() + b2.bErr2();
      if( db2 > factor2k * ddb2 )
      {
        if(dir*i2 < dir*ilast2) continue;
        else Continue = 1;
      }
// if q/pt of the tracks differs more, than by several sigmas (now 8 is used) - they are not neighbours
      float dp2 = (b1.p()) - (b2.p());
      dp2 *= dp2;
      const float ddp2 = b1.pErr2() + b2.pErr2();
      if( dp2 > factor2k * ddp2 )
      {
        if(dir*i2 < dir*ilast2) continue;
        else Continue = 1;
      }
// the track could not be itselfs neighbour
      if( ISUNLIKELY( iSlice1 == iSlice2 && b1.TrackID() == b2.TrackID() ) )
      {
        if(dir*i2 < dir*ilast2) continue;
        else Continue = 1;
      }

      AliHLTTPCCASliceTrackInfo *Tt2 = &fTrackInfos[fSliceTrackInfoStart[iSlice2] + b2.TrackID() ];

      if(!Continue){
// store tracks, which passed previous cuts
        T2InnerParamMemory[nVecElements] = Tt2->InnerParam();
        T2OuterParamMemory[nVecElements] = Tt2->OuterParam();
        T2InnerAlphaMemory[nVecElements] = Tt2->InnerAlpha();
        T2OuterAlphaMemory[nVecElements] = Tt2->OuterAlpha();
        b2index[nVecElements] = i2;
        nVecElements++;
      }
      if(nVecElements == ushort_v::Size || (dir*i2 == dir*ilast2 && nVecElements > 0))
      {
// convert parameters to SIMD vectors for the array of second tracks
        sfloat_m active = static_cast<sfloat_m>( ushort_v( Vc::IndexesFromZero ) < nVecElements );
//std::cout << "1  "<<active<<std::endl;
        ConvertTrackParamToVector(T2InnerParamMemory,InParT2,nVecElements);
        ConvertTrackParamToVector(T2OuterParamMemory,OutParT2,nVecElements);

        OutAlphaT2.load(T2OuterAlphaMemory);
        InAlphaT2.load(T2InnerAlphaMemory);

        const sfloat_v &yE2v = OutParT2.Y();
        const sfloat_v &yS2v = InParT2.Y();

        const sfloat_v &xE2v = OutParT2.X();
        const sfloat_v &xS2v = InParT2.X();

        sfloat_v xS2_E1v(Vc::Zero);
        sfloat_v yS2_E1v(Vc::Zero);
        sfloat_v sinS2_E1v(Vc::Zero);
        InParT2.RotateXY(OutAlphaT1 - InAlphaT2,xS2_E1v,yS2_E1v,sinS2_E1v);

        const sfloat_m &rotate = active && (CAMath::Abs(vTt2OuterAlpha - OutAlphaT2) > 0.01f || CAMath::Abs(vTt2InnerAlpha - InAlphaT2)>0.01f);

        if(!(rotate.isEmpty()))
        {// recalculate values if they could change
          vTt2OuterAlpha(rotate) = OutAlphaT2;
          vTt2InnerAlpha(rotate) = InAlphaT2;

          OutParT1.RotateXY(vTt2OuterAlpha - OutAlphaT1,xE1_E2v,yE1_E2v,sinE1_E2v,rotate);
          InParT1.RotateXY(vTt2InnerAlpha - InAlphaT1,xS1_S2v,yS1_S2v,sinS1_S2v,rotate);
          InParT1.RotateXY(vTt2OuterAlpha - InAlphaT1,xS1_E2v,yS1_E2v,sinS1_E2v,rotate);
        }

        const sfloat_v dxArr[4] = {(xE1_E2v - xE2v),
                                   (xS1_S2v - xS2v), 
                                   (xS1_E2v - xE2v), 
                                   (xS2_E1v - xE1v)};
        const sfloat_v dyArr[4] = {(yE1_E2v - yE2v), 
                                   (yS1_S2v - yS2v), 
                                   (yS1_E2v - yE2v), 
                                   (yS2_E1v - yE1v)};

        for(int iCycl=0; iCycl<1; iCycl++)
        {
          const sfloat_v dx_1 = xE2v - xS1_E2v;
          const sfloat_v t_sin1 = k1 * dx_1 + sinS1_E2v;
// check, whether inner parameters of both tracks in the pair could be transported to the outer parameters of the other track in the pair
          const sfloat_v k2   = InParT2.QPt() * fSliceParam.cBz();
          const sfloat_v dx_2 = xE1v - xS2_E1v;
          const sfloat_v t_sin2 = k2 * dx_2 + sinS2_E1v;
          active &= CAMath::Abs( t_sin1 ) <= 0.999f && CAMath::Abs(t_sin2) <= 0.999f;
          if(active.isEmpty()) continue;
// find the closest pair of the tracks parameters and store them to Thelp and Thelp1
          const sfloat_v dxyArr[4] = {dxArr[0]*dxArr[0] + dyArr[0]*dyArr[0],
                                      dxArr[1]*dxArr[1] + dyArr[1]*dyArr[1], 
                                      dxArr[2]*dxArr[2] + dyArr[2]*dyArr[2], 
                                      dxArr[3]*dxArr[3] + dyArr[3]*dyArr[3]};

          short_v idx(Vc::Zero), idx_temp1(Vc::Zero), idx_temp2((short int)2);
          sfloat_v dx = CAMath::Abs(dxyArr[0]);
          sfloat_v dx_temp1 = CAMath::Abs(dxyArr[0]), dx_temp2 = CAMath::Abs(dxyArr[2]);
          const sfloat_m &tmp1mask = dxyArr[1] < dx_temp1;
          const sfloat_m &tmp2mask = dxyArr[3] < dx_temp2;
          dx_temp1(tmp1mask) = dxyArr[1]; 
          idx_temp1(static_cast<short_m>(tmp1mask)) = 1;
          dx_temp2(tmp2mask) = dxyArr[3];
          idx_temp2(static_cast<short_m>(tmp2mask)) = 3;

          if(number == 1) 
            { dx = dx_temp2; idx = idx_temp2; }
          else if (number == 0)
          {
            const sfloat_m &Isdx2 = dx_temp2 < dx_temp1;
            const short_m &Isdx2_short = static_cast<short_m>(Isdx2);

            dx(Isdx2) = dx_temp2;
            idx(Isdx2_short) = idx_temp2;
            dx(!Isdx2) = dx_temp1;
            idx(!Isdx2_short) = idx_temp1;
          }

          AliHLTTPCCATrackParamVector Thelp, Thelp1;
          sfloat_v a1(Vc::Zero);
          sfloat_v a2(Vc::Zero);

          if(number == 0)
          {
            const sfloat_m &IsIdx0 = active && CAMath::Abs(static_cast<sfloat_v>(idx) - 0.f) < 0.1f;
            const sfloat_m &IsIdx1 = active && CAMath::Abs(static_cast<sfloat_v>(idx) - 1.f) < 0.1f;
            Thelp.SetTrackParam(OutParT1, IsIdx0 );
            Thelp1.SetTrackParam(OutParT2, IsIdx0 );
            a1( IsIdx0 ) = OutAlphaT1;
            a2( IsIdx0 ) = OutAlphaT2;
            dx( IsIdx0 ) = dxArr[0];
            Thelp.SetTrackParam(InParT1, IsIdx1 );
            Thelp1.SetTrackParam(InParT2, IsIdx1 );
            a1( IsIdx1 ) = InAlphaT1;
            a2( IsIdx1 ) = InAlphaT2;
            dx( IsIdx1 ) = dxArr[1];
          }
          const sfloat_m &IsIdx2 = active && CAMath::Abs(static_cast<sfloat_v>(idx) - 2.f) < 0.1f;
          const sfloat_m &IsIdx3 = active && CAMath::Abs(static_cast<sfloat_v>(idx) - 3.f) < 0.1f;
          Thelp.SetTrackParam(InParT1, IsIdx2 );
          Thelp1.SetTrackParam(OutParT2, IsIdx2 );
          a1( IsIdx2 ) = InAlphaT1;
          a2( IsIdx2 ) = OutAlphaT2;
          dx( IsIdx2 ) = dxArr[2];

          Thelp.SetTrackParam(OutParT1, IsIdx3 );
          Thelp1.SetTrackParam(InParT2, IsIdx3 );
          a1( IsIdx3 ) = OutAlphaT1;
          a2( IsIdx3 ) = InAlphaT2;
          dx( IsIdx3 ) = dxArr[3];
 // on the first iteration we process only overlaped tracks
          if(number == 1)  active &= (CAMath::Abs(dx)<0.1f || dx > 0.f);
// on second - only nonoverlaped
          if(number == 0)  active &= (CAMath::Abs(dx)<0.1f || dx < 0.f);
          if(active.isEmpty()) continue;
// rotate track parameters to the same coordinate system
          active &= Thelp.Rotate( a2 - a1 , 0.999f, active);
          if(active.isEmpty()) continue;
// calculate distance between tracks
          dr_min2_localv = (Thelp.Y()-Thelp1.Y())*(Thelp.Y()-Thelp1.Y()) + (Thelp.Z()-Thelp1.Z())*(Thelp.Z()-Thelp1.Z()) + dx*dx;

          if ( number == 1 ) active &= (dr_min2_localv <= maxLv*maxLv);
          if(active.isEmpty()) continue;
          maxL2v(active) = dr_min2_localv; // save dist^2 for lastIRow calculation
// transport tracks to the middle point by X coordinate
          const sfloat_v &ToX = 0.5f * (Thelp.X() + Thelp1.X());
          active &= Thelp.TransportToX( ToX, sfloat_v(fSliceParam.cBz()), 0.999f, active);
          if(active.isEmpty()) continue;
          active &= Thelp1.TransportToX( ToX, sfloat_v(fSliceParam.cBz()), 0.999f, active);
          if(active.isEmpty()) continue;
// if tracks are too far one from each other after propagation - they could not be neighbours
          active &= CAMath::Abs(Thelp1.Y() - Thelp.Y()) <= 10.f;
          active &= CAMath::Abs(Thelp1.Z() - Thelp.Z()) <= 10.f;
          active &= CAMath::Abs(Thelp1.SinPhi() - Thelp.SinPhi()) <= 0.15f;
          if(active.isEmpty()) continue;
// merge parameters and calculate chi2
          sfloat_v C[15], r[5], chi2(Vc::Zero);
          FilterTracks(Thelp.GetPar(), Thelp.GetCov(), Thelp1.GetPar(), Thelp1.GetCov(), r, C, chi2, active);
// if after merging parameters are infinite, or diagonal elements of cov. matrix are negative - tracks could not be neighbours
          for ( int i = 0; i < 15; i++ ) active &= CAMath::Finite( C[i] );
          for ( int i = 0; i < 5; i++ ) active &= CAMath::Finite( r[i] );
          active &= ( C[0] > 0 ) && ( C[2] > 0 ) && ( C[5] > 0 ) && ( C[9] > 0 ) && ( C[14] > 0 );
// if obtained chi2 value is too high - tracks could not be neighbours
          if(number == 1) active &= (chi2<=100.f);
          if(number == 0) {
            active &= (chi2<=300.f);
            dr_min2_localv(active) = chi2;
          }
          if(active.isEmpty()) continue;

          for(int iV=0; iV < nVecElements; iV++)
          {
// determine, whether neighbour is inner or outer
            if(!active[iV]) continue;
            AliHLTTPCCASliceTrackInfo *T1, *T2;
            int NextNew, SliceNextNew, PrevNew, SlicePrevNew;

            bool IsNext = (b1.InnerRow() < B2[b2index[iV]].InnerRow()) ||
                          (b1.InnerRow() == B2[b2index[iV]].InnerRow() && 
                           b1.OuterRow() < B2[b2index[iV]].OuterRow()) ||
                          (b1.InnerRow() == B2[b2index[iV]].InnerRow() && 
                           b1.OuterRow() == B2[b2index[iV]].OuterRow() && 
                           b1.TrackID() < B2[b2index[iV]].TrackID());
            if(IsNext)
            {
              T1 = &fTrackInfos[fSliceTrackInfoStart[iSlice1] + b1.TrackID() ];
              T2 = &fTrackInfos[fSliceTrackInfoStart[iSlice2] + B2[b2index[iV]].TrackID() ];

              NextNew = B2[b2index[iV]].TrackID();
              SliceNextNew = iSlice2;
              PrevNew = b1.TrackID();
              SlicePrevNew = iSlice1;
            }
            else
            {
              T2 = &fTrackInfos[fSliceTrackInfoStart[iSlice1] + b1.TrackID()  ];
              T1 = &fTrackInfos[fSliceTrackInfoStart[iSlice2] + B2[b2index[iV]].TrackID()  ];

              NextNew = b1.TrackID();
              SliceNextNew = iSlice1;
              PrevNew = B2[b2index[iV]].TrackID();
              SlicePrevNew = iSlice2;
            }
// if current neighbour is better, then previus one - make a reference to it
            if(T1->ChiNext > dr_min2_localv[iV] && T2->ChiPrev > dr_min2_localv[iV]) // clone was found
            {
              // reestimate end row and index
              maxLv = sqrt(maxL2v[iV]);
              if (number == 1) {
                // find end row
                const float x0 = fSliceParam.RowX(b1.OuterRow());
                for (; fSliceParam.RowX(lastIRow) - x0 > maxLv[iV]; lastIRow--);
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
                for (; x0 - fSliceParam.RowX(lastIRow) > maxL; lastIRow++);
                  // find end index
                for(; lastIRow <= firstIRow; lastIRow++)
                  if(LastTrIR[lastIRow] != 50000) {
       //           ilast2 = LastTrIR[lastIRow];
                  break;
                }
              }
       // std::cout << b1.OuterRow() << " " << lastIRow << " " << sqrt(maxL) << " " << ifirst2 << " " << ilast2 << std::endl; // dbg
#endif
              if(T1->NextNeighbour() > -1)
              {
                AliHLTTPCCASliceTrackInfo *T3 = &fTrackInfos[fSliceTrackInfoStart[T1->SliceNextNeighbour()] + T1->NextNeighbour() ];
                T3->SetPrevNeighbour(-2); // mark that T3 lose best neighbour (so there can be other neighbour for T3)
                T3->ChiPrev = 10000000;
              }
              if(T2->PrevNeighbour() > -1)
              {
                AliHLTTPCCASliceTrackInfo *T3 = &fTrackInfos[fSliceTrackInfoStart[T2->SlicePrevNeighbour()] + T2->PrevNeighbour() ];
                T3->SetNextNeighbour(-2);
                T3->ChiNext = 10000000;
              }
              T1->SetNextNeighbour( NextNew );
              T1->SetSliceNextNeighbour( SliceNextNew );
              T1->ChiNext = dr_min2_localv[iV];

              T2->SetPrevNeighbour( PrevNew );
              T2->SetSlicePrevNeighbour( SlicePrevNew );
              T2->ChiPrev = dr_min2_localv[iV];
            }
          }
        }
        nVecElements = 0;
      }
    }
  }
  
}

void AliHLTTPCCAMerger::Merging(int number)
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
  // int prevSlice[fgkNSlices];

  int mid = fgkNSlices / 2 - 1 ;
  int last = fgkNSlices - 1 ;

  for ( int iSlice = 0; iSlice < fgkNSlices/2; iSlice++ ) {
    nextSlice[iSlice] = iSlice + 1;
//     prevSlice[iSlice] = iSlice - 1;
    oppSlice [iSlice] = 22 - iSlice;
    if(iSlice == 11) oppSlice[iSlice]=23;
  }

  for ( int iSlice = fgkNSlices/2; iSlice < fgkNSlices; iSlice++ ) {
    nextSlice[iSlice] = iSlice - 1;
//     prevSlice[iSlice] = iSlice + 1;
  }

  if ( mid < 0 ) mid = 0; // to avoid compiler warning
  if ( last < 0 ) last = 0; //
  nextSlice[ mid ] = 0;
//   prevSlice[ 0 ] = mid;
//   prevSlice[ last ] = fgkNSlices / 2;
  nextSlice[ fgkNSlices/2 ] = last;

  int maxNSliceTracks = 0;
  for ( int iSlice = 0; iSlice < fgkNSlices; iSlice++ ) {
    if ( maxNSliceTracks < fSliceNTrackInfos[iSlice] ) maxNSliceTracks = fSliceNTrackInfos[iSlice];
  }

  AliHLTTPCCABorderTrack *bCurrIR = new AliHLTTPCCABorderTrack[maxNSliceTracks*fgkNSlices];
  AliHLTTPCCABorderTrack *bCurrOR = new AliHLTTPCCABorderTrack[maxNSliceTracks*fgkNSlices];
  unsigned short FirstTrIR[fgkNSlices][AliHLTTPCCAParameters::MaxNumberOfRows8]; // index of the first track on row
  unsigned short LastTrIR[fgkNSlices][AliHLTTPCCAParameters::MaxNumberOfRows8];

  unsigned short nCurr[fgkNSlices];
  for(unsigned char iSl=0; iSl<fgkNSlices; iSl++)
  {
    for(unsigned char iRow=0; iRow<fSliceParam.NRows(); iRow++)
    {
      FirstTrIR[iSl][iRow] = 50000;
      LastTrIR[iSl][iRow]  = 50000;
    }
    
    nCurr[iSl] = 0;
// make border tracks for each sector, sort them by inner row
    MakeBorderTracks(bCurrIR + maxNSliceTracks*iSl, nCurr[iSl], iSl);
    std::sort(bCurrIR+maxNSliceTracks*iSl, bCurrIR+maxNSliceTracks*iSl+nCurr[iSl], CompareInnerRow);

    for(unsigned short itr=0; itr < nCurr[iSl]; itr++)
      bCurrOR[itr + maxNSliceTracks*iSl] = bCurrIR[itr + maxNSliceTracks*iSl];
   // std::sort(bCurrOR+maxNSliceTracks*iSl, bCurrOR+maxNSliceTracks*iSl+nCurr[iSl], CompareOuterRow);

     // save track indices range for each row
    if(nCurr[iSl] > 0)
    {
      unsigned char curRow = bCurrIR[maxNSliceTracks*iSl].InnerRow();
      FirstTrIR[iSl][curRow] = 0;
      for(unsigned short itr = 1; itr < nCurr[iSl]; itr++)
      {
        if( bCurrIR[maxNSliceTracks*iSl+itr].InnerRow() < curRow )
        {
          // for (int iR = bCurrIR[maxNSliceTracks*iSl+itr].InnerRow() + 1; iR < curRow; iR++) // check for a jump
          //   FirstTrIR[iSl][iR] = FirstTrIR[iSl][curRow];
          LastTrIR[iSl][curRow] = itr - 1;
          curRow = bCurrIR[maxNSliceTracks*iSl + itr].InnerRow();
          FirstTrIR[iSl][curRow] = itr;
        }
      }
      LastTrIR[iSl][curRow] = nCurr[iSl] - 1;
      // for(unsigned char iRow=0; iRow<AliHLTTPCCAParameters::NumberOfRows; iRow++) { // dbg
      //   std::cout << int(iSl) << " " << int(iRow) << " " << FirstTrIR[iSl][iRow] << " " << LastTrIR[iSl][iRow] << std::endl;
      //   if (FirstTrIR[iSl][iRow] != 50000 ) std::cout <<  bCurrIR[maxNSliceTracks*iSl+FirstTrIR[iSl][iRow]].InnerRow() << std::endl;
      //   if (LastTrIR[iSl][iRow] != 50000 ) std::cout <<   bCurrIR[maxNSliceTracks*iSl+LastTrIR[iSl][iRow]].InnerRow() << std::endl;
      // }
    }
// create links to neighbour tracks clones in the same sector
    MergeBorderTracks(bCurrOR + maxNSliceTracks*iSl, nCurr[iSl], iSl,
                      bCurrIR + maxNSliceTracks*iSl, nCurr[iSl], iSl,
                      number, FirstTrIR[iSl], LastTrIR[iSl]);
  }
  
  if (! fgDoNotMergeBorders )
    for(int iSl=0; iSl<fgkNSlices; iSl++)
    {
        //  create links to neighbour tracks in the neighbour sector in the same xy-plane
      MergeBorderTracks( bCurrOR+maxNSliceTracks*iSl,            nCurr[iSl],            iSl,
      bCurrIR+maxNSliceTracks*nextSlice[iSl], nCurr[nextSlice[iSl]], nextSlice[iSl],
      number, FirstTrIR[nextSlice[iSl]], LastTrIR[nextSlice[iSl]] ); // merge upper edges
      MergeBorderTracks( bCurrOR+maxNSliceTracks*nextSlice[iSl], nCurr[nextSlice[iSl]], nextSlice[iSl],
      bCurrIR+maxNSliceTracks*iSl,            nCurr[iSl],            iSl,
      number, FirstTrIR[iSl],            LastTrIR[iSl] );            // merge lower edges

      if(iSl < fgkNSlices / 2)
      {
          //  create links to neighbour tracks with the oposit sector (in z direction)
        MergeBorderTracks(bCurrOR+maxNSliceTracks*iSl, nCurr[iSl], iSl, 
        bCurrIR+maxNSliceTracks*oppSlice[iSl], nCurr[oppSlice[iSl]], oppSlice[iSl],
        number, FirstTrIR[oppSlice[iSl]], LastTrIR[oppSlice[iSl]]);
        MergeBorderTracks(bCurrOR+maxNSliceTracks*oppSlice[iSl], nCurr[oppSlice[iSl]], oppSlice[iSl], 
        bCurrIR+maxNSliceTracks*iSl, nCurr[iSl], iSl,
        number, FirstTrIR[iSl],           LastTrIR[iSl]);
      }
    }

  if ( bCurrIR ) delete[] bCurrIR;
  if ( bCurrOR ) delete[] bCurrOR;

#ifdef USE_TIMERS
  timer.Stop();
  fTimers[3+(1-number)] = timer.RealTime();
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
  AliHLTTPCCASliceTrackInfo *tmpT = new AliHLTTPCCASliceTrackInfo[fMaxTrackInfos];
  int nEndTracks = 0; // n tracks after merging.
  int tmpSliceTrackInfoStart[fgkNSlices];

  int nTrNew[fgkNSlices];

  for(int iSlice=0; iSlice < fgkNSlices; iSlice++) nTrNew[iSlice]=0;

  int nH = 0;

///Scalar version at revision 10273

// merge tracks, using obtained links to neighbours
  for ( int iSlice = 0; iSlice < fgkNSlices; iSlice++ ) {

    tmpSliceTrackInfoStart[iSlice] = nEndTracks;
    assert( iSlice == 0 || nEndTracks == tmpSliceTrackInfoStart[iSlice-1] + nTrNew[iSlice-1] );

    AliHLTTPCCASliceTrackInfo *vTrackOld[short_v::Size];
    AliHLTTPCCATrackParam StartPoint[short_v::Size];
    AliHLTTPCCATrackParam EndPoint[short_v::Size];
    sfloat_v::Memory StartAlpha, EndAlpha;
    int nVecElements = 0;

    sfloat_v vStartAlpha(Vc::Zero);
    sfloat_v vEndAlpha(Vc::Zero);
    AliHLTTPCCATrackParamVector vStartPoint;
    AliHLTTPCCATrackParamVector vEndPoint;

    AliHLTTPCCASliceTrackInfo *TInfos[10000];
    bool IsSorted[10000];
// Resort tracks.
    int nSorted = 0;
// First write tracks, which has inner (previous) neighbour
    for(int i=0; i< fSliceNTrackInfos[iSlice]; i++)
    {
      IsSorted[i] = 0;
      TInfos[i] = 0;
      AliHLTTPCCASliceTrackInfo *tr = fTrackInfos + fSliceTrackInfoStart[iSlice] + i;
      if(tr->PrevNeighbour() > -1)
      {
        TInfos[nSorted] = tr;
        IsSorted[i] = 1;
        nSorted++;
      }
    }
// Then - tracks without any neighbours
    for(int i=0; i< fSliceNTrackInfos[iSlice]; i++)
    {
      AliHLTTPCCASliceTrackInfo *tr = fTrackInfos + fSliceTrackInfoStart[iSlice] + i;
      if(tr->PrevNeighbour() < 0 && tr->NextNeighbour() < 0 && !IsSorted[i])
      {
        TInfos[nSorted] = tr;
        IsSorted[i] = 1;
        nSorted++;
      }
    }
// Then - tracks with outetr (next) neighbours only
    for(int i=0; i< fSliceNTrackInfos[iSlice]; i++)
    {
      AliHLTTPCCASliceTrackInfo *tr = fTrackInfos + fSliceTrackInfoStart[iSlice] + i;
      if(!IsSorted[i])
      {
        TInfos[nSorted] = tr;
        IsSorted[i] = 1;
        nSorted++;
      }
    }

    for ( int itr = 0; itr < fSliceNTrackInfos[iSlice]; itr++ ) {

      AliHLTTPCCASliceTrackInfo &trackOld = *TInfos[itr];

      bool Continue = 0;
// If track is already used or has previous neighbours - do not use it
      if ( trackOld.Used() ) Continue = 1;
      if ( trackOld.PrevNeighbour() > -1 ) Continue = 1;

      if(!Continue){
// Store selected tracks
        vTrackOld[nVecElements] = &trackOld;
        StartPoint[nVecElements] = trackOld.InnerParam();
        EndPoint[nVecElements] = trackOld.OuterParam();
        StartAlpha[nVecElements] = trackOld.InnerAlpha();
        EndAlpha[nVecElements] = trackOld.OuterAlpha();
        nVecElements++;
      }
      if((nVecElements == short_v::Size) || (itr == fSliceNTrackInfos[iSlice]-1))
      {
// Convert their parameters to SIMD vectors
        ushort_m active_short = ushort_v( Vc::IndexesFromZero ) < nVecElements;
        sfloat_m active = static_cast<sfloat_m>( active_short );

        int hits[2000][ushort_v::Size];
        ushort_v::Memory nHits;
        ushort_v firstHit = (unsigned short int)1000;
        ushort_v::Memory SliceNextNeighbour;
        short_v::Memory NextNeighbour;
        ushort_v jSlice(iSlice);
        short_v jtr = (short int) -1;
        ushort_v vNHits(Vc::Zero);

        short_v vNextNeighbour;
        ushort_v vSliceNextNeighbour;

        ConvertTrackParamToVector(StartPoint,vStartPoint,nVecElements);
        ConvertTrackParamToVector(EndPoint,vEndPoint,nVecElements);
        vStartAlpha.load(StartAlpha);
        vEndAlpha.load(EndAlpha);

        const sfloat_m &invert1 = active && (vEndPoint.X() < vStartPoint.X());
        if( !(invert1.isEmpty()))
        {
          AliHLTTPCCATrackParamVector helpPoint = vEndPoint;
          vEndPoint.SetTrackParam(vStartPoint, invert1);
          vStartPoint.SetTrackParam(helpPoint, invert1);
          sfloat_v helpAlpha = vEndAlpha;
          vEndAlpha(invert1) = vStartAlpha;
          vStartAlpha(invert1) = helpAlpha;
        }

        for(int iV=0; iV<ushort_v::Size; iV++)
        {
          if(!active[iV]) continue;
          vTrackOld[iV]->SetUsed( 1 ); // ste track as used

          for ( int jhit = 0; jhit < vTrackOld[iV]->NClusters(); jhit++ ) {
            int id = vTrackOld[iV]->FirstClusterRef() + jhit;
            hits[static_cast <unsigned short>(firstHit[iV])+jhit][iV] = id;
          }
          nHits[iV] = vTrackOld[iV]->NClusters();
          NextNeighbour[iV] = vTrackOld[iV]->NextNeighbour(); // obtaine the outer neighbour index
          SliceNextNeighbour[iV] = vTrackOld[iV]->SliceNextNeighbour();
        }
        vNHits.load(nHits);
        vNextNeighbour.load(NextNeighbour);
        vSliceNextNeighbour.load(SliceNextNeighbour);

        const sfloat_m &isMerged = active && (static_cast<sfloat_v>(vNextNeighbour) > -1);
        sfloat_m IsNeighbour = isMerged;

        if(!(isMerged.isEmpty()))
        {
          jtr(static_cast<short_m>(isMerged)) = vNextNeighbour;
          jSlice(static_cast<ushort_m>(isMerged)) = vSliceNextNeighbour;
        }

        while ( !(IsNeighbour.isEmpty()) ) // while there are still outer neighbours
        {
          AliHLTTPCCATrackParam InnerParam[short_v::Size];
          AliHLTTPCCATrackParam OuterParam[short_v::Size];
          sfloat_v::Memory InnerAlpha;
          sfloat_v::Memory OuterAlpha;
          AliHLTTPCCATrackParamVector vInnerParam;
          AliHLTTPCCATrackParamVector vOuterParam;
          sfloat_v vInnerAlpha(Vc::Zero);
          sfloat_v vOuterAlpha(Vc::Zero);

          ushort_v::Memory NClusters;
          ushort_v vNClusters;

          ushort_v::Memory SegmSliceNextNeighbour;
          short_v::Memory SegmNextNeighbour;
          short_v vSegmNextNeighbour;
          ushort_v  vSegmSliceNextNeighbour;

          AliHLTTPCCASliceTrackInfo *segment[ushort_v::Size];
          short_v::Memory Used;
          short_v vsUsed;
          short_m vUsed(false);
// take the next neighbour
          for(int iV=0; iV<ushort_v::Size; iV++)
          {
            if(!IsNeighbour[iV]) continue;
            segment[iV] = &fTrackInfos[fSliceTrackInfoStart[jSlice[iV]] + jtr[iV]];
            InnerParam[iV] = segment[iV]->InnerParam();
            OuterParam[iV] = segment[iV]->OuterParam();
            InnerAlpha[iV] = segment[iV]->InnerAlpha();
            OuterAlpha[iV] = segment[iV]->OuterAlpha();
            Used[iV] = segment[iV]->Used();
            NClusters[iV] = segment[iV]->NClusters();
            SegmNextNeighbour[iV] = segment[iV]->NextNeighbour();
            SegmSliceNextNeighbour[iV] = segment[iV]->SliceNextNeighbour();
          }
          vsUsed.load(Used);
          vNClusters.load(NClusters);
          vUsed = vsUsed > 0;
          vSegmNextNeighbour.load(SegmNextNeighbour);
          vSegmSliceNextNeighbour.load(SegmSliceNextNeighbour);

          IsNeighbour &= !(static_cast<sfloat_m>(vUsed));
          if ( IsNeighbour.isEmpty() ) break;

          ConvertTrackParamToVector(InnerParam,vInnerParam,nVecElements);
          ConvertTrackParamToVector(OuterParam,vOuterParam,nVecElements);
          vInnerAlpha.load(InnerAlpha);
          vOuterAlpha.load(OuterAlpha);

          for(int iV=0; iV<ushort_v::Size; iV++)
          {
            if(!IsNeighbour[iV]) continue;
            segment[iV]->SetUsed( 1 );
          }
          sfloat_m dir(false);
          ushort_v startHit = firstHit + vNHits;
          const sfloat_v d00 = vStartPoint.GetDistXZ2( vInnerParam );
          const sfloat_v d01 = vStartPoint.GetDistXZ2( vOuterParam );
          const sfloat_v d10 = vEndPoint.GetDistXZ2( vInnerParam );
          const sfloat_v d11 = vEndPoint.GetDistXZ2( vOuterParam );
          const sfloat_v dz00 = abs( vStartPoint.Z() - vInnerParam.Z() );
          const sfloat_v dz01 = abs( vStartPoint.Z() - vOuterParam.Z() );
          const sfloat_v dz10 = abs( vEndPoint.Z() - vInnerParam.Z() );
          const sfloat_v dz11 = abs( vEndPoint.Z() - vOuterParam.Z() );

          const sfloat_m &case1 = IsNeighbour &&
            (d00 <= d01 && d00 <= d10 && d00 <= d11) &&
            (dz11 >= dz00 && dz11 >= dz01 && dz11 >= dz10 );
              /*                  0----1
               * connection like : \
               *                    0------1
               */
          const sfloat_m &case2 = IsNeighbour &&
            (d01 < d00 && d01 <= d10 && d01 <= d11) &&
            (dz10 > dz00 && dz10 >= dz01 && dz10 >= dz11 );
              /*                       0----1
               * connection like :      \
               *                    0----1
               */
          const sfloat_m &case3 = IsNeighbour &&
            (d10 < d00 && d10 <= d01 && d10 <= d11) &&
            (dz01 > dz00 && dz01 >= dz10 && dz01 >= dz11 );
              /*                   0---1
               * connection like :    /
               *                     0-------1
               */
          const sfloat_m &case4 = IsNeighbour &&
            (d11 < d00 && d11 <= d10 && d10 <= d01) &&
            (dz00 > dz01 && dz00 >= dz10 && dz00 > dz11);
              /*                      0--1
               * connection like :        \
               *                    0------1
               */
          IsNeighbour &= case1 || case2 || case3 || case4;
          
          if(!(case1.isEmpty()))
          {
            vStartPoint.SetTrackParam( vOuterParam, case1);
            vStartAlpha(case1) = vOuterAlpha;
            firstHit(static_cast<ushort_m>(case1)) -= vNClusters;
            startHit(static_cast<ushort_m>(case1)) = firstHit;
          }
          if(!(case2.isEmpty()))
          {
            vStartPoint.SetTrackParam( vInnerParam, case2);
            vStartAlpha(case2) = vInnerAlpha;
            firstHit(static_cast<ushort_m>(case2)) -= vNClusters;
            startHit(static_cast<ushort_m>(case2)) = firstHit;
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

          const sfloat_m &m1 = IsNeighbour && (vEndPoint.X() < vOuterParam.X());
          vEndPoint.SetTrackParam( vOuterParam, m1);
          vEndAlpha(m1) = vOuterAlpha;

          const sfloat_m &m2 = IsNeighbour && (vEndPoint.X() < vInnerParam.X());
          vEndPoint.SetTrackParam( vInnerParam, m2);
          vEndAlpha(m2) = vInnerAlpha;

          const sfloat_m &m3 = IsNeighbour && (vStartPoint.X() > vInnerParam.X());
          vStartPoint.SetTrackParam( vInnerParam, m3 );
          vStartAlpha(m3) = vInnerAlpha;

          const sfloat_m &m4 = IsNeighbour && (vStartPoint.X() > vOuterParam.X());
          vStartPoint.SetTrackParam( vOuterParam, m4 );
          vStartAlpha(m4) = vOuterAlpha;

          const sfloat_m dir_float = (case1 || case4);
          dir = dir_float;
// add hits of the neighbour
          for(int iV=0; iV<ushort_v::Size; iV++)
          {
            if(!IsNeighbour[iV]) continue;
            for ( int jhit = 0; jhit < segment[iV]->NClusters(); jhit++ ) {
              int id = segment[iV]->FirstClusterRef() + jhit;
              hits[static_cast <unsigned short>(startHit[iV])+( dir[iV] ?( static_cast <unsigned short>(vNClusters[iV])-1-jhit ) :jhit )][iV] = id;
            }
          }
          vNHits(static_cast<ushort_m>(IsNeighbour)) += vNClusters;
          
          jtr = -1;
          IsNeighbour &= (static_cast<sfloat_v>(vSegmNextNeighbour) > -1);
          if(!(IsNeighbour.isEmpty()))
          {
            jtr(static_cast<ushort_m>(IsNeighbour)) = vSegmNextNeighbour;
            jSlice(static_cast<short_m>(IsNeighbour)) = vSegmSliceNextNeighbour;
          }
        }

        for(int iV=0; iV<ushort_v::Size; iV++)
        {
          if(!active[iV]) continue;
          if ( vEndPoint.X()[iV] < vStartPoint.X()[iV] ) { // swap
            for ( int i = 0; i < vNHits[iV]; i++ ) hits[i][iV] = hits[firstHit[iV]+vNHits[iV]-1-i][iV];
            firstHit[iV] = 0;
          }
        }

        const sfloat_m &invert2 = isMerged && (vEndPoint.X() < vStartPoint.X());
        if( !(invert2.isEmpty()))
        {
          AliHLTTPCCATrackParamVector helpPoint = vEndPoint;
          vEndPoint.SetTrackParam(vStartPoint, invert2);
          vStartPoint.SetTrackParam(helpPoint, invert2);
          sfloat_v helpAlpha = vEndAlpha;
          vEndAlpha(invert2) = vStartAlpha;
          vStartAlpha(invert2) = helpAlpha;
        }

        sfloat_m fitted = isMerged;
// Refit tracks, which have been merged.
        vNHits.store(nHits);
        AliHLTTPCCATrackParamVector vHelpEndPoint = vStartPoint;
        sfloat_v vHelpEndAlpha = vStartAlpha;
        fitted &= FitTrack( vHelpEndPoint, vHelpEndAlpha, hits, firstHit, nHits, nVecElements,fitted, 0 );
        AliHLTTPCCATrackParamVector vHelpStartPoint = vHelpEndPoint;
        sfloat_v vHelpStartAlpha = vHelpEndAlpha;
        fitted &= FitTrack( vHelpStartPoint, vHelpStartAlpha, hits, firstHit, nHits, nVecElements,fitted, 1 );
        vNHits.load(nHits);
        active &= (!isMerged);
        active |= fitted;

        for(int iV=0; iV < ushort_v::Size; iV++)
        {
          if ( !active[iV] ) continue;
          if ( fitted[iV] ) { // merged
            StartPoint[iV] = AliHLTTPCCATrackParam( vHelpStartPoint, iV );
            EndPoint[iV] = AliHLTTPCCATrackParam( vHelpEndPoint, iV );
          }
          else { // not merged
            StartPoint[iV] = AliHLTTPCCATrackParam( vStartPoint, iV );
            EndPoint[iV] = AliHLTTPCCATrackParam( vEndPoint, iV );
          }
        }
        vHelpStartAlpha.store(&(StartAlpha[0]), active && fitted ); // work around for Vc-0.99.71
        vHelpEndAlpha.store(&(EndAlpha[0]), active && fitted );
        vStartAlpha.store(&(StartAlpha[0]), active && !fitted );
        vEndAlpha.store(&(EndAlpha[0]), active && !fitted );
        
        for(int iV=0; iV<ushort_v::Size; iV++)
        {
          if ( !active[iV] ) continue;
          int h[1000];
          for(int iClu = 0; iClu < vNHits[iV]; iClu++) h[iClu] = hits[iClu + firstHit[iV]][iV];
          int *usedHits = h; // get begin of array
// If track has been merged, resort hits, rid of double hits.
          if (isMerged[iV]) {
            //std::sort( usedHits, usedHits + vNHits[iV], TrackHitsCompare(fClusterInfos) ); // sort hits by X (iRow) // TODO normal sort
              // rid of double hits
            int ihit2 = 0; // ihit in the output array
            for(int ihit = 1; ihit < vNHits[iV]; ihit++)
            {
              if( ISUNLIKELY(usedHits[ihit2] == usedHits[ihit]) ) continue;
              ++ihit2;
              if( ISUNLIKELY(     ihit2  != ihit      ) )
                usedHits[ihit2] = usedHits[ihit];
            }
            nHits[iV] = ihit2+1;
          }
// on the final stage stor data to the global tracker
          if(number == 0)
          {
            AliHLTTPCCAMergedTrack &mergedTrack = outTracks[nOutTracks];
            mergedTrack.SetNClusters( nHits[iV] );
            mergedTrack.SetFirstClusterRef( nOutTrackClusters );
            mergedTrack.SetInnerParam( StartPoint[iV] );
            mergedTrack.SetInnerAlpha( StartAlpha[iV] );
            mergedTrack.SetOuterParam( EndPoint[iV] );
            mergedTrack.SetOuterAlpha( EndAlpha[iV] );

            for ( int i = 0; i < nHits[iV]; i++ ) {
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

            track = *vTrackOld[iV];

            track.SetFirstClusterRef(nH);
            track.SetNClusters(nHits[iV]);
            track.SetUsed(0);
            track.SetPrevNeighbour(-1);
            track.SetNextNeighbour(-1);
            track.SetSlicePrevNeighbour(-1);
            track.SetSliceNextNeighbour(-1);
            track.ChiPrev = 10000000;
            track.ChiNext = 10000000;
            track.SetInnerParam( StartPoint[iV] );
            track.SetInnerAlpha( StartAlpha[iV] );
            track.SetOuterParam( EndPoint[iV] );
            track.SetOuterAlpha( EndAlpha[iV] );

            track.fInnerRow = (fClusterInfos[usedHits[0]]).IRow();
            track.fOuterRow = (fClusterInfos[usedHits[nHits[iV]-1]]).IRow();

            for(int iClu=0; iClu < nHits[iV]; iClu++) tmpH[nH + iClu] = fClusterInfos[usedHits[iClu]];
            nH += nHits[iV];
          }

          nTrNew[iSlice]++;
          nEndTracks++;
        }

        nVecElements = 0;
      }
    }
  }

  if (fClusterInfos) delete[] fClusterInfos;
  fClusterInfos = tmpH;

  if (fClusterInfos) delete[] fTrackInfos;
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
