/*
 * This file is part of TPCCATracker package
 * Copyright (C) 2007-2020 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2020 Goethe University of Frankfurt
 *               2007-2020 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Sergey Gorbunov
 *               2007-2019 Maksym Zyzak
 *               2007-2014 Igor Kulakov
 *               2014-2020 Grigory Kozlov
 *
 * TPCCATracker is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * TPCCATracker is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */


//#include "AliHLTTPCCASliceTrack.h"
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

// ---
#include "AliHLTTPCCAPerformance.h"
#include "AliHLTTPCCAGBTracker.h"
// ---

//#include "AliHLTTPCCASliceTrackVector.h"
//#include "AliHLTTPCCAMergedTrackVector.h"

#include <iostream>
using std::cout;
using std::endl;

#ifdef MAIN_DRAW
#include "AliHLTTPCCADisplay.h"
#endif // MAIN_DRAW

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
    : fSliceParam()
    , fMaxClusterInfos( 0 )
    , fClusterInfos( 0 )
    , fMaxTrackInfos( 0 )
    , fTrackInfos( 0 )
    , fOutput( 0 )
    , fNMergedSegments( 0 )
    , fNMergedSegmentClusters( 0 )
#if 0
    , fptTrackInfoPT( 0 )
#endif

{
  //* constructor
  Clear();
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
  if ( fClusterInfos ) delete[] fClusterInfos;
  if ( fOutput ) delete[] ( ( char* )( fOutput ) );
}

void AliHLTTPCCAMerger::Clear()
{
  for ( int i = 0; i < fgkNSlices; ++i ) {
    fkSlices[i] = 0;
  }
}

void AliHLTTPCCAMerger::SetSlices (int i, AliHLTTPCCATracker *sl )
{
  //copy sector parameters information
  slices[i] = sl;
}

//void AliHLTTPCCAMerger::SetSliceData( int index, const AliHLTTPCCASliceOutput *sliceData )
void AliHLTTPCCAMerger::SetSliceData( int index, AliHLTTPCCASliceOutput *sliceData )
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

  fNMergedSegments = 0;
  fNMergedSegmentClusters = 0;

// 1) copy information from the sector tracker
#ifdef TETA
    UnpackSlicesPT();
#else
  UnpackSlices();
#endif
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
#ifndef TETA
  FindNeighbourTracks(1);
  Merging(1);
#else
  MergeUpPT(2);
  MergeUpPT(0);
  MergeUpPT(1);
  MergeUpPT(3);
#ifdef USE_TIMERS
  timer.Stop();
  fTimers[1] = timer.RealTime();
  timer.Start();
#endif
  MergingPTmultimap();
//  MergingPT(0);
#ifdef USE_TIMERS
  timer.Stop();
  fTimers[2] = timer.RealTime();
#endif
  ClearMemoryPT();
#endif

#ifndef TETA

#ifdef USE_TIMERS
  timer.Stop();
  fTimers[1] = timer.RealTime();

// 3) merge overlaping tracks, store the tracks to the global tracker
  timer.Start();
#endif // USE_TIMERS
  FindNeighbourTracks(0);
  Merging(0);
#endif

#ifndef TETA

#ifdef USE_TIMERS
  timer.Stop();
  fTimers[2] = timer.RealTime();
#endif // USE_TIMERS

#endif

#else // DO_NOT_MERGE  // just to save tracks in output format

#ifndef TETA

#ifdef USE_TIMERS
  timer.Start();
#endif // USE_TIMERS
  Merging(0);
#ifdef USE_TIMERS
  timer.Stop();
  fTimers[1] = timer.RealTime();
#endif // USE_TIMERS

#endif // DO_NOT_MERGE

#endif
}

#if 0
void AliHLTTPCCAMerger::UnpackSlicesPT()
{
  int nTracksTotal = 0;
  int nTrackClustersTotal = 0;
  unsigned int trArraySize = 0;
  for ( int iSlice = 0; iSlice < fgkNSlices; iSlice++ ) {
//  for ( int iSlice = 12; iSlice < 24; iSlice++ ) {
    if ( !fkSlices[iSlice] ) continue;
    nTracksTotal += fkSlices[iSlice]->NTracks();
    trArraySize += fkSlices[iSlice]->NTracks();
    if( fkSlices[iSlice]->NTracks()%float_v::Size != 0 ) {
      trArraySize += float_v::Size - fkSlices[iSlice]->NTracks()%float_v::Size;
    }
    nTrackClustersTotal += fkSlices[iSlice]->NTrackClusters();
  }
  fTracksTotal = nTracksTotal;

  // book/clean memory if necessary - temporary
  {
    {
      fMaxTrackInfos = ( int ) ( nTracksTotal );
    }
    {
      if ( fClusterInfos ) delete[] fClusterInfos;
      fMaxClusterInfos = ( int ) ( nTrackClustersTotal );
      fClusterInfos = new AliHLTTPCCAClusterInfo [fMaxClusterInfos];
    }

    if ( fOutput ) delete[] ( ( char* )( fOutput ) );
    int size = fOutput->EstimateSize( nTracksTotal, nTrackClustersTotal );
    fOutput = ( AliHLTTPCCAMergerOutput* )( new float2[size/sizeof( float2 )+1] );
  }
  // ---
  int nTracksCurrent = 0;
  int nClustersCurrent = 0;
  for ( int iSlice = 0; iSlice < fgkNSlices; iSlice++ ) {
        if ( !fkSlices[iSlice] ) continue;
        fptSliceFirstHit[iSlice] = nClustersCurrent;
        const AliHLTTPCCASliceOutput &slice = *( fkSlices[iSlice] );

        for ( int itr = 0; itr < slice.NTracks(); itr += uint_v::Size ) {
          int nTracksVector = uint_v::Size;
          if(slice.NTracks() - itr < uint_v::Size )
            nTracksVector = slice.NTracks() - itr;
          unsigned int nCluNew = 0;
          const AliHLTTPCCASliceTrackVector &sTrackV = slice.TrackV( itr / float_v::Size );
          for(int iV=0; iV < nTracksVector; iV++) {
              if( !sTrackV.Active()[iV] ) continue;
            int nHits(0);
            for ( int iTrClu = 0; iTrClu < sTrackV.NClusters()[iV]; iTrClu++ ) {
    // unpack cluster information
              AliHLTTPCCAClusterInfo &clu = fClusterInfos[nClustersCurrent + nCluNew + nHits];
              int ic = sTrackV.FirstClusterRef()[iV] + iTrClu;
              clu.SetISlice( iSlice );
              clu.SetIRow( slice.ClusterIDrc( ic ).Row() );
              clu.SetIClu( slice.ClusterIDrc( ic ).Cluster() );
              float2 yz = slice.ClusterUnpackedYZ( ic );
              clu.SetX( slice.ClusterUnpackedX( ic ) );
              clu.SetY( yz.x );
              clu.SetZ( yz.y );
              nHits++;
            }
            nCluNew += nHits;
            nTracksCurrent++;
          }
          nClustersCurrent += nCluNew;

            //when we turn off the extrapolation step in the tracklet constructor, we have parameters in the last point, not in the first!
            //that's why the fitting direction should be changed
        }
  }
  // ---
  vInnerX = (float*) _mm_malloc(sizeof(float)*(trArraySize), float_v::Size*4);
  vOuterX = (float*) _mm_malloc(sizeof(float)*(trArraySize), float_v::Size*4);
  vInnerY = (float*) _mm_malloc(sizeof(float)*(trArraySize), float_v::Size*4);
  vOuterY = (float*) _mm_malloc(sizeof(float)*(trArraySize), float_v::Size*4);
  vInnerZ = (float*) _mm_malloc(sizeof(float)*(trArraySize), float_v::Size*4);
  vOuterZ = (float*) _mm_malloc(sizeof(float)*(trArraySize), float_v::Size*4);
  vTeta = (float*) _mm_malloc(sizeof(float)*(trArraySize), float_v::Size*4);
//  vAngle = (float*) _mm_malloc(sizeof(float)*(trArraySize), float_v::Size*4);
  vTrackID = (int*) _mm_malloc(sizeof(int)*(trArraySize), float_v::Size*4);
//  vFirstRow = (int*) _mm_malloc(sizeof(int)*(trArraySize), float_v::Size*4);
  vLastRow = (int*) _mm_malloc(sizeof(int)*(trArraySize), float_v::Size*4);
  fTrackLinks.get_allocator().allocate(nTracksTotal*2);
  sliceTrackId = (HitInfo*) _mm_malloc(sizeof(HitInfo)*(trArraySize), sizeof(HitInfo));
  sMaxGape = 5;
  int trackCounter(0), trackOffset(0), trackOffsetV(0);
  for ( int iSlice = 0; iSlice < fgkNSlices; iSlice++ ) {	// TODO optimization: possible to calculate next row with track. Probably, will not work on high track multiplicity
      sTrackMap[iSlice][0] = -2;
    if ( !fkSlices[iSlice] ) continue;
    const AliHLTTPCCASliceOutput &slice = *( fkSlices[iSlice] );
    fptSliceFirstTrack[iSlice] = trackOffset;
    fptSliceFirstTrackV[iSlice] = trackOffsetV;
    int rowCounter(0);
    for( unsigned int iRow = 0; iRow <= 45; iRow++ ) {
      sTrackMap[iSlice][iRow] = -1;
      for( int iTr = 0; iTr < slice.NTracks(); iTr++ ) {
	if( iTr > nTracksTotal ) break;
        // ---
        unsigned int iv = iTr/float_v::Size;
        unsigned int it = iTr%float_v::Size;
        const AliHLTTPCCASliceTrackVector &sTrackV = slice.TrackV( iv );
        int ncl((int)(sTrackV.NClusters()[it])), fstcl((int)(sTrackV.FirstClusterRef()[it]));

        if( slice.ClusterIDrc( fstcl ).Row() == iRow ) {
          AliHLTTPCCAClusterInfo &innclu = fClusterInfos[fstcl+fptSliceFirstHit[iSlice]];
          AliHLTTPCCAClusterInfo &outclu = fClusterInfos[fstcl+fptSliceFirstHit[iSlice] + ncl - 1];
          int lastRow = outclu.IRow();
          vInnerX[trackCounter] = innclu.X();
          vInnerY[trackCounter] = innclu.Y();
          vInnerZ[trackCounter] = innclu.Z();
          vOuterX[trackCounter] = outclu.X();
          vOuterY[trackCounter] = outclu.Y();
          vOuterZ[trackCounter] = outclu.Z();
          vTrackID[trackCounter] = iTr + trackOffset;
          vLastRow[trackCounter] = lastRow;

          if( sTrackMap[iSlice][iRow] == -1 ) sTrackMap[iSlice][iRow] = trackCounter;
          sliceTrackId[trackCounter].slice = iSlice;
          sliceTrackId[trackCounter].id = iTr;
          trackCounter++;
        }
      }
    }
    trackOffset += fkSlices[iSlice]->NTracks();
    trackOffsetV += fkSlices[iSlice]->NTracksV();
    tracksOnSlice[iSlice] = trackCounter;
  }
  int storedTracks = trackCounter;
  for( int i = trackCounter; i < trArraySize; i++ ) {
      vInnerX[i] = 0;
      vInnerY[i] = 0;
      vInnerZ[i] = 0;
      vOuterX[i] = 0;
      vOuterY[i] = 0;
      vOuterZ[i] = 0;
      vTrackID[i] = 0;
//      vFirstRow[i] = 0;
      vLastRow[i] = 0;
  }
  fptSliceFirstTrack[fgkNSlices] = trackCounter;
  for( int iTv = 0; iTv < storedTracks; iTv += float_v::Size ) {
      float_v& vX0 = reinterpret_cast<float_v&>(vInnerX[iTv]);
      float_v& vX1 = reinterpret_cast<float_v&>(vOuterX[iTv]);
      float_v& vZ0 = reinterpret_cast<float_v&>(vInnerZ[iTv]);
      float_v& vZ1 = reinterpret_cast<float_v&>(vOuterZ[iTv]);
      float_v& teta = reinterpret_cast<float_v&>(vTeta[iTv]);

      float_v x = vX1 - vX0;
      float_v z = vZ1 - vZ0;
      teta = atan( x / z );
  }
}
#endif

#if 0
void AliHLTTPCCAMerger::ClearMemoryPT()
{
  _mm_free(vInnerX);
  _mm_free(vOuterX);
  _mm_free(vInnerY);
  _mm_free(vOuterY);
  _mm_free(vInnerZ);
  _mm_free(vOuterZ);
  _mm_free(vTeta);
//  _mm_free(vAngle);
  _mm_free(vTrackID);
  _mm_free(vLastRow);
  fTrackLinks.clear();
  _mm_free(sliceTrackId);
}
#endif

#if 0
void AliHLTTPCCAMerger::MergeUpPT( int s )
{
  int trackOffset(0);
  int dir(1);
  int gape(8);
  float *nTs = (float*) _mm_malloc(sizeof(float_v), float_v::Size*4);
  float *nXIs = (float*) _mm_malloc(sizeof(float_v), float_v::Size*4);
  float *nYIs = (float*) _mm_malloc(sizeof(float_v), float_v::Size*4);
  float *nZIs = (float*) _mm_malloc(sizeof(float_v), float_v::Size*4);
  for( int iSlice = 0; iSlice < fgkNSlices; iSlice++ ) {
    if ( !fkSlices[iSlice] ) continue;
    int pSlice(iSlice), nSlice(iSlice);
    if( s == 0 || s == 1) {
      if( iSlice < fgkNSlices/2 ) {
        pSlice = iSlice > 0 ? iSlice - 1 : fgkNSlices/2 - 1;
        nSlice = iSlice < fgkNSlices/2 - 1 ? iSlice + 1 : 0;
      }
      else {
        pSlice = iSlice > fgkNSlices/2 ? iSlice - 1 : fgkNSlices - 1;
        nSlice = iSlice < fgkNSlices - 1 ? iSlice + 1 : fgkNSlices/2;
      }
      if( s == 0 ) pSlice = nSlice;
      if( iSlice > 11 ) dir = -1;
    }
    if( s == 2 ) {
      pSlice = iSlice;
      dir = 0;
    }
    if( s == 3 ) {
      pSlice = 22 - iSlice;
      if( iSlice == 11 ) pSlice = 23;
      if( iSlice == 23 ) pSlice = 11;
    }
    if ( !fkSlices[pSlice] ) continue;
    int endTrack = fkSlices[iSlice]->NTracks() + trackOffset;
    int startTrack = trackOffset%float_v::Size == 0 ? trackOffset : trackOffset - trackOffset%float_v::Size;
    for( int iTr = startTrack; iTr < endTrack; iTr += float_v::Size ) {
      float_m active( (uint_v(iTr) + uint_v( Vc::IndexesFromZero )) < endTrack );
      active &= float_m( (uint_v(iTr) + uint_v( Vc::IndexesFromZero )) >= trackOffset );
      float_v &thisXOut = reinterpret_cast<float_v&>(vOuterX[iTr]);
      float_v &thisYOut = reinterpret_cast<float_v&>(vOuterY[iTr]);
      float_v &thisZIn = reinterpret_cast<float_v&>(vInnerZ[iTr]);
      float_v &thisZOut = reinterpret_cast<float_v&>(vOuterZ[iTr]);
//	int_v &thisRow = reinterpret_cast<int_v&>(vLastRow[iTr]);
      float_v &thisTeta = reinterpret_cast<float_v&>(vTeta[iTr]);
      float_v firstTrackV(-1.f), lastTrackV(-1.f);
      for( int iV = 0; iV < int_v::Size; iV++ ) {			//TODO: try to vectorize
	int thisRow = vLastRow[iTr+iV];
	if( !active[iV] /*|| thisRow > 41*/ ) continue;
	int fstn = 0;
	if( s == 0 || s == 1 ) fstn = 1;
	for( int nt = fstn; nt < gape; nt++ ) {
	  if( sTrackMap[pSlice][thisRow + nt] >= 0 ) {
	    int fr = (thisRow + nt) < 46 ? (thisRow + nt) : 45;
	    firstTrackV[iV] = sTrackMap[pSlice][fr];
	    break;
	  }
	}
	if( firstTrackV[iV] < 0 ) continue;
	for( int nt = thisRow+gape; nt < 72; nt++ ) {
	  if( firstTrackV[iV] == -1 ) break;
	  if( sTrackMap[pSlice][nt] >= 0 ) {
	    lastTrackV[iV] = sTrackMap[pSlice][nt];
	    break;
	  }
	}
	if( lastTrackV[iV] == -1 ) lastTrackV[iV] = tracksOnSlice[pSlice]-1;
      }
      float_m nmask(firstTrackV >= float_v(0));
      if( nmask.isEmpty() ) {
	continue;
      }
      float_v nstepsV = lastTrackV - firstTrackV + float_v(1);
      float_m mst(firstTrackV<float_v(0.f));
      nstepsV(mst) = 0;
      int nsteps = nstepsV.max();
      for( int i = 0; i < nsteps; i++ ) {
	float_m combo(float_v(i) < nstepsV);
	combo &= nmask;
	combo &= active;
	if( combo.isEmpty() ) continue;
	float_v ttracks(firstTrackV);
	ttracks(combo) += float_v(i);
	float_v &nextTeta = reinterpret_cast<float_v&>(nTs[0]);
	float_v &nextXIn = reinterpret_cast<float_v&>(nXIs[0]);
	float_v &nextYIn = reinterpret_cast<float_v&>(nYIs[0]);
	float_v &nextZIn = reinterpret_cast<float_v&>(nZIs[0]);
	int tTr[float_v::Size];
	for( int iV = 0; iV < float_v::Size; iV++ ) {
	  if( !combo[iV] ) {
	    nTs[iV] = -1;
	    nXIs[iV] = -1;
	    nYIs[iV] = -1;
	    nZIs[iV] = -1;
	    continue;
	  }
	  tTr[iV] = (int)ttracks[iV];
	  nTs[iV] = vTeta[tTr[iV]];
	  nXIs[iV] = vInnerX[tTr[iV]];
	  nYIs[iV] = vInnerY[tTr[iV]];
	  nZIs[iV] = vInnerZ[tTr[iV]];
	}
	float_v dTetaV = abs(thisTeta - nextTeta);
	float_m tetamask( dTetaV >= 3.f );
	if( !tetamask.isEmpty() ) dTetaV(tetamask) -= 3.f;
	combo &= float_m( dTetaV <= 0.3f );
	if( s == 1 || s == 0 ) {
	  float_m mask(thisXOut < nextXIn + 10.f);
	  if( s== 0 ) {
	    mask &= float_m(dir*thisYOut > 0.f);
	    mask &= float_m(dir*nextYIn < 0.f);
	  }
	  if( s == 1 ) {
	    mask &= float_m(dir*thisYOut < 0.f);
	    mask &= float_m(dir*nextYIn > 0.f);
	  }
	  combo &= mask;
	}
	if( s == 2 ) {
	  float_m mask(abs( nextZIn - thisZOut ) < 10.f);
	  mask &= float_m(abs( nextYIn - thisYOut ) < 5.f);
	  combo &= mask;
	}
	if( s == 3 ) {
	  float_m mask(abs(thisZOut) < 10.f);
	  mask &= float_m(abs(nextZIn) < 10.f);
	  combo &= mask;
	}
	if( combo.isEmpty() ) continue;
	for( int t = 0; t < float_v::Size; t++ ) {
	  if( !combo[t] ) continue;
	  int iMerge = vTrackID[iTr+t];
	  int jMerge = vTrackID[tTr[t]];

	  int iMergeVs = sliceTrackId[iTr+t].id;
	  int jMergeVs = sliceTrackId[tTr[t]].id;
	  int iMergeVv = iMergeVs / float_v::Size;
	  int iMergeVi = iMergeVs % float_v::Size;
	  int jMergeVv = jMergeVs / float_v::Size;
	  int jMergeVi = jMergeVs % float_v::Size;

	  if( s == 1 || s == 0 ) {
	    if( ((fabs(thisZIn[t]) < fabs(thisZOut[t])) && (fabs(thisZOut[t]) > fabs(nextZIn[t]))) || ((fabs(thisZIn[t]) > fabs(thisZOut[t])) && (fabs(thisZOut[t]) < fabs(nextZIn[t]))) )
	      if( fabs(thisZIn[t] - thisZOut[t]) > 3.f || fabs(nextZIn[t] - thisZOut[t]) > 3.f ) continue;
	  }
	  fMaxMerged[0]++;
	  AliHLTTPCCASliceOutput &slice1 = *( fkSlices[iSlice] );
	  AliHLTTPCCASliceOutput &slice2 = *( fkSlices[pSlice] );
	  AliHLTTPCCASliceTrackVector &sTrackV1 = slice1.TrackV( iMergeVv );
	  AliHLTTPCCASliceTrackVector &sTrackV2 = slice2.TrackV( jMergeVv );
	  if( !sTrackV1.Active()[iMergeVi] || !sTrackV2.Active()[jMergeVi] ) continue;	//TODO: delete
	  fTrackLinks.insert( std::pair<HitInfo, HitInfo>( HitInfo( iMerge, iSlice ), HitInfo( jMerge, pSlice ) ) );
	  if( sTrackV1.Used()[iMergeVi] || sTrackV2.Used()[jMergeVi] ) fMaxMerged[1]++;
	  sTrackV1.SetUsed( iMergeVi, 1 );
	  sTrackV2.SetUsed( jMergeVi, 2 );
	}
      }
    }
    trackOffset = tracksOnSlice[iSlice];
  }
  _mm_free(nTs);
  _mm_free(nXIs);
  _mm_free(nYIs);
  _mm_free(nZIs);
}
#endif

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
    {
      fMaxTrackInfos = ( int ) ( nTracksTotal );
      fTrackInfos.resize(fMaxTrackInfos);
    }

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
      if(slice.NTracks() - itr < int(uint_v::Size) )
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
          AliHLTTPCCAClusterInfo &clu = fClusterInfos[nClustersCurrent + nCluNew + (unsigned int)nHits[iV]];
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
      fitted &= FitTrack( startPoint, startAlpha, endPoint, endAlpha, hits, nHits, nHits, nTracksVector, 1 );
      for(int iV=0; iV < nTracksVector; iV++)
      {
        endPoint[iV] = startPoint[iV];
        endAlpha[iV] = startAlpha[iV];
      }
      fitted &= FitTrack( endPoint, endAlpha, startPoint, startAlpha, hits, nHits, nHits, nTracksVector, 0 );
#else
      fitted &= static_cast<float_m>( uint_v( Vc::IndexesFromZero ) < nTracksVector );

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
      for( unsigned int iV=0; iV<float_v::Size; iV++ ) {
        if(!fitted[iV]) continue;
          // if the track fitted correctly store the track
        AliHLTTPCCASliceTrackInfo &track = fTrackInfos[nTracksCurrent];

#ifdef CALC_DCA_ON
        auto inTrPar = AliHLTTPCCATrackParam( vStartPoint, iV );
        track.SetInnerParam( inTrPar );
        point_3d dca;
        inTrPar.GetDCAPoint( 0.f, 0.f, 0.f, dca.x, dca.y, dca.z, fSliceParam.cBz( ) );
        if( inTrPar.GetZ() < 0 ) dca_left.push_back(dca);
        else dca_right.push_back(dca);
#else
        track.SetInnerParam( AliHLTTPCCATrackParam( vStartPoint, iV ) );
#endif
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
        track.fOuterRow = (fClusterInfos[hits[(unsigned int)nHits[iV]-1][iV]]).IRow();
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

#if 1
float_m AliHLTTPCCAMerger::FitTrack( AliHLTTPCCATrackParamVector &t, float_v &Alpha0V,
                                      int hits[2000][uint_v::Size], uint_v &firstHits, uint_v &NTrackHits,
                                      int &nTracksV, float_m active0, bool dir )
{
  // Fit the track
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

    const float_m broken = savedActive && (!rotated || !transported /*|| !filtered*/);
    if ( !broken.isEmpty() ) {
      t.TransportToXWithMaterial( xLast, linearization, fitPar, fSliceParam.cBz( ), 0.999f, transported && !filtered ); // transport back if hit can't be added. TODO with out material
      t.Rotate( -rotateA, linearization, .999f, rotated && (!transported || !filtered) );
    }

    for(int iV=0; iV < nTracksV; iV++) {
      if( !(active[iV]) ) continue;

      const unsigned int& jhit = HitIndex( firstHits, uint_v(NTrackHits), dir, iV, ihit );
      hitsNew[(unsigned int)nHitsNew[iV]][iV] = hits[jhit][iV];
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
#else
float_m AliHLTTPCCAMerger::FitTrack( AliHLTTPCCATrackParamVector &t, float_v &Alpha0V,
                                      int hits[2000][uint_v::Size], uint_v &firstHits,uint_v &NTrackHits,
                                      int &nTracksV, float_m active0, bool dir )
{
  // Fit the track
  AliHLTTPCCATrackParamVector::AliHLTTPCCATrackFitParam fitPar;

  AliHLTTPCCATrackLinearisationVector linearization( t );

  t.CalculateFitParameters( fitPar );

  const int MaxNHits = 4*AliHLTTPCCAParameters::MaxNumberOfRows8; // koeff 4 reserves place for several turn
  int hitsNew[MaxNHits][uint_v::Size];
  uint_v nHitsNew(Vc::Zero);

  uint_v nHits(NTrackHits);
  nHits.setZero(static_cast<uint_m>(!active0));

  int nHitsMax = nHits.max();
//  int nHitsMin = nHits.min();

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
      hitsNew[(unsigned int)nHitsNew[iV]][iV] = hits[jhit][iV];
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
#endif

float_m AliHLTTPCCAMerger::FitTrackMerged( AliHLTTPCCATrackParamVector &t, float_v &Alpha0V,
                                      int hits[100][uint_v::Size], uint_v &firstHits, uint_v &NTrackHits,
                                      int &nTracksV, float_m active0, bool dir )
{
  // Fit the track
  AliHLTTPCCATrackParamVector::AliHLTTPCCATrackFitParam fitPar;

  AliHLTTPCCATrackLinearisationVector linearization( t );

  t.CalculateFitParameters( fitPar );

  const int MaxNHits = NTrackHits.max();//4*AliHLTTPCCAParameters::MaxNumberOfRows8; // koeff 4 reserves place for several turn
  uint_v nHitsNew(Vc::Zero);

  uint_v nHits(NTrackHits);
  nHits.setZero(static_cast<uint_m>(!active0));

  int nHitsMax = nHits.max();

    // pack hits
  float xVs[MaxNHits*float_v::Size] __attribute__ ((aligned(float_v::Size*4)));
  float yVs[MaxNHits*float_v::Size] __attribute__ ((aligned(float_v::Size*4)));
  float zVs[MaxNHits*float_v::Size] __attribute__ ((aligned(float_v::Size*4)));
  float sliceAlphaVs[MaxNHits*float_v::Size] __attribute__ ((aligned(float_v::Size*4)));
  unsigned int RowVs[MaxNHits*float_v::Size] __attribute__ ((aligned(float_v::Size*4)));

  // ---
//  float slVs[MaxNHits*float_v::Size] __attribute__ ((aligned(float_v::Size*4)));
  // ---

  for ( int ihit = 0; ihit < nHitsMax; ihit++ ) {
    const float_m &active = static_cast<float_m>( ihit < nHits ) && active0;

    uint_v jhitV;
    if( dir == 0 ) jhitV = firstHits + (uint_v)ihit;
    else jhitV = firstHits + nHits - (uint_v)(ihit + 1);
    for(int iV=0; iV < nTracksV; iV++) {
      if( !active[iV] ) continue;
      const unsigned int jhit = jhitV[iV];//firstHits[iV] + ihit;
      const AliHLTTPCCAClusterInfo &h = fClusterInfos[hits[jhit][iV]];
      sliceAlphaVs[ihit*float_v::Size+iV] = slices[h.ISlice()]->Param().Alpha();
      xVs[ihit*float_v::Size+iV] = h.X();
      yVs[ihit*float_v::Size+iV] = h.Y();
      zVs[ihit*float_v::Size+iV] = h.Z();
      RowVs[ihit*uint_v::Size+iV] = (unsigned int)h.IRow();
    }
  }

    // fit
  bool first = true;
  float_m active = active0; // stop on a hit if this hit can't be added
  for ( int ihit = 0; ihit < nHitsMax; ihit++ ) {
    active &= static_cast<float_m>( ihit < nHits );

    if(active.isEmpty()) continue;

    const float_v &xV = reinterpret_cast<float_v&>(xVs[ihit*float_v::Size]);
    const float_v &yV = reinterpret_cast<float_v&>(yVs[ihit*float_v::Size]);
    const float_v &zV = reinterpret_cast<float_v&>(zVs[ihit*float_v::Size]);
    const float_v &sliceAlphaV = reinterpret_cast<float_v&>(sliceAlphaVs[ihit*float_v::Size]);
    const uint_v &RowV = reinterpret_cast<uint_v&>(RowVs[ihit*uint_v::Size]);

    const float_m savedActive = active;
    const float_v rotateA = sliceAlphaV - Alpha0V;
    float_m rotated(active);
//#define SKIP_CALC
#ifndef SKIP_CALC
    if( ISUNLIKELY( !(!CAMath::IsZero(rotateA) && active).isEmpty() ) ) { // track crosses a sector border very rarely
      rotated = t.Rotate( rotateA, linearization, .999f, active);
      active &= rotated;
    }
    const float_v xLast = t.X();
#endif

    Alpha0V(active) = sliceAlphaV;
    const float_m &transported = t.TransportToXWithMaterial( xV, linearization, fitPar, fSliceParam.cBz( ), 0.999f, active);
#ifndef SKIP_CALC
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
#endif
    nHitsNew(uint_m(active))++;
  }

  float_m ok = active0 && float_m(uint_v(nHitsNew) >= 3) && t.IsNotDiverged();
#ifndef SKIP_CALC
  t.SetQPt( float_v(1.e-8f), CAMath::Abs( t.QPt() ) < 1.e-8f );
  t.NormilizeSignCosPhi( linearization, ok );
#endif

#ifdef SHORT_ARRAY_TEST
  _mm_free(xVs1);
  _mm_free(yVs1);
  _mm_free(zVs1);
  _mm_free(sliceAlphaVs1);
  _mm_free(RowVs1);
#endif

  return ok;
}

void AliHLTTPCCAMerger::MakeBorderTracks( AliHLTTPCCABorderTrack B[], unsigned int &nB, unsigned char &iSlice)
{
  //* prepare slice tracks for merging with next/previous/same sector

  for ( int itr = 0; itr < fSliceNTrackInfos[iSlice]; itr++ ) {
    const AliHLTTPCCASliceTrackInfo &track = fTrackInfos[ fSliceTrackInfoStart[iSlice] + itr ];

    if( int(track.NClusters()) > fSliceParam.NRows()-3 ) continue;

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
#define M_TEMP
#ifndef M_TEMP
  active &= CAMath::Abs(Thelp1.SinPhi() - Thelp.SinPhi()) <= 0.15f;
#else
  active &= CAMath::Abs(Thelp1.SinPhi() - Thelp.SinPhi()) <= 0.25f;
#endif
  if(active.isEmpty()) return;

    // merge parameters and calculate chi2
  float_v C[15], r[5], chi2(Vc::Zero);
  FilterTracks(Thelp.GetPar(), Thelp.GetCov(), Thelp1.GetPar(), Thelp1.GetCov(), r, C, chi2, active);

    // if after merging parameters are infinite, or diagonal elements of cov. matrix are negative - tracks could not be neighbours
  for ( int i = 0; i < 15; i++ ) active &= CAMath::Finite( C[i] );
  for ( int i = 0; i < 5; i++ ) active &= CAMath::Finite( r[i] );
  active &= ( C[0] > 0 ) && ( C[2] > 0 ) && ( C[5] > 0 ) && ( C[9] > 0 ) && ( C[14] > 0 );

    // if obtained chi2 value is too high - tracks could not be neighbours
//  active &= (chi2 <= 100.f) && float_m(number == 1) || (chi2 <= 300.f) && float_m(number == 0);
  active &= (chi2 <= 150.f) && float_m(number == 1) || (chi2 <= 300.f) && float_m(number == 0);
  active &= (chi2 < bestChi2) || float_m(number != 0); // for number == 0

  min_chi2(active) = minL2v; // use minL2v with number == 1
  min_chi2(active && float_m(number == 0) ) = chi2;
}

void AliHLTTPCCAMerger::FindMinMaxIndex( int N2, const unsigned int FirstTrIR[], const unsigned int LastTrIR[], int minIRow, int maxIRow, int &min, int &max )
{
    // find min index
  min = -1;
  for(; minIRow < fSliceParam.NRows(); minIRow++){
    if (LastTrIR[minIRow] != 50000) {
      min = LastTrIR[minIRow];
      break;
    }
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
      for( ; nVecElements < int(uint_v::Size) && dir*i2 <= dir*ilast2; i2 += dir ) {

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

        const AliHLTTPCCABorderTrack &b2iV = B2[(unsigned int)b2index[iV]];

        if( ISUNLIKELY( b1.InnerRow() <= b2iV.InnerRow()+1 && b1.OuterRow() >= b2iV.OuterRow()-1 ) ) continue;
        if( ISUNLIKELY( b2iV.InnerRow() <= b1.InnerRow()+1 && b2iV.OuterRow() >= b1.OuterRow()-1 ) ) continue;

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
        fNMergedSegments++;
        fNMergedSegmentClusters += T1->NClusters();

        T2->SetPrevNeighbour( PrevNew );
        T2->SetSlicePrevNeighbour( SlicePrevNew );
        T2->ChiPrev = bestChi2;
        fNMergedSegments++;
        fNMergedSegmentClusters += T2->NClusters();
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

  if (number == 1) { // with number == 0 only parallel tracks are merged, they should be at the same sector
  if (! fgDoNotMergeBorders ) {
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
#define DO_OP_CROSS_MERDE
#ifdef DO_OP_CROSS_MERDE
        for( int ii = -1; ii < 2; ii++ ) {
          //  create links to neighbour tracks with the oposit sector (in z direction)
          if( oppSlice[iSl] + ii >= 0 && oppSlice[iSl] + ii < 24 ) {
            MergeBorderTracks( bCurrOR+maxNSliceTracks*iSl, nCurr[iSl], iSl,
                           bCurrIR+maxNSliceTracks*(oppSlice[iSl]+ii), nCurr[oppSlice[iSl]+ii], oppSlice[iSl]+ii,
                           number, FirstTrIR[oppSlice[iSl]+ii], LastTrIR[oppSlice[iSl]+ii] );
            MergeBorderTracks( bCurrOR+maxNSliceTracks*(oppSlice[iSl]+ii), nCurr[oppSlice[iSl]+ii], oppSlice[iSl]+ii,
                           bCurrIR+maxNSliceTracks*iSl, nCurr[iSl], iSl,
                           number, FirstTrIR[iSl],           LastTrIR[iSl] );
          }
        }
#else
          //  create links to neighbour tracks with the oposit sector (in z direction)
        MergeBorderTracks( bCurrOR+maxNSliceTracks*iSl, nCurr[iSl], iSl,
                           bCurrIR+maxNSliceTracks*(oppSlice[iSl]), nCurr[oppSlice[iSl]], oppSlice[iSl],
                           number, FirstTrIR[oppSlice[iSl]], LastTrIR[oppSlice[iSl]] );
        MergeBorderTracks( bCurrOR+maxNSliceTracks*(oppSlice[iSl]), nCurr[oppSlice[iSl]], oppSlice[iSl],
                           bCurrIR+maxNSliceTracks*iSl, nCurr[iSl], iSl,
                           number, FirstTrIR[iSl],           LastTrIR[iSl] );
#endif
      }
    }
  }
  }

  if ( bCurrIR ) delete[] bCurrIR;
  if ( bCurrOR ) delete[] bCurrOR;

#ifdef USE_TIMERS
  timer.Stop();
  fTimers[3+(1-number)] = timer.RealTime();
#endif // USE_TIMERS
}

#if 0
void AliHLTTPCCAMerger::MergingPTmultimap()
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
  int nEndTracks = 0; // n tracks after merging.
  int tmpSliceTrackInfoStart[fgkNSlices];

  int nTrNew[fgkNSlices] = {0};
  int nH = 0;
  bool* usedTracks = new bool[fMaxTrackInfos];

  TrMerge* merge_candidates = new TrMerge[fMaxMerged[0] + fMaxMerged[1]+10];
  TrSort* merge_sort_helper = new TrSort[fMaxMerged[0] + fMaxMerged[1]+10];

  int nMerged(0), nMergedHits(0);
  for( auto it = fTrackLinks.begin(); it != fTrackLinks.end(); ++it ) {
    merge_candidates[nMerged].nTracks = 2;
    merge_candidates[nMerged].iTrack[0] = it->first.id;
    merge_candidates[nMerged].iTrack[1] = it->second.id;
    merge_candidates[nMerged].iSlice[0] = it->first.slice;
    merge_candidates[nMerged].iSlice[1] = it->second.slice;
    // ---
    AliHLTTPCCASliceOutput &slice1 = *( fkSlices[it->first.slice] );
    AliHLTTPCCASliceOutput &slice2 = *( fkSlices[it->second.slice] );
    int vv1 = (it->first.id - fptSliceFirstTrack[it->first.slice])/float_v::Size;
    int vv2 = (it->second.id - fptSliceFirstTrack[it->second.slice])/float_v::Size;
    int ve1 = (it->first.id - fptSliceFirstTrack[it->first.slice])%float_v::Size;
    int ve2 = (it->second.id - fptSliceFirstTrack[it->second.slice])%float_v::Size;
    AliHLTTPCCASliceTrackVector &sTrackV1 = slice1.TrackV( vv1 );
    AliHLTTPCCASliceTrackVector &sTrackV2 = slice2.TrackV( vv2 );
    merge_sort_helper[nMerged].nHits = sTrackV1.NClusters()[ve1] + sTrackV2.NClusters()[ve2];
    merge_sort_helper[nMerged].trId = nMerged;
    auto it_next = fTrackLinks.find( it->second );
    if( it_next != fTrackLinks.end() ) {
      nMerged++;
      merge_candidates[nMerged] = merge_candidates[nMerged-1];
      merge_candidates[nMerged].nTracks++;
      merge_candidates[nMerged].iTrack[2] = it_next->second.id;
      merge_candidates[nMerged].iSlice[2] = it_next->second.slice;

      AliHLTTPCCASliceOutput &slice3 = *( fkSlices[it_next->second.slice] );
      int vv3 = (it_next->second.id - fptSliceFirstTrack[it_next->second.slice])/float_v::Size;
      int ve3 = (it_next->second.id - fptSliceFirstTrack[it_next->second.slice])%float_v::Size;
      AliHLTTPCCASliceTrackVector &sTrackV3 = slice3.TrackV( vv3 );
      merge_sort_helper[nMerged] = merge_sort_helper[nMerged-1];
      merge_sort_helper[nMerged].nHits += sTrackV3.NClusters()[ve3];

      merge_sort_helper[nMerged].trId = nMerged;
      assert( merge_sort_helper[nMerged].nHits < 100 );
    }
    nMergedHits += merge_sort_helper[nMerged].nHits;
    nMerged++;
  }

  std::sort(&(merge_sort_helper[0]), &(merge_sort_helper[nMerged]), TrSort::trComp);
  int mergeCounter = 0;
  int nTracksCurrent = 0;
  int nClustersCurrent = 0;
  int nTracksVector(0);
  int hits[100][uint_v::Size];
  float *nHitsS = (float*) _mm_malloc(sizeof(float_v), float_v::Size*4);
  float *startAlphaS = (float*) _mm_malloc(sizeof(float_v), float_v::Size*4);
  float *endAlphaS = (float*) _mm_malloc(sizeof(float_v), float_v::Size*4);
  unsigned int tr_used[uint_v::Size], tr_slice[uint_v::Size];
  int nnext[float_v::Size], fstclnext[2][float_v::Size], nclnext[2][float_v::Size], slicenext[2][float_v::Size], trnext[2][float_v::Size];

  AliHLTTPCCATrackParam pStartPoint[uint_v::Size];
  AliHLTTPCCASliceTrackVector *sTrackV[3] = {0};

  for( int iTr = 0; iTr < nMerged; iTr++ ) {
    TrMerge &m_cand = merge_candidates[merge_sort_helper[iTr].trId];
    if ( !fkSlices[m_cand.iSlice[0]] ) continue;
    const AliHLTTPCCASliceOutput &slice = *( fkSlices[m_cand.iSlice[0]] );
    if( nTracksVector == 0 ) {
      for( int i = 0; i < uint_v::Size; i++ ) nHitsS[i] = 0;
    }

    nnext[nTracksVector] = 0;
    int vv[3], ve[3];
    vv[0] = ( m_cand.iTrack[0] - fptSliceFirstTrack[m_cand.iSlice[0]] ) / float_v::Size;
    ve[0] = ( m_cand.iTrack[0] - fptSliceFirstTrack[m_cand.iSlice[0]] ) % float_v::Size;
    sTrackV[0] = &slice.TrackV( vv[0] );
    vv[1] = ( m_cand.iTrack[1] - fptSliceFirstTrack[m_cand.iSlice[1]] ) / float_v::Size;
    ve[1] = ( m_cand.iTrack[1] - fptSliceFirstTrack[m_cand.iSlice[1]] ) % float_v::Size;
    const AliHLTTPCCASliceOutput &slice1 = *( fkSlices[m_cand.iSlice[1]] );
    sTrackV[1] = &slice1.TrackV( vv[1] );
    nnext[nTracksVector]++;
    fstclnext[0][nTracksVector] = sTrackV[1]->FirstClusterRef()[ve[1]];
    nclnext[0][nTracksVector] = sTrackV[1]->NClusters()[ve[1]];
    slicenext[0][nTracksVector] = m_cand.iSlice[1];
    trnext[0][nTracksVector] = m_cand.iTrack[1] - fptSliceFirstTrack[m_cand.iSlice[1]];
    if( sTrackV[0]->Used()[ve[0]] != 7 && sTrackV[1]->Used()[ve[1]] != 7 ) {
      int nHitCurrent(0);
      pStartPoint[nTracksVector] = AliHLTTPCCATrackParam( sTrackV[0]->InnerParam(), ve[0] );
      tr_used[nTracksVector] = m_cand.iTrack[0] - fptSliceFirstTrack[m_cand.iSlice[0]];
      tr_slice[nTracksVector] = m_cand.iSlice[0];
      startAlphaS[nTracksVector] = slices[m_cand.iSlice[0]]->Param().Alpha();
      endAlphaS[nTracksVector]   = startAlphaS[nTracksVector];
      if( m_cand.nTracks > 2 ) {
	vv[2] = ( m_cand.iTrack[2] - fptSliceFirstTrack[m_cand.iSlice[2]] ) / float_v::Size;
	ve[2] = ( m_cand.iTrack[2] - fptSliceFirstTrack[m_cand.iSlice[2]] ) % float_v::Size;
	const AliHLTTPCCASliceOutput &slice2 = *( fkSlices[m_cand.iSlice[2]] );
	sTrackV[2] = &slice2.TrackV( vv[2] );
	nnext[nTracksVector]++;
	fstclnext[1][nTracksVector] = sTrackV[2]->FirstClusterRef()[ve[2]];
	nclnext[1][nTracksVector] = sTrackV[2]->NClusters()[ve[2]];
	slicenext[1][nTracksVector] = m_cand.iSlice[2];
	trnext[1][nTracksVector] = m_cand.iTrack[2] - fptSliceFirstTrack[m_cand.iSlice[2]];
      }
      for( int jTr = 0; jTr < m_cand.nTracks; jTr++ ) {
	int fst_h = sTrackV[jTr]->FirstClusterRef()[ve[jTr]] + fptSliceFirstHit[m_cand.iSlice[jTr]];
        for( int iH = 0; iH < sTrackV[jTr]->NClusters()[ve[jTr]]; iH++ ) {
          hits[nHitCurrent][nTracksVector] = fst_h + iH;
          nHitsS[nTracksVector]++;
          nHitCurrent++;
        }
      }
      nTracksVector++;
    }

    if( nTracksVector == float_v::Size || iTr >= nMerged - 1 ) {
      float_v &nHitsF = reinterpret_cast<float_v&>(nHitsS[0]);
      uint_v nHits(nHitsF);
      float_v &startAlpha = reinterpret_cast<float_v&>(startAlphaS[0]);
      float_v &endAlpha = reinterpret_cast<float_v&>(endAlphaS[0]);
      float_m fitted = float_m(true);
      fitted &= static_cast<float_m>(static_cast<uint_v>(nHits) >= 4);
      fitted &= static_cast<float_m>( uint_v( Vc::IndexesFromZero ) < nTracksVector );
      AliHLTTPCCATrackParamVector vEndPoint;

      const AliHLTTPCCATrackParam *pStartPointC[uint_v::Size] = {0};
      for( int i = 0; i < uint_v::Size; i++ ) pStartPointC[i] = &pStartPoint[i];
      ConvertPTrackParamToVector(pStartPointC,vEndPoint,nTracksVector); // save as end because it will be fitted

      float_v vEndAlpha(Vc::Zero);
      vEndAlpha = startAlpha;
      uint_v firstHits(Vc::Zero);
      // refit in the forward direction: going from the first hit to the last, mask "fitted" marks with 0 tracks, which are not fitted correctly
      if( !fitted.isFull() ) {
        uint_m skip(!fitted);
        nHits(skip) = uint_v(0);
      }
      fitted &= FitTrackMerged( vEndPoint,   vEndAlpha,   hits, firstHits, nHits, nTracksVector, fitted, 0 );
float_m fitted_tmp = fitted;
      // if chi2 per degree of freedom > 3. sigma - mark track with 0
      fitted &= vEndPoint.Chi2()  < 18.f*static_cast<float_v>(vEndPoint.NDF());
      AliHLTTPCCATrackParamVector vStartPoint(vEndPoint);
      float_v vStartAlpha(vEndAlpha);
      // refit in the backward direction: going from the last hit to the first
      fitted &= FitTrackMerged( vStartPoint, vStartAlpha, hits, firstHits, nHits, nTracksVector, fitted, 1 );
      // if chi2 per degree of freedom > 3. sigma - mark track with 0
      fitted &= vStartPoint.Chi2() < 9.f*static_cast<float_v>(vStartPoint.NDF());	//TODO: improve this cut
fitted = fitted_tmp;
      for(int iV=0; iV<float_v::Size; iV++) {
        if(!fitted[iV]) continue;
      	// if the track fitted correctly store the track
      	const AliHLTTPCCASliceOutput &slice0 = *( fkSlices[tr_slice[iV]] );
      	int vv0 = tr_used[iV] / float_v::Size;
      	int ve0 = tr_used[iV] % float_v::Size;
      	AliHLTTPCCASliceTrackVector &sTrackV0 = slice0.TrackV( vv0 );
      	sTrackV0.SetUsed( ve0, 7 );
      	sTrackV0.SetNSegments( nnext[iV], ve0 );
      	for( int i = 0; i < nnext[iV]; i++ ) {
      	  sTrackV0.SetNClustersSeg( nclnext[i][iV], i, ve0 );
          sTrackV0.SetFirstClusterRefSeg( fstclnext[i][iV], i, ve0 );
          sTrackV0.SetSliceSeg( slicenext[i][iV], i, ve0 );
          int vv01 = (int)(trnext[i][iV]) / float_v::Size;
          int ve01 = (int)(trnext[i][iV]) % float_v::Size;
          const AliHLTTPCCASliceOutput &slice01 = *( fkSlices[(int)(slicenext[i][iV])] );
          AliHLTTPCCASliceTrackVector &sTrackV01 = slice01.TrackV( vv01 );
          sTrackV01.SetActive( ve01, false );
      	}
      	fSliceNTrackInfos[ (int)tr_slice[iV] ]++;
      	nClustersCurrent += nHits[iV];
      }
      nTracksVector = 0;
    }
  }

  for ( int iSlice = 0; iSlice < fgkNSlices; iSlice++ ) {
    if ( !fkSlices[iSlice] ) continue;
    const AliHLTTPCCASliceOutput &slice = *( fkSlices[iSlice] );
    for( int iTr = 0; iTr < slice.NTracks(); iTr += float_v::Size ) {
      unsigned int iv = iTr/float_v::Size;
      const AliHLTTPCCASliceTrackVector &sTrackV = slice.TrackV( iv );
      for( int iV = 0; iV < float_v::Size; iV++ ) {
	if( !(sTrackV.Active()[iV]) ) continue;
	const unsigned int NHits = sTrackV.NClusters()[iV];
	nClustersCurrent += NHits;//sTrackV.NClusters()[iV];
	nTracksCurrent++;
      }
    }
  }
  fOutput->SetNTracks( nTracksCurrent );
  fOutput->SetNTrackClusters( nClustersCurrent );

  _mm_free(nHitsS);
  _mm_free(startAlphaS);
  _mm_free(endAlphaS);

  delete [] merge_candidates;
  delete [] merge_sort_helper;
  delete [] usedTracks;
#ifdef USE_TIMERS
  timer.Stop();
//  fTimers[5+(1-number)] = timer.RealTime();
#endif // USE_TIMERS
}
#endif

#if 0
void AliHLTTPCCAMerger::MergingPT(int number)
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

  AliHLTTPCCAClusterInfo *tmpH = new AliHLTTPCCAClusterInfo[fMaxClusterInfos+100];	//TODO: fMaxClusterInfos may be not enough. Check it.
  Vc::vector<AliHLTTPCCASliceTrackInfo> tmpT(fMaxTrackInfos+10);
  int nEndTracks = 0; // n tracks after merging.
  int tmpSliceTrackInfoStart[fgkNSlices];

  int nTrNew[fgkNSlices] = {0};
  int nH = 0;

  //---
  int mergeCounter = 0;
  int nTracksCurrent = 0;
  int nClustersCurrent = 0;
  // ---
//#define FULLFIT
#define NOFIT1
#ifdef FULLFIT
  std::sort(&(fTrList[0]), &(fTrList[fTracksTotal]), TrSort::trComp);
  float_v startAlphaN;
  float_v endAlphaN;
  int hitsN[1000][uint_v::Size];
  uint_v nHitsN;//, nHits1;
  const AliHLTTPCCATrackParam *pStartPointN[uint_v::Size] = {0};
  int nTracksVectorN(0);
  float_m mergedN;
  int_v tr_usedN, tr_slice;
  for( int iTr = 0; iTr < fTracksTotal; iTr++ ) {
      bool lastTrack(iTr >= fTracksTotal - 1);
      int itr = fTrList[iTr].trId;
      int iSlice = fTrList[iTr].iSlice;
      if ( !fkSlices[iSlice] ) continue;
      if( nTracksVectorN == 0 ) nHitsN = uint_v(0);
      bool mergeTr(false);
      AliHLTTPCCASliceTrackInfo &mTrack = fTrackInfos[fptSliceFirstTrack[iSlice] + itr];
      const AliHLTTPCCASliceOutput &slice = *( fkSlices[iSlice] );
      const AliHLTTPCCASliceTrack &sTrack = slice.Track( itr );
      if( mTrack.Used() == 1 || mTrack.Used() == 2 ) mergeCounter++;
      if( mTrack.Used() > 1 || mTrack.Used() < 0 ) continue;
      if(mTrack.Used() == 1) {
        mergedN[nTracksVectorN] = true;
      	mergeTr = true;
      }
      else {
        mergedN[nTracksVectorN] = false;
// ---
#ifdef NOFIT1
        if(!lastTrack) {
          AliHLTTPCCASliceTrackInfo &track0 = tmpT[nTracksCurrent];
          mTrack.SetUsed(3);
          track0.SetUsed(3);
          track0.SetInnerParam( sTrack.Param() );
          track0.SetInnerAlpha( slices[iSlice]->Param().Alpha() );
//  track.SetOuterParam( AliHLTTPCCATrackParam( vEndPoint,   iV ) );
//  track.SetOuterAlpha( vEndAlpha[iV] );
          track0.SetFirstClusterRef( nClustersCurrent );
          track0.SetNClusters( mTrack.NClusters() );
          track0.fInnerRow = (fClusterInfos[mTrack.FirstClusterRef()]).IRow();
          for( unsigned int i = 0; i < mTrack.NClusters(); i++ ) {
            tmpH[nClustersCurrent + i] = fClusterInfos[mTrack.FirstClusterRef() + i];
          }
          nTracksCurrent++;
          fSliceNTrackInfos[ iSlice ]++;
          nClustersCurrent += mTrack.NClusters();
          continue;
        }
#endif
// ---
      }
      tr_usedN[nTracksVectorN] = itr;
      tr_slice[nTracksVectorN] = iSlice;
      for ( int iTrClu = 0; iTrClu < mTrack.NClusters(); iTrClu++ ) {
        // unpack cluster information
	int ic = mTrack.FirstClusterRef() + iTrClu;
        hitsN[iTrClu][nTracksVectorN] = ic;
        nHitsN[nTracksVectorN]++;
      }
      if( mergeTr ) {
      	const AliHLTTPCCASliceOutput &sliceNext = *( fkSlices[mTrack.SliceNextNeighbour()] );
      	AliHLTTPCCASliceTrackInfo &mTrack2 = fTrackInfos[mTrack.NextNeighbour()];
      	int maxHits = nHitsN[nTracksVectorN] + mTrack2.NClusters();
      	int id = mTrack2.FirstClusterRef();
      	for ( int iTrClu = nHitsN[nTracksVectorN]; iTrClu < maxHits; iTrClu++ ) {
      	  // unpack cluster information
      	  int ic = mTrack2.FirstClusterRef() + iTrClu;
      	  hitsN[iTrClu][nTracksVectorN] = id;
      	  nHitsN[nTracksVectorN]++;
      	  id++;
      	}
      }
      pStartPointN[nTracksVectorN] = &sTrack.Param();
      startAlphaN[nTracksVectorN] = slices[iSlice]->Param().Alpha();
      endAlphaN[nTracksVectorN]   = startAlphaN[nTracksVectorN];
      nTracksVectorN++;
      if( nTracksVectorN == float_v::Size || iTr >= fTracksTotal - 1 ) {
	float_m fitted = float_m(true);
	fitted &= static_cast<float_m>(static_cast<uint_v>(nHitsN) >= 4);
	const uint_v NAllHits(nHitsN);

	fitted &= static_cast<float_m>( uint_v( Vc::IndexesFromZero ) < nTracksVectorN );
	mergedN &= static_cast<float_m>( uint_v( Vc::IndexesFromZero ) < nTracksVectorN );
	AliHLTTPCCATrackParamVector vEndPoint;
	ConvertPTrackParamToVector(pStartPointN,vEndPoint,nTracksVectorN); // save as end because it will be fitted
	float_v vEndAlpha(Vc::Zero);
	vEndAlpha = startAlphaN;

	uint_v firstHits(Vc::Zero);
	// refit in the forward direction: going from the first hit to the last, mask "fitted" marks with 0 tracks, which are not fitted correctly
	if( !fitted.isFull() ) {
	  uint_m skip(!fitted);
	  nHitsN(skip) = uint_v(0);
	}
	fitted &= FitTrack( vEndPoint,   vEndAlpha,   hitsN, firstHits, nHitsN, nHitsN, nTracksVectorN, fitted, 0 );
	// if chi2 per degree of freedom > 3. sigma - mark track with 0
        fitted &= vEndPoint.Chi2()  < 18.f*static_cast<float_v>(vEndPoint.NDF());

	AliHLTTPCCATrackParamVector vStartPoint(vEndPoint);
	float_v vStartAlpha(vEndAlpha);
	// refit in the backward direction: going from the last hit to the first
	fitted &= FitTrack( vStartPoint, vStartAlpha, hitsN, firstHits, nHitsN, nHitsN, nTracksVectorN, fitted, 1 );
	// if chi2 per degree of freedom > 3. sigma - mark track with 0
	fitted &= vStartPoint.Chi2() < 9.f*static_cast<float_v>(vStartPoint.NDF());	//TODO: improve this cut

	for(int iV=0; iV<float_v::Size; iV++) {
	  if(!fitted[iV]) continue;
	  // if the track fitted correctly store the track
	  AliHLTTPCCASliceTrackInfo &track = tmpT[nTracksCurrent];
	  AliHLTTPCCASliceTrackInfo &mTrack1 = fTrackInfos[fptSliceFirstTrack[int(tr_slice[iV])] + int(tr_usedN[iV])];
	  if( mTrack1.Used() == 1 ) {
	    AliHLTTPCCASliceTrackInfo &track2 = fTrackInfos[mTrack1.NextNeighbour()];
	    track.SetUsed(7);
	    mTrack1.SetUsed(7);
	    track2.SetUsed(-1);
	    mergeCounter--;
	  }
	  else {
	    mTrack1.SetUsed(3);
	    track.SetUsed(3);
	  }
	  track.SetInnerParam( AliHLTTPCCATrackParam( vStartPoint, iV ) );
	  track.SetInnerAlpha( vStartAlpha[iV] );
	  track.SetOuterParam( AliHLTTPCCATrackParam( vEndPoint,   iV ) );
	  track.SetOuterAlpha( vEndAlpha[iV] );
	  track.SetFirstClusterRef( nClustersCurrent );
	  track.SetNClusters( nHitsN[iV] );
	  track.fInnerRow = (fClusterInfos[hitsN[0][iV]]).IRow();
	  for( unsigned int i = 0; i < nHitsN[iV]; i++ ) {
	    tmpH[nClustersCurrent + i] = fClusterInfos[hitsN[i][iV]];
	  }
	  nTracksCurrent++;
	  fSliceNTrackInfos[ iSlice ]++;
	  nClustersCurrent += nHitsN[iV];
	}
	nTracksVectorN = 0;
      }
  }
#endif
  // ---
#ifndef FULLFIT
  for( int iSlice = 0; iSlice < fgkNSlices; iSlice++ ) {
    fSliceTrackInfoStart[ iSlice ] = nTracksCurrent;
    fSliceNTrackInfos[ iSlice ] = 0;

    if ( !fkSlices[iSlice] ) continue;
    const AliHLTTPCCASliceOutput &slice = *( fkSlices[iSlice] );

    float_v startAlpha;
    float_v endAlpha;
    int hits[1000][uint_v::Size];
    uint_v nHits;//, nHits1;
    const AliHLTTPCCATrackParam *pStartPoint[uint_v::Size] = {0};
    int nTracksVector(0);
    unsigned int nCluNew(0);

    std::sort(&(fTrList[fptSliceFirstTrack[iSlice]]), &(fTrList[fptSliceFirstTrack[iSlice+1]]), TrSort::trComp);
    float_m merged;
    int_v tr_used;
    for( int itr1 = 0; itr1 <= slice.NTracks(); itr1++ ) {
	if( itr1 < slice.NTracks() ) {
      int itr = fTrList[fptSliceFirstTrack[iSlice] + itr1].trId;
      if( nTracksVector == 0 ) nHits = uint_v(0);
      bool mergeTr(false);
      AliHLTTPCCASliceTrackInfo &mTrack = fTrackInfos[fptSliceFirstTrack[iSlice] + itr];
      if( mTrack.Used() == 1 || mTrack.Used() == 2 ) mergeCounter++;
      if( mTrack.Used() > 1 || mTrack.Used() < 0 ) continue;
      if(mTrack.Used() == 1) {
        merged[nTracksVector] = true;
	mergeTr = true;
      }
      else {
        merged[nTracksVector] = false;
      }
      tr_used[nTracksVector] = itr;
      const AliHLTTPCCASliceTrack &sTrack = slice.Track( itr );
      for ( int iTrClu = 0; iTrClu < mTrack.NClusters(); iTrClu++ ) {
        // unpack cluster information
        int ic = mTrack.FirstClusterRef() + iTrClu;
        hits[iTrClu][nTracksVector] = ic;
        nHits[nTracksVector]++;
      }
      if( mergeTr ) {
	  const AliHLTTPCCASliceOutput &sliceNext = *( fkSlices[mTrack.SliceNextNeighbour()] );
	  AliHLTTPCCASliceTrackInfo &mTrack2 = fTrackInfos[mTrack.NextNeighbour()];
	  int maxHits = nHits[nTracksVector] + mTrack2.NClusters();
	  int id = mTrack2.FirstClusterRef();
	  for ( int iTrClu = nHits[nTracksVector]; iTrClu < maxHits; iTrClu++ ) {
	          // unpack cluster information
	    int ic = mTrack2.FirstClusterRef() + iTrClu;
	    hits[iTrClu][nTracksVector] = id;
	    nHits[nTracksVector]++;
	    id++;
	  }
      }
      pStartPoint[nTracksVector] = &sTrack.Param();
      startAlpha[nTracksVector] = slices[iSlice]->Param().Alpha();
      endAlpha[nTracksVector]   = startAlpha[nTracksVector];
      nTracksVector++;
	}
      assert( nTracksVector <= float_v::Size );
      if( nTracksVector == float_v::Size || itr1 >= slice.NTracks() - 1 ) {
	AliHLTTPCCATrackParam* endParamCopy = new AliHLTTPCCATrackParam[float_v::Size];
	AliHLTTPCCATrackParam* startParamCopy = new AliHLTTPCCATrackParam[float_v::Size];
        float_m fitted = float_m(true);
        fitted &= static_cast<float_m>(static_cast<uint_v>(nHits) >= 4);
        const uint_v NAllHits(nHits);
        fitted &= static_cast<float_m>( uint_v( Vc::IndexesFromZero ) < nTracksVector );
        merged &= static_cast<float_m>( uint_v( Vc::IndexesFromZero ) < nTracksVector );
        // refit the track
        // start from startPoint
        AliHLTTPCCATrackParamVector vEndPoint;
        ConvertPTrackParamToVector(pStartPoint,vEndPoint,nTracksVector); // save as end because it will be fitted
        float_v vEndAlpha(Vc::Zero);
        vEndAlpha = startAlpha;

        uint_v firstHits(Vc::Zero);
        // refit in the forward direction: going from the first hit to the last, mask "fitted" marks with 0 tracks, which are not fitted correctly
        if( !fitted.isFull() ) {
            uint_m skip(!fitted);
            nHits(skip) = uint_v(0);
        }
        fitted &= FitTrack( vEndPoint,   vEndAlpha,   hits, firstHits, nHits, nTracksVector, fitted, 0 );
        // if chi2 per degree of freedom > 3. sigma - mark track with 0
        fitted &= vEndPoint.Chi2()  < 18.f*static_cast<float_v>(vEndPoint.NDF());
        AliHLTTPCCATrackParamVector vStartPoint(vEndPoint);
        float_v vStartAlpha(vEndAlpha);
        // refit in the backward direction: going from the last hit to the first
        fitted &= FitTrack( vStartPoint, vStartAlpha, hits, firstHits, nHits, nTracksVector, fitted, 1 );
        // if chi2 per degree of freedom > 3. sigma - mark track with 0
        fitted &= vStartPoint.Chi2() < 9.f*static_cast<float_v>(vStartPoint.NDF());	//TODO: improve this cut
        for(int iV=0; iV<float_v::Size; iV++) {
          if(!fitted[iV]) continue;
          AliHLTTPCCASliceTrackInfo &mTrack0 = fTrackInfos[fptSliceFirstTrack[iSlice] + int(tr_used[iV])];
          // if the track fitted correctly store the track
          AliHLTTPCCASliceTrackInfo &track = tmpT[nTracksCurrent];
          AliHLTTPCCASliceTrackInfo &mTrack1 = fTrackInfos[fptSliceFirstTrack[iSlice] + int(tr_used[iV])];
          if( mTrack1.Used() == 1 ) {
              AliHLTTPCCASliceTrackInfo &track2 = fTrackInfos[mTrack1.NextNeighbour()];
              track.SetUsed(7);
              mTrack1.SetUsed(7);
              track2.SetUsed(-1);
              mergeCounter--;
// ---
  track.DzDs1 = mTrack1.DzDs1;
  track.QPt1 = mTrack1.QPt1;
// ---
          }
          else {
              mTrack1.SetUsed(3);
              track.SetUsed(3);
// ---
  track.DzDs1 = -1;
  track.QPt1 = -1;
// ---
          }

          track.SetInnerParam( AliHLTTPCCATrackParam( vStartPoint, iV ) );
          track.SetInnerAlpha( vStartAlpha[iV] );
          track.SetOuterParam( AliHLTTPCCATrackParam( vEndPoint,   iV ) );
          track.SetOuterAlpha( vEndAlpha[iV] );
          track.SetFirstClusterRef( nClustersCurrent );
          track.SetNClusters( nHits[iV] );
          track.fInnerRow = (fClusterInfos[hits[0][iV]]).IRow();
// ---
  track.DzDs0 = mTrack1.DzDs0;
  track.QPt0 = mTrack1.QPt0;
// ---
          for ( unsigned int i = 0; i < nHits[iV]; i++ ) {
              tmpH[nClustersCurrent + i] = fClusterInfos[hits[i][iV]];
          }
          nTracksCurrent++;
          fSliceNTrackInfos[ iSlice ]++;
          nClustersCurrent += nHits[iV];
        }
          nTracksVector = 0;
          delete [] endParamCopy;
          delete [] startParamCopy;
      }
    }
  }
#endif

  TrSort* unmergedTrList = new TrSort[mergeCounter];
  int refitCounter = 0;
#ifdef FULLFIT
  for( int iTr = 0; iTr < fTracksTotal; iTr++ ) {
    int itr = fTrList[iTr].trId;
    int iSlice = fTrList[iTr].iSlice;
    if ( !fkSlices[iSlice] ) continue;
    AliHLTTPCCASliceTrackInfo &mTrack = fTrackInfos[fptSliceFirstTrack[iSlice] + itr];
    if( mTrack.Used() == 1 || mTrack.Used() == 2 ) {
      unmergedTrList[refitCounter].trId = fptSliceFirstTrack[iSlice] + itr;
      unmergedTrList[refitCounter].nHits = mTrack.NClusters();
      unmergedTrList[refitCounter].iSlice = iSlice;
      refitCounter++;
    }
  }
#endif

#ifndef FULLFIT
  for(int iSlice = 0; iSlice < fgkNSlices; iSlice++) {
    if ( !fkSlices[iSlice] ) continue;
    const AliHLTTPCCASliceOutput &slice = *( fkSlices[iSlice] );
    for( int itr1 = 0; itr1 < slice.NTracks(); itr1++ ) {
      int itr = fTrList[fptSliceFirstTrack[iSlice] + itr1].trId;
      AliHLTTPCCASliceTrackInfo &mTrack = fTrackInfos[fptSliceFirstTrack[iSlice] + itr];
      if( mTrack.Used() == 1 || mTrack.Used() == 2 ) {
        unmergedTrList[refitCounter].trId = fptSliceFirstTrack[iSlice] + itr;
        unmergedTrList[refitCounter].nHits = mTrack.NClusters();
        unmergedTrList[refitCounter].iSlice = iSlice;
        refitCounter++;
      }
    }
  }
#endif
  std::sort(&(unmergedTrList[0]), &(unmergedTrList[refitCounter]), TrSort::trComp);
  const AliHLTTPCCATrackParam *pStartPoint1[uint_v::Size] = {0};
  int nTracksVector = 0;
  float_v startAlpha, endAlpha;
  uint_v nHits(Vc::Zero);
  int hits[100][int_v::Size];
  int firstUnmerged(nTracksCurrent), nUnmerged(0);
int_v trIds;
  for( int iTr = 0; iTr < refitCounter; iTr++ ) {
      const AliHLTTPCCASliceOutput &slice = *( fkSlices[unmergedTrList[iTr].iSlice] );
      const AliHLTTPCCASliceTrack &sTrack = slice.Track( unmergedTrList[iTr].trId - fptSliceFirstTrack[unmergedTrList[iTr].iSlice] );
      AliHLTTPCCASliceTrackInfo &mTrack = fTrackInfos[unmergedTrList[iTr].trId];
trIds[nTracksVector] = unmergedTrList[iTr].trId;
      pStartPoint1[nTracksVector] = &sTrack.Param();
      startAlpha[nTracksVector] = slices[unmergedTrList[iTr].iSlice]->Param().Alpha();
      endAlpha[nTracksVector]   = startAlpha[nTracksVector];

      for ( int iTrClu = 0; iTrClu < sTrack.NClusters(); iTrClu++ ) {
              int ic = mTrack.FirstClusterRef() + iTrClu;
              hits[iTrClu][nTracksVector] = ic;
              nHits[nTracksVector]++;
            }

      nTracksVector++;
      if( (nTracksVector == float_v::Size) || (iTr == (refitCounter-1)) ) {
	  float_m fitted = float_m(true);
	  fitted &= static_cast<float_m>(static_cast<uint_v>(nHits) >= 4);
	  fitted &= static_cast<float_m>( uint_v( Vc::IndexesFromZero ) < nTracksVector );
	  // refit the track
	  // start from startPoint
	  AliHLTTPCCATrackParamVector vEndPoint;
	  ConvertPTrackParamToVector(pStartPoint1,vEndPoint,nTracksVector); // save as end because it will be fitted
	  float_v vEndAlpha(Vc::Zero);
	  vEndAlpha = startAlpha;
	  uint_v firstHits(Vc::Zero);
	  fitted &= FitTrack( vEndPoint,   vEndAlpha,   hits, firstHits, nHits, nTracksVector, fitted, 0 );
	  // if chi2 per degree of freedom > 3. sigma - mark track with 0
	  fitted &= vEndPoint.Chi2()  < 9.f*static_cast<float_v>(vEndPoint.NDF());
	  AliHLTTPCCATrackParamVector vStartPoint(vEndPoint);
	  float_v vStartAlpha(vEndAlpha);
	  // refit in the backward direction: going from the last hit to the first
	  fitted &= FitTrack( vStartPoint, vStartAlpha, hits, firstHits, nHits, nTracksVector, fitted, 1 );
	  // if chi2 per degree of freedom > 3. sigma - mark track with 0
	  fitted &= vStartPoint.Chi2() < 9.f*static_cast<float_v>(vStartPoint.NDF());

	  for(int iV=0; iV<float_v::Size; iV++) {
	      if(!fitted[iV]) continue;
	      AliHLTTPCCASliceTrackInfo &track = tmpT[nTracksCurrent];
	      track.SetInnerParam( AliHLTTPCCATrackParam( vStartPoint, iV ) );
	      track.SetInnerAlpha( vStartAlpha[iV] );
	      track.SetOuterParam( AliHLTTPCCATrackParam( vEndPoint,   iV ) );
	      track.SetOuterAlpha( vEndAlpha[iV] );
	      track.SetFirstClusterRef( nClustersCurrent );
	      track.SetNClusters( nHits[iV] );
	      track.fInnerRow = (fClusterInfos[hits[0][iV]]).IRow();
              track.SetUsed(5);
// ---
  AliHLTTPCCASliceTrackInfo &mTrackT = fTrackInfos[((unsigned int)(trIds[iV]))];
  track.DzDs0 = mTrackT.DzDs0;
  track.QPt0 = mTrackT.QPt0;
  track.DzDs1 = -1;
  track.QPt1 = -1;
// ---
	      for ( unsigned int i = 0; i < nHits[iV]; i++ ) {
	        tmpH[nClustersCurrent + i] = fClusterInfos[hits[i][iV]];
	      }
	      nTracksCurrent++;
	      nUnmerged++;
	      nClustersCurrent += nHits[iV];
	  }
	  nTracksVector = 0;
	  nHits = 0;
      }
  }
  delete [] unmergedTrList;
  if (fClusterInfos) delete[] fClusterInfos;
  fClusterInfos = tmpH;
  fTrackInfos = tmpT;
  outTracks = new AliHLTTPCCAMergedTrack[nTracksCurrent];
  outClusterIDsrc = new DataCompressor::SliceRowCluster[nClustersCurrent];
  outClusterPackedAmp = new UChar_t [nClustersCurrent];

#ifdef FULLFIT
  for( int iTr = 0; iTr < nTracksCurrent; iTr++ ) {
    AliHLTTPCCASliceTrackInfo &mTrack = fTrackInfos[iTr];
    if( mTrack.Used() != 3 && mTrack.Used() != 7 && mTrack.Used() != 5 ) continue;
    const unsigned int NHits = mTrack.NClusters();
    outTracks[nOutTracks].AssignTrack(mTrack, nOutTrackClusters);
    if( mTrack.Used() == 7 ) outTracks[nOutTracks].SetMerged();
    for( unsigned int i = 0; i < NHits; i++ ) {
      AliHLTTPCCAClusterInfo &clu = fClusterInfos[mTrack.FirstClusterRef()+i];
      outClusterIDsrc[nOutTrackClusters+i] = DataCompressor::SliceRowCluster( clu.ISlice(), clu.IRow(), clu.IClu() );
    }
    nOutTracks++;
    nOutTrackClusters += NHits;
    nEndTracks++;
  }
#endif

#ifndef FULLFIT
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

    // -- Resort tracks to proceed faster --- ???
    vector<unsigned int> firstInChainIndex(fSliceNTrackInfos[iSlice]);
    int nChains = 0;

    // store tracks, which are not merged. And save indexes of the most previous(inner) merged tracks
    for(int iT=0; iT< fSliceNTrackInfos[iSlice]; iT++) {
      const int index = fSliceTrackInfoStart[iSlice] + iT;
      const AliHLTTPCCASliceTrackInfo& tr = fTrackInfos[index];
      if( tr.Used() != 3 && tr.Used() != 7 ) continue;
      const unsigned int NHits = tr.NClusters();

      // on the final stage store data to the global tracker
      outTracks[nOutTracks].AssignTrack(tr, nOutTrackClusters);
      if( tr.Used() == 7 ) outTracks[nOutTracks].SetMerged();
      for ( unsigned int i = 0; i < NHits; i++ ) {
        AliHLTTPCCAClusterInfo &clu = fClusterInfos[tr.FirstClusterRef()+i];
        outClusterIDsrc[nOutTrackClusters+i] = DataCompressor::SliceRowCluster( clu.ISlice(), clu.IRow(), clu.IClu() );
      }
      nOutTracks++;
      nOutTrackClusters += NHits;
      nTrNew[iSlice]++;
      nEndTracks++;
    } // if no merged
  }

  for( int iTr = firstUnmerged; iTr < firstUnmerged + nUnmerged; iTr++ ) {
      const AliHLTTPCCASliceTrackInfo& tr = fTrackInfos[iTr];
      const unsigned int NHits = tr.NClusters();
      outTracks[nOutTracks].AssignTrack(tr, nOutTrackClusters);
// ---
      if( tr.Used() == 7 ) outTracks[nOutTracks].SetMerged();
// ---
      for ( unsigned int i = 0; i < NHits; i++ ) {
        AliHLTTPCCAClusterInfo &clu = fClusterInfos[tr.FirstClusterRef()+i];
        outClusterIDsrc[nOutTrackClusters+i] = DataCompressor::SliceRowCluster( clu.ISlice(), clu.IRow(), clu.IClu() );
      }
      nOutTracks++;
      nOutTrackClusters += NHits;
  }
#endif

  for(int iSlice=0; iSlice < fgkNSlices; iSlice++ )
  {
    fSliceNTrackInfos[iSlice] = nTrNew[iSlice];
    fSliceTrackInfoStart[iSlice] = tmpSliceTrackInfoStart[iSlice];
  }
  fOutput->SetNTracks( nOutTracks );
  fOutput->SetNTrackClusters( nOutTrackClusters );
  fOutput->SetPointers();

  for ( int itr = 0; itr < nOutTracks; itr++ ) {
    fOutput->SetTrack( itr, outTracks[itr] );
    if( outTracks[itr].IsMerged() ) fOutput->SetMerged( itr );
  }

  for ( int ic = 0; ic < nOutTrackClusters; ic++ ) {
    fOutput->SetClusterIDsrc( ic, outClusterIDsrc[ic] );
    fOutput->SetClusterPackedAmp( ic, outClusterPackedAmp[ic] );
  }

    if (outTracks) delete[] outTracks;
    if (outClusterIDsrc) delete[] outClusterIDsrc;
    if (outClusterPackedAmp) delete[] outClusterPackedAmp;

#ifdef USE_TIMERS
  timer.Stop();
//  fTimers[5+(1-number)] = timer.RealTime();
#endif // USE_TIMERS
}
#endif

void AliHLTTPCCAMerger::Merging(int number)
{
#ifdef USE_TIMERS
  Stopwatch timer;
  timer.Start();
#endif // USE_TIMERS

#define MERGEFIX
  int nOutTracks = 0;
  int nOutTrackClusters = 0;

  AliHLTTPCCAMergedTrack *outTracks = 0;
  DataCompressor::SliceRowCluster *outClusterIDsrc = 0;
  UChar_t  *outClusterPackedAmp = 0;

  if(number == 0)
  {
    outTracks = new AliHLTTPCCAMergedTrack[fMaxTrackInfos + fNMergedSegments];
    outClusterIDsrc = new DataCompressor::SliceRowCluster[fMaxClusterInfos + fNMergedSegmentClusters];
    outClusterPackedAmp = new UChar_t [fMaxClusterInfos + fNMergedSegmentClusters];
  }

  AliHLTTPCCAClusterInfo *tmpH = new AliHLTTPCCAClusterInfo[fMaxClusterInfos + fNMergedSegmentClusters];
  Vc::vector<AliHLTTPCCASliceTrackInfo> tmpT(fMaxTrackInfos + fNMergedSegments);
  int nEndTracks = 0; // n tracks after merging.
  int tmpSliceTrackInfoStart[fgkNSlices];

  int nTrNew[fgkNSlices] = {0};

  int nH = 0;

#ifdef MERGEFIX
  int oldToNewTrackIndexes[fMaxTrackInfos + fNMergedSegments*2];
  vector<int> mergedSermentsOldIndexes;
#endif

// merge tracks, using obtained links to neighbours
  for ( unsigned int iSlice = 0; iSlice < fgkNSlices; iSlice++ ) {
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
#ifdef MERGEFIX
        if(number == 0) {
          outTracks[nOutTracks].AssignTrack(tr, nOutTrackClusters);
          for ( unsigned int i = 0; i < tr.NClusters(); i++ ) {
            AliHLTTPCCAClusterInfo &clu = fClusterInfos[tr.FirstClusterRef()+i];
            outClusterIDsrc[nOutTrackClusters+i] = DataCompressor::SliceRowCluster( clu.ISlice(), clu.IRow(), clu.IClu() );
          }
          if( tr.IsMerged() ) outTracks[nOutTracks].SetMerged();
            nOutTracks++;
            nOutTrackClusters += tr.NClusters();
        }

        if(number == 1) {
          AliHLTTPCCASliceTrackInfo &track0 = tmpT[nEndTracks];
          track0 = tr;
          track0.SetFirstClusterRef( nH );
          track0.ChiPrev = 1e10f;
          track0.ChiNext = 1e10f;
           track0.SetUsed( 0 );
           if( tr.IsMerged() ) track0.SetMerged();

          for( unsigned int iClu=0; iClu < tr.NClusters(); iClu++) tmpH[nH + iClu] = fClusterInfos[tr.FirstClusterRef()+iClu];
          nH += tr.NClusters();
          oldToNewTrackIndexes[index] = nEndTracks;
        }
        nTrNew[iSlice]++;
        nEndTracks++;
#endif
        continue;
      }

     if(tr.PrevNeighbour() >= 0 || tr.NextNeighbour() >= 0) {
#ifdef MERGEFIX
       if(number == 0) {
	 outTracks[nOutTracks].AssignTrack(tr, nOutTrackClusters);
	 for ( unsigned int i = 0; i < tr.NClusters(); i++ ) {
	   AliHLTTPCCAClusterInfo &clu = fClusterInfos[tr.FirstClusterRef()+i];
	   outClusterIDsrc[nOutTrackClusters+i] = DataCompressor::SliceRowCluster( clu.ISlice(), clu.IRow(), clu.IClu() );
	 }
          if( tr.IsMerged() ) outTracks[nOutTracks].SetMerged();
	 nOutTracks++;
	 nOutTrackClusters += tr.NClusters();
       }

       if(number == 1) {
         AliHLTTPCCASliceTrackInfo &track0 = tmpT[nEndTracks];
         track0 = tr;
         track0.SetFirstClusterRef( nH );
         track0.ChiPrev = 1e10f;
         track0.ChiNext = 1e10f;
          track0.SetUsed( 0 );
          if( tr.IsMerged() ) track0.SetMerged();

         for( unsigned int iClu=0; iClu < tr.NClusters(); iClu++) tmpH[nH + iClu] = fClusterInfos[tr.FirstClusterRef()+iClu];
         nH += tr.NClusters();
         oldToNewTrackIndexes[index] = nEndTracks;
       }
       nTrNew[iSlice]++;
       nEndTracks++;
#endif
       continue;
     }

      const unsigned int NHits = tr.NClusters();

        // on the final stage stor data to the global tracker
      if(number == 0) {
        outTracks[nOutTracks].AssignTrack(tr, nOutTrackClusters);

        for ( unsigned int i = 0; i < NHits; i++ ) {
          AliHLTTPCCAClusterInfo &clu = fClusterInfos[tr.FirstClusterRef()+i];
          outClusterIDsrc[nOutTrackClusters+i] = DataCompressor::SliceRowCluster( clu.ISlice(), clu.IRow(), clu.IClu() );
        }
        if( tr.IsMerged() ) outTracks[nOutTracks].SetMerged();
        nOutTracks++;
        nOutTrackClusters += NHits;
      }
        // else restore tracks, obtained after merging
      if(number == 1) {
        AliHLTTPCCASliceTrackInfo &track = tmpT[nEndTracks];

        track = tr;

        track.SetFirstClusterRef( nH );
        track.ChiPrev = 1e10f;
        track.ChiNext = 1e10f;
        if( tr.IsMerged() ) track.SetMerged();

        for( unsigned int iClu=0; iClu < NHits; iClu++) tmpH[nH + iClu] = fClusterInfos[tr.FirstClusterRef()+iClu];
        nH += NHits;
      }

#ifdef MERGEFIX
      oldToNewTrackIndexes[index] = nEndTracks;
#endif
      nTrNew[iSlice]++;
      nEndTracks++;
    } // if no merged

#ifdef MERGEFIX
    int_v segmentCounter( 0 );
    vector<int_v> segmentNumbers;
#endif

    for ( int itr = 0; ; ) {
#ifdef MERGEFIX
	segmentCounter = int_v( 0 );
	segmentNumbers.clear();
#endif
        // pack data
      int nVecElements = 0;
      uint_v iIndexes(Vc::Zero);
      for ( ; nVecElements < int(uint_v::Size) && itr < nChains; itr++ ) {
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
      if( ISUNLIKELY(!(invert1.isEmpty())) ) {
        AliHLTTPCCATrackParamVector helpPoint = vEndPoint;
        vEndPoint.SetTrackParam(vStartPoint, invert1);
        vStartPoint.SetTrackParam(helpPoint, invert1);
        float_v helpAlpha = vEndAlpha;
        vEndAlpha(invert1) = vStartAlpha;
        vStartAlpha(invert1) = helpAlpha;
      }

      const int_m &activeI = static_cast<int_m>(active);
      const uint_m &activeU = static_cast<uint_m>(active);
      uint_v vNHits(Vc::Zero);
      for( unsigned int i = 0; i < float_v::Size; i++ ) {
	if( !activeU[i] ) continue;
	vNHits[i] = fTrackInfos[(unsigned int)iIndexes[i]].NClusters();
      }

      int_v vFirstClusterRef(Vc::Zero);
      for( unsigned int i = 0; i < float_v::Size; i++ ) {
      	if( !activeI[i] ) continue;
      	vFirstClusterRef[i] = fTrackInfos[(unsigned int)iIndexes[i]].FirstClusterRef();
      }

      for ( unsigned int jhit = 0; jhit < vNHits.max(); jhit++ ) {
        const int_m mask = int_m(active) && int_m(jhit < vNHits);
        int_v id = vFirstClusterRef + static_cast<int>(jhit);
        for( unsigned int iV=0; iV<int_v::Size; iV++ ) {
          if(!mask[iV]) continue;
          hits[static_cast <unsigned int>(firstHit[iV])+jhit][iV] = id[iV];
        }
      }

      uint_v jIndexes = iIndexes;
#ifdef MERGEFIX
      segmentNumbers.push_back( iIndexes );
      segmentCounter( int_m( active ) )++;
#endif
      float_m isNeighbour = active;
      while (1) // while there are still outer neighbours
      {
        const int_m &isNeighbourI = static_cast<int_m>(isNeighbour);
        const uint_m &isNeighbourU = static_cast<uint_m>(isNeighbour);
        int_v vNextNeighbour(Vc::Zero);
        for( unsigned int i = 0; i < float_v::Size; i++ ) {
          if( !isNeighbourI[i] ) continue;
          vNextNeighbour[i] = fTrackInfos[(unsigned int)jIndexes[i]].NextNeighbour();
        }
        uint_v vSliceNextNeighbour(Vc::Zero);
        for( unsigned int i = 0; i < float_v::Size; i++ ) {
          if( !isNeighbourU[i] ) continue;
          vSliceNextNeighbour[i] = fTrackInfos[(unsigned int)jIndexes[i]].SliceNextNeighbour();
        }

        isNeighbour &= (static_cast<float_v>(vNextNeighbour) > -1);
        if (isNeighbour.isEmpty()) break;

          // take the next neighbour
        const int_m &isNeighbourI0 = static_cast<int_m>(isNeighbour);
        for( unsigned int i = 0; i < float_v::Size; i++ ) {
          if( !isNeighbourI0[i] ) continue;
          jIndexes[i] = fSliceTrackInfoStart[(unsigned int)vSliceNextNeighbour[i]];
        }
        jIndexes += vNextNeighbour;
        int_v vUsed(Vc::Zero);
        for( unsigned int i = 0; i < float_v::Size; i++ ) {
          if( !isNeighbourI0[i] ) continue;
          vUsed[i] = fTrackInfos[(unsigned int)jIndexes[i]].Used();
        }

        isNeighbour &= !static_cast<float_m>(vUsed > 0);

        if ( isNeighbour.isEmpty() ) break;

        isNeighbour &= AddNeighbour( jIndexes, nVecElements, isNeighbour,
                                     hits, firstHit, vStartPoint, vEndPoint, vStartAlpha, vEndAlpha, vNHits );
#ifdef MERGEFIX
        segmentNumbers.push_back( jIndexes );
        segmentCounter( int_m( isNeighbour ) )++;
#endif
      } // while isNeighbour

      const float_m &swap = active && (vEndPoint.X() < vStartPoint.X());
      if( !(swap.isEmpty())) {
        for( unsigned int iV=0; iV<float_v::Size; iV++ ) {
          if(!swap[iV]) continue;
          for ( unsigned int i = 0; i < (unsigned int)vNHits[iV]; i++ ) hits[i][iV] = hits[(unsigned int)(firstHit[iV]+vNHits[iV]-1-i)][iV];
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
       active &= float_m( vNHits == nHits );
      vNHits = nHits;

        // store tracks
      for( unsigned int iV=0; iV<float_v::Size; iV++ ) {
        if(!active[iV]) continue;

        int h[1000];
        for( unsigned int iClu = 0; iClu < vNHits[iV]; iClu++)
          h[iClu] = hits[iClu + (unsigned int)firstHit[iV]][iV];

        int *usedHits = h; // get begin of array
          // If track has been merged, resort hits, rid of double hits.
#ifndef AVX1V
          std::sort( usedHits, usedHits + vNHits[iV], TrackHitsCompare(fClusterInfos) ); // sort hits by X (iRow) // TODO normal sort
#endif

          // rid of double hits
        unsigned int ihit2 = 0; // ihit in the output array
        char irow = (unsigned int)fClusterInfos[usedHits[0]].IRow();
        for( unsigned int ihit = 1; ihit < vNHits[iV]; ihit++) {
          if( ISUNLIKELY(usedHits[ihit2] == usedHits[ihit]) ) continue;
          if( ISUNLIKELY( irow == fClusterInfos[usedHits[ihit]].IRow() ) ) continue;
          ++ihit2;
          irow = fClusterInfos[usedHits[ihit]].IRow();
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
          mergedTrack.SetMerged();
          mergedTrack.SetNoUsed();
          mergedTrack.SetNoLooper();

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
           track.SetUsed( 0 );
           track.SetMerged();
#ifdef MERGEFIX
         for( int ii = 0; ii < segmentCounter[iV]; ii++ ) {
           mergedSermentsOldIndexes.push_back( segmentNumbers[ii][iV] );
         }
#endif

          track.SetInnerParam( AliHLTTPCCATrackParam( vHelpStartPoint, iV ) );
          track.SetInnerAlpha( vHelpStartAlpha[iV] );
          track.SetOuterParam( AliHLTTPCCATrackParam( vHelpEndPoint, iV ) );
          track.SetOuterAlpha( vHelpEndAlpha[iV] );

          track.fInnerRow = (fClusterInfos[usedHits[0]]).IRow();
          track.fOuterRow = (fClusterInfos[usedHits[(unsigned int)nHits[iV]-1]]).IRow();

          for( unsigned int iClu=0; iClu < nHits[iV]; iClu++)
            tmpH[nH + iClu] = fClusterInfos[usedHits[iClu]];

          nH += nHits[iV];
        }
        nTrNew[iSlice]++;
        nEndTracks++;
      } // for iV

    } // for itr
  } // for iSlice
#ifdef MERGEFIX
  for( unsigned int i = 0; i < mergedSermentsOldIndexes.size(); i++ ) {
    tmpT[oldToNewTrackIndexes[mergedSermentsOldIndexes[i]]].SetUsed(1);
  }
#endif

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
    if ( fOutput ) delete[] ( ( char* )( fOutput ) );
    int size = fOutput->EstimateSize( nOutTracks, nOutTrackClusters );
    fOutput = ( AliHLTTPCCAMergerOutput* )( new float2[size/sizeof( float2 )+1] );

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
  const uint_m &maskU = static_cast<uint_m>(mask);
  uint_v jNHits(Vc::Zero);
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    if( !maskU[i] ) continue;
    jNHits[i] = fTrackInfos[(unsigned int)jIndexes[i]].NClusters();
  }
  float_v vInnerAlpha(Vc::Zero);
  float_v vOuterAlpha(Vc::Zero);
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    if( !mask[i] ) continue;
    vInnerAlpha[i] = fTrackInfos[(unsigned int)jIndexes[i]].InnerAlpha();
    vOuterAlpha[i] = fTrackInfos[(unsigned int)jIndexes[i]].OuterAlpha();
  }

  AliHLTTPCCATrackParamVector vInnerParam;
  AliHLTTPCCATrackParamVector vOuterParam;

  const AliHLTTPCCATrackParam *pInnerParam[int_v::Size] = {0};
  const AliHLTTPCCATrackParam *pOuterParam[int_v::Size] = {0};
  for( unsigned int iV = 0; iV < float_v::Size; iV++ ) {
    if(!mask[iV]) continue;
    AliHLTTPCCASliceTrackInfo &segment = fTrackInfos[jIndexes[iV]];
    pInnerParam[iV] = &segment.InnerParam();
    pOuterParam[iV] = &segment.OuterParam();

#ifndef MERGEFIX
    fTrackInfos[jIndexes[iV]].fUsed = 1;
#endif
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
    ( (dz11 >= dz00 && dz11 >= dz01 && dz11 >= dz10 ) ||
    ( CAMath::Abs( dz00 - dz11 ) < float_v(1.) && CAMath::Abs( dz11 - dz10 ) < float_v(1.) &&CAMath::Abs( dz01 - dz11 ) < float_v(1.) ) );
    /*                  0----1
     * connection like : \
     *                    0------1
     */
  const float_m &case2 = mask &&
    (d01 < d00 && d01 <= d10 && d01 <= d11) &&
    ( (dz10 > dz00 && dz10 >= dz01 && dz10 >= dz11 ) ||
    ( CAMath::Abs( dz10 - dz00 ) < float_v(1.) && CAMath::Abs( dz01 - dz10 ) < float_v(1.) &&CAMath::Abs( dz10 - dz11 ) < float_v(1.) ) );
    /*                       0----1
     * connection like :      \
     *                    0----1
     */
  const float_m &case3 = mask &&
    (d10 < d00 && d10 <= d01 && d10 <= d11) &&
    ( ( dz01 > dz00 && dz01 >= dz10 && dz01 >= dz11 ) ||
    ( CAMath::Abs( dz01 - dz00 ) < float_v(1.) && CAMath::Abs( dz01 - dz10 ) < float_v(1.) &&CAMath::Abs( dz01 - dz11 ) < float_v(1.) ) );
    /*                   0---1
     * connection like :    /
     *                     0-------1
     */
  const float_m &case4 = mask &&
    (d11 < d00 && d11 <= d10 && d10 <= d01) &&
    ( (dz00 > dz01 && dz00 >= dz10 && dz00 > dz11) ||
    ( CAMath::Abs( dz01 - dz00 ) < float_v(1.) && CAMath::Abs( dz00 - dz10 ) < float_v(1.) &&CAMath::Abs( dz00 - dz11 ) < float_v(1.) ) );
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
  int_v jFirstClusterRef(Vc::Zero);
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    if( !mask[i] ) continue;
    jFirstClusterRef[i] = fTrackInfos[(unsigned int)jIndexes[i]].FirstClusterRef();
  }

  for( unsigned int iV=0; iV<float_v::Size; iV++ ) {
    if(!mask[iV]) continue;
    for ( unsigned int jhit = 0; jhit < jNHits[iV]; jhit++ ) {
      const int& id = jFirstClusterRef[iV] + jhit;
      const unsigned int& index = HitIndex( startHit, jNHits, dir[iV], iV, jhit );
      hits[index][iV] = id;
    }
  }
  vNHits(static_cast<uint_m>(mask)) += jNHits;
  return mask;
}
