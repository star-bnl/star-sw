// @(#) $Id: AliHLTTPCCATrackletConstructor.cxx,v 1.8 2012/08/14 16:30:42 fisyak Exp $
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


#include "AliHLTTPCCATracker.h"
#include "AliHLTTPCCATrackParamVector.h"
#include "AliHLTTPCCAParameters.h"
#include "AliHLTTPCCAGrid.h"
#include "AliHLTTPCCAMath.h"
#include "AliHLTTPCCADef.h"
#include "AliHLTTPCCATracklet.h"
#include "AliHLTTPCCATrackletConstructor.h"
#include "AliHLTArray.h"
#include "debug.h"
#include <iomanip>

#ifdef DRAW3
#include "AliHLTTPCCADisplay.h"
#endif //DRAW3

#ifdef DRAW2
#include "AliHLTTPCCADisplay.h"
#endif //DRAW2

#ifdef DRAW
#include "AliHLTTPCCADisplay.h"
#endif //DRAW

#include <limits>
#include <Vc/limits>

#include <valgrind/memcheck.h>


 //#define LOSE_DEBUG

using std::endl;

/*
 * Tracklets start as WaitingForFitLinkedHits. Once the start row is reached they go to
 * FitLinkedHits. When the seed is fitted they go to ExtrapolateUp. At some row upwards
 * extrapolation will stop and the tracklet goes into WaitingForExtrapolateDown. Once all tracklets
 * in the vector are done with ExtrapolateUp down extrapolation starts from the highest row where
 * fitting was done. Once a tracklet reaches its fEndRow it goes to the ExtrapolateDown state until
 * extrapolation is done, when it will go into DoneStage.
 *
 * Tracklets in the vector whose trackIndex is < nTracks are in the NullStage.
 */
enum Stage_t {
  WaitingForFitLinkedHits = 0,
  FitLinkedHits,
  ExtrapolateUp,
  WaitingForExtrapolateDown,
  ExtrapolateDown,
  DoneStage,
  NullStage
};

struct AliHLTTPCCATrackletConstructor::TrackMemory {
  short_v fStartRow;  // min row index with only the fitted part
  ushort_v fEndRow;    // max row index with only the fitted part
  short_v fFirstRow;  // min row index with the extrapolated parts
  ushort_v fLastRow;   // max row index with the extrapolated parts
  short_v fCurrentHitIndex; // indef of the current hit
  short_v fStage; // reco stage
  ushort_v fNHits; // n track hits
  short_v fRemainingGap; // n missed hits during search
  sfloat_v fLastY; // Y of the last fitted cluster
  sfloat_v fLastZ; // Z of the last fitted cluster
  TrackParamVector fParam;

  inline TrackMemory()
    : fStartRow( Vc::Zero ),
    fCurrentHitIndex( -1 ),
    fStage( Vc::Zero ),
    fNHits( 2 ), // the first two hits are certain
    fRemainingGap( AliHLTTPCCAParameters::MaximumExtrapolationRowGap ),
    fLastY( Vc::Zero ),
    fLastZ( Vc::Zero )
  {}
};

class InitTracklets
{
  public:
    InitTracklets( AliHLTTPCCATrackletConstructor::TrackMemory &_r, const SliceData &data,
        const Tracker &tracker,
        TrackletVector &trackletVector, const ushort_v &_trackIndex )
      : r( _r ), fData( data ), fTracker( tracker ),
      fTrackletVector( trackletVector ), trackIndex( _trackIndex ) {}

    void operator()( int rowIndex );

  private:
    AliHLTTPCCATrackletConstructor::TrackMemory &r;
    const SliceData &fData;
    const Tracker &fTracker;
    TrackletVector &fTrackletVector;
    const ushort_v &trackIndex;
};

/**
 * Fit tracklet to the string of hits connected by links
 */
inline void AliHLTTPCCATrackletConstructor::FitTracklet( TrackMemory &r, int rowIndex,
    ushort_v trackIndex, TrackletVector &trackletVector )
{
  const AliHLTTPCCARow &row = fData.Row( rowIndex );

  short_m active = r.fStage == FitLinkedHits;
  
  const int rowStep = AliHLTTPCCAParameters::RowStep;
  assert( rowStep <= 2 );
  if (rowStep == 2){
    active = active && ( r.fStartRow & short_v( Vc::One ) ) == ( rowIndex & 1 );
  }
  
  if ( ISUNLIKELY( active.isEmpty() ) ) {
    return;
  }
  const sfloat_m activeF( active );
  debugF() << "FitTracklet(" << rowIndex << ") stage: " << r.fStage << " r.fStartRow: " << r.fStartRow << " active: " << active << endl;

  assert( active == ( active && r.fStage == FitLinkedHits ) );
  assert( active == ( active && r.fCurrentHitIndex >= 0 ) );
  assert( active == ( active && rowIndex > r.fStartRow ) );
  //assert( r.fActive == ( ( rowIndex - r.fStartRow ) % 2 == 0 ) );
  assert( active == ( active && r.fCurrentHitIndex < row.NHits() ) );

  const ushort_v oldHitIndex = static_cast<ushort_v>( r.fCurrentHitIndex );
#ifndef NVALGRIND
  VALGRIND_CHECK_VALUE_IS_DEFINED( active );
  VALGRIND_CHECK_VALUE_IS_DEFINED( oldHitIndex );
  for ( int i = 0; i < oldHitIndex.Size; ++i ) {
    if ( active[i] ) {
      debugF() << rowIndex << " - " << i << " - " << oldHitIndex[i] << endl;
      VALGRIND_CHECK_VALUE_IS_DEFINED( fData.HitLinkUpData( row )[oldHitIndex[i]] );
    }
  }
#endif
  r.fCurrentHitIndex.gather( fData.HitLinkUpData( row ), oldHitIndex, active ); // set to next linked hit
  debugF() << "r.fCurrentHitIndex changed from " << oldHitIndex << " to " << r.fCurrentHitIndex << endl;

  const sfloat_v x = fData.RowX( rowIndex ); // convert to sfloat_v once now
  const sfloat_v y( fData.HitDataY( row ), oldHitIndex, active );
  const sfloat_v z( fData.HitDataZ( row ), oldHitIndex, active );
  VALGRIND_CHECK_VALUE_IS_DEFINED( x );
  VALGRIND_CHECK_VALUE_IS_DEFINED( y );
  VALGRIND_CHECK_VALUE_IS_DEFINED( z );

  debugF() << "x, y, z: " << x << y << z << endl;

    // correct SinPhi if it is necessary
    // calculate displacement to previous hit
  const sfloat_v dx = x - r.fParam.X();
  VALGRIND_CHECK_VALUE_IS_DEFINED( dx );
  VALGRIND_CHECK_VALUE_IS_DEFINED( r.fLastY );
  const sfloat_v dy = y - r.fLastY;
  VALGRIND_CHECK_VALUE_IS_DEFINED( dy );
  const sfloat_v dz = z - r.fLastZ;
  debugF() << "dx, dy, dz: " << dx << dy << dz << endl;
  r.fLastY( activeF ) = y;
  r.fLastZ( activeF ) = z;

  sfloat_v err2Y, err2Z;
  sfloat_v sinPhi = r.fParam.GetSinPhi();
  VALGRIND_CHECK_VALUE_IS_DEFINED( sinPhi );

  const sfloat_v ri = sfloat_v( Vc::One ) / CAMath::Sqrt( dx * dx + dy * dy ); // RSqrt
  const sfloat_m fragile =
      static_cast<sfloat_m>( r.fNHits < AliHLTTPCCAParameters::MinimumHitsForFragileTracklet ) || CAMath::Abs( r.fParam.SinPhi() ) >= .99f;
  VALGRIND_CHECK_VALUE_IS_DEFINED( fragile );
  sinPhi( fragile ) = dy * ri;
  VALGRIND_CHECK_VALUE_IS_DEFINED( sinPhi );

  assert( ( x == 0 && activeF ).isEmpty() );
  const sfloat_m transported =
    r.fParam.TransportToX( x, sinPhi, fTracker.Param().cBz(), -1.f, activeF );
#ifdef LOSE_DEBUG
  if (!(activeF && (!transported)).isEmpty()) std::cout << "FitTracklet-TransportToX(...) lose track: " << (activeF && (!transported)) << std::endl;
#endif
  VALGRIND_CHECK_VALUE_IS_DEFINED( sinPhi );
///mvz start 20.01.2010
//  fTracker.GetErrors2( rowIndex, r.fParam.GetZ(), sinPhi, r.fParam.GetDzDs(), &err2Y, &err2Z );
  fTracker.GetErrors2( rowIndex, r.fParam, &err2Y, &err2Z );
///mvz end 20.01.2010
//   std::cout << err2Y << " " << err2Z << std::endl;
//   err2Y = 0.12*0.12; err2Z = 0.16*0.16;
  
  VALGRIND_CHECK_VALUE_IS_DEFINED( active );
  {
    const short_m hitAdded = active &&
      static_cast<short_m>( r.fParam.Filter( transported, y, z, err2Y, err2Z, .99f ) );
    VALGRIND_CHECK_VALUE_IS_DEFINED( hitAdded );
    trackletVector.SetRowHits( rowIndex, trackIndex, oldHitIndex, hitAdded );
    ++r.fNHits( hitAdded );
    r.fEndRow( hitAdded ) = rowIndex;
  }

  VALGRIND_CHECK_VALUE_IS_DEFINED( r.fCurrentHitIndex );
  const short_m fittingDone = r.fCurrentHitIndex < 0 && active;
  debugF() << "fittingDone = " << fittingDone << endl;
  if ( ISUNLIKELY( !fittingDone.isEmpty() ) ) {
    ++r.fStage( fittingDone ); // goes to ExtrapolateUp if fitting is done (no other hit linked)
    r.fLastRow( fittingDone ) = rowIndex;
    debugF() << "hits: " << r.fNHits << ", sinphi: " << r.fParam.SinPhi() << endl;
    const short_m invalidTracklet = ( r.fNHits < 3
        || static_cast<short_m>( CAMath::Abs( r.fParam.SinPhi() ) > .999f ) ) && fittingDone;
    r.fNHits.setZero( invalidTracklet );
    r.fStage( invalidTracklet ) = DoneStage; // XXX: NullStage?
    debugF() << "r.fStage: " << r.fStage << endl;
  }
}

/**
 * Now that we have a tracklet try to find more hits that would fit with this tracklet
 */
short_m AliHLTTPCCATrackletConstructor::ExtrapolateTracklet( TrackMemory &r, int rowIndex, ushort_v trackIndex,
   TrackletVector &trackletVector, const short_m &mask )
{
  //assert( active == ( active && ( r.fStage == ExtrapolateUp || r.fStage == ExtrapolateDown ) ) );
  // reconstruction of tracklets, tracklets update step

  const AliHLTTPCCARow &row = fData.Row( rowIndex );

  short_m active = mask;
  --r.fRemainingGap( active );
  debugF() << "ExtrapolateTracklet(" << rowIndex << ") fRemainingGap: " << r.fRemainingGap << endl;

  const sfloat_v x = fData.RowX( rowIndex );

//     // correct SinPhi if it is necessary
//     // calculate displacement to previous hit
//   const sfloat_v dx = x - r.fParam.X();
//   VALGRIND_CHECK_VALUE_IS_DEFINED( dx );
//   VALGRIND_CHECK_VALUE_IS_DEFINED( r.fLastY );
//   const sfloat_v dy = y - r.fLastY;
//   VALGRIND_CHECK_VALUE_IS_DEFINED( dy );
//   const sfloat_v dz = z - r.fLastZ;
//   debugF() << "dx, dy, dz: " << dx << dy << dz << endl;
//   r.fLastY( activeF ) = y;
//   r.fLastZ( activeF ) = z;
// 
//   sfloat_v err2Y, err2Z;
//  sfloat_v sinPhi = r.fParam.GetSinPhi();
//   VALGRIND_CHECK_VALUE_IS_DEFINED( sinPhi );
// 
//   const sfloat_v ri = sfloat_v( Vc::One ) / CAMath::Sqrt( dx * dx + dy * dy ); // RSqrt
//  const sfloat_m fragile =
//      static_cast<sfloat_m>( r.fNHits < AliHLTTPCCAParameters::MinimumHitsForFragileTracklet ) || CAMath::Abs( r.fParam.SinPhi() ) >= .99f;
//   VALGRIND_CHECK_VALUE_IS_DEFINED( fragile );
//   sinPhi( fragile ) = dy * ri;
//   VALGRIND_CHECK_VALUE_IS_DEFINED( sinPhi );
#ifdef LOSE_DEBUG
//   if (!(active && (fragile)).isEmpty()) std::cout << "ExtrapolateTracklet-SinPhi is bad: " << (active && (fragile)) << " SinPhi = " << sinPhi << " NTrackletHits = " << r.fNHits << std::endl;
#endif

  
  sfloat_m activeF( active );
  assert( ( x == 0 && activeF ).isEmpty() );

//   sfloat_m transport = r.fParam.TransportToXWithMaterial( x,  fTracker.Param().Bz(), .99f );
//   sfloat_m transport = r.fParam.TransportToX( x, sinPhi, fTracker.Param().cBz(), .99f, activeF );
   sfloat_m transport = r.fParam.TransportToX( x, r.fParam.SinPhi(), fTracker.Param().cBz(), .99f, activeF );
#ifdef LOSE_DEBUG
//   if (!(activeF && (!transport)).isEmpty())
    std::cout << "ExtrapolateTracklet-TransportToX(...) lose track: " << (activeF && (!transport))
//        << " SinPhi = " << sinPhi << " NTrackletHits = " << r.fNHits
        << std::endl
        << " x = " << x << " y = " << r.fParam.Y()
//         << " z = " << r.fParam.Z()
        << " p = " << 1./r.fParam.QPt()
        << std::endl
        << " Err2Y = " << r.fParam.GetErr2Y()
        << " Err2Z = " << r.fParam.GetErr2Z()
        << " Err2SinPhi = " << r.fParam.GetErr2SinPhi()
        << " Err2DzDs = " << r.fParam.GetErr2DzDs()
        << " Err2QPt = " << r.fParam.GetErr2QPt()
        << std::endl;
#endif
  activeF &= transport;

  active = static_cast<short_m>( activeF );
  if ( row.NHits() < 1 || active.isEmpty() ) {
    ++r.fStage( r.fRemainingGap == 0 && active ); // go to WaitingForExtrapolateDown or DoneStage if the gap got too big
#ifndef NODEBUG
    debugF() << "r.fStage: " << r.fStage << " after ExtrapolateTracklet found empty row and fRemainingGap == " << r.fRemainingGap << endl;
#endif
    // no hits in this row, skip (don't do this before TransportToX)
    return short_m( Vc::Zero );
  }

  //std::cout << std::setw( 4 ) << rowIndex << ": " << active << endl;

  const sfloat_v fY = r.fParam.Y();
  const sfloat_v fZ = r.fParam.Z();
  const ushort_v fIndYmin = fData.Row( rowIndex ).Grid().GetBinBounded( fY - 1.f, fZ - 1.f );
  ushort_v::Memory fHitIndexes;
  sfloat_v::Memory fBestY, fBestZ;
  fHitIndexes = std::numeric_limits<ushort_v>::max();

  foreach_bit( int trackletIndex, active ) {
    float minRadius2 = std::numeric_limits<float>::max();

    const float y = fY[trackletIndex];
    const float z = fZ[trackletIndex];
    const int indYmin = fIndYmin[trackletIndex];

    {
      const int end = fData.FirstHitInBin( row, indYmin + 2 );
      for ( int hitIndex = fData.FirstHitInBin( row, indYmin ); hitIndex < end; hitIndex += float_v::Size ) {
        float_v yz; 
	yz.load( fData.HitDataY( row ) + hitIndex, Vc::Unaligned);
        const float_v dy = yz - y;
	yz.load( fData.HitDataZ( row ) + hitIndex, Vc::Unaligned);
//        const float_v dz = float_v::loadUnaligned( fData.HitDataZ( row ) + hitIndex ) - z;
        const float_v dz = yz - z;
        float_v radius2 = std::numeric_limits<float_v>::max();
        radius2( uint_v( Vc::IndexesFromZero ) + hitIndex < end ) = dy * dy + dz * dz; // XXX Manhattan distance
//         std::cout << y << " dy = "  << dy << std::endl  <<
//             z << " dz = "  << dz << std::endl  <<
//             "  Manhattan distance = " << radius2 << std::endl;
        const float min = radius2.min();
        if ( min < minRadius2 ) {
          minRadius2 = min;
          int i = ( radius2 == min ).firstOne();
          assert( minRadius2 == radius2[i] );
          fBestY[trackletIndex] = dy[i];
          fBestZ[trackletIndex] = dz[i];
          fHitIndexes[trackletIndex] = hitIndex + i;
        }
      }
    }
    {
      const int nY = row.Grid().Ny();
      const int end = fData.FirstHitInBin( row, indYmin + nY + 2 );
      for ( int hitIndex = fData.FirstHitInBin( row, indYmin + nY ); hitIndex < end; hitIndex += float_v::Size ) {
        float_v yz; 
	yz.load( fData.HitDataY( row ) + hitIndex, Vc::Unaligned );
        const float_v dy = yz - y;
	yz.load( fData.HitDataZ( row ) + hitIndex, Vc::Unaligned );
        const float_v dz = yz - z;
//        const float_v dy = float_v::loadUnaligned( fData.HitDataY( row ) + hitIndex ) - y;
//        const float_v dz = float_v::loadUnaligned( fData.HitDataZ( row ) + hitIndex ) - z;
        float_v radius2 = std::numeric_limits<float_v>::max();
        radius2( uint_v( Vc::IndexesFromZero ) + hitIndex < end ) = dy * dy + dz * dz; // XXX Manhattan distance
        const float min = radius2.min();
        if ( min < minRadius2 ) {
          minRadius2 = min;
          int i = ( radius2 == min ).firstOne();
          assert( minRadius2 == radius2[i] );
          fBestY[trackletIndex] = dy[i];
          fBestZ[trackletIndex] = dz[i];
          fHitIndexes[trackletIndex] = hitIndex + i;
        }
      }
    }
  }

  const ushort_v best( fHitIndexes );
  active &= best != std::numeric_limits<ushort_v>::max();
  // XXX use this branch? with 8 or 16 tracklets extrapolated at the same time this might be an
  // unlikely return? Also see above.
//X   if ( active.isEmpty() ) {
//X     return;
//X   }

  sfloat_v err2Y, err2Z;
///mvz start 20.01.2010
//  fTracker.GetErrors2( rowIndex, r.fParam, &err2Y, &err2Z ); // IKu !? give very small error !?
  fTracker.GetErrors2( rowIndex, r.fParam, &err2Y, &err2Z );
///mvz end 20.01.2010

//   std::cout << err2Y << " " << err2Z << std::endl;
//   err2Y = 0.12*0.12; err2Z = 0.16*0.16;
  
  const sfloat_v dy( fBestY );
  const sfloat_v dz( fBestZ );

  const sfloat_v kFactor = AliHLTTPCCAParameters::HitPickUpFactor * AliHLTTPCCAParameters::HitPickUpFactor * 3.5f * 3.5f;
  const sfloat_v two( 2.f );

  const sfloat_v sy2 = CAMath::Min( two, kFactor * ( r.fParam.GetErr2Y() + err2Y ) );
  const sfloat_v sz2 = CAMath::Min( two, kFactor * ( r.fParam.GetErr2Z() + err2Z ) );

  activeF = static_cast<sfloat_m>( active );
  debugF() << "activeF: " << activeF;
  activeF &= dy * dy <= sy2 && dz * dz <= sz2;
  debugF() << " -> " << activeF;
  sfloat_m filtred = r.fParam.FilterDelta( activeF, dy, dz, err2Y, err2Z, .99f );
#ifdef LOSE_DEBUG
  if (!(activeF && (!filtred)).isEmpty()) std::cout << "ExtrapolateTracklet-FilterDelta(...) lose track: " << (activeF && (!filtred)) << std::endl;
#endif
  activeF &= filtred;
  debugF() << " -> " << activeF;
  active = static_cast<short_m>( activeF );
  debugF() << " -> " << active << endl;
  
  debugF() << "SetRowHits( " << rowIndex << ", " << best << ", " << active << ")" << endl;

  trackletVector.SetRowHits( rowIndex, trackIndex, best, active );
  ++r.fNHits( active );
  r.fRemainingGap( active ) = AliHLTTPCCAParameters::MaximumExtrapolationRowGap;
  ++r.fStage( r.fRemainingGap == 0 && mask ); // go to WaitingForExtrapolateDown or DoneStage if the gap got too big
  return active;
}

void AliHLTTPCCATrackletConstructor::run()
{
  
#ifndef NVALGRIND
  for ( int i = 0; i < AliHLTTPCCAParameters::NumberOfRows; ++i ) {
    const AliHLTTPCCARow &row = fData.Row( i );
    for ( int hit = 0; hit < row.NHits(); ++hit ) {
      debugF() << i << ", " << hit << endl;
      const short tmp = fData.HitLinkUpDataS( row, hit );
      VALGRIND_CHECK_VALUE_IS_DEFINED( tmp );
    }
  }
#endif
  assert( *fTracker.NTracklets() < 32768 );
  const short nTracks = *fTracker.NTracklets();

  for ( int trackIteration = 0; trackIteration * ushort_v::Size < nTracks; ++trackIteration ) {
    const ushort_v trackIndex( ushort_v( Vc::IndexesFromZero ) + trackIteration * ushort_v::Size );
    ushort_m active = trackIndex < nTracks;
    TrackMemory r;

    //std::cout << "----------------------------------------------------" << endl;
    r.fStage( !active ) = NullStage;

    // if rowStep = 2 TrackletStartHits need to be sorted such that all even start rows come first. The odd start rows - last.
    r.fStartRow.gather( fTracker.TrackletStartHits(), &HitId::fRow, trackIndex, active );
    r.fCurrentHitIndex.gather( fTracker.TrackletStartHits(), reinterpret_cast<short int AliHLTTPCCAHitId::*>( &HitId::fHit ), trackIndex, active );
    r.fEndRow = static_cast<ushort_v>( r.fStartRow );
    r.fLastRow = static_cast<ushort_v>( r.fStartRow );
    r.fStartRow( !active ) = std::numeric_limits<short_v>::max();
    r.fFirstRow = r.fStartRow;

    const sfloat_v zero( Vc::Zero );
    const sfloat_v one( Vc::One );
    r.fParam.SetSinPhi(  zero );
    r.fParam.SetDzDs(    zero );
    r.fParam.SetQPt(     zero );
    r.fParam.SetSignCosPhi( one );
    r.fParam.SetChi2(    zero );
    r.fParam.SetNDF(       -3 );
    r.fParam.SetCov(  0,  one );
    r.fParam.SetCov(  1, zero );
    r.fParam.SetCov(  2,  one );
    r.fParam.SetCov(  3, zero );
    r.fParam.SetCov(  4, zero );
    r.fParam.SetCov(  5,  one );
    r.fParam.SetCov(  6, zero );
    r.fParam.SetCov(  7, zero );
    r.fParam.SetCov(  8, zero );
    r.fParam.SetCov(  9,  one );
    r.fParam.SetCov( 10, zero );
    r.fParam.SetCov( 11, zero );
    r.fParam.SetCov( 12, zero );
    r.fParam.SetCov( 13, zero );
    r.fParam.SetCov( 14, 10.f );

//     const short_m evenRows = ( r.fStartRow & short_v( Vc::One ) ) > short_v( Vc::Zero );
// 
//     // fStartRow should be sorted, with the one exception of odd/even boundary
//     if ( r.fStartRow != r.fStartRow.sorted() ) {
//       debugF() << r.fStartRow << r.fStartRow.sorted() << evenRows << evenRows.isMix() << endl;
//       assert( r.fStartRow == r.fStartRow.sorted() || evenRows.isMix() );
//     }
    {
      InitTracklets init( r, fData, fTracker, fTrackletVectors[trackIteration], trackIndex );
      r.fStartRow.callWithValuesSorted( init );
    }

///mvz start
    sfloat_v qpt_approx(Vc::Zero);
    if(0){
 //     ushort_v oldHitInd;

      short_v curHitInd = r.fCurrentHitIndex;
      short_v startRow = r.fStartRow;
      short_v endRow = r.fStartRow;

      short_v nHits(curHitInd!=-1);
      int rowInd = r.fStartRow.min();
      const short_v activatRow = r.fStartRow;
      short_m act = activatRow == rowInd;
      short_m mask_end(curHitInd==-1);

      while( !mask_end.isFull())
      {
        act = act || activatRow == rowInd;
        ushort_v oldHitInd(Vc::Zero);
        oldHitInd(act) =static_cast<ushort_v>(curHitInd) ;
//        std::cout <<"row  "<< rowInd << std::endl;
//        std::cout <<"hit  "<< oldHitInd << std::endl;
//        std::cout <<"act  "<< act << std::endl;
        const AliHLTTPCCARow &row = fData.Row( rowInd );
        curHitInd.gather( fData.HitLinkUpData( row ), oldHitInd, act );
        mask_end = mask_end || (curHitInd == -1 && activatRow < rowInd );
//        std::cout <<"mask   "<<mask_end << std::endl;

        act = act && curHitInd != -1;
        nHits(act)++;
        endRow(act)++;
        rowInd++;
      }
//      std::cout << nHits << std::endl;
//      std::cout << startRow <<"  "<< endRow<< std::endl;
      short_v middleRow = (endRow - startRow)*0.5;

      rowInd = r.fStartRow.min();
      act = activatRow == rowInd;
      curHitInd = r.fCurrentHitIndex;
      mask_end = curHitInd==-1;

      short_m IsStart  = startRow == rowInd;
      short_m IsEnd    = endRow == rowInd;
      short_m IsMiddle = middleRow == rowInd;

      sfloat_v xStart = Vc::Zero;
      sfloat_v yStart = Vc::Zero;

      sfloat_v xEnd = Vc::Zero;
      sfloat_v yEnd = Vc::Zero;

      sfloat_v xMiddle = Vc::Zero;
      sfloat_v yMiddle = Vc::Zero;


      while( !mask_end.isFull())
      {
        IsStart  = startRow == rowInd;
        IsEnd    = endRow == rowInd;
        IsMiddle = middleRow == rowInd;

        act = act || activatRow == rowInd;
        ushort_v oldHitInd(Vc::Zero);
        oldHitInd(act) = static_cast<ushort_v>(curHitInd) ;
        const AliHLTTPCCARow &row = fData.Row( rowInd );

        xStart(IsStart) = fData.RowX( rowInd );
        yStart.gather( fData.HitDataY( row ), static_cast<ushort_v>(curHitInd), IsStart);

        xEnd(IsEnd) = fData.RowX( rowInd );
        yEnd.gather( fData.HitDataY( row ), static_cast<ushort_v>(curHitInd), IsEnd);

        xMiddle(IsMiddle) = fData.RowX( rowInd );
        yMiddle.gather( fData.HitDataY( row ), static_cast<ushort_v>(curHitInd), IsMiddle);

        curHitInd.gather( fData.HitLinkUpData( row ), oldHitInd, act );
        mask_end = mask_end || (curHitInd == -1 && activatRow < rowInd );
        act = act && curHitInd != -1;
        rowInd++;
      }

      sfloat_m approx = r.fCurrentHitIndex != -1;

      sfloat_v m12(Vc::One); sfloat_v m23(Vc::One); sfloat_v x0(Vc::One); sfloat_v y0(Vc::One); sfloat_v R(Vc::One);

      m12(approx) = (yStart - yMiddle)/(xStart - xMiddle);
      m23(approx) = (yEnd   - yMiddle)/(xEnd   - xMiddle);

      y0(approx) = 0.5 * (m12*(yStart + yMiddle) - m23*(yEnd + yMiddle) + (xStart - xEnd))/(m12-m23);
      x0(approx) = 0.5 * (m12*(yStart + yMiddle) + (xStart + xMiddle)) - y0*m12;

      sfloat_m straight = m12 == m23 && xStart != xMiddle && xEnd != xMiddle;

      R(approx)   = CAMath::Sqrt((x0 - xStart)*(x0 - xStart)+(y0 - yStart)*(y0 - yStart));

      qpt_approx(approx && !straight) = Vc::One/(R*fTracker.Param().cBz());
      qpt_approx(approx && !straight) = CAMath::Abs(qpt_approx);
      
      qpt_approx(approx && !straight) = (m12-m23)/CAMath::Abs(m12-m23) * qpt_approx;

      std::cout << qpt_approx << std::endl;
      r.fParam.SetQPt(     qpt_approx);
    }
///mvz end*/

    const int rowStep = AliHLTTPCCAParameters::RowStep;
    { // fit and extrapolate upwards
//       int rowIndex = r.fStartRow.min() + 4;
//       const short_v activationRow = r.fStartRow + 4;
      int rowIndex = r.fStartRow.min() + rowStep*2;
      const short_v activationRow = r.fStartRow + rowStep*2;
      debugF() << "============================================= Start Fitting Upwards =============================================" << endl;
//#define DISABLE_HIT_SEARCH
#ifdef DISABLE_HIT_SEARCH // iklm
      while ( rowIndex < AliHLTTPCCAParameters::NumberOfRows && !( r.fStage <= FitLinkedHits ).isEmpty() ) {
        ++r.fStage( rowIndex == activationRow ); // goes to FitLinkedHits on activation row
        FitTracklet( r, rowIndex, trackIndex, fTrackletVectors[trackIteration] );
        ++rowIndex;
      }

      const short_m ready = r.fStage <= DoneStage;
      const sfloat_v x( fData.RowX(), sfloat_v::IndexType( r.fEndRow ) );
      debugF() << x << sfloat_v::IndexType( r.fEndRow ) << sfloat_v( fData.RowX(), sfloat_v::IndexType( Vc::IndexesFromZero ) ) << endl;
      assert( ( x == 0 && static_cast<sfloat_m>( ready ) ).isEmpty() );
      const short_m transported = static_cast<short_m>( r.fParam.TransportToX(
          x, fTracker.Param().cBz(), .999f, static_cast<sfloat_m>( ready ) ) );
#ifdef LOSE_DEBUG
      if (!(static_cast<sfloat_m>( ready ) && (!transported)).isEmpty())
        std::cout << "Run-TransportToX(...) lose track: " << (static_cast<sfloat_m>( ready ) && (!transported)) << std::endl;
#endif

      debugF() << "============================================= Stop Fitting Upwards ==============================================" << endl;
#ifdef DRAW
      foreach_bit( int ii, r.fStage < DoneStage ) {
        TrackParam t( r.fParam, ii );
        AliHLTTPCCADisplay::Instance().ClearView();
        AliHLTTPCCADisplay::Instance().DrawSlice( &fTracker, 0 );
        AliHLTTPCCADisplay::Instance().DrawSliceHits();
        AliHLTTPCCADisplay::Instance().DrawTrackParam( t );
        AliHLTTPCCADisplay::Instance().Ask();
      }
#endif
    } // END: fit and extrapolate upwards
#else // DISABLE_HIT_SEARCH
    while ( rowIndex < fTracker.Param().NRows() && ( r.fStage == ExtrapolateUp ).isEmpty() ) {
      ++r.fStage( rowIndex == activationRow ); // goes to FitLinkedHits on activation row
      FitTracklet( r, rowIndex, trackIndex, fTrackletVectors[trackIteration] );
      ++rowIndex;
    }

    debugF() << "========================================== Start Extrapolating Upwards ==========================================" << endl;

    while ( rowIndex < fTracker.Param().NRows() && !( r.fStage <= FitLinkedHits ).isEmpty() ) {
      ++r.fStage( rowIndex == activationRow ); // goes to FitLinkedHits on activation row
      const short_m toExtrapolate = (r.fStage == ExtrapolateUp);
      const short_m &extrapolated = ExtrapolateTracklet( r, rowIndex, trackIndex, fTrackletVectors[trackIteration], toExtrapolate );
// #ifdef LOSE_DEBUG
//       if (!(toExtrapolate && (!extrapolated)).isEmpty())
//         std::cout << "Run-ExtrapolateTracklet(...)0 lose track: " << (toExtrapolate && (!extrapolated)) << std::endl;
// #endif
      r.fLastRow( extrapolated ) = rowIndex;
      FitTracklet( r, rowIndex, trackIndex, fTrackletVectors[trackIteration] );
      ++rowIndex;
    }
    debugF() << "============================================= Stop Fitting Upwards ==============================================" << endl;
#ifdef DRAW
    foreach_bit( int ii, r.fStage < DoneStage ) {
      TrackParam t( r.fParam, ii );
      AliHLTTPCCADisplay::Instance().ClearView();
      AliHLTTPCCADisplay::Instance().DrawSlice( &fTracker, 0 );
      AliHLTTPCCADisplay::Instance().DrawSliceHits();
      AliHLTTPCCADisplay::Instance().DrawTrackParam( t );
      AliHLTTPCCADisplay::Instance().Ask();
    }
#endif
    short_m mask;
    while ( rowIndex < fTracker.Param().NRows() && !( mask = r.fStage == ExtrapolateUp ).isEmpty() ) {
      const short_m &extrapolated = ExtrapolateTracklet( r, rowIndex, trackIndex, fTrackletVectors[trackIteration], mask );
      r.fLastRow( extrapolated ) = rowIndex;
// #ifdef LOSE_DEBUG
//       if (!(mask && (!extrapolated)).isEmpty())
//         std::cout << "Run-ExtrapolateTracklet(...)1 lose track: " << (mask && (!extrapolated)) << std::endl;
// #endif
      ++rowIndex;
    }
    debugF() << "=========================================== Stop Extrapolating Upwards ==========================================" << endl;
  }
  { // extrapolate downwards
    r.fRemainingGap = AliHLTTPCCAParameters::MaximumExtrapolationRowGap; // allow full gaps again
    ++r.fStage( r.fStage == ExtrapolateUp ); // FitLinkedHits/ExtrapolateUp went so high that no gap put the tracklet into WaitingForExtrapolateDown
    const short_m ready = r.fStage == WaitingForExtrapolateDown;
    debugF() << "ready to extrapolate downwards: " << ready << endl;
    ++r.fStage( ready ); // the wait is over

    if(1){ // set track parameters to x of end row of the fitting stage
      const sfloat_v x( fData.RowX(), sfloat_v::IndexType( r.fEndRow ) );
      debugF() << x << sfloat_v::IndexType( r.fEndRow ) << sfloat_v( fData.RowX(), sfloat_v::IndexType( Vc::IndexesFromZero ) ) << endl;
      assert( ( x == 0 && static_cast<sfloat_m>( ready ) ).isEmpty() );
      const short_m transported = static_cast<short_m>( r.fParam.TransportToX(
          x, fTracker.Param().cBz(), .999f, static_cast<sfloat_m>( ready ) ) );
#ifdef LOSE_DEBUG
      if (!(static_cast<sfloat_m>( ready ) && (!transported)).isEmpty())
        std::cout << "Run-TransportToX(...) lose track: " << (static_cast<sfloat_m>( ready ) && (!transported)) << std::endl;
#endif
      ++r.fStage( !transported ); // all those where transportation failed go to DoneStage

    }
#ifdef DRAW
    foreach_bit( int ii, r.fStage < DoneStage ) {
      TrackParam t( r.fParam, ii );
      AliHLTTPCCADisplay::Instance().ClearView();
      AliHLTTPCCADisplay::Instance().DrawSlice( &fTracker, 0 );
      AliHLTTPCCADisplay::Instance().DrawSliceHits();
      AliHLTTPCCADisplay::Instance().DrawTrackParam( t, 2 );
      AliHLTTPCCADisplay::Instance().Ask();
    }
#endif
    debugF() << "========================================= Start Extrapolating Downwards =========================================" << endl;
    int rowIndex = r.fEndRow.max() - 1;
//     int rowIndex = r.fLastRow.max() - 1;
    const int minMaskedExtrRow = r.fStartRow.min();
    short_m mask;
    while ( rowIndex >= minMaskedExtrRow && !( mask = r.fStage == ExtrapolateDown ).isEmpty() ) {
      mask &= rowIndex < r.fEndRow;
      mask &= ( ( rowIndex - r.fStartRow ) & short_v( std::numeric_limits<short_v::EntryType>::min() + 1 ) ) != short_v( Vc::Zero );
      const short_m &extrapolated = ExtrapolateTracklet( r, rowIndex, trackIndex, fTrackletVectors[trackIteration], mask );
// #ifdef LOSE_DEBUG
//       if (!(mask && (!extrapolated)).isEmpty())
//         std::cout << "Run-ExtrapolateTracklet(...)2 lose track: " << (mask && (!extrapolated)) << std::endl;
// #endif
      r.fFirstRow( extrapolated ) = rowIndex;
      --rowIndex;
    }
    while ( rowIndex >= 0 && !( mask = r.fStage == ExtrapolateDown ).isEmpty() ) {
      const short_m &extrapolated = ExtrapolateTracklet( r, rowIndex, trackIndex, fTrackletVectors[trackIteration], mask );
// #ifdef LOSE_DEBUG
//       if (!(mask && (!extrapolated)).isEmpty())
//         std::cout << "Run-ExtrapolateTracklet(...)3 lose track: " << (mask && (!extrapolated)) << std::endl;
// #endif
      r.fFirstRow( extrapolated ) = rowIndex;
      --rowIndex;
    }
    r.fFirstRow = CAMath::Min( r.fFirstRow, r.fStartRow );
#ifdef DRAW
    foreach_bit( int ii, r.fStage < NullStage ) {
      TrackParam t( r.fParam, ii );
      AliHLTTPCCADisplay::Instance().ClearView();
      AliHLTTPCCADisplay::Instance().DrawSlice( &fTracker, 0 );
      AliHLTTPCCADisplay::Instance().DrawSliceHits();
      AliHLTTPCCADisplay::Instance().DrawTrackParam( t, 4 );
      AliHLTTPCCADisplay::Instance().Ask();
    }
#endif
    
    ///mvz start
    //std::cout << "fitted   " << r.fParam.QPt() <<std::endl;
    //r.fParam.SetQPt(     qpt_approx);
    ///mvz end
    
  }
#endif // DISABLE_HIT_SEARCH

  sfloat_m trackletOkF( r.fNHits >= AliHLTTPCCAParameters::MinimumHitsForTracklet );
    for ( int i = 0; i < 15; ++i ) {
      trackletOkF &= CAMath::Finite( r.fParam.Cov()[i] );
    }
    for ( int i = 0; i < 5; ++i ) {
      trackletOkF &= CAMath::Finite( r.fParam.Par()[i] );
    }

    // 80 is row 0; if X is that small this track is garbage.
    // XXX does this happen at all? If yes, under what conditions?
    assert( ( r.fParam.X() > 50.f && trackletOkF ) == trackletOkF );
    trackletOkF &= r.fParam.X() > 50.f // TODO: read from file!!! 


    // there must be errors and they must be positive or this track is garbage
                                                    && r.fParam.Err2QPt()    > Vc::Zero;
    trackletOkF &= r.fParam.Err2Y()      > Vc::Zero && r.fParam.Err2Z()      > Vc::Zero;
    trackletOkF &= r.fParam.Err2SinPhi() > Vc::Zero && r.fParam.Err2DzDs()   > Vc::Zero;
#ifdef LOSE_DEBUG
    if (!(!trackletOkF).isEmpty())
      std::cout << "Run-trackletOkF  lose track: " << (!trackletOkF) << std::endl;
#endif
    debugF() << r.fParam << "-> trackletOk: " << trackletOkF << endl;

    const short_m trackletOk( trackletOkF );
    r.fNHits.setZero( !trackletOk );

    //////////////////////////////////////////////////////////////////////
    //
    //////////////////////////////////////////////////////////////////////

    TrackletVector &tracklet = fTrackletVectors[trackIteration];
    tracklet.SetNHits( r.fNHits );

    if ( !( r.fNHits > 0 ).isEmpty() ) {
#ifdef DRAW
      foreach_bit( int ii, r.fStage < DoneStage ) {
        TrackParam t( r.fParam, ii );
        AliHLTTPCCADisplay::Instance().ClearView();
        AliHLTTPCCADisplay::Instance().DrawSlice( &fTracker, 0 );
        AliHLTTPCCADisplay::Instance().DrawSliceHits();
        AliHLTTPCCADisplay::Instance().DrawTrackParam( t, 2 );
        AliHLTTPCCADisplay::Instance().Ask();
      }
#endif
      // start and end rows of the tracklet
      tracklet.SetFirstRow( static_cast<ushort_v>( r.fFirstRow ) );
      tracklet.SetLastRow( r.fLastRow );

///mvz start 25.01.2010
      sfloat_m MinQPt = CAMath::Abs(r.fParam.QPt()) < AliHLTTPCCAParameters::MinimumQPt;
      sfloat_v NewQPt = r.fParam.QPt();
      NewQPt(MinQPt) = AliHLTTPCCAParameters::MinimumQPt;
      r.fParam.SetQPt(NewQPt);
//      r.fParam.SetQPt( CAMath::Max( AliHLTTPCCAParameters::MinimumQPt, r.fParam.QPt() ) );
///mvz end 25.01.2010
      tracklet.SetParam( r.fParam );

      debugTS() << r.fFirstRow << r.fLastRow << endl;
      debugTS() << "set hit weigths from row " << r.fFirstRow.min() << " until row " << r.fLastRow.max() << endl;
      const ushort_v &weight = SliceData::CalculateHitWeight( r.fNHits, trackIndex );
      // for all rows where we have a hit let the fTracker know what weight our hits have
      for ( int rowIndex = r.fFirstRow.min(); rowIndex <= r.fLastRow.max(); ++rowIndex ) {
        const ushort_v &hitIndex = tracklet.HitIndexAtRow( rowIndex );
        debugTS() << "MaximizeHitWeight at " << rowIndex << hitIndex << " with " << weight << endl;
        fData.MaximizeHitWeight( fData.Row( rowIndex ), hitIndex, weight );
      }
    }
  }
}

void InitTracklets::operator()( int rowIndex )
{
  if ( ISUNLIKELY( rowIndex >= fTracker.Param().NRows() ) ) {
    return;
  }
  debugF() << "InitTracklets(" << rowIndex << ")" << endl;
//std::cout<< "InitTracklets(" << rowIndex << ")" << std::endl;
  const int rowStep = AliHLTTPCCAParameters::RowStep;
  assert( rowIndex < fTracker.Param().NRows() - rowStep );
  // the first hit is a special case (easy)
  const ushort_m &mask = rowIndex == r.fStartRow;
  const sfloat_m maskF( mask );
  {
    const AliHLTTPCCARow &row = fData.Row( rowIndex );
    VALGRIND_CHECK_MEM_IS_ADDRESSABLE( &row, sizeof( AliHLTTPCCARow ) );
    const sfloat_v x = fData.RowX( rowIndex );
    const ushort_v &hitIndex = static_cast<ushort_v>( r.fCurrentHitIndex );
    const sfloat_v y( fData.HitDataY( row ), hitIndex, mask );
    const sfloat_v z( fData.HitDataZ( row ), hitIndex, mask );
    r.fParam.SetX( x, maskF );
    r.fParam.SetY( y, maskF );
    r.fParam.SetZ( z, maskF );
    fTrackletVector.SetRowHits( rowIndex, trackIndex, hitIndex, mask );

    r.fCurrentHitIndex.gather( fData.HitLinkUpData( row ), hitIndex, mask ); // set to next linked hit
    // the first hit in the Tracklet is guaranteed to have a link up, since StartHitsFinder
    // ensures it
    assert( ( r.fCurrentHitIndex >= 0 && mask ) == mask );
  }

  rowIndex += rowStep;
  {
    const AliHLTTPCCARow &row = fData.Row( rowIndex );
    const sfloat_v x = fData.RowX( rowIndex );
    const ushort_v &hitIndex = static_cast<ushort_v>( r.fCurrentHitIndex );
    const sfloat_v y( fData.HitDataY( row ), hitIndex, mask );
    const sfloat_v z( fData.HitDataZ( row ), hitIndex, mask );
    const sfloat_v &dx = x - r.fParam.X();
    const sfloat_v &dy = y - r.fParam.Y();
    const sfloat_v &dz = z - r.fParam.Z();
    const sfloat_v &ri = sfloat_v( Vc::One ) / CAMath::Sqrt( dx * dx + dy * dy ); // RSqrt
    const sfloat_v &sinPhi = dy * ri;
    r.fParam.SetSinPhi( sinPhi,  maskF );
    r.fParam.SetDzDs  ( dz * ri, maskF );
    sfloat_v err2Y, err2Z;
///mvz start 20.01.2010
//    fTracker.GetErrors2( rowIndex, r.fParam, &err2Y, &err2Z );
    fTracker.GetErrors2( rowIndex, r.fParam, &err2Y, &err2Z );
///mvz end 20.01.2010
    r.fParam.SetCov( 0, err2Y, maskF );
    r.fParam.SetCov( 2, err2Z, maskF );

    const sfloat_m transported = r.fParam.TransportToX( x, sinPhi, fTracker.Param().cBz(), -1.f, maskF );
    // assert( transported == maskF );
///mvz start 20.01.2010
//    fTracker.GetErrors2( rowIndex, r.fParam.GetZ(), sinPhi, r.fParam.GetDzDs(), &err2Y, &err2Z );
    fTracker.GetErrors2( rowIndex, r.fParam, &err2Y, &err2Z );
///mvz end 20.01.2010
    const short_m hitAdded( r.fParam.Filter( maskF, y, z, err2Y, err2Z, .99f ) );
    // assert( hitAdded == mask );
    UNUSED_PARAM2( transported, hitAdded );
    fTrackletVector.SetRowHits( rowIndex, trackIndex, hitIndex, mask );

    r.fLastY( maskF ) = y;
    r.fLastZ( maskF ) = z;
    r.fCurrentHitIndex.gather( fData.HitLinkUpData( row ), hitIndex, mask ); // set to next linked hit

    // the second hit in the Tracklet is also guaranteed to have a link up, since StartHitsFinder
    // ensures it
    assert( ( r.fCurrentHitIndex >= 0 && mask ) == mask );
  }
}

