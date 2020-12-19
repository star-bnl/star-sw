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

#include "AliHLTTPCCAHitArea.h"

#ifdef MAIN_DRAW
#include "AliHLTTPCCADisplay.h"
#endif // MAIN_DRAW

#include <limits>
#include <Vc/limits>

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
  int_v fStartRow;  // min row index with only the fitted part
  uint_v fEndRow;    // max row index with only the fitted part
  int_v fFirstRow;  // min row index with the extrapolated parts
  uint_v fLastRow;   // max row index with the extrapolated parts
  int_v fCurrentHitIndex; // indef of the current hit (for chain-fit stage it is next candidate to filtrate, for extra stage it is last filtered hit)
  int_v fStage; // reco stage
  uint_v fNHits; // n track hits
  int_v fRemainingGap; // n missed hits during search
  float_v fLastY; // Y of the last fitted cluster
  float_v fLastZ; // Z of the last fitted cluster
  TrackParamVector fParam;
  uint_m fIsFragile; // is extrapolation and finding of additional hits needed
  // uint_m fIsOnChain;
    int_m fIsFitted;
  
  inline TrackMemory()
    : fStartRow( Vc::Zero ),
    fCurrentHitIndex( -1 ),
    fStage( Vc::Zero ),
    fNHits( 2 ), // the first two hits are certain
    fRemainingGap( AliHLTTPCCAParameters::MaximumExtrapolationRowGap ),
    fLastY( Vc::Zero ),
    fLastZ( Vc::Zero ),
    fIsFragile( Vc::Zero )
  {}

  int_m IsInvalid() {
    return fNHits < 3 ||
           static_cast<int_m>( CAMath::Abs( fParam.SinPhi() ) > .999f  ||
           fParam.Chi2() >= float_v(15.f)*static_cast<float_v>(fParam.NDF()) );
  }

  std::ostream &operator<<( std::ostream &out ) {
#define SEP endl
//#define SEP " "
    out << "SR " << fStartRow << SEP;
    out << "ER " << fEndRow << SEP;
    out << "FR " << fFirstRow << SEP;
    out << "LR " << fLastRow << SEP;
    out << "CI " << fCurrentHitIndex << SEP;
    out << "St " << fStage << SEP;
    out << "NH " << fNHits << SEP;
    out << "RG " << fRemainingGap << SEP;
    out << "LY " << fLastY << SEP;
    out << "LZ " << fLastZ << SEP;
    out << "IF " << fIsFragile << SEP;
    out << fParam;
    return out;
#undef SEP
  };
};

class InitTracklets
{
  public:
    InitTracklets( AliHLTTPCCATrackletConstructor::TrackMemory &_r, SliceData &data,
        const Tracker &tracker,
        TrackletVector &trackletVector, const uint_v &_trackIndex, uint_m mask )
      : r( _r ), fData( data ), fTracker( tracker ),
      fTrackletVector( trackletVector ), trackIndex( _trackIndex ), tMask(mask) {}

    void operator()( int rowIndex );

  private:
    AliHLTTPCCATrackletConstructor::TrackMemory &r;
    SliceData &fData;
    const Tracker &fTracker;
    TrackletVector &fTrackletVector;
    const uint_v &trackIndex;
    uint_m tMask;
};

/**
 * Fit tracklet to the string of hits connected by links
 */
void AliHLTTPCCATrackletConstructor::FitTracklet( TrackMemory &r, const int rowIndex,
    const uint_v trackIndex, TrackletVector &trackletVector )
{
  const AliHLTTPCCARow &row = fData.Row( rowIndex );

  const int_m active = r.fStage == FitLinkedHits; // active chains (ones, which contains no fitted hits).
  
  assert( AliHLTTPCCAParameters::RowStep == 1 );
  
  debugF() << "FitTracklet(" << rowIndex << ") stage: " << r.fStage << " r.fStartRow: " << r.fStartRow << " active: " << active << endl;

  assert( active == ( active && r.fStage == FitLinkedHits ) );
  assert( active == ( active && r.fCurrentHitIndex >= 0 ) );
  assert( active == ( active && rowIndex > r.fStartRow ) );
  //assert( r.fActive == ( ( rowIndex - r.fStartRow ) % 2 == 0 ) );
  ASSERT( active == ( active && r.fCurrentHitIndex < row.NHits() ),
    active << r.fCurrentHitIndex << row.NHits() );

  const uint_v oldHitIndex = static_cast<uint_v>( r.fCurrentHitIndex );

  int_v isUsed;
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    if( !active[i] ) continue;
    isUsed[i] = fData.HitDataIsUsed( row )[(unsigned int)oldHitIndex[i]];
  }
  const int_m fitMask = active && ( (isUsed != int_v(2)) && (isUsed != int_v(3)) ); // mask to add a new hit. // don't take hits from other tracks. In order to reduce calculations.

  const float_v x = fData.RowX( rowIndex ); // convert to float_v once now
  const float_v y = fData.HitPDataY( row , oldHitIndex, float_m(fitMask));
  const float_v z = fData.HitPDataZ( row , oldHitIndex, float_m(fitMask));

  debugF() << "x, y, z: " << x << y << z << endl;

  float_m activeF( static_cast<float_m>( fitMask ) ); // create float_v mask
    // correct SinPhi if it is necessary
    // calculate displacement to previous hit
  const float_v dx = x - r.fParam.X();
  const float_v dy = y - r.fLastY;
  VALGRIND_CHECK_VALUE_IS_DEFINED( dy );
  const float_v dz = z - r.fLastZ;
  debugF() << "dx, dy, dz: " << dx << dy << dz << endl;
  r.fLastY( activeF ) = y;
  r.fLastZ( activeF ) = z;

  float_v err2Y, err2Z;
  float_v sinPhi = r.fParam.GetSinPhi();


  const float_m fragile =
      static_cast<float_m>( r.fNHits < uint_v(AliHLTTPCCAParameters::MinimumHitsForFragileTracklet) ) || CAMath::Abs( r.fParam.SinPhi() ) >= .99f;
  sinPhi( fragile ) = dy * CAMath::RSqrt( dx * dx + dy * dy );


  assert( ( x == 0 && activeF ).isEmpty() );
  
  activeF = r.fParam.TransportToX( x, sinPhi, fTracker.Param().cBz(), -1.f, activeF );
  
  if ( !activeF.isEmpty() )
  {
    fTracker.GetErrors2( rowIndex, r.fParam, &err2Y, &err2Z );
    const int_m hitAdded = static_cast<int_m>( r.fParam.Filter( activeF, y, z, err2Y, err2Z, .99f ) );
    trackletVector.SetRowHits( rowIndex, trackIndex, static_cast<uint_v>(r.fCurrentHitIndex), hitAdded );
    ++r.fNHits( static_cast<uint_m>(hitAdded) );
    r.fEndRow( hitAdded ) = rowIndex;
      
    ASSERT( ( (row.NHits() > static_cast<uint_v>(oldHitIndex) ) && hitAdded ) == hitAdded,
      row.NHits() << static_cast<uint_v>(oldHitIndex) << hitAdded );
    fData.SetHitAsUsedInTrackFit( row, static_cast<uint_v>( oldHitIndex ), hitAdded );
  }
#ifdef VC_GATHER_SCATTER
  r.fCurrentHitIndex.gather( fData.HitLinkUpData( row ), static_cast<uint_v>( oldHitIndex ), active ); // set to next linked hit
#else
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    if( !active[i] ) continue;
    r.fCurrentHitIndex[i] = fData.HitLinkUpData( row )[(unsigned int)oldHitIndex[i]];
  }
#endif

  const int_m fittingDone = r.fCurrentHitIndex < 0 && active;
  debugF() << "fittingDone = " << fittingDone << endl;
//  if ( ISUNLIKELY( !fittingDone.isEmpty() ) ) {
  ++r.fStage( fittingDone ); // goes to ExtrapolateUp if fitting is done (no other hit linked)
  r.fLastRow( fittingDone ) = rowIndex;
  debugF() << "hits: " << r.fNHits << ", sinphi: " << r.fParam.SinPhi() << endl;
  

  const int_m invalidTracklet = r.IsInvalid() && fittingDone;
  
  r.fNHits.setZero( invalidTracklet );
  r.fStage( invalidTracklet || (fittingDone && !r.fIsFragile) ) = DoneStage;
  
  debugF() << "r.fStage: " << r.fStage << endl;
//  }
} // FitTracklet

void AliHLTTPCCATrackletConstructor::FindNextHit( TrackMemory &r, const AliHLTTPCCARow &row,
                                                  float_v &dy_best, float_v &dz_best, int_m &active)
{ 
  const float_v fY = r.fParam.Y();
  const float_v fZ = r.fParam.Z();
  const uint_v fIndYmin = row.Grid().GetBinBounded( fY - AliHLTTPCCAParameters::MinCellSize*.5f, fZ - AliHLTTPCCAParameters::MinCellSize*.5f );

  int_v fHitIndexes(-1);

  for(int trackletIndex =0; trackletIndex<int_v::Size; trackletIndex++)
  {
    if(!active[trackletIndex]) continue;
    float minRadius2 = std::numeric_limits<float>::max();

    const float y = fY[trackletIndex];
    const float z = fZ[trackletIndex];
    const int indYmin = fIndYmin[trackletIndex];

    { // look at iY = 0,1; iZ = 0
      const int end = fData.FirstHitInBin( row, indYmin + 2 );
      for ( int hitIndex = fData.FirstHitInBin( row, indYmin ); hitIndex < end; hitIndex += float_v::Size ) {
        float_v yz;
	yz = fData.HitPDataY( row, hitIndex );
        const float_v dy = yz - y;
	yz = fData.HitPDataZ( row, hitIndex );
        const float_v dz = yz - z;
        float_v radius2 = std::numeric_limits<float_v>::max();
        radius2( float_m(uint_v( Vc::IndexesFromZero ) + uint_v(hitIndex) < uint_v(end)) ) = dy * dy + dz * dz; // XXX Manhattan distance
        const float min = radius2.min();
        if ( min < minRadius2 ) {
          minRadius2 = min;
          int i = ( radius2 == min ).firstOne();
          assert( minRadius2 == radius2[i] );
          dy_best[trackletIndex] = dy[i];
          dz_best[trackletIndex] = dz[i];
          fHitIndexes[trackletIndex] = hitIndex + i;
        }
      }
    }
    { // look at iY = 0,1; iZ = 1
      const int nY = row.Grid().Ny();
      const int end = fData.FirstHitInBin( row, indYmin + nY + 2 );
      for ( int hitIndex = fData.FirstHitInBin( row, indYmin + nY ); hitIndex < end; hitIndex += float_v::Size ) {
        float_v yz;
        yz = fData.HitPDataY( row, hitIndex );
        const float_v dy = yz - y;
	yz = fData.HitPDataZ( row, hitIndex );
        const float_v dz = yz - z;
        float_v radius2 = std::numeric_limits<float_v>::max();
        radius2( float_m(uint_v( Vc::IndexesFromZero ) + uint_v(hitIndex) < uint_v(end)) ) = dy * dy + dz * dz; // XXX Manhattan distance
        const float min = radius2.min();
        if ( min < minRadius2 ) {
          minRadius2 = min;
          int i = ( radius2 == min ).firstOne();
          assert( minRadius2 == radius2[i] );
          dy_best[trackletIndex] = dy[i];
          dz_best[trackletIndex] = dz[i];
          fHitIndexes[trackletIndex] = hitIndex + i;
        }
      }
    }
  }

  r.fCurrentHitIndex( active ) = fHitIndexes;
//  r.fCurrentHitIndex( static_cast<int_m>( row.NHits() <= 0 ) ) = -1; // sometimes row.NHits() == 0 but firstHitInBin != 0 CHECKME. But same checked in Extend\ExtrapolateTracklet
  active &= (r.fCurrentHitIndex != int_v(-1));
} // FindNextHit

/**
 * Now that we have a tracklet try to find more hits that would fit with this tracklet
 */
int_m AliHLTTPCCATrackletConstructor::ExtrapolateTracklet( TrackMemory &r, const int rowIndex, const uint_v trackIndex,
   TrackletVector &trackletVector, const bool dir, const int_m &mask )
{
  //assert( active == ( active && ( r.fStage == ExtrapolateUp || r.fStage == ExtrapolateDown ) ) );
  // reconstruction of tracklets, tracklets update step
  
  const AliHLTTPCCARow &row = fData.Row( rowIndex );

  int_m active = mask;
  --r.fRemainingGap( active );
  assert( (r.fRemainingGap >= 0 || !active).isFull() );
  debugF() << "ExtrapolateTracklet(" << rowIndex << ") fRemainingGap: " << r.fRemainingGap << endl;

  
  const float_v x = fData.RowX( rowIndex );
  
  float_m activeF( active ); // get float mask
  assert( ( x == 0 && activeF ).isEmpty() );
  float_m transport = r.fParam.TransportToX( x, r.fParam.SinPhi(), fTracker.Param().cBz(), .99f, activeF );
  activeF &= transport;
  active = static_cast<int_m>( activeF );

  if ( row.NHits() < 1 || active.isEmpty() ) {
    ++r.fStage( r.fRemainingGap == 0 && mask ); // go to WaitingForExtrapolateDown or DoneStage if the gap got too big
#ifndef NODEBUG
    debugF() << "r.fStage: " << r.fStage << " after ExtrapolateTracklet found empty row and fRemainingGap == " << r.fRemainingGap << endl;
#endif
    // no hits in this row, skip (don't do this before TransportToX)
    ASSERT( (r.fRemainingGap >= 0).isFull(),
      r.fRemainingGap );
    return int_m( Vc::Zero );
  }

    // find next hit
  int_m linkMask = active; linkMask &= int_m(Vc::Zero);
  
    // try to find hit by trying all closed hits
  float_v dy_tmp(Vc::Zero), dz_tmp(Vc::Zero);
  int_m findMask  = active && !linkMask;
  
  FindNextHit(r, row, dy_tmp, dz_tmp, findMask);

  const float_v dy = dy_tmp;
  const float_v dz = dz_tmp;
  active = linkMask || findMask;

    // check if found hit is acceptable
  float_v err2Y(Vc::Zero), err2Z(Vc::Zero);
  fTracker.GetErrors2( rowIndex, r.fParam, &err2Y, &err2Z );

  const float_v kFactor = AliHLTTPCCAParameters::HitPickUpFactor * AliHLTTPCCAParameters::HitPickUpFactor * 3.5f * 3.5f;

#ifdef TRACKLET_EXT
  const float_v twentyfive (25.f );
  const float_v sy2 = CAMath::Min( twentyfive, kFactor * ( r.fParam.GetErr2Y() + err2Y ) );
  const float_v sz2 = CAMath::Min( twentyfive, kFactor * ( r.fParam.GetErr2Z() + err2Z ) );
#else
  const float_v two( 2.f );
  const float_v sy2 = CAMath::Min( two, kFactor * ( r.fParam.GetErr2Y() + err2Y ) );
  const float_v sz2 = CAMath::Min( two, kFactor * ( r.fParam.GetErr2Z() + err2Z ) );
#endif

  activeF = static_cast<float_m>( active );
  debugF() << "activeF: " << activeF;
  activeF &= dy * dy <= sy2 && dz * dz <= sz2;
  debugF() << " -> " << activeF;

  
  float_m filtred = r.fParam.FilterDelta( activeF, dy, dz, err2Y, err2Z, .99f );
  activeF &= filtred;
  debugF() << " -> " << activeF;
  active = static_cast<int_m>( activeF );
  debugF() << " -> " << active << endl;

  debugF() << "SetRowHits( " << rowIndex << ", " << r.fCurrentHitIndex << ", " << active << ")" << endl;

  ASSERT( ( (row.NHits() > r.fCurrentHitIndex) && active) == active,
          row.NHits() << r.fCurrentHitIndex << active );
  fData.SetHitAsUsedInTrackExtend( row, static_cast<uint_v>(r.fCurrentHitIndex), active );
  
  trackletVector.SetRowHits( rowIndex, trackIndex,  static_cast<uint_v>(r.fCurrentHitIndex), active );
  ++r.fNHits( static_cast<uint_m>(active) );
  r.fRemainingGap( active ) = AliHLTTPCCAParameters::MaximumExtrapolationRowGap;
  ++r.fStage( r.fRemainingGap == 0 && mask ); // go to WaitingForExtrapolateDown or DoneStage if the gap got too big
//  r.fStage( r.fNHits > 10 && mask ) = DoneStage; // don't need long tracklets. merger will do work.
 
  r.fLastRow(  active && int_m( dir) ) = rowIndex;
  r.fFirstRow( active && int_m(!dir) ) = rowIndex;
  return active;
} // ExtrapolateTracklet

int_m AliHLTTPCCATrackletConstructor::ExtendTracklet( TrackMemory &r, const int rowIndex, const uint_v trackIndex,
   TrackletVector &trackletVector, const bool dir, const int_m &mask )
{
  //assert( activeExtraMask == ( activeExtraMask && ( r.fStage == ExtrapolateUp || r.fStage == ExtrapolateDown ) ) );
  // reconstruction of tracklets, tracklets update step
  assert( AliHLTTPCCAParameters::RowStep == 1 );

  
  int_m activeExtraMask = mask;
  const int_m activeFitMask = (r.fStage == FitLinkedHits); // active chains (ones, which contains no fitted hits).
//  const int_m activeFitMask = (r.fStage == FitLinkedHits) && r.fCurrentHitIndex >= 0; // active chains (ones, which contains no fitted hits).
  
  const AliHLTTPCCARow &row = fData.Row( rowIndex );
    
  assert( activeFitMask == ( activeFitMask && r.fStage == FitLinkedHits ) );
  assert( activeFitMask == ( activeFitMask && r.fCurrentHitIndex >= 0 ) );
  assert( activeFitMask == ( activeFitMask && rowIndex > r.fStartRow ) );
  ASSERT( activeFitMask == ( activeFitMask && r.fCurrentHitIndex < row.NHits() ),
    activeFitMask << r.fCurrentHitIndex << row.NHits() );
  assert( (activeFitMask && activeExtraMask).isEmpty() );
//  std::cout<<" - r.fCurrentHitIndex: "<<r.fCurrentHitIndex<<"\n";
  
  const uint_v oldHitIndex = static_cast<uint_v>( r.fCurrentHitIndex );


    
  --r.fRemainingGap( activeExtraMask );
  debugF() << "ExtrapolateTracklet(" << rowIndex << ") fRemainingGap: " << r.fRemainingGap << endl;
  
  // -- TRANSPORT --
  const float_v x = fData.RowX( rowIndex );
//  const int_v isUsed(fData.HitDataIsUsed( row ), static_cast<uint_v>(oldHitIndex), activeFitMask); // Here it takes a lot of time...
  int_v isUsed;
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    if( !activeFitMask[i] ) continue;
    isUsed[i] = fData.HitDataIsUsed( row )[(unsigned int)oldHitIndex[i]];
  }
  const int_m fitMask = activeFitMask && (isUsed != int_v(2)) && (isUsed != int_v(3)) && (r.fCurrentHitIndex >= 0); // mask to add a new hit. // don't take hits from other tracks. In order to reduce calculations.

  float_m activeFitMaskF( static_cast<float_m>( fitMask ) ); // create float_v mask
  float_m activeExtraMaskF( static_cast<float_m>(activeExtraMask) );
    
  assert( (row.NHits() > oldHitIndex || !fitMask).isFull() );
  float_v y = fData.HitPDataY( row, oldHitIndex, activeFitMaskF );
  float_v z = fData.HitPDataZ( row, oldHitIndex, activeFitMaskF );
  
  const float_v dx = x - r.fParam.X();

  float_v dy = y - r.fLastY;
  float_v dz = z - r.fLastZ;
  r.fLastY( activeFitMaskF ) = y;
  r.fLastZ( activeFitMaskF ) = z;
  
  float_v sinPhi = r.fParam.GetSinPhi();

  const float_m fragile = activeFitMaskF &&
      ( static_cast<float_m>( r.fNHits < uint_v(AliHLTTPCCAParameters::MinimumHitsForFragileTracklet) ) || CAMath::Abs( r.fParam.SinPhi() ) >= .99f );
  sinPhi( fragile ) = dy * CAMath::RSqrt( dx * dx + dy * dy );
  
  float_v maxSinPhi = .99f;
  maxSinPhi( activeFitMaskF ) = -1.f;
  
  assert( ( (x == 0) && (activeExtraMaskF || activeFitMaskF) ).isEmpty() );

  float_m transport = r.fParam.TransportToX( x, sinPhi, fTracker.Param().cBz(), maxSinPhi, activeExtraMaskF || activeFitMaskF );
  activeExtraMaskF &= transport;
  activeFitMaskF &= transport;

  if ( row.NHits() < 1 || ( activeExtraMaskF || activeFitMaskF ).isEmpty() ) { 
      // no hits in this row, skip (don't do this before TransportToX)
      // end with extrapolation
    ++r.fStage( r.fRemainingGap == 0 && mask ); // go to WaitingForExtrapolateDown or DoneStage if the gap got too big
#ifndef NODEBUG
    debugF() << "r.fStage: " << r.fStage << " after ExtrapolateTracklet found empty row and fRemainingGap == " << r.fRemainingGap << endl;
#endif
    
      // end with chain fit
#ifdef VC_GATHER_SCATTER
    r.fCurrentHitIndex.gather( fData.HitLinkUpData( row ), static_cast<uint_v>( oldHitIndex ), activeFitMask ); // prepare new hit for fit // TODO 2 dir??
#else
    for( unsigned int i = 0; i < float_v::Size; i++ ) {
      if( !activeFitMask[i] ) continue;
      r.fCurrentHitIndex[i] = fData.HitLinkUpData( row )[(unsigned int)oldHitIndex[i]];
    }
#endif
    
    const int_m fittingDone = r.fCurrentHitIndex < 0 && activeFitMask;
    ++r.fStage( fittingDone ); // goes to ExtrapolateUp if fitting is done (no other hit linked)
    r.fLastRow( fittingDone ) = rowIndex;

    const int_m invalidTracklet = r.IsInvalid() && fittingDone;
    r.fNHits.setZero( invalidTracklet );
    r.fStage( invalidTracklet || (fittingDone && !r.fIsFragile) ) = DoneStage;
    
    return int_m( Vc::Zero );
  }

  // -- FIND A NEXT HIT --
  activeExtraMask = static_cast<int_m>( activeExtraMaskF );

    // try find hit by using links
  int_m linkMask = int_m(Vc::Zero);//activeExtraMask && r.fIsOnChain; - // use links during extrapolation is turned off
  
  dy = y - r.fParam.Y();
  dz = z - r.fParam.Z();

    // try to find hit by trying all closed hits
  float_v  dy_tmp(Vc::Zero), dz_tmp(Vc::Zero);
  int_m findMask  = activeExtraMask && !linkMask; // try to find only if there is no chain
  if ( !findMask.isEmpty() ) {
    FindNextHit(r, row, dy_tmp, dz_tmp, findMask);
  }
  activeExtraMask = linkMask || findMask;
  activeExtraMaskF = static_cast<float_m>( activeExtraMask );
  
  dy( static_cast<float_m>( findMask ) ) = dy_tmp;
  dz( static_cast<float_m>( findMask ) ) = dz_tmp;

    // check if found hit is acceptable
  float_v err2Y, err2Z;
  fTracker.GetErrors2( rowIndex, r.fParam, &err2Y, &err2Z );

  const float_v kFactor = AliHLTTPCCAParameters::HitPickUpFactor * AliHLTTPCCAParameters::HitPickUpFactor * 3.5f * 3.5f;

#ifdef TRACKLET_EXT
  const float_v twentyfive( 25.f );
  const float_v sy2 = CAMath::Min( twentyfive, kFactor * ( r.fParam.GetErr2Y() + err2Y ) );
  const float_v sz2 = CAMath::Min( twentyfive, kFactor * ( r.fParam.GetErr2Z() + err2Z ) );
#else
  const float_v two( 2.f );
  const float_v sy2 = CAMath::Min( two, kFactor * ( r.fParam.GetErr2Y() + err2Y ) );
  const float_v sz2 = CAMath::Min( two, kFactor * ( r.fParam.GetErr2Z() + err2Z ) );
#endif


  activeExtraMaskF &= dy * dy <= sy2 && dz * dz <= sz2;

  activeFitMaskF &= (dy * dy <= sy2 && dz * dz <= sz2) || float_m(r.fNHits < uint_v(3)); // very strong cut for ghosts.
 
    // -- FILTER THE NEXT HIT --
  const float_m filtred = r.fParam.FilterDelta( activeExtraMaskF || activeFitMaskF, dy, dz, err2Y, err2Z, .99f );
  activeExtraMaskF &= filtred;
  activeExtraMask = static_cast<int_m>( activeExtraMaskF );
  activeFitMaskF &= filtred;
  const int_m hitAdded = static_cast<int_m>( activeFitMaskF );

  
    // -- SAVE THE NEXT HIT --
  trackletVector.SetRowHits( rowIndex, trackIndex,  static_cast<uint_v>(r.fCurrentHitIndex), activeExtraMask || hitAdded );
#ifdef VC_GATHER_SCATTER
  r.fCurrentHitIndex.gather( fData.HitLinkUpData( row ), static_cast<uint_v>( oldHitIndex ), uint_m(activeFitMask) ); // prepare new hit for fit
#else
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    if( !activeFitMask[i] ) continue;
    r.fCurrentHitIndex[i] = fData.HitLinkUpData( row )[(unsigned int)oldHitIndex[i]];
  }
#endif
  

  ASSERT( ( (row.NHits() > r.fCurrentHitIndex) && activeExtraMask) == activeExtraMask,
          row.NHits() << r.fCurrentHitIndex << activeExtraMask );
  fData.SetHitAsUsedInTrackExtend( row, static_cast<uint_v>( r.fCurrentHitIndex ), activeExtraMask ); // TODO 1 function
  fData.SetHitAsUsedInTrackFit(    row, static_cast<uint_v>( oldHitIndex ),        hitAdded );
  
  ++r.fNHits( static_cast<uint_m>( activeExtraMask || hitAdded ) );
  r.fRemainingGap( activeExtraMask ) = AliHLTTPCCAParameters::MaximumExtrapolationRowGap;

    // -- MOVE TO NEXT STAGE --
  ++r.fStage( (r.fRemainingGap == 0) && mask ); // go to WaitingForExtrapolateDown or DoneStage if the gap got too big TODO not active
  ASSERT( (r.fRemainingGap >= 0).isFull(),
    r.fRemainingGap << mask << activeExtraMask );
//  r.fStage( r.fNHits > 10 && mask ) = DoneStage; // don't need long tracklets. merdger will do work.
  
  const int_m fittingDone = r.fCurrentHitIndex < 0 && activeFitMask;
  ++r.fStage( fittingDone ); // goes to ExtrapolateUp if fitting is done (no other hit linked)
  r.fLastRow( fittingDone ) = rowIndex;

  const int_m invalidTracklet = r.IsInvalid() && fittingDone;
  r.fNHits.setZero( invalidTracklet );
  r.fStage( invalidTracklet || (fittingDone && !r.fIsFragile) ) = DoneStage; // TODO unmark used hits
  
  r.fEndRow( hitAdded ) = rowIndex;

  r.fLastRow(  activeExtraMask && int_m( dir) ) = rowIndex;
  r.fFirstRow( activeExtraMask && int_m(!dir) ) = rowIndex;
  return activeExtraMask;
} // ExtendTracklet

void AliHLTTPCCATrackletConstructor::CreateStartSegmentV( const int rowIndex, const int iter )
{
  const AliHLTTPCCARow &row = fData.Row( rowIndex );
  // references to the rows above and below
  const int rowStep = AliHLTTPCCAParameters::RowStep;

  const AliHLTTPCCARow &rowUp = fData.Row( rowIndex + rowStep );
  const AliHLTTPCCARow &rowDn = fData.Row( rowIndex - rowStep );
  const AliHLTTPCCARow &rowUpUp = fData.Row( rowIndex + rowStep + rowStep );

  const int numberOfHits = row.NUnusedHits();
  const int numberOfHitsUp = rowUp.NUnusedHits();
  const int numberOfHitsUpUp = rowUpUp.NUnusedHits();
  const int numberOfHitsDown = rowDn.NUnusedHits();
  if ( numberOfHits == 0 ) {
    debugS() << "no hits in this row" << std::endl;
    return;
  }
  if ( numberOfHitsDown == 0 || numberOfHitsUp == 0 || numberOfHitsUpUp == 0 ) {
    debugS() << "no hits in neighbouring rows" << std::endl;
    return;
  }

  // the axis perpendicular to the rows
  const float xDn = fData.RowX( rowIndex - rowStep );
  const float x   = fData.RowX( rowIndex     );
  const float xUp = fData.RowX( rowIndex + rowStep );
  const float xUpUp = fData.RowX( rowIndex + rowStep + rowStep );

  // distance of the rows (absolute and relative)
  const float UpDx = xUp - x;
  const float DnDx = xDn - x;
  const float UpTx = xUp / x;
  const float DnTx = xDn / x;

  const int hitsStartOffset = *fTracker.NTracklets();
  Vc::vector<AliHLTTPCCAStartHitId>& startHits = fTracker.TrackletStartHits();
  unsigned int startHitsCount = 0;

  std::vector<int> hitsDn, hitsMid, hitsUp;
  int currentHit = 0;

  static const float kAreaSizeY = AliHLTTPCCAParameters::NeighbourAreaSizeTgY[iter];
  static const float kAreaSizeZ = AliHLTTPCCAParameters::NeighbourAreaSizeTgZ[iter];
  static const int kMaxN = 20; // TODO minimaze
  const float chi2Cut = AliHLTTPCCAParameters::NeighbourChiCut[iter]*AliHLTTPCCAParameters::NeighbourChiCut[iter] * 4.f * ( UpDx * UpDx + DnDx * DnDx );

  typedef HitArea::NeighbourData NeighbourData;
  for ( unsigned int hitIndex = 0; hitIndex < numberOfHits; hitIndex += int_v::Size ) {

    uint_v hitIndexes( uint_v(Vc::IndexesFromZero) + hitIndex );
    const int_m &validHitsMask = hitIndexes < numberOfHits;

    // coordinates of the hit in the current row
    float_v y(Vc::Zero), z(Vc::Zero), yUp(Vc::Zero), zUp(Vc::Zero), yDn(Vc::Zero), zDn(Vc::Zero);

    y = fData.UnusedHitPDataY( row, hitIndexes, static_cast<float_m>(validHitsMask) );
    z = fData.UnusedHitPDataZ( row, hitIndexes, static_cast<float_m>(validHitsMask) );

    yUp = y * UpTx; // suppose vertex at (0,0,0)
    yDn = y * DnTx; // TODO change name
    zUp = z * UpTx;
    zDn = z * DnTx;

      // find all neighbours in upper area (kMaxN at max)
    HitArea areaUp( rowUp, fData, yUp, zUp, UpDx*kAreaSizeY, UpDx*kAreaSizeZ, validHitsMask );

    uint_v maxUpperNeighbourIndex = uint_v(Vc::Zero);
    int upperNeighbourIndex = 0;
    NeighbourData neighUp[kMaxN];
    while ( !( areaUp.GetNext( &neighUp[upperNeighbourIndex] ) ).isEmpty() ) {

#ifdef DRAW_NEIGHBOURSFINDING
      neighUpY[upperNeighbourIndex] = neighUp[upperNeighbourIndex].fY;
      neighUpZ[upperNeighbourIndex] = neighUp[upperNeighbourIndex].fZ;
#endif
      neighUp[upperNeighbourIndex].fY = DnDx * ( neighUp[upperNeighbourIndex].fY - y );
      neighUp[upperNeighbourIndex].fZ = DnDx * ( neighUp[upperNeighbourIndex].fZ - z );

      maxUpperNeighbourIndex(neighUp[upperNeighbourIndex].fValid)++;
      ++upperNeighbourIndex;

      if ( ISUNLIKELY(upperNeighbourIndex >= kMaxN) ){
        break;
      }
    }

    int nNeighDn = 0;
    if ( upperNeighbourIndex <= 0 ) continue;// only if there are hits in the upper area

    int_v bestDn(-1);
    int_v bestUp(-1);
    float_v bestD = chi2Cut; // d must be smaller than chi2Cut to be considered at all
    int_v bestUpUp(-1);
    float_v bestUU = chi2Cut;
    NeighbourData neighDn;

      // iterate over all hits in lower area
    uint_m nextMask;
    yDn = y - neighUp[upperNeighbourIndex].fY;
    zDn = z - neighUp[upperNeighbourIndex].fZ;

    HitArea areaDn( rowDn, fData, yDn, zDn, -DnDx*kAreaSizeY, -DnDx*kAreaSizeZ, validHitsMask );
//    HitArea areaDn( rowDn, fData, yDn, zDn, -DnDx*kAreaSizeY, -DnDx*kAreaSizeZ/2, validHitsMask );	// -0.5% eff; +0.7ms time
    while ( !( nextMask = areaDn.GetNext( &neighDn ) ).isEmpty() ) {
      if ( ISUNLIKELY( ((uint_v(Vc::Zero) < maxUpperNeighbourIndex) && neighDn.fValid).isEmpty() ) ) continue; // no both neighbours

      assert( (neighDn.fLinks < rowDn.NUnusedHits() || !neighDn.fValid).isFull() );
      nNeighDn++;
        // store distance from (y,z) times UpDx
      neighDn.fY = UpDx * ( neighDn.fY - y );
      neighDn.fZ = UpDx * ( neighDn.fZ - z );

      int_m dnMask( Vc::Zero ); // mask for appropriate neibours-pairs

        // iterate over the upper hits we found before and find the one with lowest curvature
      uint_v curMaxUpperNeighbourIndex = maxUpperNeighbourIndex;
      curMaxUpperNeighbourIndex(!neighDn.fValid) = 0;
      const int maxMaxUpperNeighbourIndex = curMaxUpperNeighbourIndex.max();
      for ( int i = 0; i < maxMaxUpperNeighbourIndex; ++i ) {
        float_m masksf( neighDn.fValid & neighUp[i].fValid ); // only store links for actually useful data.

        const float_v dy = neighDn.fY - neighUp[i].fY;
        const float_v dz = neighDn.fZ - neighUp[i].fZ;
        const float_v d = dy * dy + dz * dz;
        masksf &= d < bestD;
        bestD( masksf ) = d;
        const int_m masks( masksf );
        dnMask |= masks;
        bestUp( masks ) = neighUp[i].fLinks;
      }
      bestDn( dnMask ) = neighDn.fLinks;
    }
    assert( (bestD < chi2Cut || float_m( bestUp == -1 && bestDn == -1 )).isFull() );

      // store the link indexes we found
    assert( ((bestUp >= -1) && (bestUp < rowUp.NUnusedHits()) && validHitsMask) == validHitsMask );
    assert( ((bestDn >= -1) && (bestDn < rowDn.NUnusedHits()) && validHitsMask) == validHitsMask );
    int_m setSegment(validHitsMask && bestDn >= int_v(Vc::Zero) && bestUp >= int_v(Vc::Zero));
#ifdef FOURHITSEGMENTS
    for( unsigned int iV = 0; iV < float_v::Size; iV++ ) {
      if( !setSegment[iV] ) continue;
      hitsDn.push_back(bestDn[iV]);
      hitsMid.push_back(hitIndexes[iV]);
      hitsUp.push_back(bestUp[iV]);
    }

    if( hitsMid.size() == 0 ) continue;
//    if( hitsMid.size() - currentHit >= float_v::Size ) {
      uint_v nh1( hitsMid[currentHit] ), nh2( hitsUp[currentHit] ), nh3( Vc::Zero ), nh0( Vc::Zero );

      for( unsigned int iV = 1; iV < float_v::Size; iV++ ) {
        nh1[iV] = hitsMid[currentHit+iV];
        nh2[iV] = hitsUp[currentHit+iV];
        nh0[iV] = hitsDn[currentHit+iV];
      }

      float_m nhactive( nh1>=0 && nh2>=0 && nh0>=0 && nh1<row.NHits() && nh2<rowUp.NHits() && nh0<rowDn.NHits() );

      float_v Ytr1 = fData.UnusedHitPDataY( row, nh1, nhactive );
      float_v Ztr1 = fData.UnusedHitPDataZ( row, nh1, nhactive );
      float_v Ytr2 = fData.UnusedHitPDataY( rowUp, nh2, nhactive );
      float_v Ztr2 = fData.UnusedHitPDataZ( rowUp, nh2, nhactive );
      float_v Y3t = Ytr2 + Ytr2 - Ytr1;
      float_v Z3t = Ztr2 + Ztr2 - Ztr1;
      HitArea areaUpUp( rowUpUp, fData, Y3t, Z3t, -DnDx*kAreaSizeY, -DnDx*kAreaSizeZ/2, (int_m)nhactive );
      while ( !( nextMask = areaUpUp.GetNext( &neighDn ) ).isEmpty() ) {
        neighDn.fY = DnDx * ( neighDn.fY - Ytr2 );
        neighDn.fZ = DnDx * ( neighDn.fZ - Ztr2 );
        const float_v dy = UpDx * ( Ytr1 - Ytr2 ) - neighDn.fY;
        const float_v dz = UpDx * ( Ztr1 - Ztr2 ) - neighDn.fZ;
        const float_v d = dy * dy + dz * dz;
        float_m dnMaskU( Vc::Zero );
        float_m maskU(nextMask);
        maskU &= d < bestUU;
        bestUU( maskU ) = d;
        dnMaskU |= maskU;
        bestUpUp( (int_m)dnMaskU ) = neighDn.fLinks;
      }
      currentHit += float_v::Size;
//    }
      setSegment &= int_m( bestUpUp >= 0 );
#endif

    fData.SetUnusedHitLinkUpData( row, rowUp, hitIndexes, bestUp, setSegment );
    fData.SetUnusedHitLinkDownData( row, rowDn, hitIndexes, bestDn, setSegment );
    for( unsigned int iV = 0; iV < float_v::Size; iV++ ) {
      if( !setSegment[iV] ) continue;
      unsigned int hit0 = rowDn.HitIndex()[(unsigned int)bestDn[iV]];
      fData.SetHitLinkUpData( rowDn, hit0, row.HitIndex()[(unsigned int)hitIndexes[iV]] );
      if( ISUNLIKELY( startHitsCount > 0 && startHits[hitsStartOffset + startHitsCount - 1].HitIndex() == hit0 ) ) continue;
      if( ISUNLIKELY( startHitsCount > 1 && startHits[hitsStartOffset + startHitsCount - 2].HitIndex() == hit0 ) ) continue;
      if( ISUNLIKELY( startHitsCount > 2 && startHits[hitsStartOffset + startHitsCount - 3].HitIndex() == hit0 ) ) continue;
      if( ISUNLIKELY( startHitsCount > 3 && startHits[hitsStartOffset + startHitsCount - 4].HitIndex() == hit0 ) ) continue;

      startHits[hitsStartOffset + startHitsCount++].Set( rowIndex - rowStep, hit0, 3 );
    }
  } // for hitIndex
  hitsDn.clear();
  hitsMid.clear();
  hitsUp.clear();

  *fTracker.NTracklets() += startHitsCount;
}


#ifdef MAIN_DRAW
#include "AliHLTTPCCADisplay.h"
#include "TApplication.h"
#endif //DRAW

#ifdef V7
void AliHLTTPCCATrackletConstructor::run( unsigned int firstRow, unsigned int &tracksSaved, unsigned int i_it )
#else
void AliHLTTPCCATrackletConstructor::run( unsigned int firstRow, unsigned int &tracksSaved )
#endif
{

#ifdef V7
  if( i_it >= 0 ) {
    fData.CleanUsedHits( 1+firstRow, false );
    fData.CleanUsedHits( 2+firstRow, false );
    fData.CleanUsedHits( 3+firstRow, false );
    CreateStartSegmentV( 2+firstRow, i_it );
#ifdef MAIN_DRAW
    if ( AliHLTTPCCADisplay::Instance().DrawType() == 1 )
    {
      AliHLTTPCCADisplay &disp = AliHLTTPCCADisplay::Instance();
      disp.ClearView();
      disp.SetSliceView();
      disp.SetCurrentSlice( &fTracker );
      disp.DrawSlice( &fTracker, 0 );
      disp.DrawSliceHits(1, 0.5);
      disp.DrawSliceLinks(-1,-1,1);
      AliHLTTPCCADisplay::Instance().SaveCanvasToFile("NFinderTestXYZ.pdf");
      disp.Ask();
    }
#endif
  }

#endif
  //
  assert( *fTracker.NTracklets() < 32768 );
  const int nTracks = *fTracker.NTracklets();

#ifndef NO_NTRACKLET_FIX
  int newTr = 0;
#endif

  unsigned int tracksSavedV = tracksSaved / float_v::Size;
  for ( int trackIteration = tracksSavedV; trackIteration * uint_v::Size < nTracks; ++trackIteration ) {
    const uint_v trackIndex( uint_v( Vc::IndexesFromZero ) + uint_v(trackIteration * uint_v::Size) );
    const uint_m active = trackIndex < nTracks && trackIndex >= tracksSaved;
    if( active.isEmpty() ) continue;
    TrackMemory r;
    r.fStage( !active ) = NullStage;

      // if rowStep = 2 TrackletStartHits need to be sorted such that all even start rows come first. The odd start rows - last.
    uint_v length(Vc::Zero);
    for( unsigned int i = 0; i < float_v::Size; i++ ) {
      if( !active[i] ) continue;
      r.fStartRow[i] = fTracker.TrackletStartHit((unsigned int)trackIndex[i]).fRow;
      r.fCurrentHitIndex[i] = fTracker.TrackletStartHit((unsigned int)trackIndex[i]).fHit;
      length[i] = fTracker.TrackletStartHit((unsigned int)trackIndex[i]).fLength;
    }
    r.fEndRow = static_cast<uint_v>( r.fStartRow );
    r.fLastRow = static_cast<uint_v>( r.fStartRow );
//    r.fStartRow( !active ) = std::numeric_limits<int_v>::max();
    r.fStartRow( !active ) = std::numeric_limits<int>::max();
    r.fFirstRow = r.fStartRow;

#ifdef EXTEND_ALL_TRAKCS
r.fIsFragile = uint_m(true);
#else
    const uint_v MaxNHitsForFragileTracklet(6);
    r.fIsFragile = (length < MaxNHitsForFragileTracklet);
#endif
    const float_v zero( Vc::Zero );
    const float_v one( Vc::One );
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

    {
      InitTracklets init( r, fData, fTracker, fTrackletVectors[trackIteration], trackIndex, active );
      r.fStartRow.callWithValuesSorted( init );
    }
    
//#define USE_COUNTERS
#ifdef USE_COUNTERS
    static unsigned int counters[6] = { 0, 0, 0, 0, 0, 0 };
    counters[0]++;
#endif // USE_COUNTERS

    const int rowStep = AliHLTTPCCAParameters::RowStep;
    { // fit and extrapolate upwards
      int rowIndex = r.fStartRow.min() + rowStep*2;
      const int_v activationRow = r.fStartRow + rowStep*2;
      debugF() << "============================================= Start Fitting Upwards =============================================" << endl;
      // TODO think about getting parameters of all tracks (fragile and !fragile) in the same point (begin or end of track). Needed by Merger
// #define DISABLE_HIT_SEARCH
#ifdef DISABLE_HIT_SEARCH
     while ( rowIndex < fTracker.Param().NRows() && !( r.fStage <= FitLinkedHits ).isEmpty() ) {
#ifdef USE_COUNTERS
       counters[1]++;
#endif // USE_COUNTERS
       
        ++r.fStage( rowIndex == activationRow ); // goes to FitLinkedHits on activation row
        FitTracklet( r, rowIndex, trackIndex, fTrackletVectors[trackIteration] );
        ++rowIndex;
      }


      const int_m ready = r.fStage <= DoneStage;
      const float_v x( fData.RowX(), float_v::IndexType( r.fEndRow ) );
//      const float_v x( fData.RowX(), float_v::IndexType( r.fStartRow ) );
      debugF() << x << float_v::IndexType( r.fEndRow ) << float_v( fData.RowX(), float_v::IndexType( Vc::IndexesFromZero ) ) << endl;
      assert( ( x == 0 && static_cast<float_m>( ready ) ).isEmpty() );
      //assert ( ( (r.fParam.X() == x) && static_cast<float_m>( ready )) == static_cast<float_m>( ready ));

      const int_m transported = static_cast<int_m>( r.fParam.TransportToX(
                                                           x, fTracker.Param().cBz(), .999f, static_cast<float_m>( ready ) ) );
      
      debugF() << "============================================= Stop Fitting Upwards ==============================================" << endl;
#ifdef MAIN_DRAW
      if ( AliHLTTPCCADisplay::Instance().DrawType() == 10 ) {
        foreach_bit( int ii, r.fStage < DoneStage ) {
          TrackParam t( r.fParam, ii );
          AliHLTTPCCADisplay::Instance().ClearView();
          AliHLTTPCCADisplay::Instance().DrawSlice( &fTracker, 0 );
          AliHLTTPCCADisplay::Instance().DrawSliceHits();
          AliHLTTPCCADisplay::Instance().DrawTrackParam( t );
        }
        AliHLTTPCCADisplay::Instance().Ask();
      }
#endif
#else // DISABLE_HIT_SEARCH
        // fit all chains
      while ( rowIndex < fTracker.Param().NRows() && ( r.fStage == ExtrapolateUp ).isEmpty() ) {
#ifdef USE_COUNTERS
        counters[1]++;
#endif // USE_COUNTERS
        
        r.fStage( rowIndex == activationRow ) = FitLinkedHits; // goes to FitLinkedHits on activation row

        ExtendTracklet( r, rowIndex, trackIndex, fTrackletVectors[trackIteration], 1, int_m(Vc::Zero) );
        ++rowIndex;
      }

      debugF() << "========================================== Start Extrapolating Upwards ==========================================" << endl;

        // some chains are fitted, some - are extrapolated
      assert( (r.fRemainingGap >= 0).isFull() );
      while ( rowIndex < fTracker.Param().NRows() && !( r.fStage <= ExtrapolateUp ).isEmpty() ) {
#ifdef USE_COUNTERS
       counters[2]++;
#endif // USE_COUNTERS

        r.fStage( rowIndex == activationRow ) = FitLinkedHits; // goes to FitLinkedHits on activation row
        const int_m toExtrapolate = (r.fStage == ExtrapolateUp);
        ExtendTracklet( r, rowIndex, trackIndex, fTrackletVectors[trackIteration], 1, toExtrapolate );
        ++rowIndex;
      }

      debugF() << "============================================= Stop Fitting Upwards ==============================================" << endl;

        // end extrapolate chains
      int_m mask;
      assert( (r.fRemainingGap >= 0).isFull() );
      while ( rowIndex < fTracker.Param().NRows() && !( mask = r.fStage == ExtrapolateUp ).isEmpty() ) {
#ifdef USE_COUNTERS
        counters[3]++;
#endif // USE_COUNTERS

        assert ( (r.fStage != FitLinkedHits).isFull() );
        ExtrapolateTracklet( r, rowIndex, trackIndex, fTrackletVectors[trackIteration], 1, mask );
        ++rowIndex;
      }
      debugF() << "=========================================== Stop Extrapolating Upwards ==========================================" << endl;
    }
      // r.fStage (r.fNHits < MaxNHitsForFragileTracklet) = DoneStage; // TODO investigate
    { // extrapolate downwards
      r.fRemainingGap = AliHLTTPCCAParameters::MaximumExtrapolationRowGap; // allow full gaps again
      ++r.fStage( r.fStage == ExtrapolateUp ); // FitLinkedHits/ExtrapolateUp went so high that no gap put the tracklet into WaitingForExtrapolateDown
      const int_m ready = r.fStage == WaitingForExtrapolateDown;
      debugF() << "ready to extrapolate downwards: " << ready << endl;
      ++r.fStage( ready ); // the wait is over

      if(1){ // set track parameters to x of end row of the fitting stage
        //const float_v x( fData.RowX(), float_v::IndexType( r.fEndRow ) );
        const float_v x( fData.RowX(), float_v::IndexType( r.fStartRow ), static_cast<float_m>(ready));
        debugF() << x << float_v::IndexType( r.fEndRow ) << float_v( fData.RowX(), float_v::IndexType( Vc::IndexesFromZero ) ) << endl;
        assert( ( x == 0 && static_cast<float_m>( ready ) ).isEmpty() );
        const int_m transported = static_cast<int_m>( r.fParam.TransportToX(
                                                            x, fTracker.Param().cBz(), .999f, static_cast<float_m>( ready ) ) );
        ++r.fStage( !transported ); // all those where transportation failed go to DoneStage
      }
#ifdef MAIN_DRAW
      if ( AliHLTTPCCADisplay::Instance().DrawType() == 1 ) {
        for(int ii=0; ii<int_v::Size; ii++)
        {
          if(!(r.fStage[ii] < DoneStage)) continue;
          TrackParam t( r.fParam, ii );
          AliHLTTPCCADisplay::Instance().ClearView();
          AliHLTTPCCADisplay::Instance().DrawSlice( &fTracker, 0 );
          AliHLTTPCCADisplay::Instance().DrawSliceHits();
          AliHLTTPCCADisplay::Instance().DrawTrackParam( t, 2 );
        }
        AliHLTTPCCADisplay::Instance().Ask();
      }
#endif
      debugF() << "========================================= Start Extrapolating Downwards =========================================" << endl;
      int_v tmpRow(r.fStartRow); // take only one hit from fitted chain
      tmpRow(!active) = fTracker.Param().NRows()-1;
      int rowIndex = tmpRow.max();
        // extrapolate along fitted chains
      int_m mask; 
      const int minMaskedExtrRow = r.fStartRow.min();
      while ( rowIndex >= minMaskedExtrRow && !( mask = r.fStage == ExtrapolateDown ).isEmpty() ) {
#ifdef USE_COUNTERS
       counters[4]++;
#endif // USE_COUNTERS
       mask &= rowIndex < r.fStartRow + 1;
//       mask &= ( ( rowIndex - r.fStartRow ) & int_v( std::numeric_limits<int_v::EntryType>::min() + 1 ) ) != int_v( Vc::Zero ); // CHECKME why do we need this?
       mask &= ( ( rowIndex - r.fStartRow ) != int_v( Vc::Zero ) & int_v( std::numeric_limits<int_v::EntryType>::min() + 1 ) != int_v( Vc::Zero ) ); // CHECKME why do we need this?
       ExtrapolateTracklet( r, rowIndex, trackIndex, fTrackletVectors[trackIteration], 0, mask );
       --rowIndex;
      }
        // extrapolote downwards and find addition hits TODO try to delete that in new version
      while ( rowIndex >= 0 && !( mask = r.fStage == ExtrapolateDown ).isEmpty() ) {
#ifdef USE_COUNTERS
       counters[5]++;
#endif // USE_COUNTERS
       ExtrapolateTracklet( r, rowIndex, trackIndex, fTrackletVectors[trackIteration], 0, mask );
        --rowIndex;
      }
      r.fFirstRow = CAMath::Min( r.fFirstRow, r.fStartRow );
#ifdef MAIN_DRAW
      if ( AliHLTTPCCADisplay::Instance().DrawType() == 10 ) {
        for(int ii=0; ii<int_v::Size; ii++)
        {
          if(!(r.fStage[ii] < NullStage)) continue;
//         foreach_bit( int ii, r.fStage < NullStage ) {
          TrackParam t( r.fParam, ii );
          AliHLTTPCCADisplay::Instance().ClearView();
          AliHLTTPCCADisplay::Instance().DrawSlice( &fTracker, 0 );
          AliHLTTPCCADisplay::Instance().DrawSliceHits();
          AliHLTTPCCADisplay::Instance().DrawTrackParam( t, 4 );
//          AliHLTTPCCADisplay::Instance().Ask();
        }
        AliHLTTPCCADisplay::Instance().Ask();
      }
#endif

#endif // DISABLE_HIT_SEARCH
    }
#ifdef USE_COUNTERS
    std::cout << "Counters= " << counters[0] << " " << counters[1] << " " << counters[2] << " " << counters[3] << " " << counters[4] << " " << counters[5] << std::endl;
#endif // USE_COUNTERS
       
    float_m trackletOkF( r.fNHits >= uint_v(AliHLTTPCCAParameters::MinimumHitsForTracklet) );
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
      && r.fParam.Err2QPt()    > float_v(Vc::Zero);
    trackletOkF &= r.fParam.Err2Y()      > float_v(Vc::Zero) && r.fParam.Err2Z()      > float_v(Vc::Zero);
    trackletOkF &= r.fParam.Err2SinPhi() > float_v(Vc::Zero) && r.fParam.Err2DzDs()   > float_v(Vc::Zero);
//    trackletOkF &= ( r.fParam.Chi2()/static_cast<float_v>(r.fParam.NDF()) < 25.f ); // TODO
    debugF() << r.fParam << "-> trackletOk: " << trackletOkF << endl;

    const int_m trackletOk( trackletOkF );
    r.fNHits.setZero( !trackletOk );

      //////////////////////////////////////////////////////////////////////
      //
      //////////////////////////////////////////////////////////////////////
    TrackletVector &tracklet = fTrackletVectors[trackIteration];
    tracklet.SetNHits( r.fNHits, active );

    if ( !( r.fNHits > 0 ).isEmpty() ) {
#ifdef MAIN_DRAW
      if ( AliHLTTPCCADisplay::Instance().DrawType() == 10 ) {
        for(int ii=0; ii<int_v::Size; ii++)
        {
          if(!(r.fStage[ii] < DoneStage)) continue;
//         foreach_bit( int ii, r.fStage < DoneStage ) {
          TrackParam t( r.fParam, ii );
          AliHLTTPCCADisplay::Instance().ClearView();
          AliHLTTPCCADisplay::Instance().DrawSlice( &fTracker, 0 );
          AliHLTTPCCADisplay::Instance().DrawSliceHits();
          AliHLTTPCCADisplay::Instance().DrawTrackParam( t, 2 );
//          AliHLTTPCCADisplay::Instance().Ask();
        }
        AliHLTTPCCADisplay::Instance().Ask();
      }
#endif
        // start and end rows of the tracklet
      tracklet.SetFirstRow( static_cast<uint_v>( r.fFirstRow ), active );
      tracklet.SetLastRow( r.fLastRow, active );

        ///mvz start 25.01.2010
      const float_m MinQPt = CAMath::Abs(r.fParam.QPt()) < AliHLTTPCCAParameters::MinimumQPt;
      float_v NewQPt = r.fParam.QPt();
      NewQPt(MinQPt) = AliHLTTPCCAParameters::MinimumQPt;
      r.fParam.SetQPt(NewQPt);
        //      r.fParam.SetQPt( CAMath::Max( AliHLTTPCCAParameters::MinimumQPt, r.fParam.QPt() ) );
        ///mvz end 25.01.2010
      tracklet.SetParam( r.fParam, (float_m)active );

#ifndef NO_NTRACKLET_FIX
      for( int iV = 0; iV < float_v::Size; iV++ ) {
	if( active[iV] ) newTr++;
      }
#endif

      debugTS() << r.fFirstRow << r.fLastRow << endl;
      debugTS() << "set hit weigths from row " << r.fFirstRow.min() << " until row " << r.fLastRow.max() << endl;
//      const uint_v &weight = SliceData::CalculateHitWeight( r.fNHits, trackIndex );
        // for all rows where we have a hit let the fTracker know what weight our hits have
      for ( unsigned int rowIndex = r.fFirstRow.min(); rowIndex <= r.fLastRow.max(); ++rowIndex ) {
        const uint_v &hitIndex = tracklet.HitIndexAtRow( rowIndex );
        fData.MaximizeHitWeight( fData.Row( rowIndex ), hitIndex, r.fNHits );
#ifdef V7
        if( i_it >= 0 ) {
          fData.SetHitAsUsed( fData.Row( rowIndex ), hitIndex, int_m(hitIndex<fData.Row( rowIndex ).NHits()) );
        }
#endif
      }
    }
  }

#ifndef NO_NTRACKLET_FIX
  *fTracker.NTracklets() -= ( nTracks - tracksSaved - newTr );
  tracksSaved += newTr;
#else
  tracksSaved += (nTracks - tracksSaved);
#endif
}

void InitTracklets::operator()( int rowIndex )
{
  if ( ISUNLIKELY( rowIndex >= fTracker.Param().NRows() || rowIndex < 0 ) ) {
    return;
  }
  debugF() << "InitTracklets(" << rowIndex << ")" << endl;
  const int rowStep = AliHLTTPCCAParameters::RowStep;
  assert( rowIndex < fTracker.Param().NRows() - rowStep );
    // the first hit is a special case (easy)
  int_m mask = rowIndex == r.fStartRow;
  float_m maskF( mask );
  {
    const AliHLTTPCCARow &row = fData.Row( rowIndex );
    VALGRIND_CHECK_MEM_IS_ADDRESSABLE( &row, sizeof( AliHLTTPCCARow ) );
    const float_v x = fData.RowX( rowIndex );
    const uint_v &hitIndex = static_cast<uint_v>( r.fCurrentHitIndex );
    const float_v y = fData.HitPDataY( row, hitIndex, maskF );
    const float_v z = fData.HitPDataZ( row, hitIndex, maskF );
    r.fParam.SetX( x, maskF );
    r.fParam.SetY( y, maskF );
    r.fParam.SetZ( z, maskF );
    fTrackletVector.SetRowHits( rowIndex, trackIndex, hitIndex, mask );

      // mark first hit as used
    int_v isUsed;
    for( unsigned int i = 0; i < float_v::Size; i++ ) {
      if( !mask[i] ) continue;
      isUsed[i] = fData.HitDataIsUsed( row )[(unsigned int)r.fCurrentHitIndex[i]];
    }
    fData.SetHitAsUsedInTrackFit( row, static_cast<uint_v>( r.fCurrentHitIndex ), mask && ( isUsed == int_v(1) ) );
#ifdef VC_GATHER_SCATTER
    r.fCurrentHitIndex.gather( fData.HitLinkUpData( row ), hitIndex, mask ); // set to next linked hit
#else
    for( unsigned int i = 0; i < float_v::Size; i++ ) {
      if( !mask[i] ) continue;
      r.fCurrentHitIndex[i] = fData.HitLinkUpData( row )[(unsigned int)hitIndex[i]];
    }
#endif
    // the first hit in the Tracklet is guaranteed to have a link up, since StartHitsFinder
    // ensures it
    assert( ( r.fCurrentHitIndex >= 0 && mask ) == mask );
  }

  mask &= (r.fCurrentHitIndex >= 0);
  maskF &= static_cast<float_m>(r.fCurrentHitIndex >= 0);
  rowIndex += rowStep;
  {
    const AliHLTTPCCARow &row = fData.Row( rowIndex );
    const float_v x = fData.RowX( rowIndex );
    const uint_v &hitIndex = static_cast<uint_v>( r.fCurrentHitIndex );
    const float_v y = fData.HitPDataY( row, hitIndex, maskF );
    const float_v z = fData.HitPDataZ( row, hitIndex, maskF );
    const float_v &dx = x - r.fParam.X();
    const float_v &dy = y - r.fParam.Y();
    const float_v &dz = z - r.fParam.Z();
    const float_v &ri = float_v( Vc::One ) * CAMath::RSqrt( dx * dx + dy * dy );
    const float_v &sinPhi = dy * ri;
    r.fParam.SetSinPhi( sinPhi,  maskF );
    r.fParam.SetDzDs  ( dz * ri, maskF );
    float_v err2Y, err2Z;
    fTracker.GetErrors2( rowIndex, r.fParam, &err2Y, &err2Z );
    r.fParam.SetCov( 0, err2Y, maskF );
    r.fParam.SetCov( 2, err2Z, maskF );

    const float_m transported = r.fParam.TransportToX( x, sinPhi, fTracker.Param().cBz(), -1.f, maskF );
    // assert( transported == maskF );
    fTracker.GetErrors2( rowIndex, r.fParam, &err2Y, &err2Z );
    const int_m hitAdded( r.fParam.Filter( maskF, y, z, err2Y, err2Z, .99f ) );
    // assert( hitAdded == mask );
    UNUSED_PARAM2( transported, hitAdded );
    fTrackletVector.SetRowHits( rowIndex, trackIndex, hitIndex, mask );

    r.fLastY( maskF ) = y;
    r.fLastZ( maskF ) = z;
    
      // mark 2-nd hit as used
    int_v isUsed;
    for( unsigned int i = 0; i < float_v::Size; i++ ) {
      if( !mask[i] ) continue;
      isUsed[i] = fData.HitDataIsUsed( row )[(unsigned int)r.fCurrentHitIndex[i]];
    }
    fData.SetHitAsUsedInTrackFit( row, static_cast<uint_v>( r.fCurrentHitIndex ), static_cast<int_m>(mask) && ( isUsed == int_v(1) ) );
#ifdef VC_GATHER_SCATTER
    r.fCurrentHitIndex.gather( fData.HitLinkUpData( row ), hitIndex, mask ); // set to next linked hit
#else
    for( unsigned int i = 0; i < float_v::Size; i++ ) {
      if( !mask[i] ) continue;
      r.fCurrentHitIndex[i] = fData.HitLinkUpData( row )[(unsigned int)hitIndex[i]];
    }
#endif
    // the second hit in the Tracklet is also guaranteed to have a link up, since StartHitsFinder
    // ensures it
    assert( ( r.fCurrentHitIndex >= 0 && mask ) == mask );
  }
}
