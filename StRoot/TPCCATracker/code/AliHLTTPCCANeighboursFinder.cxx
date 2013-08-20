// @(#) $Id: AliHLTTPCCANeighboursFinder.cxx,v 1.14 2013/08/20 16:05:09 fisyak Exp $
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

#include "AliHLTTPCCANeighboursFinder.h"
#include "AliHLTTPCCAMath.h"
#include "AliHLTTPCCAHitArea.h"
#include "AliHLTTPCCATracker.h"
#include "AliHLTArray.h"
#include "AliHLTVector.h"

#ifndef HLTCA_STANDALONE
#include "AliHLTTPCCADisplay.h"
#endif

#ifdef USE_TBB
#include <tbb/parallel_for.h>
#include <tbb/blocked_range.h>
#endif //USE_TBB

// #ifdef DRAW
// #include "AliHLTTPCCADisplay.h"
// #endif

#include "debug.h"
#ifndef NVALGRIND 
#include <valgrind/memcheck.h>
#endif
using namespace Vc;

bool DRAW_EVERY_LINK = false;

#ifdef USE_TBB
inline void AliHLTTPCCATracker::NeighboursFinder::operator()( const tbb::blocked_range<int> &r ) const
{
  for ( int i = r.begin(); i < r.end(); ++i ) {
    executeOnRow( i );
  }
}
#endif //USE_TBB

inline void AliHLTTPCCATracker::NeighboursFinder::executeOnRow( int rowIndex ) const
{
  debugS() << "NeighboursFinder on row " << rowIndex << std::endl;
  const AliHLTTPCCARow &row = fData.Row( rowIndex );

  // references to the rows above and below
  const int rowStep = AliHLTTPCCAParameters::RowStep;

  const AliHLTTPCCARow &rowUp = fData.Row( rowIndex + rowStep );
  const AliHLTTPCCARow &rowDn = fData.Row( rowIndex - rowStep );

  
  const int numberOfHits = row.NHits();
  const int numberOfHitsUp = rowUp.NHits();
  const int numberOfHitsDown = rowDn.NHits();
  if ( numberOfHits == 0 ) {
    debugS() << "no hits in this row" << std::endl;
    return;
  }
  if ( numberOfHitsDown == 0 || numberOfHitsUp == 0 ) {
    debugS() << "no hits in neighbouring rows" << std::endl;
    return;
  }

  // the axis perpendicular to the rows
  const float xDn = fData.RowX( rowIndex - rowStep );
  const float x   = fData.RowX( rowIndex     );
  const float xUp = fData.RowX( rowIndex + rowStep );

  // distance of the rows (absolute and relative)
  const float UpDx = xUp - x;
  const float DnDx = xDn - x;
  const float UpTx = xUp / x;
  const float DnTx = xDn / x;
//   std::cout << UpDx << " " << DnDx << std::endl;
  
  // UpTx/DnTx is used to move the HitArea such that central events are preferred (i.e. vertices
  // coming from y = 0, z = 0).

  /* find the connection between hits in the row below, this hit and a hit in the row above
   * minimizing the difference in slope, i.e.
   * ______________________________________
   *  \                           |dy1
   *   \                          |       dx1
   * ___\_________________________|________
   *     \     is better than      \      dx2
   * _____\______________________dy2\______
   *
   *
   * The slope is easily calculated as dy/dx ( dz/dx ).
   * Comparison: dy1/dx1 - dy2/dx2 = dslopeY
   * => dy1 * dx2 - dy2 * dx1 = dslopeY * dx1 * dx2
   *
   * y+z Comparison is done with:
   * dslopeY^2 + dslopeZ^2
   *
   * The constant ( dx1^2 + dx2^2 ) is multiplied into the chi2Cut
   */

//   static const float kAreaSize = 1.f;
  static const float kAreaSizeY = AliHLTTPCCAParameters::NeighbourAreaSizeTgY[fIter];
  static const float kAreaSizeZ = AliHLTTPCCAParameters::NeighbourAreaSizeTgZ[fIter];
  static const int kMaxN = 20; // TODO minimaze
  const float chi2Cut = AliHLTTPCCAParameters::NeighbourChi2Cut[fIter]*AliHLTTPCCAParameters::NeighbourChi2Cut[fIter] * 4.f * ( UpDx * UpDx + DnDx * DnDx );

  // some step sizes on the current row. the ushorts in hits multiplied with the step size give
  // the offset on the grid

  typedef HitArea::NeighbourData NeighbourData;

  STATIC_ASSERT( short_v::Size % float_v::Size == 0, Short_Vector_Size_is_not_a_multiple_of_Float_Vector_Size );
  for ( int hitIndex = 0; hitIndex < numberOfHits; hitIndex += short_v::Size ) {
    NeighbourData neighUp[kMaxN];
// #ifdef DRAW
// #define DRAW_NEIGHBOURSFINDING
// #endif
#ifdef DRAW_NEIGHBOURSFINDING
    sfloat_v neighUpY[kMaxN];
    sfloat_v neighUpZ[kMaxN];
#endif      
    const short_m &validHitsMask = short_v( Vc::IndexesFromZero ) + hitIndex < numberOfHits;

    // if the rows above and below both have hits (because if they don't, we don't need to
    // bother to search for a path)
    int upperNeighbourIndex = 0;

    // coordinates of the hit in the current row
    sfloat_v y, z, yUpTx, zUpTx, yDnTx, zDnTx;
    y = fData.HitDataY( row, hitIndex );
    z = fData.HitDataZ( row, hitIndex );
#ifdef DRAW_NEIGHBOURSFINDING
    std::cout << " centr y = " << y << " centr z = " << z << std::endl;
#endif
    yUpTx = y * UpTx; // suppose vertex at (0,0,0)
    yDnTx = y * DnTx;
    zUpTx = z * UpTx;
    zDnTx = z * DnTx;

    // create and initialize area hit iterators
    HitArea areaUp( rowUp, fData, yUpTx, zUpTx, UpDx*kAreaSizeY, UpDx*kAreaSizeZ, validHitsMask );

    // iterate over hits in upper area (kMaxN at max)
    while ( areaUp.GetNext( &neighUp[upperNeighbourIndex] ) ) {
      assert( neighUp[upperNeighbourIndex].fLinks < rowUp.NHits() || !neighUp[upperNeighbourIndex].fValid );
      debugS() << neighUp[upperNeighbourIndex];
#ifndef NVALGRIND
      VALGRIND_CHECK_VALUE_IS_DEFINED( neighUp[upperNeighbourIndex].fLinks );
      VALGRIND_CHECK_VALUE_IS_DEFINED( neighUp[upperNeighbourIndex].fValid );
      VALGRIND_CHECK_VALUE_IS_DEFINED( neighUp[upperNeighbourIndex].fY );
      VALGRIND_CHECK_VALUE_IS_DEFINED( neighUp[upperNeighbourIndex].fZ );
#endif
#ifdef DRAW_NEIGHBOURSFINDING
      neighUpY[upperNeighbourIndex] = neighUp[upperNeighbourIndex].fY;
      neighUpZ[upperNeighbourIndex] = neighUp[upperNeighbourIndex].fZ;
#endif      
      neighUp[upperNeighbourIndex].fY = DnDx * ( neighUp[upperNeighbourIndex].fY - y );
      neighUp[upperNeighbourIndex].fZ = DnDx * ( neighUp[upperNeighbourIndex].fZ - z );
#ifndef NVALGRIND
      VALGRIND_CHECK_VALUE_IS_DEFINED( neighUp[upperNeighbourIndex].fY );
      VALGRIND_CHECK_VALUE_IS_DEFINED( neighUp[upperNeighbourIndex].fZ );
#endif
      ++upperNeighbourIndex;
      //assert( (upperNeighbourIndex < kMaxN) || ("" == " too small array ") );
      if ( upperNeighbourIndex >= kMaxN ){
        // std::cout << "W AliHLTTPCCANeighboursFinder: Warning: Too many neighbours, some of them won't be considered \ Too small array. " << std::endl;
        break;
      }
    }

    int nNeighDn = 0;
    if ( upperNeighbourIndex > 0 ) { // only if there are hits in the upper area
      short_v bestDn = -1;
      short_v bestUp = -1;
      sfloat_v bestD = chi2Cut; // d must be smaller than chi2Cut to be considered at all
      NeighbourData neighDn;

      // iterate over all hits in lower area
      HitArea areaDn( rowDn, fData, yDnTx, zDnTx, -DnDx*kAreaSizeY, -DnDx*kAreaSizeZ, validHitsMask );
      while ( areaDn.GetNext( &neighDn ) ) {
        assert( neighDn.fLinks < rowDn.NHits() || !neighDn.fValid );
#ifndef NVALGRIND
        VALGRIND_CHECK_VALUE_IS_DEFINED( neighDn.fLinks );
        VALGRIND_CHECK_VALUE_IS_DEFINED( neighDn.fValid );
        VALGRIND_CHECK_VALUE_IS_DEFINED( neighDn.fY );
        VALGRIND_CHECK_VALUE_IS_DEFINED( neighDn.fZ );
#endif
        debugS() << neighDn;

        nNeighDn++;
#ifdef DRAW_NEIGHBOURSFINDING
        sfloat_v neighDnY = neighDn.fY;
        sfloat_v neighDnZ = neighDn.fZ;
#endif        
        // store distance from (y,z) times UpDx
        neighDn.fY = UpDx * ( neighDn.fY - y );
        neighDn.fZ = UpDx * ( neighDn.fZ - z );

        short_m dnMask( Vc::Zero );

        // iterate over the upper hits we found before and find the one with lowest curvature
        // (change of slope)
        for ( int i = 0; i < upperNeighbourIndex; ++i ) {
          sfloat_m masks( neighDn.fValid & neighUp[i].fValid ); // only store links for actually useful data.
          const sfloat_v dy = neighDn.fY - neighUp[i].fY;
          const sfloat_v dz = neighDn.fZ - neighUp[i].fZ;
#ifdef DRAW_NEIGHBOURSFINDING
          AliHLTTPCCADisplay::Instance().ClearView();
          AliHLTTPCCADisplay::Instance().SetSliceView();
          AliHLTTPCCADisplay::Instance().SetCurrentSlice(fTracker);
          AliHLTTPCCADisplay::Instance().DrawSlice( fTracker, 1 );
          AliHLTTPCCADisplay::Instance().DrawPoint(xDn, neighDnY[0], neighDnZ[0], 2-2 );
          AliHLTTPCCADisplay::Instance().DrawPoint(x, y[0], z[0], 8-2 );
          AliHLTTPCCADisplay::Instance().DrawPoint(xUp, neighUpY[i][0], neighUpZ[i][0], 4-2 );
          AliHLTTPCCADisplay::Instance().DrawSliceHits(-1, 0.5);
          AliHLTTPCCADisplay::Instance().Ask();
#endif
          const sfloat_v d = dy * dy + dz * dz;
          masks &= d < bestD;
          bestD( masks ) = d;
          const short_m bigMask( masks );
          dnMask |= bigMask;
          bestUp( bigMask ) = neighUp[i].fLinks;
          debugS() << "best up: " << bigMask << " " << bestUp << std::endl;
        }
        bestDn( dnMask ) = neighDn.fLinks;
        debugS() << "best down: " << dnMask << " " << bestDn << std::endl;
      }

      //debugS() << "Links for row " << rowIndex << ", hit " << hitIndex << ": " << bestUp << " " << bestDn << std::endl;

      assert( bestD < chi2Cut || ( bestUp == -1 && bestDn == -1 ) );

      debugS() << "Set Link Data: Up = " << bestUp << ", Down = " << bestDn << std::endl;

      // store the link indexes we found
      const ushort_v &hitIndexes = ushort_v( Vc::IndexesFromZero ) + hitIndex;
      short_m nonUsedMask = validHitsMask && ( short_v(fData.HitDataIsUsed( row ), hitIndexes) == short_v( Vc::Zero ) );
      assert( ((bestUp >= -1) && (bestUp < rowUp.NHits()) && nonUsedMask) == nonUsedMask );
      assert( ((bestDn >= -1) && (bestDn < rowDn.NHits()) && nonUsedMask) == nonUsedMask );
      fData.SetHitLinkUpData( row, hitIndexes, bestUp, nonUsedMask);
      fData.SetHitLinkDownData( row, hitIndexes, bestDn, nonUsedMask);

#ifdef DRAW
      if ( DRAW_EVERY_LINK ) {
        foreach_bit ( int i, validHitsMask && bestUp != -1 && bestDn != -1 ) {
          AliHLTTPCCADisplay::Instance().DrawSliceLink( rowIndex, hitIndex + i, -1, -1, 1 );
        }
        AliHLTTPCCADisplay::Instance().Update();
      }
#endif
    }
  } // for hitIndex

}

void AliHLTTPCCATracker::NeighboursFinder::execute()
{
  const int numberOfRows = fTracker->Param().NRows();

#ifdef DRAW
  AliHLTTPCCADisplay::Instance().SetSliceView();
  AliHLTTPCCADisplay::Instance().ClearView();
  AliHLTTPCCADisplay::Instance().SetCurrentSlice( fTracker );
  AliHLTTPCCADisplay::Instance().DrawSlice( fTracker, 0 );
  AliHLTTPCCADisplay::Instance().DrawSliceHits();
#endif
  //tbb::affinity_partitioner partitioner;
  const int rowStep = AliHLTTPCCAParameters::RowStep;
#ifdef USE_TBB  
  tbb::parallel_for( tbb::blocked_range<int>( rowStep, numberOfRows - rowStep, 1000 ), *this );//, partitioner );
#else  //USE_TBB  
  for ( int iRow = rowStep; iRow < numberOfRows - rowStep; ++iRow ) {
    executeOnRow( iRow );
  }
#endif //USE_TBB  

//   for ( int i = 0; i < AliHLTTPCCAParameters::NumberOfRows; ++i ) {
//     const AliHLTTPCCARow &row = fData.Row( i );
//     for ( int hit = 0; hit < row.NHits(); ++hit ) {
//       std::cout << hit << " " << fData.HitLinkUpData( row )[hit] << std::endl;
//       
//     }
//   }
}
