/*
 This file is property of and copyright by the ALICE HLT Project        *
 ALICE Experiment at CERN, All rights reserved.                         *
 See cxx source for full Copyright notice                               *
*/
#ifndef ALIHLTTPCCANEIGHBOURSFINDER_H
#define ALIHLTTPCCANEIGHBOURSFINDER_H

#include "AliHLTTPCCATracker.h"
#include "AliHLTTPCCAHitArea.h"

#ifdef USE_TBB
#include <tbb/blocked_range.h>
#endif //USE_TBB

/**
 * @class AliHLTTPCCANeighboursFinder
 */
class AliHLTTPCCATracker::NeighboursFinder
{
  public:
    class ExecuteOnRow;
    NeighboursFinder( AliHLTTPCCATracker *tracker, SliceData &sliceData, int iIter ) : fTracker( tracker ), fData( sliceData ), fIter(iIter) {}
    void execute();
  
#ifdef USE_TBB
    void operator()( const tbb::blocked_range<int> &r ) const;
#endif //USE_TBB
  
  private:
    void executeOnRow( int rowIndex ) const;

    AliHLTTPCCATracker *fTracker;
    SliceData &fData;
    int fIter; // current iteration of finding
};

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

//#define USE_CURV_CUT // TODO don't work, problem with error estimation
#ifdef USE_CURV_CUT  
  float curv2Cut =  AliHLTTPCCAParameters::NeighbourCurvCut[fIter] * .5 * ( UpDx * DnDx * ( UpDx - DnDx ) ); // max curv^2*(dx1*dx2*(dx1+dx2))^2 = (dslopeY^2+dslopeZ^2)*(dx1*dx2)^2 = (dy1*dx2-dy2*dx1)^2+(dz1*dx2-dz2*dx1)^2
  curv2Cut *= curv2Cut;
  const float UpErr2 = (rowIndex - rowStep < AliHLTTPCCAParameters::NumberOfInnerRows) ? 0.06*0.06 : 0.12*0.12,
              DnErr2 = (rowIndex + rowStep < AliHLTTPCCAParameters::NumberOfInnerRows) ? 0.06*0.06 : 0.12*0.12;

//  std::cout << UpDx << " " <<  DnDx << std::endl;
//  std::cout << "before " << 4.f * ( UpDx * UpDx + DnDx * DnDx ) << " now: " << 1/4.f * ( UpDx * DnDx * ( UpDx - DnDx ) ) * ( UpDx * DnDx * ( UpDx - DnDx ) ) << std::endl;
#else // USE_CURV_CUT  
  const float chi2Cut = AliHLTTPCCAParameters::NeighbourChiCut[fIter]*AliHLTTPCCAParameters::NeighbourChiCut[fIter] * 4.f * ( UpDx * UpDx + DnDx * DnDx );
#endif // USE_CURV_CUT  
  // some step sizes on the current row. the ushorts in hits multiplied with the step size give
  // the offset on the grid

  typedef HitArea::NeighbourData NeighbourData;

  STATIC_ASSERT( short_v::Size % float_v::Size == 0, Short_Vector_Size_is_not_a_multiple_of_Float_Vector_Size );
  for ( int hitIndex = 0; hitIndex < numberOfHits; hitIndex += short_v::Size ) {

// #ifdef DRAW
// #define DRAW_NEIGHBOURSFINDING
// #endif
#ifdef DRAW_NEIGHBOURSFINDING
    sfloat_v neighUpY[kMaxN];
    sfloat_v neighUpZ[kMaxN];
#endif      

    ushort_v hitIndexes = ushort_v( Vc::IndexesFromZero ) + hitIndex;
    const short_m &validHitsMask = hitIndexes < numberOfHits;
    
    // if the rows above and below both have hits (because if they don't, we don't need to
    // bother to search for a path)


    // coordinates of the hit in the current row
    //ik 04/17/14    sfloat_v y, z, yUp, zUp, yDn, zDn;
    sfloat_v y(Vc::Zero), z(Vc::Zero), yUp(Vc::Zero), zUp(Vc::Zero), yDn(Vc::Zero), zDn(Vc::Zero); 
    // y = fData.HitDataY( row, hitIndexes, static_cast<sfloat_m>(validHitsMask) );
    // z = fData.HitDataZ( row, hitIndexes, static_cast<sfloat_m>(validHitsMask) );
    y = fData.HitDataY( row, hitIndex );
    z = fData.HitDataZ( row, hitIndex );
#ifdef DRAW_NEIGHBOURSFINDING
    std::cout << " centr y = " << y << " centr z = " << z << std::endl;
#endif
    yUp = y * UpTx; // suppose vertex at (0,0,0)
    yDn = y * DnTx; // TODO change name
    zUp = z * UpTx;
    zDn = z * DnTx;

#ifdef USE_CURV_CUT
    const sfloat_v UpDy = yUp - y,
       DnDy = yDn - y;
    const sfloat_v UpR2 = UpDy*UpDy + UpDx*UpDx,
      DnR2 = DnDy*DnDy + DnDx*DnDx;
    sfloat_v chi2Cut = curv2Cut + (UpErr2/UpR2 + DnErr2/DnR2)*(UpDx*DnDx)*(UpDx*DnDx); // additional error in dSlopes TODO Take into account z. TODO Use more precise error estimation // TODO change name // TODO optimize
    // std::cout << rowIndex << " " << curv2Cut << UpErr2/UpR2*(UpDx*DnDx)*(UpDx*DnDx) << DnErr2/DnR2*(UpDx*DnDx)*(UpDx*DnDx) << std::endl;
    // std::cout << UpErr2 << " " << DnErr2 << " " << UpDy << " " << DnDy << " " << UpR2 << " " << DnR2 << std::endl;
#endif //  USE_CURV_CUT

      // find all neighbours in upper area (kMaxN at max)
    HitArea areaUp( rowUp, fData, yUp, zUp, UpDx*kAreaSizeY, UpDx*kAreaSizeZ, validHitsMask );

    ushort_v maxUpperNeighbourIndex = ushort_v(Vc::Zero);
    int upperNeighbourIndex = 0;
    NeighbourData neighUp[kMaxN];
    while ( !( areaUp.GetNext( &neighUp[upperNeighbourIndex] ) ).isEmpty() ) {
      assert( neighUp[upperNeighbourIndex].fLinks < rowUp.NHits() || !neighUp[upperNeighbourIndex].fValid );
      debugS() << neighUp[upperNeighbourIndex];

#ifdef DRAW_NEIGHBOURSFINDING
      neighUpY[upperNeighbourIndex] = neighUp[upperNeighbourIndex].fY;
      neighUpZ[upperNeighbourIndex] = neighUp[upperNeighbourIndex].fZ;
#endif      
      neighUp[upperNeighbourIndex].fY = DnDx * ( neighUp[upperNeighbourIndex].fY - y );
      neighUp[upperNeighbourIndex].fZ = DnDx * ( neighUp[upperNeighbourIndex].fZ - z );
      
      maxUpperNeighbourIndex(neighUp[upperNeighbourIndex].fValid)++;
      ++upperNeighbourIndex;

      //assert( (upperNeighbourIndex < kMaxN) || ("" == " too small array ") );
      if ( ISUNLIKELY(upperNeighbourIndex >= kMaxN) ){
        // std::cout << "W AliHLTTPCCANeighboursFinder: Warning: Too many neighbours, some of them won't be considered \ Too small array. " << std::endl;
        break;
      }
    }

    int nNeighDn = 0;
    if ( upperNeighbourIndex <= 0 ) continue;// only if there are hits in the upper area
     
    short_v bestDn(-1);
    short_v bestUp(-1);
    sfloat_v bestD = chi2Cut; // d must be smaller than chi2Cut to be considered at all
    NeighbourData neighDn;

      // iterate over all hits in lower area
    ushort_m nextMask;
    HitArea areaDn( rowDn, fData, yDn, zDn, -DnDx*kAreaSizeY, -DnDx*kAreaSizeZ, validHitsMask );
    while ( !( nextMask = areaDn.GetNext( &neighDn ) ).isEmpty() ) {
        // for (int i0 = 0; i0 < downNeighbourIndex; i0++) {
        //   NeighbourData &neighDn = neighDown[i0];
        
      if ( ISUNLIKELY( ((Vc::Zero < maxUpperNeighbourIndex) && neighDn.fValid).isEmpty() ) ) continue; // no both neighbours
        
      assert( neighDn.fLinks < rowDn.NHits() || !neighDn.fValid );
      debugS() << neighDn;

      nNeighDn++;
#ifdef DRAW_NEIGHBOURSFINDING
      sfloat_v neighDnY = neighDn.fY;
      sfloat_v neighDnZ = neighDn.fZ;
#endif        
        // store distance from (y,z) times UpDx
      neighDn.fY = UpDx * ( neighDn.fY - y );
      neighDn.fZ = UpDx * ( neighDn.fZ - z );

      short_m dnMask( Vc::Zero ); // mask for appropriate neibours-pairs

        // iterate over the upper hits we found before and find the one with lowest curvature
        // (change of slope)

        // for ( int i = 0; !( (ushort_v(i) < maxUpperNeighbourIndex) && nextMask).isEmpty(); ++i ) { // slower
      for ( int i = 0; i < upperNeighbourIndex; ++i ) {
        sfloat_m masksf( neighDn.fValid & neighUp[i].fValid ); // only store links for actually useful data.
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
        masksf &= d < bestD;
        bestD( masksf ) = d;
        const short_m masks( masksf );
        dnMask |= masks;
        bestUp( masks ) = neighUp[i].fLinks;
        debugS() << "best up: " << masks << " " << bestUp << std::endl;
      }
      bestDn( dnMask ) = neighDn.fLinks;
      debugS() << "best down: " << dnMask << " " << bestDn << std::endl;
    }
      // sfloat_m mask_out = bestD < chi2Cut; sfloat_v bestD_out = -1; bestD_out(mask_out) = bestD; std::cout << "-" << bestD_out << chi2Cut<< std::endl; // dbg

      //debugS() << "Links for row " << rowIndex << ", hit " << hitIndex << ": " << bestUp << " " << bestDn << std::endl;

    assert( bestD < chi2Cut || ( bestUp == -1 && bestDn == -1 ) );

    debugS() << "Set Link Data: Up = " << bestUp << ", Down = " << bestDn << std::endl;

      // store the link indexes we found
    const short_m nonUsedMask = validHitsMask && ( short_v(fData.HitDataIsUsed( row ), hitIndexes) == short_v( Vc::Zero ) );
    assert( ((bestUp >= -1) && (bestUp < rowUp.NHits()) && nonUsedMask) == nonUsedMask );
    assert( ((bestDn >= -1) && (bestDn < rowDn.NHits()) && nonUsedMask) == nonUsedMask );
    fData.SetHitLinkUpData( row, hitIndexes, bestUp, nonUsedMask);
    fData.SetHitLinkDownData( row, hitIndexes, bestDn, nonUsedMask);

// #ifdef DRAW TODO uncomment and fix problem with DRAW_EVERY_LINK
//     if ( DRAW_EVERY_LINK ) {
//       foreach_bit ( int i, validHitsMask && bestUp != -1 && bestDn != -1 ) {
//         AliHLTTPCCADisplay::Instance().DrawSliceLink( rowIndex, hitIndex + i, -1, -1, 1 );
//       }
//       AliHLTTPCCADisplay::Instance().Update();
//     }
// #endif
  } // for hitIndex

}


#endif
