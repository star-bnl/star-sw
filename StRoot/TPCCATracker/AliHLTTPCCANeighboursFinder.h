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

#ifndef ALIHLTTPCCANEIGHBOURSFINDER_H
#define ALIHLTTPCCANEIGHBOURSFINDER_H

#include "AliHLTTPCCATracker.h"
#include "AliHLTTPCCAHitArea.h"
#include <iostream>
#include <queue>
using std::cout;
using std::endl;

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
#if 0
    void executeOnRowV1( int rowIndex ) const;
#endif

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

  
  const unsigned int numberOfHits = row.NUnusedHits();
  const unsigned int numberOfHitsUp = rowUp.NUnusedHits();
  const unsigned int numberOfHitsDown = rowDn.NUnusedHits();
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

  static const float kAreaSizeY = AliHLTTPCCAParameters::NeighbourAreaSizeTgY[fIter];
  static const float kAreaSizeZ = AliHLTTPCCAParameters::NeighbourAreaSizeTgZ[fIter];
  static const int kMaxN = 20; // TODO minimaze

  float koeff = 1.;
#ifdef ITPC_TCUT
  if( fTracker->Param().NRows() > 45 ) koeff = 3.5;
#endif
//#define USE_CURV_CUT // TODO don't work, problem with error estimation
#ifdef USE_CURV_CUT  
  float curv2Cut =  AliHLTTPCCAParameters::NeighbourCurvCut[fIter] * .5 * ( UpDx * DnDx * ( UpDx - DnDx ) ); // max curv^2*(dx1*dx2*(dx1+dx2))^2 = (dslopeY^2+dslopeZ^2)*(dx1*dx2)^2 = (dy1*dx2-dy2*dx1)^2+(dz1*dx2-dz2*dx1)^2
  curv2Cut *= curv2Cut;
  const float UpErr2 = (rowIndex - rowStep < AliHLTTPCCAParameters::NumberOfInnerRows) ? 0.06*0.06 : 0.12*0.12,
              DnErr2 = (rowIndex + rowStep < AliHLTTPCCAParameters::NumberOfInnerRows) ? 0.06*0.06 : 0.12*0.12;
#else // USE_CURV_CUT  
  const float chi2Cut = AliHLTTPCCAParameters::NeighbourChiCut[fIter]*AliHLTTPCCAParameters::NeighbourChiCut[fIter] * 4.f * ( UpDx * UpDx + DnDx * DnDx ) * koeff;
#endif // USE_CURV_CUT  
  // some step sizes on the current row. the uints in hits multiplied with the step size give
  // the offset on the grid

  typedef HitArea::NeighbourData NeighbourData;

    // #ifdef DRAW
    // #define DRAW_NEIGHBOURSFINDING
    // #endif
  
//  STATIC_ASSERT( int_v::Size % float_v::Size == 0, Short_Vector_Size_is_not_a_multiple_of_Float_Vector_Size );
  for ( unsigned int hitIndex = 0; hitIndex < numberOfHits; hitIndex += int_v::Size ) {

#ifdef DRAW_NEIGHBOURSFINDING
    float_v neighUpY[kMaxN];
    float_v neighUpZ[kMaxN];
#endif      

    uint_v hitIndexes( uint_v(Vc::IndexesFromZero) + hitIndex ); 
    const int_m &validHitsMask = hitIndexes < numberOfHits;
    
      // WARNING: there is very similar code above.
    // coordinates of the hit in the current row
    float_v y(Vc::Zero), z(Vc::Zero), yUp(Vc::Zero), zUp(Vc::Zero), yDn(Vc::Zero), zDn(Vc::Zero);
    
    y = fData.UnusedHitPDataY( row, hitIndexes, static_cast<float_m>(validHitsMask) );
    z = fData.UnusedHitPDataZ( row, hitIndexes, static_cast<float_m>(validHitsMask) );
    
#ifdef DRAW_NEIGHBOURSFINDING
    std::cout << " centr y = " << y << " centr z = " << z << std::endl;
#endif
    yUp = y * UpTx; // suppose vertex at (0,0,0)
    yDn = y * DnTx; // TODO change name
    zUp = z * UpTx;
    zDn = z * DnTx;

#ifdef USE_CURV_CUT
    const float_v UpDy = yUp - y,
       DnDy = yDn - y;
    const float_v UpR2 = UpDy*UpDy + UpDx*UpDx,
      DnR2 = DnDy*DnDy + DnDx*DnDx;
    float_v chi2Cut = curv2Cut + (UpErr2/UpR2 + DnErr2/DnR2)*(UpDx*DnDx)*(UpDx*DnDx); // additional error in dSlopes TODO Take into account z. TODO Use more precise error estimation // TODO change name // TODO optimize
#endif //  USE_CURV_CUT

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
        // std::cout << "W AliHLTTPCCANeighboursFinder: Warning: Too many neighbours, some of them won't be considered \ Too small array. " << std::endl;
        break;
      }
    }

    int nNeighDn = 0;
    if ( upperNeighbourIndex <= 0 ) continue;// only if there are hits in the upper area
     
    int_v bestDn(-1);
    int_v bestUp(-1);
    float_v bestD = chi2Cut; // d must be smaller than chi2Cut to be considered at all
    NeighbourData neighDn;
    
      // iterate over all hits in lower area
    uint_m nextMask;
    HitArea areaDn( rowDn, fData, yDn, zDn, -DnDx*kAreaSizeY, -DnDx*kAreaSizeZ, validHitsMask );
    while ( !( nextMask = areaDn.GetNext( &neighDn ) ).isEmpty() ) {
      if ( ISUNLIKELY( ((uint_v(Vc::Zero) < maxUpperNeighbourIndex) && neighDn.fValid).isEmpty() ) ) continue; // no both neighbours
      
      assert( (neighDn.fLinks < rowDn.NUnusedHits() || !neighDn.fValid).isFull() );

      nNeighDn++;
#ifdef DRAW_NEIGHBOURSFINDING
      float_v neighDnY = neighDn.fY;
      float_v neighDnZ = neighDn.fZ;
#endif        
        // store distance from (y,z) times UpDx
      neighDn.fY = UpDx * ( neighDn.fY - y );
      neighDn.fZ = UpDx * ( neighDn.fZ - z );

      int_m dnMask( Vc::Zero ); // mask for appropriate neibours-pairs

        // iterate over the upper hits we found before and find the one with lowest curvature
        // (change of slope)

      uint_v curMaxUpperNeighbourIndex = maxUpperNeighbourIndex;
      curMaxUpperNeighbourIndex(!neighDn.fValid) = 0;
      const int maxMaxUpperNeighbourIndex = curMaxUpperNeighbourIndex.max();
        // for ( int i = 0; !( (uint_v(i) < maxUpperNeighbourIndex) && nextMask).isEmpty(); ++i ) { // slower
      for ( int i = 0; i < maxMaxUpperNeighbourIndex; ++i ) {
        float_m masksf( neighDn.fValid & neighUp[i].fValid ); // only store links for actually useful data.
        
        const float_v dy = neighDn.fY - neighUp[i].fY;
        const float_v dz = neighDn.fZ - neighUp[i].fZ;
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
        const float_v d = dy * dy + dz * dz;
// WRONG NEIGHBOURS DOWU EXCLUDING FIRST - FIXED
        masksf &= d < bestD;
        bestD( masksf ) = d;
        const int_m masks( masksf );
        dnMask |= masks;
//        bestUp( masks ) = static_cast<int_v>(neighUp[i].fLinks);
        bestUp( masks ) = neighUp[i].fLinks;
        debugS() << "best up: " << masks << " " << bestUp << std::endl;
      }
//      bestDn( dnMask ) = static_cast<int_v>(neighDn.fLinks);
      bestDn( dnMask ) = neighDn.fLinks;
      debugS() << "best down: " << dnMask << " " << bestDn << std::endl;
    }

    assert( (bestD < chi2Cut || float_m( bestUp == -1 && bestDn == -1 )).isFull() );

    debugS() << "Set Link Data: Up = " << bestUp << ", Down = " << bestDn << std::endl;

      // store the link indexes we found
    assert( ((bestUp >= -1) && (bestUp < rowUp.NUnusedHits()) && validHitsMask) == validHitsMask );
    assert( ((bestDn >= -1) && (bestDn < rowDn.NUnusedHits()) && validHitsMask) == validHitsMask );
    fData.SetUnusedHitLinkUpData( row, rowUp, hitIndexes, bestUp, validHitsMask);
    fData.SetUnusedHitLinkDownData( row, rowDn, hitIndexes, bestDn, validHitsMask);
  } // for hitIndex

}

#if 0
inline void AliHLTTPCCATracker::NeighboursFinder::executeOnRowV1( int rowIndex ) const
{
  // references to the rows above and below
  const int rowStep = AliHLTTPCCAParameters::RowStep;

  if( rowIndex < rowStep ) {
    std::cout<<" - ERROR! Wrong rowIndex: "<<rowIndex<<"; rowStep: "<<rowStep<<"\n";
    return;
  }

  const AliHLTTPCCARow &row = fData.Row( rowIndex );

  const AliHLTTPCCARow &rowUp = fData.Row( rowIndex + rowStep );
  const AliHLTTPCCARow &rowDn = fData.Row( rowIndex - rowStep );


  const int numberOfHits = row.NUnusedHits();
  const int numberOfHitsUp = rowUp.NUnusedHits();
  const int numberOfHitsDown = rowDn.NUnusedHits();
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

  static const float kAreaSizeY = AliHLTTPCCAParameters::NeighbourAreaSizeTgY[fIter];
  static const float kAreaSizeZ = AliHLTTPCCAParameters::NeighbourAreaSizeTgZ[fIter];
  static const int kMaxN = 20; // TODO minimaze

  const float chi2Cut = AliHLTTPCCAParameters::NeighbourChiCut[fIter]*AliHLTTPCCAParameters::NeighbourChiCut[fIter] * 4.f * ( UpDx * UpDx + DnDx * DnDx );

  std::vector<unsigned int> hits0, hits1, hits2, hits2temp;

  unsigned int iH1 = 0;
  unsigned int cH = 0;
  unsigned int bestH[3];
  bool emptyN(true);
  float dMin = std::numeric_limits<float>::max();
    if( hits2.size() == 0 && iH1 >= numberOfHits ) return;	//TODO check this
    do {
//      if( iH1 == numberOfHits ) break;	//TODO check do we need this
      //TODO put used condition here for collecting neighboring triplets if possible

      float y, z, yUp, zUp, yDn, zDn;

      y = fData.UnusedHitPDataY( row, iH1 );
      z = fData.UnusedHitPDataZ( row, iH1 );
      yDn = y * DnTx; // TODO change name
      zDn = z * DnTx;
      yUp = y * UpTx; // suppose vertex at (0,0,0)
      zUp = z * UpTx;
      //
      const float minYDn = yDn + DnDx*kAreaSizeY;
      const float maxYDn = yDn - DnDx*kAreaSizeY;
      float minZDn(zDn), maxZDn(zDn);
      if( minZDn > 0 ) minZDn += 0.1*DnDx*kAreaSizeZ;
      else minZDn += DnDx*kAreaSizeZ;
      if( maxZDn < 0 ) maxZDn -= 0.1*DnDx*kAreaSizeZ;
      else maxZDn -= DnDx*kAreaSizeZ;
      //
//      HitAreaScalar areaDn( rowDn, rowIndex - rowStep, fData, yDn, zDn, -DnDx*kAreaSizeY, -DnDx*kAreaSizeZ );
      HitAreaScalar areaDn( rowDn, rowIndex - rowStep, fData, minYDn, minZDn, maxYDn, maxZDn );
//      std::cout<<"AreaDn: Row: "<<rowIndex - rowStep<<";   Y: "<<yDn<<";   Z: "<<zDn<<";   dy: "<<-DnDx*kAreaSizeY<<";   dz: "<<-DnDx*kAreaSizeZ<<"\n";
      int imh0 = 0;
      int imh2 = 0;
      int dnN = 0;
      bool firstIteration(true);
      while( areaDn.GetNext(imh0) ) {
	dnN++;
	if( dnN > 20 ) break;
//	if( fabs(fData.UnusedHitPDataZ( rowDn, imh0 ) - z) > fabs(DnDx*kAreaSizeZ) ) continue;	// -0.5% efficiency; nice speedup
	if( fabs(fData.UnusedHitPDataZ( rowDn, imh0 ) - z) > fabs(DnDx*kAreaSizeZ) && fabs(fData.UnusedHitPDataZ( rowDn, imh0 )) > fabs(z) ) continue;	// does not decrease efficieny; low speedup
//	if( fabs(fData.UnusedHitPDataY( rowDn, imh0 ) - y) > fabs(DnDx*kAreaSizeY) ) continue;
//	float tY = y + y - fData.UnusedHitPDataY( rowDn, imh0 );
//	float tZ = z + z - fData.UnusedHitPDataZ( rowDn, imh0 );
	if(firstIteration) {
	  float tY = y + y - fData.UnusedHitPDataY( rowDn, imh0 );
	  float tZ = z + z - fData.UnusedHitPDataZ( rowDn, imh0 );
	  const float minYUp = tY - UpDx*kAreaSizeY;
	  const float maxYUp = tY + UpDx*kAreaSizeY;
	  float minZUp(tZ), maxZUp(tZ);
	  if( minZUp > 0 ) minZUp -= 0.1*UpDx*kAreaSizeZ*0.7;
	  else minZUp -= UpDx*kAreaSizeZ*0.7;
	  if( maxZUp < 0 ) maxZUp += 0.1*UpDx*kAreaSizeZ*0.7;
	  else maxZUp += UpDx*kAreaSizeZ*0.7;
	  //
//	  HitAreaScalar areaUp( rowUp, rowIndex + rowStep, fData, yUp, zUp, UpDx*kAreaSizeY, UpDx*kAreaSizeZ );
//	  HitAreaScalar areaUp( rowUp, rowIndex + rowStep, fData, tY, tZ, UpDx*kAreaSizeY, UpDx*kAreaSizeZ*0.7 );
	  HitAreaScalar areaUp( rowUp, rowIndex + rowStep, fData, minYUp, minZUp, maxYUp, maxZUp );
	  int upN = 0;
	  while( areaUp.GetNext(imh2) ) {
	    hits2.push_back(imh2);
	    hits1.push_back(iH1);
	    hits0.push_back(imh0);
	    hits2temp.push_back(imh2);
	    upN++;
	    if( hits1.size() == float_v::Size ) {
	      float_v Y0, Y1, Y2, Z0, Z1, Z2;
	      fData.GetHitCoordinateVectors( rowDn, hits0, &Y0, &Z0 );
	      fData.GetHitCoordinateVectors( row, hits1, &Y1, &Z1 );
	      fData.GetHitCoordinateVectors( rowUp, hits2, &Y2, &Z2 );
	      Y2 = DnDx * ( Y2 - Y1 );
	      Z2 = DnDx * ( Z2 - Z1 );
	      Y0 = UpDx * ( Y0 - Y1 );
	      Z0 = UpDx * ( Z0 - Z1 );
	      Y2 = Y0 - Y2;
	      Z2 = Z0 - Z2;
	      const float_v d = Y2 * Y2 + Z2 * Z2;
	      for( int ii = 0; ii < float_v::Size; ii++ ) {
		if( hits1[ii] == cH ) {
		  if( dMin > d[ii] ){
		    dMin = d[ii];
		    bestH[0] = hits0[ii];
		    bestH[1] = hits1[ii];
		    bestH[2] = hits2[ii];
		    emptyN = false;
		  }
		}
		else {
		  if( !emptyN ) {
		      if( dMin < chi2Cut ) fData.SetUnusedHitLinkDataScalar( row, rowUp, rowDn, bestH );
		      dMin = std::numeric_limits<float>::max();
		  }
		  dMin = d[ii];
		  cH = hits1[ii];
		  bestH[0] = hits0[ii];
		  bestH[1] = hits1[ii];
		  bestH[2] = hits2[ii];
		  emptyN = false;
		}
	      }
	      hits0.clear();
	      hits1.clear();
	      hits2.clear();
	    }
	    if( upN > 20 ) break;
	  }
	}
	else {
	  for( unsigned int iStep = 0; iStep < hits2temp.size(); iStep++ ) {
	    hits0.push_back(imh0);
	    hits1.push_back(iH1);
	    hits2.push_back(hits2temp[iStep]);
	    if( hits1.size() == float_v::Size ) {
              float_v Y0, Y1, Y2, Z0, Z1, Z2;
              fData.GetHitCoordinateVectors( rowDn, hits0, &Y0, &Z0 );
              fData.GetHitCoordinateVectors( row, hits1, &Y1, &Z1 );
              fData.GetHitCoordinateVectors( rowUp, hits2, &Y2, &Z2 );
              Y2 = DnDx * ( Y2 - Y1 );
              Z2 = DnDx * ( Z2 - Z1 );
              Y0 = UpDx * ( Y0 - Y1 );
              Z0 = UpDx * ( Z0 - Z1 );
              Y2 = Y0 - Y2;
              Z2 = Z0 - Z2;
              const float_v d = Y2 * Y2 + Z2 * Z2;
              for( int ii = 0; ii < float_v::Size; ii++ ) {
  		if( hits1[ii] == cH ) {
  		  if( dMin > d[ii] ){
  		    dMin = d[ii];
  		    bestH[0] = hits0[ii];
  		    bestH[1] = hits1[ii];
  		    bestH[2] = hits2[ii];
  		  }
  		}
  		else {
  		  if( !emptyN ) {
  		    if( dMin < chi2Cut ) fData.SetUnusedHitLinkDataScalar( row, rowUp, rowDn, bestH );
  		    dMin = std::numeric_limits<float>::max();
  		  }
  		  dMin = d[ii];
  		  cH = hits1[ii];
  		  bestH[0] = hits0[ii];
  		  bestH[1] = hits1[ii];
  		  bestH[2] = hits2[ii];
  		}
  	      }
              hits0.clear();
              hits1.clear();
              hits2.clear();
	    }
	  }
	}
	firstIteration = false;
      }
      hits2temp.clear();
      iH1++;
      firstIteration = true;
    } while( /*hits0.size() < float_v::Size && hits1.size() < float_v::Size && hits2.size() < float_v::Size &&*/ iH1 < numberOfHits );
    float_v Y0, Y1, Y2, Z0, Z1, Z2;
    unsigned int hs1 = hits1.size();
    fData.GetHitCoordinateVectors( rowDn, hits0, &Y0, &Z0 );
    fData.GetHitCoordinateVectors( row, hits1, &Y1, &Z1 );
    fData.GetHitCoordinateVectors( rowUp, hits2, &Y2, &Z2 );
    Y2 = DnDx * ( Y2 - Y1 );
    Z2 = DnDx * ( Z2 - Z1 );
    Y0 = UpDx * ( Y0 - Y1 );
    Z0 = UpDx * ( Z0 - Z1 );
    Y2 = Y0 - Y2;
    Z2 = Z0 - Z2;
    const float_v d = Y2 * Y2 + Z2 * Z2;
    for( int ii = 0; ii < hs1; ii++ ) {
	if( hits1[ii] == cH ) {
	  if( dMin > d[ii] ){
	    dMin = d[ii];
	    bestH[0] = hits0[ii];
	    bestH[1] = hits1[ii];
	    bestH[2] = hits2[ii];
	  }
	}
	else {
	    if( dMin < chi2Cut ) fData.SetUnusedHitLinkDataScalar( row, rowUp, rowDn, bestH );
	  dMin = d[ii];
	  cH = hits1[ii];
	  bestH[0] = hits0[ii];
	  bestH[1] = hits1[ii];
	  bestH[2] = hits2[ii];
	}
      }
    if( dMin < chi2Cut ) fData.SetUnusedHitLinkDataScalar( row, rowUp, rowDn, bestH );
    hits0.clear();
    hits1.clear();
    hits2.clear();
}
#endif


#endif
