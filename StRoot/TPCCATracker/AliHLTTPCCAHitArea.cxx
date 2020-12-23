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

#include "AliHLTTPCCAHitArea.h"
#include "AliHLTTPCCASliceDataVector.h"
#include "AliHLTTPCCAGrid.h"
#include "AliHLTTPCCARow.h"
#include "debug.h"

AliHLTTPCCAHitArea::AliHLTTPCCAHitArea( const AliHLTTPCCARow &row, const AliHLTTPCCASliceData &slice,
    const float_v &y, const float_v &z, float dy, float dz, int_m mask )
  : fRow( row ), fSlice( slice ),
  fHitYlst( Vc::Zero ),
  fIh( Vc::Zero ),
  fNy( fRow.Grid().Ny() )
{
  const AliHLTTPCCAGrid &grid = fRow.Grid();

  const float_v minZ = z - dz;
  const float_v maxZ = z + dz;
  const float_v minY = y - dy;
  const float_v maxY = y + dy;

  uint_v bYmin, bZmin, bYmax; // boundary bin indexes
  grid.GetBinBounded( minY, minZ, &bYmin, &bZmin );
  grid.GetBinBounded( maxY, maxZ, &bYmax, &fBZmax );

  fBDY = ( bYmax - bYmin + 1 ); // bin index span in y direction

  fIndYmin = ( bZmin * fNy + bYmin ); // same as grid.GetBin(fMinY, fMinZ), i.e. the smallest bin index of interest
  // fIndYmin + fBDY then is the largest bin index of interest with the same Z

  fIz = bZmin;

  const int_m invalidMask = !mask;
  if ( !invalidMask.isEmpty() ) {
    debugS() << "not all parts of the HitArea are valid: " << mask << std::endl;

    fBZmax.setZero( invalidMask );
    fBDY.setZero( invalidMask );
    fIndYmin.setZero( invalidMask );
    fIz.setZero( invalidMask );

      // for given fIz (which is min atm.) get
#ifdef VC_GATHER_SCATTER
    fIh.gather( fSlice.FirstUnusedHitInBin( fRow ), fIndYmin, mask ); // first and
    fHitYlst.gather( fSlice.FirstUnusedHitInBin( fRow ), fIndYmin + fBDY, mask ); // last hit index in the bin
#else
    for( unsigned int i = 0; i < float_v::Size; i++ ) {
      if( !mask[i] ) continue;
      fIh[i] = (int)fSlice.FirstUnusedHitInBin( fRow )[(unsigned int)fIndYmin[i]];
      fHitYlst[i] = (int)fSlice.FirstUnusedHitInBin( fRow )[(unsigned int)(fIndYmin[i] + fBDY[i])];
    }
#endif
  } else {
    fIh = fSlice.FirstUnusedHitInBin( fRow, fIndYmin );
    fHitYlst = fSlice.FirstUnusedHitInBin( fRow, fIndYmin + fBDY );
//    for( unsigned int i = 0; i < float_v::Size; i++ ) {
//      fIh[i] = (int)fSlice.FirstUnusedHitInBin( fRow, fIndYmin )[i];
//      fHitYlst[i] = (int)fSlice.FirstUnusedHitInBin( fRow, fIndYmin + fBDY )[i];
//    }
  }

  debugS() << "HitArea created:\n"
    << "bYmin:    " << bYmin << "\n"
    << "bZmin:    " << bZmin << "\n"
    << "bYmax:    " << bYmax << "\n"
    << "fBZmax:   " << fBZmax << "\n"
    << "fBDY:     " << fBDY << "\n"
    << "fIndYmin: " << fIndYmin << "\n"
    << "fIz:      " << fIz << "\n"
    << "fHitYlst: " << fHitYlst << "\n"
    << "fIh:      " << fIh << "\n"
    << "fNy:      " << fNy << std::endl;
  
  ASSERT( (fHitYlst <= fRow.NUnusedHits() || invalidMask).isFull(), fHitYlst << " <= " << fRow.NUnusedHits() );
}

uint_m AliHLTTPCCAHitArea::GetNext( NeighbourData *data )
{
  // get next hit index
  uint_m yIndexOutOfRange = fIh >= fHitYlst;     // current y is not in the area
  uint_m nextZIndexOutOfRange = fIz >= fBZmax;   // there isn't any new z-line

  if ( yIndexOutOfRange.isFull() && nextZIndexOutOfRange.isFull() ) { // all iterators are over the end
    if (data)
      data->fValid = uint_m(false);
    return uint_m(false);
  }

    // at least one entry in the vector has (fIh >= fHitYlst && fIz < fBZmax)
  uint_m needNextZ = yIndexOutOfRange && !nextZIndexOutOfRange;

    // skip as long as fIh is outside of the interesting bin y-index
  while ( !needNextZ.isEmpty() ) {
    
    ++fIz( needNextZ );
    nextZIndexOutOfRange = fIz >= fBZmax;
    
      // get next hit
    fIndYmin( needNextZ ) += fNy;
#ifdef VC_GATHER_SCATTER
    fIh.gather( fSlice.FirstUnusedHitInBin( fRow ), fIndYmin, needNextZ ); // get first hit in cell, if z-line is new
    fHitYlst.gather( fSlice.FirstUnusedHitInBin( fRow ), fIndYmin + fBDY, needNextZ );
#else
    for( unsigned int i = 0; i < float_v::Size; i++ ) {
      if( !needNextZ[i] ) continue;
      fIh[i] = (int)fSlice.FirstUnusedHitInBin( fRow )[(unsigned int)fIndYmin[i]];
      fHitYlst[i] = (int)fSlice.FirstUnusedHitInBin( fRow )[(unsigned int)(fIndYmin[i] + fBDY[i])];
    }
#endif
    assert( (fHitYlst <= fRow.NUnusedHits() || !needNextZ).isFull() );
    
    yIndexOutOfRange = fIh >= fHitYlst;

    needNextZ = yIndexOutOfRange && !nextZIndexOutOfRange;
  }

  if (data) {
    data->fValid = !yIndexOutOfRange;
    const float_m valid( data->fValid );

    data->fY = fSlice.UnusedHitPDataY( fRow, fIh, valid );
    data->fZ = fSlice.UnusedHitPDataZ( fRow, fIh, valid );
    data->fLinks = fIh;
  }
  
  ++fIh;

  return !yIndexOutOfRange;
}

uint_v AliHLTTPCCAHitArea::NHits()
{ // suppose we didn't call GetNextHit yet!
  uint_v nHits(Vc::Zero);
  uint_v iz = fIz;
  uint_v indYmin = fIndYmin;
  uint_v ih = fIh;
  uint_v hitYlst = fHitYlst;
  uint_m needNextZ = iz < fBZmax;
  nHits += hitYlst - ih;
  while ( !needNextZ.isEmpty() ) {
    ++iz( needNextZ );   // get new z-line
    indYmin( needNextZ ) += fNy;
#ifdef VC_GATHER_SCATTER
    ih.gather( fSlice.FirstUnusedHitInBin( fRow ), indYmin, needNextZ ); // get first hit in cell, if z-line is new
    hitYlst.gather( fSlice.FirstUnusedHitInBin( fRow ), indYmin + fBDY, needNextZ );
#else
    for( unsigned int i = 0; i < float_v::Size; i++ ) {
      if( !needNextZ[i] ) continue;
      ih[i] = (int)fSlice.FirstUnusedHitInBin( fRow )[(unsigned int)indYmin[i]];
      hitYlst[i] = (int)fSlice.FirstUnusedHitInBin( fRow )[(unsigned int)(indYmin[i] + fBDY[i])];
    }
#endif
    nHits( needNextZ ) += hitYlst - ih;
    
    needNextZ = iz < fBZmax;
  }

  return nHits;
}
