//-*- Mode: C++ -*-
// @(#) $Id: AliHLTTPCCAHitArea.h,v 1.1.1.1 2010/07/26 20:55:38 ikulakov Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCAHITAREA_H
#define ALIHLTTPCCAHITAREA_H


#include "AliHLTTPCCADef.h"
#include "AliHLTArray.h"
#include "AliHLTTPCCAGrid.h"
#include "AliHLTTPCCARow.h"
#include "AliHLTTPCCASliceDataVector.h"

/**
 * @class ALIHLTTPCCAHitArea
 *
 * This class is used to _iterate_ over the hit data via GetNext
 */
class AliHLTTPCCAHitArea
{
  public:
    struct NeighbourData
    {
      uint_m fValid;
      uint_v fLinks;
      float_v fY, fZ;
    };

    AliHLTTPCCAHitArea( const AliHLTTPCCARow &row, const AliHLTTPCCASliceData &slice, const float_v &y, const float_v &z, float dy, float dz, int_m mask );

    /**
     * look up the next hit in the requested area.
     * Sets h to the coordinates and returns the index for the hit data
     */
    uint_m GetNext( NeighbourData *data = 0 );

    uint_v NHits(); // can be called only before GetNext
  
  protected:
    const AliHLTTPCCARow &fRow;
    const AliHLTTPCCASliceData &fSlice;

    uint_v fBZmax;   // maximal Z bin index
    uint_v fBDY;     // Y distance of bin indexes
    uint_v fIndYmin; // minimum index for
    uint_v fIz;      // current Z bin index (incremented while iterating)
    uint_v fHitYlst; //
    uint_v fIh;      // hit index iterating inside the bins
    int fNy;      // Number of bins in Y direction
};

typedef AliHLTTPCCAHitArea HitArea;

#include <Vc/IO>

static inline std::ostream &operator<<( std::ostream &out, const HitArea::NeighbourData &n )
{
  return out << n.fValid << " " << n.fLinks << " " << n.fY << " " << n.fZ << std::endl;
}

class AliHLTTPCCAHitAreaScalar
{
  public:
  struct NeighbourData
  {
    uint_m fValid;
    uint_v fLinks;
    float_v fY, fZ;
  };
//    AliHLTTPCCAHitAreaScalar( const AliHLTTPCCARow &row, const unsigned int iRow, const AliHLTTPCCASliceData &slice, const float &y, const float &z, float dy, float dz );
  AliHLTTPCCAHitAreaScalar( const AliHLTTPCCARow &row, const unsigned int iRow, const AliHLTTPCCASliceData &slice, float minY, float minZ, float maxY, float maxZ );

    ~AliHLTTPCCAHitAreaScalar() {};

    bool GetNext( int& i );

    bool GetNext( NeighbourData *data );

  protected:
    const AliHLTTPCCARow &fRow;
    const AliHLTTPCCASliceData &fSlice;

    unsigned int fBZmax;   // maximal Z bin index
    unsigned int fBDY;     // Y distance of bin indexes
    unsigned int fIndYmin; // minimum index for
    unsigned int fIz;      // current Z bin index (incremented while iterating)
    unsigned int fHitYlst; //
    unsigned int fIh;      // hit index iterating inside the bins
    int fNy;      // Number of bins in Y direction
};

typedef AliHLTTPCCAHitAreaScalar HitAreaScalar;

inline AliHLTTPCCAHitAreaScalar::AliHLTTPCCAHitAreaScalar( const AliHLTTPCCARow &row, const unsigned int iRow, const AliHLTTPCCASliceData &slice, float minY, float minZ, float maxY, float maxZ )
  : fRow( row ), fSlice( slice )
  , fHitYlst( 0 )
  , fIh( 0 )
  , fNy( fRow.Grid().Ny() )
{

  const AliHLTTPCCAGrid &grid = fRow.Grid();

  unsigned int bYmin, bZmin, bYmax; // boundary bin indexes
  grid.GetBinBounded( minY, minZ, &bYmin, &bZmin );
  grid.GetBinBounded( maxY, maxZ, &bYmax, &fBZmax );

  fBDY = ( bYmax - bYmin + 1 ); // bin index span in y direction

  fIndYmin = ( bZmin * fNy + bYmin ); // same as grid.GetBin(fMinY, fMinZ), i.e. the smallest bin index of interest

  fIz = bZmin;

  fIh = (int)fSlice.FirstUnusedHitInBin( fRow )[fIndYmin];
  fHitYlst = (int)fSlice.FirstUnusedHitInBin( fRow )[fIndYmin + fBDY];
}

inline bool AliHLTTPCCAHitAreaScalar::GetNext( int& i )
{
  bool yIndexOutOfRange = fIh >= fHitYlst;     // current y is not in the area
  bool nextZIndexOutOfRange = fIz >= fBZmax;   // there isn't any new z-line

  if ( yIndexOutOfRange && nextZIndexOutOfRange ) { // all iterators are over the end
    return false;
  }
  // at least one entry in the vector has (fIh >= fHitYlst && fIz < fBZmax)
  bool needNextZ = yIndexOutOfRange && !nextZIndexOutOfRange;

  // skip as long as fIh is outside of the interesting bin y-index
  while ( ISLIKELY( needNextZ ) ) {
    fIz++;   // get new z-line
    // get next hit
    fIndYmin += fNy;
    fIh = fSlice.FirstUnusedHitInBin( fRow )[fIndYmin]; // get first hit in cell, if z-line is new
    fHitYlst = fSlice.FirstUnusedHitInBin( fRow )[fIndYmin + fBDY];

    yIndexOutOfRange = fIh >= fHitYlst;
    nextZIndexOutOfRange = (fIz >= fBZmax);
    needNextZ = yIndexOutOfRange && !nextZIndexOutOfRange;
  }

  i = fIh; // return
  fIh++; // go to next
  return !yIndexOutOfRange;
}

inline bool AliHLTTPCCAHitAreaScalar::GetNext( NeighbourData *data )
{
  bool yIndexOutOfRange = fIh >= fHitYlst;     // current y is not in the area
  bool nextZIndexOutOfRange = fIz >= fBZmax;   // there isn't any new z-line

  if ( yIndexOutOfRange && nextZIndexOutOfRange ) { // all iterators are over the end
    if (data)
      data->fValid = uint_m(false);
    return false;
  }
  // at least one entry in the vector has (fIh >= fHitYlst && fIz < fBZmax)
  bool needNextZ = yIndexOutOfRange && !nextZIndexOutOfRange;

  // skip as long as fIh is outside of the interesting bin y-index
  while ( ISLIKELY( needNextZ ) ) {
    fIz++;   // get new z-line
    // get next hit
    fIndYmin += fNy;
    fIh = fSlice.FirstUnusedHitInBin( fRow )[fIndYmin]; // get first hit in cell, if z-line is new
    fHitYlst = fSlice.FirstUnusedHitInBin( fRow )[fIndYmin + fBDY];

    yIndexOutOfRange = fIh >= fHitYlst;
    nextZIndexOutOfRange = (fIz >= fBZmax);
    needNextZ = yIndexOutOfRange && !nextZIndexOutOfRange;
  }
  uint_v indexes(Vc::IndexesFromZero);
  indexes += fIh;
  if (data) {
    data->fValid = (uint_m)(indexes <= (uint_v)fHitYlst);
    const float_m valid( data->fValid );

    data->fY = fSlice.UnusedHitPDataY( fRow, indexes, valid );
    data->fZ = fSlice.UnusedHitPDataZ( fRow, indexes, valid );
    data->fLinks = indexes;
  }
  fIh++; // go to next
  return !yIndexOutOfRange;
}

#endif
