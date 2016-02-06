//-*- Mode: C++ -*-
// @(#) $Id: AliHLTTPCCAHitArea.h,v 1.1 2016/02/05 23:27:28 fisyak Exp $
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

struct AliHLTTPCCAGrid;
class AliHLTTPCCARow;
class AliHLTTPCCASliceData;

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
      short_m fValid;
      short_v fLinks;
      sfloat_v fY, fZ;
    };

    AliHLTTPCCAHitArea( const AliHLTTPCCARow &row, const AliHLTTPCCASliceData &slice, const sfloat_v &y, const sfloat_v &z, float dy, float dz, short_m mask );

    /**
     * look up the next hit in the requested area.
     * Sets h to the coordinates and returns the index for the hit data
     */
    ushort_m GetNext( NeighbourData *data );

  protected:
    const AliHLTTPCCARow &fRow;
    const AliHLTTPCCASliceData &fSlice;

    ushort_v fBZmax;   // maximal Z bin index
    ushort_v fBDY;     // Y distance of bin indexes
    ushort_v fIndYmin; // minimum index for
    ushort_v fIz;      // current Z bin index (incremented while iterating)
    ushort_v fHitYlst; //
    ushort_v fIh;      // hit index iterating inside the bins
    int fNy;      // Number of bins in Y direction
};

typedef AliHLTTPCCAHitArea HitArea;

#include <Vc/IO>

static inline std::ostream &operator<<( std::ostream &out, const HitArea::NeighbourData &n )
{
  return out << n.fValid << " " << n.fLinks << " " << n.fY << " " << n.fZ << std::endl;
}

#endif
