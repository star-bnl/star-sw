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

#endif
