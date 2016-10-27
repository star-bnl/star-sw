//-*- Mode: C++ -*-
// @(#) $Id: AliHLTTPCCATrack.h,v 1.1 2016/02/05 23:27:29 fisyak Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCATRACK_H
#define ALIHLTTPCCATRACK_H

#include "AliHLTTPCCADef.h"
#include "AliHLTTPCCATrackParam.h"
#include "AliHLTTPCCAParameters.h"
#include "AliHLTTPCCAHitId.h"
#include "AliHLTArray.h"

/**
 * @class ALIHLTTPCCAtrack
 *
 * The class describes the [partially] reconstructed TPC track [candidate].
 * The class is dedicated for internal use by the AliHLTTPCCATracker algorithm.
 */
class AliHLTTPCCATrack
{
  friend class AliHLTTPCCATrackletSelector;
  public:
    AliHLTTPCCATrack() : fNumberOfHits( 0 ) {}
    short NumberOfHits() const { return fNumberOfHits; }
    const AliHLTTPCCATrackParam &Param() const { return fParam; };
    const AliHLTTPCCAHitId &HitId( int i ) const { return fHitIdArray[i]; }

  private:
    AliHLTTPCCATrackParam fParam; // track parameters
    AliHLTFixedArray<AliHLTTPCCAHitId, AliHLTArraySize<AliHLTTPCCAParameters::MaxNumberOfRows8> > fHitIdArray;
    short fNumberOfHits;      // number of hits in the track

    // disallow copy
    AliHLTTPCCATrack( const AliHLTTPCCATrack & );
    AliHLTTPCCATrack &operator=( const AliHLTTPCCATrack & );
};

typedef AliHLTTPCCATrack Track;

#endif
