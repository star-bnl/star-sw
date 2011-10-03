//-*- Mode: C++ -*-

//* This file is property of and copyright by the ALICE HLT Project        *
//* ALICE Experiment at CERN, All rights reserved.                         *
//* See cxx source for full Copyright notice                               *

#ifndef ALIHLTTPCCANEIGHBOURSFINDER_H
#define ALIHLTTPCCANEIGHBOURSFINDER_H

#include "AliHLTTPCCATracker.h"

namespace tbb
{
  template<typename T> class blocked_range;
} // namespace tbb

/**
 * @class AliHLTTPCCANeighboursFinder
 */
class AliHLTTPCCATracker::NeighboursFinder
{
  public:
    class ExecuteOnRow;
    NeighboursFinder( AliHLTTPCCATracker *tracker, SliceData &sliceData ) : fTracker( tracker ), fData( sliceData ) {}
    void execute();

    void operator()( const tbb::blocked_range<int> &r ) const;

  private:
    void executeOnRow( int rowIndex ) const;

    AliHLTTPCCATracker *fTracker;
    SliceData &fData;
};

#endif
