/*
 This file is property of and copyright by the ALICE HLT Project        *
 ALICE Experiment at CERN, All rights reserved.                         *
 See cxx source for full Copyright notice                               *
*/
#ifndef ALIHLTTPCCANEIGHBOURSFINDER_H
#define ALIHLTTPCCANEIGHBOURSFINDER_H

#include "AliHLTTPCCATracker.h"

#ifdef USE_TBB
namespace tbb
{
  template<typename T> class blocked_range;
} // namespace tbb
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


#endif
