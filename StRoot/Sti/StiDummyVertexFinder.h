#ifndef StiDummyVertexFinder_H
#define StiDummyVertexFinder_H 1
#include "StiVertexFinder.h"

///A dummy vertex finder that uses the vertex stored in StEvent prior to calling the
///tracker.
class StiDummyVertexFinder : public StiVertexFinder
{
public:
  /// Find the vertex(es) associated with the given event
  StiHit * findVertex(StEvent * event);
};


#endif
