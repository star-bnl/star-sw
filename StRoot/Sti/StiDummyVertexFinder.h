#ifndef StiDummyVertexFinder_H
#define StiDummyVertexFinder_H 1
#include "Sti/StiVertexFinder.h"

///A dummy vertex finder that uses the vertex stored in StEvent prior to calling the
///tracker.
class StiDummyVertexFinder : public StiVertexFinder
{
 public:
  StiDummyVertexFinder(const string & name);
  virtual ~StiDummyVertexFinder();
  virtual StiHit * findVertex(StEvent * event);
};


#endif
