#ifndef StiStarVertexFinder_H
#define StiStarVertexFinder_H 1
#include "Sti/StiVertexFinder.h"
#include "StMaker.h"
//#include "StGenericVertexFinderMaker

class StiStarVertexFinder : public StMaker, public StiVertexFinder
{
 public:
  StiStarVertexFinder(const string & name);
  virtual ~StiStarVertexFinder();
  StiHit * findVertex(StEvent * event);
};


#endif
