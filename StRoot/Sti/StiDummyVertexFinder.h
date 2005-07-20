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
  int fit(StEvent * event);
  StiHit* getVertex(int index=0);
  void clear();
  int  size() const {return (mVertex!=0);}
private:  
StiHit* mVertex;	  
};


#endif
