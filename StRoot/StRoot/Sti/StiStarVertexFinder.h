#ifndef StiStarVertexFinder_H
#define StiStarVertexFinder_H 1
#include "Sti/StiVertexFinder.h"
class StGenericVertexFinder;
class StiStarVertexFinder : public StiVertexFinder
{
public:
  StiStarVertexFinder(const string & name);
  virtual ~StiStarVertexFinder();
  int fit(StEvent*);                     // fit the vertex
  StiHit * getVertex(int index);	
  int size() const;	
  void clear();
private:
StGenericVertexFinder* mGVF;
};


#endif
