#ifndef StvStarVertexFinder_H
#define StvStarVertexFinder_H 1
#include "Stv/StvVertexFinder.h"
class StGenericVertexFinder;
class StvStarVertexFinder : public StvVertexFinder
{
public:
  StvStarVertexFinder(const char* name);
  virtual ~StvStarVertexFinder();
  int Fit(StEvent*);                     // fit the vertex
  void Clear(const char *opt=0);
private:
  int GetVertex(int index,double xyz[3],double err[6]);	
private:
StGenericVertexFinder* mGVF;
};


#endif
