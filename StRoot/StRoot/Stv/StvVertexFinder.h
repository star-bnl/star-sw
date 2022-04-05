#ifndef StvVertexFinder_H
#define StvVertexFinder_H 1
#include <TNamed.h>
#include <vector>
#include "StvStl.h"

class StEvent;
class StvHit;
class StvKonst_st;

/*!
An abstract class defining  the interface to the vertex finder.
*/
class StvVertexFinder : public TNamed
{
public:
  StvVertexFinder(const char* name);
  virtual ~StvVertexFinder();
  /// Find the vertex(es) associated with the given event
  virtual  int Fit(StEvent*)=0;                     // fit the vertex
  virtual  int GetVertex(int index,double xyz[3],double err[6]) =0;	
  virtual void Clear(const char * opt=0);
          void SetCons(const StvKonst_st *kons){mKons = kons;}

const StvHits &Result();

protected:
  StvVertexFinder();//not implemented
  const StvKonst_st *mKons;
  int mResulted;
  StvHits mResult;
};

#endif
