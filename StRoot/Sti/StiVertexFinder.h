#ifndef StiVertexFinder_H
#define StiVertexFinder_H 1
#include "Sti/Base/Factory.h"
#include "Sti/Base/Named.h"
#include <vector>

class StEvent;
class StiHit;

/*!
An abstract class defining  the interface to the vertex finder.
*/
class StiVertexFinder : public Named
{
public:
  StiVertexFinder(const string & name);
  virtual ~StiVertexFinder();
  /// Find the vertex(es) associated with the given event
  virtual int fit(StEvent*)=0;                     // fit the vertex
  virtual StiHit * getVertex(int index) =0;	
  virtual int  size() const =0;	
  Factory<StiHit>* getHitFactory();
  virtual void clear()=0;

const std::vector<StiHit*> *result();

protected:
  StiVertexFinder();//not implemented
  Factory<StiHit>* _hitFactory;
std::vector<StiHit*> _result;
};

inline Factory<StiHit>* StiVertexFinder::getHitFactory()
{
  return _hitFactory;
}

#endif
