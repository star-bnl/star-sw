#ifndef StiVertexFinder_H
#define StiVertexFinder_H 1
#include "Sti/Base/Factory.h"
#include "Sti/Base/Named.h"

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
  virtual StiHit * findVertex(StEvent * event)=0;	
  Factory<StiHit>* getHitFactory();
  virtual void setVertex(StiHit * vertex);
  virtual StiHit * getVertex() const;
 protected:
  StiVertexFinder();//not implemented
  Factory<StiHit>* _hitFactory;
  StiHit * _vertex;
};

inline Factory<StiHit>* StiVertexFinder::getHitFactory()
{
  return _hitFactory;
}

inline StiHit * StiVertexFinder::getVertex() const
{
  return _vertex;
}

inline void StiVertexFinder::setVertex(StiHit * vertex)
{
  _vertex = vertex;
}

#endif
