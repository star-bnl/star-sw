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
 protected:
	StiVertexFinder();//not implemented
	Factory<StiHit>* _hitFactory;
};

inline Factory<StiHit>* StiVertexFinder::getHitFactory()
{
	return _hitFactory;
}

#endif
