#ifndef StiStarVertexFinder_H
#define StiStarVertexFinder_H 1

#include "Sti/StiVertexFinder.h"
#include "Sti/StiHitLoader.h"

/*!
An abstract class defining  the interface to the vertex finder.
*/
class StiStarVertexFinder : public StiVertexFinder, public StiHitLoader<StEvent,StiDetectorBuilder>
{
public:
  StiStarVertexFinder();
  StiStarVertexFinder(StiHitContainer * hitContainer,
		      Factory<StiHit> * hitFactory,
		      StiDetectorBuilder * detector);
  ~StiStarVertexFinder();
  /// Find the main vertex associated with the given event
  virtual StiHit * findVertex(StEvent * event);	
  /// Load vertices from StEvent
  virtual void loadHits(StEvent* source);
};


#endif
