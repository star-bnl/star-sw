#include "Sti/StiDummyVertexFinder.h"
#include "Sti/StiHit.h"
#include "StThreeVectorF.hh"
#include "StEvent.h"
#include "StPrimaryVertex.h"
#include "Sti/StiToolkit.h"
#include "Sti/Base/Factory.h"

/// Return the main vertex held by the given StEvent
/// A null pointer is returned if StEvent holds no valid
/// vertex. 
StiHit * StiDummyVertexFinder::findVertex(StEvent * event)
{
  StiHit * vertex = 0;
  if (event->primaryVertex()) 
    {
      vertex = StiToolkit::instance()->getHitFactory()->getInstance();
      const StThreeVectorF& vp = event->primaryVertex()->position();
      const StThreeVectorF& ve = event->primaryVertex()->positionError();
      vertex->set(0, event->primaryVertex(), 0., vp.x(),vp.y(),vp.z(),ve.x(),0.,0.,ve.y(),0.,ve.z());
    }
  return vertex;
}
