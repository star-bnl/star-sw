#include <iostream>
#include <stdexcept>
#include <cmath>
#include "Sti/Base/Factory.h"
#include "Sti/StiHit.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiDetector.h"
#include "Sti/StiDetectorBuilder.h"
#include "Sti/Star/StiStarVertexFinder.h"
#include "StEvent.h"
#include "StPrimaryVertex.h"
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"

StiStarVertexFinder::StiStarVertexFinder()
  : StiHitLoader<StEvent,StiDetectorBuilder>("StarVertexFinder",0,0,0)
{}
    
StiStarVertexFinder::StiStarVertexFinder(StiHitContainer* hitContainer,
				 Factory<StiHit>*hitFactory,
				 StiDetectorBuilder * detector)
  : StiHitLoader<StEvent,StiDetectorBuilder>("StarVertexFinder",hitContainer,hitFactory,detector)
{}

StiStarVertexFinder::~StiStarVertexFinder()
{}

/// Find the main vertex associated with the given event
/// This version returns the vertex already stored by StEvent
StiHit * StiStarVertexFinder::findVertex(StEvent * event)
{
  StiHit * vertex = 0;
  if (event->primaryVertex()) 
    {
      vertex = _hitFactory->getInstance();
      const StThreeVectorF& vp = event->primaryVertex()->position();
      const StThreeVectorF& ve = event->primaryVertex()->positionError();
      double angle=atan2(vp.y(),vp.x())-M_PI/4.;
      int sector = int(2.*angle/M_PI);
      cout << "angle:"<<angle<<" sector"<<endl;
      StiDetector * detector = _detector->getDetector(0,sector);
      vertex->setGlobal(detector,
			event->primaryVertex(),
			vp.x(),
			vp.y(),
			vp.z(),
			0.);
      ////error ve.x()*ve.x(),0.,0.,ve.y()*ve.y(),0.,ve.z()*ve.z());
    }
  return vertex;
}

void StiStarVertexFinder::loadHits(StEvent* source)
{
  /* nothing done here */
}
