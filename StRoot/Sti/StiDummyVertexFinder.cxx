#include <stdexcept>
#include "Sti/StiDummyVertexFinder.h"
#include "Sti/StiHit.h"
#include "StThreeVectorF.hh"
#include "StEvent.h"
#include "StPrimaryVertex.h"
#include "Sti/Base/Factory.h"


StiDummyVertexFinder::StiDummyVertexFinder(const string & name)
  : StiVertexFinder(name)
{}

StiDummyVertexFinder::~StiDummyVertexFinder()
{}

/// Return the main vertex held by the given StEvent
/// A null pointer is returned if StEvent holds no valid
/// vertex. 
StiHit * StiDummyVertexFinder::findVertex(StEvent * event)
{
  //cout <<"StiDummyVertexFinder::findVertex(StEvent * event) -I- Started"<<endl;
  StiHit * vertex = 0;
  if (event->primaryVertex()) 
    {
      //cout <<"StiDummyVertexFinder::findVertex(StEvent * event) -I- primaryVertex exist"<<endl;
      Factory<StiHit>*facto=getHitFactory();
      if (!facto)
	throw runtime_error("StiDummyVertexFinder::findVertex() -F- Factory<StiHit>*==0");
      //cout <<"StiDummyVertexFinder::findVertex(StEvent * event) -I- facto is OK"<<endl;
      vertex = getHitFactory()->getInstance();
      if (!vertex) 
	throw runtime_error("StiDummyVertexFinder::findVertex(StEvent * event) -I- primaryVertex exist");
      const StThreeVectorF& vp = event->primaryVertex()->position();
      const StThreeVectorF& ve = event->primaryVertex()->positionError();
      //cout <<"StiDummyVertexFinder::findVertex(StEvent * event) -I- set hit parameters"<<endl;
      //cout << "x:"<< vp.x() << "+-" << ve.x()<<endl;
      //cout << "y:"<< vp.y() << "+-" << ve.y()<<endl;
      //cout << "z:"<< vp.z() << "+-" << ve.z()<<endl;
      vertex->set(0, event->primaryVertex(), 
		  0., 
		  vp.x(),vp.y(),vp.z(),
		  ve.x()*ve.x(),0.,0.,ve.y()*ve.y(),0.,ve.z()*ve.z());
    }
  setVertex(vertex);
  return vertex;
}
