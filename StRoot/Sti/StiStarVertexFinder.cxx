#include <stdexcept>
#include "Sti/StiStarVertexFinder.h"
#include "Sti/StiHit.h"
#include "StThreeVectorF.hh"
#include "StEvent.h"
#include "StPrimaryVertex.h"
#include "Sti/Base/Factory.h"
#include "StGenericVertexMaker/StGenericVertexMaker.h"
#include "StGenericVertexMaker/StGenericVertexFinder.h"
#include "StMaker.h"

StiStarVertexFinder::StiStarVertexFinder(const string & name)
  : StiVertexFinder(name) //, StGenericVertexFinderMaker()
{}

StiStarVertexFinder::~StiStarVertexFinder()
{}

/// Return the main vertex held by the given StEvent
/// A null pointer is returned if StEvent holds no valid
/// vertex. 
StiHit * StiStarVertexFinder::findVertex(StEvent * event)
{
  cout <<"StiStarVertexFinder::findVertex(StEvent * event) -I- Started"<<endl;

  // This factory produces an instance of the StiHit class on demand.
  // i.e. sort of a call to new StiHit
  Factory<StiHit>*facto=getHitFactory();
  if (!facto)
    // out of luck- the factory does not exist - complain vehemently
    throw runtime_error("StiStarVertexFinder::findVertex() -F- Factory<StiHit>*==0");
  cout <<"StiStarVertexFinder::findVertex(StEvent * event) -I- facto is OK"<<endl;
  StiHit * vertex = 0;
  // Get an instance of StHit from the factory
  vertex = getHitFactory()->getInstance();
  if (!vertex) 
    // out of luck - got no instance from the factory - complain 
    throw runtime_error("StiStarVertexFinder::findVertex(StEvent * event) -I- primaryVertex exist");
  
  // call the actual vertex finder method here...
  // assume the result is stored in StEvent...
  
  StGenericVertexMaker* gvm = (StGenericVertexMaker*)GetMaker("GenericVertex");
  StGenericVertexFinder* gvf = gvm->GetGenericFinder();

  //AAR - modified
  // changed to fill primary vert only if fit returns true (okay)
  if( gvf->fit(event) )
    {
      //vertex fit returns okay, so save
      gvf->FillStEvent(event);
    }

  if(event->primaryVertex())
    {
     const StThreeVectorF& vp = event->primaryVertex()->position();
     const StThreeVectorF& ve = event->primaryVertex()->positionError();
     cout <<"StiStarVertexFinder::findVertex(StEvent * event) -I- set hit parameters"<<endl;
     cout << "x:"<< vp.x() << "+-" << ve.x()<<endl;
     cout << "y:"<< vp.y() << "+-" << ve.y()<<endl;
     cout << "z:"<< vp.z() << "+-" << ve.z()<<endl;
     vertex->set(0, event->primaryVertex(), 
	      0., 
	      vp.x(),vp.y(),vp.z(),
	      ve.x()*ve.x(),0.,0.,ve.y()*ve.y(),0.,ve.z()*ve.z());
     return vertex;
    }
  else return 0;
  

}
