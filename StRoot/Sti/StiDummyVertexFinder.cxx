#include <stdexcept>
#include "Sti/StiDummyVertexFinder.h"
#include "Sti/StiHit.h"
#include "StThreeVectorF.hh"
#include "StMatrixF.hh"
#include "StEvent.h"
#include "StPrimaryVertex.h"
#include "Sti/Base/Factory.h"


//______________________________________________________________________________
StiDummyVertexFinder::StiDummyVertexFinder(const string & name)
  : StiVertexFinder(name)
{
  mVertex=0;
}

//______________________________________________________________________________
StiDummyVertexFinder::~StiDummyVertexFinder()
{}
//______________________________________________________________________________
void StiDummyVertexFinder::clear()
{
  mVertex = 0;
}

//______________________________________________________________________________
int StiDummyVertexFinder::fit(StEvent * event)
{
  //cout <<"StiDummyVertexFinder::findVertex(StEvent * event) -I- Started"<<endl;
  clear();
  StPrimaryVertex *spv = event->primaryVertex(); 
  if (!spv) return 0;
  // Get an instance of StHit from the factory
  mVertex = getHitFactory()->getInstance();
  const StThreeVectorF& vp = spv->position();
  StMatrixF cov = spv->covariantMatrix();

  cout <<"StiDummyVertexFinder::getVertex(0) -I- set hit parameters"<<endl;
  cout << "x:"<< vp.x() << "+-" << sqrt(cov[0][0])<<endl;
  cout << "y:"<< vp.y() << "+-" << sqrt(cov[1][1])<<endl;
  cout << "z:"<< vp.z() << "+-" << sqrt(cov[2][2])<<endl;
  mVertex->set(0, spv, 
	   0., 
	   vp.x(),vp.y(),vp.z(),
	   cov[0][0],
	   cov[0][1],
	   cov[0][2],
	   cov[1][1],
	   cov[1][2],
	   cov[2][2]);
  return 1;
}


//______________________________________________________________________________
StiHit* StiDummyVertexFinder::getVertex(int index) 
{
  return (index)? 0:(StiHit*)mVertex;
}		
