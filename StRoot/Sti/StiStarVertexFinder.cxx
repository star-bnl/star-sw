#include <stdexcept>
#include "Sti/StiStarVertexFinder.h"
#include "Sti/StiHit.h"
#include "StThreeVectorF.hh"
#include "StMatrixF.hh"
#include "StEvent.h"
#include "StPrimaryVertex.h"
#include "Sti/Base/Factory.h"
#include "StGenericVertexMaker/StGenericVertexMaker.h"
#include "StGenericVertexMaker/StGenericVertexFinder.h"
#include "StMaker.h"

//______________________________________________________________________________
StiStarVertexFinder::StiStarVertexFinder(const string & name)
  : StiVertexFinder(name) 
{mGVF=0;}

//______________________________________________________________________________
StiStarVertexFinder::~StiStarVertexFinder()
{}
//______________________________________________________________________________
void StiStarVertexFinder::clear()
{
  if (mGVF) mGVF->Clear();
}
//______________________________________________________________________________
int StiStarVertexFinder::size() const
{
  return (mGVF)? mGVF->size():0;
}

//______________________________________________________________________________
/// Return the main vertex held by the given StEvent
/// A null pointer is returned if StEvent holds no valid
/// vertex. 
//______________________________________________________________________________
int StiStarVertexFinder::fit(StEvent * event)
{
  cout <<"StiStarVertexFinder::fit(StEvent * event) -I- Started"<<endl;

  
  // call the actual vertex finder method here...
  // assume the result is stored in StEvent...
  if (!mGVF) {  
    StGenericVertexMaker* gvm = (StGenericVertexMaker*)StMaker::GetChain()->GetMaker("GenericVertex");
    mGVF = gvm->GetGenericFinder();
    assert(mGVF);
  }
  clear();
  //AAR - modified
  // changed to fill primary vert only if fit returns true (okay)
  int nVtx = mGVF->fit(event);
  if(nVtx)
  {
      //vertex fit returns okay, so save
      mGVF->FillStEvent(event);
  }
  return nVtx;

}

//______________________________________________________________________________
/// Return the main vertex held by the given StEvent
/// A null pointer is returned if StEvent holds no valid
/// vertex. 
//______________________________________________________________________________
StiHit * StiStarVertexFinder::getVertex(int idx) 
{
static const double fakeErr2=1e-4;
  StPrimaryVertex *spv = mGVF->getVertex(idx);
  if (!spv) return 0;
  // Get an instance of StHit from the factory
  StiHit *vertex = getHitFactory()->getInstance();
  const StThreeVectorF& vp = spv->position();
  StMatrixF cov = spv->covariantMatrix();

  cout <<"StiStarVertexFinder::getVertex("<<idx<< ") -I- set hit parameters"<<endl;
  cout << "x:"<< vp.x() << "+-" << sqrt(cov[0][0])<<endl;
  cout << "y:"<< vp.y() << "+-" << sqrt(cov[1][1])<<endl;
  cout << "z:"<< vp.z() << "+-" << sqrt(cov[2][2])<<endl;
  vertex->set(0, spv, 
	   0., 
	   vp.x(),vp.y(),vp.z(),
	   fakeErr2,
	   0,
	   0,
	   fakeErr2,
	   0,
	   fakeErr2); //Errors are zeros. Only track err accounted
  return vertex;
}
