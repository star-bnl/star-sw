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
    if ( gvm ){
      mGVF = gvm->GetGenericFinder();
      assert(mGVF);
    } else {
      LOG_WARN << "Could not find a GenericVertex instance" << endm;
    }
  }
  //AAR - modified
  // changed to fill primary vert only if fit returns true (okay)
  int nVtx=0;
  if ( mGVF ){
    clear(); // this calls mGVF->Clear() requiring knowing about GenericVertex 
    nVtx = mGVF->fit(event);
    if(nVtx){
      //vertex fit returns okay, so save
      mGVF->FillStEvent(event);
    }
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
  if (!mGVF) return 0;
  StPrimaryVertex *spv = mGVF->getVertex(idx);
  if (!spv) return  0;

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
	   cov[0][0],
	   cov[0][1],
	   cov[0][2],
	   cov[1][1],
	   cov[1][2],
	   cov[2][2]);
  return vertex;
}
