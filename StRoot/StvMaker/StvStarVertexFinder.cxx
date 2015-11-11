#include "StvMaker/StvStarVertexFinder.h"
#include "Stv/StvHit.h"
#include "StThreeVectorF.hh"
#include "StMatrixF.hh"
#include "StEvent.h"
#include "StPrimaryVertex.h"
#include "StGenericVertexMaker/StGenericVertexMaker.h"
#include "StGenericVertexMaker/StGenericVertexFinder.h"
#include "StMaker.h"
#include "StvUtil/StvNodePars.h"


//______________________________________________________________________________
StvStarVertexFinder::StvStarVertexFinder(const char *name)
  : StvVertexFinder(name) 
{mGVF=0;}

//______________________________________________________________________________
StvStarVertexFinder::~StvStarVertexFinder()
{}
//______________________________________________________________________________
void StvStarVertexFinder::Clear(const char*)
{
  if (mGVF) mGVF->Clear();
  StvVertexFinder::Clear();
}

//______________________________________________________________________________
/// Return the main vertex held by the given StEvent
/// A null pointer is returned if StEvent holds no valid
/// vertex. 
//______________________________________________________________________________
int StvStarVertexFinder::Fit(StEvent * event)
{
  cout <<"StvStarVertexFinder::fit(StEvent * event) -I- Started"<<endl;

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
    Clear(); // this calls mGVF->Clear() requiring knowing about GenericVertex 
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
int StvStarVertexFinder::GetVertex(int idx,double x[3],double e[6]) 
{
  if (!mGVF) return 1;
  StPrimaryVertex *spv = mGVF->getVertex(idx);
  if (!spv) return  2;

  // Get an instance of StHit from the factory
  const StThreeVectorF& vp = spv->position();
  StMatrixF cov = spv->covariantMatrix();

  cout <<"StvStarVertexFinder::GetVertex("<<idx<< ") -I- set hit parameters"<<endl;
  cout << "x:"<< vp.x() << "+-" << sqrt(cov[0][0])<<endl;
  cout << "y:"<< vp.y() << "+-" << sqrt(cov[1][1])<<endl;
  cout << "z:"<< vp.z() << "+-" << sqrt(cov[2][2])<<endl;

  x[0]=vp.x();x[1]=vp.y();x[2]=vp.z();

  e[0]=cov[0][0];
  e[1]=cov[1][0];e[2]=cov[1][1];
  e[3]=cov[2][0];e[4]=cov[2][1];e[5]=cov[2][2];
  assert(mGVF->IsFixed() || StvFitErrs::EmxSign(3,e)>0);
  return 0;
}
