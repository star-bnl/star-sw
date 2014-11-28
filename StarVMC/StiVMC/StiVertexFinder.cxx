#include "StiVertexFinder.h"
#include "StiHit.h"
#include "StThreeVectorF.hh"
#include "StMatrixF.hh"
#include "StEvent.h"
#include "StPrimaryVertex.h"
#include "StiFactory.h"
#include "StGenericVertexMaker/StGenericVertexMaker.h"
#include "StGenericVertexMaker/StGenericVertexFinder.h"
#include "StMaker.h"
#include "StiToolkit.h"
ClassImp(StiVertexFinder);
//______________________________________________________________________________
StiVertexFinder::StiVertexFinder(const Char_t * name): TNamed(name,""), mGVF(0) {
  _hitFactory= StiToolkit::instance()->HitFactory();
  cout <<"StiVertexFinder::StiVertexFinder() -I- Started :" << name<<endl;
}
//______________________________________________________________________________
void StiVertexFinder::Clear(const Option_t *opt) {
  if (mGVF) mGVF->Clear();
}
//______________________________________________________________________________
Int_t StiVertexFinder::size() const{
  return (mGVF)? mGVF->size():0;
}
//______________________________________________________________________________
/// Return the main vertex held by the given StEvent
/// A null pointer is returned if StEvent holds no valid
/// vertex. 
//______________________________________________________________________________
Int_t StiVertexFinder::fit(StEvent * event){
  cout <<"StiVertexFinder::fit(StEvent * event) -I- Started"<<endl;

  
  // call the actual vertex finder method here...
  // assume the result is stored in StEvent...
  if (!mGVF) {  
    StGenericVertexMaker* gvm = (StGenericVertexMaker*)StMaker::GetChain()->GetMaker("GenericVertex");
    mGVF = gvm->GetGenericFinder();
    assert(mGVF);
  }
  Clear();
  //AAR - modified
  // changed to fill primary vert only if fit returns true (okay)
  Int_t nVtx = mGVF->fit(event);
  if(nVtx)  {
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
StiHit * StiVertexFinder::getVertex(Int_t idx) {
  StPrimaryVertex *spv = mGVF->getVertex(idx);
  if (!spv) return 0;
  // Get an instance of StHit from the factory
  StiHit *vertex = HitFactory()->getInstance();
  const StThreeVectorF& vp = spv->position();
  StMatrixF cov = spv->covariantMatrix();

  cout <<"StiVertexFinder::getVertex("<<idx<< ") -I- set hit parameters"<<endl;
  cout << "x:"<< vp.x() << "+-" << sqrt(cov[0][0])<<endl;
  cout << "y:"<< vp.y() << "+-" << sqrt(cov[1][1])<<endl;
  cout << "z:"<< vp.z() << "+-" << sqrt(cov[2][2])<<endl;
  vertex->set(0, spv, 
	   vp.x(),vp.y(),vp.z(),
	   cov[0][0],
	   cov[0][1],
	   cov[0][2],
	   cov[1][1],
	   cov[1][2],
	   cov[2][2]);
  return vertex;
}
//______________________________________________________________________________
const std::vector<StiHit* >* StiVertexFinder::result() {
  _result.clear();
  StiHit *hit=0;
  for (Int_t i=0;(hit=getVertex(i));i++) {_result.push_back(hit);}	
  return &_result; 
}
