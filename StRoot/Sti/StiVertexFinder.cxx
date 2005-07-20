#include "Stiostream.h"
#include "StiVertexFinder.h"
#include "StiToolkit.h"

//______________________________________________________________________________
StiVertexFinder::StiVertexFinder(const string & name): Named(name)
{
  _hitFactory= StiToolkit::instance()->getHitFactory();
  cout <<"StiVertexFinder::StiVertexFinder() -I- Started :" << name<<endl;
}

//______________________________________________________________________________
StiVertexFinder::~StiVertexFinder()
{}
//______________________________________________________________________________
const std::vector<StiHit* >* StiVertexFinder::result()
{
	
  _result.clear();
  StiHit *hit=0;
  for (int i=0;(hit=getVertex(i));i++) {_result.push_back(hit);}	
  return &_result; 
}
