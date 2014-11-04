#include "Stiostream.h"
#include "StiVertexFinder.h"
#include "StiHit.h"
#include "StiToolkit.h"
static const double kBigErr2 = 100;
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
  for (int i=0;(hit=getVertex(i));i++) {
    if (hit->sxx() > kBigErr2) continue;
    if (hit->syy() > kBigErr2) continue;
    if (hit->szz() > kBigErr2) continue;
   _result.push_back(hit);
  }	
  return &_result; 
}
