#include "Stiostream.h"
#include "StiVertexFinder.h"
#include "StiToolkit.h"

StiVertexFinder::StiVertexFinder(const string & name)
	: Named(name),
	  _hitFactory(StiToolkit::instance()->getHitFactory()),
_vertex(0)
{
	cout <<"StiVertexFinder::StiVertexFinder() -I- Started :" << name<<endl;
}

StiVertexFinder::~StiVertexFinder()
{}

