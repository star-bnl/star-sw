//StiDetPolygonSide.cxx
//M.L. Miller (Yale Software)
//06/01

//Sti
//#include "StiHitContainer.h"
#include <iostream.h>
#include "StiDetector.h"
#include "StiDetPolygonSide.h"

ostream& operator<<(ostream& os, const StiDetector& d);

StiDetPolygonSide::StiDetPolygonSide() : mdetector(0) //, mhitvector(0)
{
}

StiDetPolygonSide::~StiDetPolygonSide()
{
}

StiDetector* StiDetPolygonSide::detector() const
{
    return mdetector;
}

void StiDetPolygonSide::setDetector(StiDetector* val)
{
    mdetector = val;
}

//unsigned int StiDetPolygonSide::numberOfHits() const
//{
//  return mhitvector->size();
//}

//Non-memebers
ostream& operator<<(ostream& os, const StiDetPolygonSide& side)
{
    return os <<(*(side.detector()));
}

