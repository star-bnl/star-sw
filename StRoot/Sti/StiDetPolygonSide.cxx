//StiDetPolygonSide.cxx
//M.L. Miller (Yale Software)
//06/01

//Sti
//#include "StiHitContainer.h"
#include "StiDetector.h"
#include "StiDetPolygonSide.h"

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
