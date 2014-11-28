/***************************************************************************
 *
 * $Id: StMcPxlSensorHitCollection.cc,v 2.1 2013/03/25 23:50:36 perev Exp $
 * $Log: StMcPxlSensorHitCollection.cc,v $
 * Revision 2.1  2013/03/25 23:50:36  perev
 * Mustafa.Pxl add
 *
 *
 **************************************************************************/
#include "StMcPxlSensorHitCollection.hh"
#include "StMcPxlHit.hh"

ClassImp(StMcPxlSensorHitCollection)

StMcPxlSensorHitCollection::StMcPxlSensorHitCollection()
{
   /* noop */
}

StMcPxlSensorHitCollection::~StMcPxlSensorHitCollection()
{
   for (unsigned int i = 0; i < mHits.size(); i++)
   {
      delete mHits[i];
      mHits[i] = 0;
   }
}

const StSPtrVecMcPxlHit&
StMcPxlSensorHitCollection::hits() const
{
   return mHits;
}

StSPtrVecMcPxlHit&
StMcPxlSensorHitCollection::hits()
{
   return mHits;
}
