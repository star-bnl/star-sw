/***************************************************************************
 *
 * $Id: StMcPxlLadderHitCollection.cc,v 2.1 2013/03/25 23:50:36 perev Exp $
 * $Log: StMcPxlLadderHitCollection.cc,v $
 * Revision 2.1  2013/03/25 23:50:36  perev
 * Mustafa.Pxl add
 *
 *
 **************************************************************************/
#include "StMcPxlLadderHitCollection.hh"
#include "StMcPxlHit.hh"
static const char rcsid[] = "$Id: StMcPxlLadderHitCollection.cc,v 2.1 2013/03/25 23:50:36 perev Exp $";

ClassImp(StMcPxlLadderHitCollection)

//_____________________________________________________________________________
StMcPxlLadderHitCollection::StMcPxlLadderHitCollection()
{
   /* noop */
}
//_____________________________________________________________________________
StMcPxlLadderHitCollection::~StMcPxlLadderHitCollection()
{
   /* noop */
}

StMcPxlSensorHitCollection*
StMcPxlLadderHitCollection::sensor(unsigned int i)
{
   return (i < kNumberOfSensors) ? &(mSensors[i]) : 0;
}

const StMcPxlSensorHitCollection*
StMcPxlLadderHitCollection::sensor(unsigned int i) const
{
   return (i < kNumberOfSensors) ? &(mSensors[i]) : 0;
}

unsigned int StMcPxlLadderHitCollection::numberOfHits() const
{
   unsigned int sum = 0;
   for (int iSen = 0; iSen < kNumberOfSensors; iSen++)
   {
      sum += mSensors[iSen].hits().size();
   }
   return sum;
}
