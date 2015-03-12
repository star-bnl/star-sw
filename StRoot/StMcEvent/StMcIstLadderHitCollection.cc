/***************************************************************************
 *
 *
 * Author: Amilkar Quintero, Feb 2015
 ***************************************************************************
 *
 * Description: Monte Carlo Ist Ladder Hit Collection class
 *
 ***************************************************************************
 *
 *
 **************************************************************************/
#include "TBrowser.h"
#include "StMcIstLadderHitCollection.hh"
#include "StMcIstHit.hh"
static const char rcsid[] = "$Id: StMcIstLadderHitCollection.cc,v 2.1 2015/03/12 23:23:43 perev Exp $";

ClassImp(StMcIstLadderHitCollection)

//_____________________________________________________________________________
StMcIstLadderHitCollection::StMcIstLadderHitCollection() { /* noop */ }
//_____________________________________________________________________________
StMcIstLadderHitCollection::~StMcIstLadderHitCollection(){ Clear();   }
//_____________________________________________________________________________
void StMcIstLadderHitCollection::Clear(const char*)
{
  /*for (int i=0; i<(int)mSensors.size(); i++) 
  {
    delete mSensors[i]; mSensors[i] = 0;
    }*/
  //mSensors.clear();
}

//_____________________________________________________________________________
StMcIstSensorHitCollection* StMcIstLadderHitCollection::sensor(unsigned int i)
{
    if (i < numberOfSensors())
        return &(mSensors[i]);
    else
        return 0;
}
//_____________________________________________________________________________
const StMcIstSensorHitCollection* StMcIstLadderHitCollection::sensor(unsigned int i) const {
    if (i < numberOfSensors())
        return &(mSensors[i]);
    else
        return 0;
}

//_____________________________________________________________________________
unsigned long StMcIstLadderHitCollection::numberOfHits() const
{
  unsigned long sum = 0;
  for (unsigned int j=0; j<numberOfSensors(); j++) {
    sum += mSensors[j].hits().size();
  }
  return sum;
}
