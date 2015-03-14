/***************************************************************************
 *
 * $Id: StMcIstLadderHitCollection.hh,v 1.0 2015/02/12 13:00:00
 *
 * Author: Amilkar Quintero, Feb 2015
 ***************************************************************************
 *
 * Description: Monte Carlo Ist Ladder Hit Collection class
 *
 ***************************************************************************/
#ifndef StMcIstLadderHitCollection_hh
#define StMcIstLadderHitCollection_hh

#include "StMcContainers.hh"
#include "StObject.h"

#include "StMcIstSensorHitCollection.hh"

class StMcIstLadderHitCollection : public StObject {
public:
    StMcIstLadderHitCollection();
    virtual ~StMcIstLadderHitCollection();
    void Clear(const char* opt="");
  // bool IsFolder() const { return true;};
  unsigned long numberOfHits() const;
  unsigned int  numberOfSensors() const {return mMaxNumberOfSensors;}
  
  StMcIstSensorHitCollection*       sensor(unsigned int);
  const StMcIstSensorHitCollection* sensor(unsigned int) const;

protected:
  enum { mMaxNumberOfSensors = 6 };
  StMcIstSensorHitCollection  mSensors[mMaxNumberOfSensors];
  
  ClassDef(StMcIstLadderHitCollection,1)
};
#endif
