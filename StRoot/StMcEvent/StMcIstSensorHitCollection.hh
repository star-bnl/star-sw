/***************************************************************************
 *
 *
 * Author: Amilkar Quintero, Feb 2015
 ***************************************************************************
 *
 * Description: Monte Carlo Ist Sensor Hit Collection class
 *
 ***************************************************************************/
#ifndef StMcIstSensorHitCollection_hh
#define StMcIstSensorHitCollection_hh
#include "StMcContainers.hh"
#include "StObject.h"

class StMcIstHit;

class StMcIstSensorHitCollection : public StObject
{
public:
    StMcIstSensorHitCollection();
    // StMcIstSensorHitCollection(const StMcIstSensorHitCollection&); use default
    // const StMcIstSensorHitCollection& operator=(const StMcIstSensorHitCollection&); use default
    virtual ~StMcIstSensorHitCollection();
    void Clear(const char* opt="");
    bool IsFolder() const { return true;};
  //virtual void Browse(TBrowser *b); 
    
    StSPtrVecMcIstHit&       hits();
    const StSPtrVecMcIstHit& hits() const;

protected:
    StSPtrVecMcIstHit mHits;
    ClassDef(StMcIstSensorHitCollection,1)
};
#endif
