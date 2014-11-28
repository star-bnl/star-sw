/***************************************************************************
 *
 * $Id: StMcPxlLadderHitCollection.hh,v 2.1 2013/03/25 23:50:36 perev Exp $
 * $Log: StMcPxlLadderHitCollection.hh,v $
 * Revision 2.1  2013/03/25 23:50:36  perev
 * Mustafa.Pxl add
 *
 *
 **************************************************************************/
#ifndef StMcPxlLadderHitCollection_hh
#define StMcPxlLadderHitCollection_hh

#include "StObject.h"
#include "StMcPxlSensorHitCollection.hh"


class StMcPxlLadderHitCollection : public StObject
{
public:
    StMcPxlLadderHitCollection();
    virtual ~StMcPxlLadderHitCollection();
    
    unsigned int numberOfHits() const;
    unsigned int numberOfSensors() const;

    StMcPxlSensorHitCollection*     sensor(unsigned int);
    const StMcPxlSensorHitCollection* sensor(unsigned int) const;

protected:
    enum { kNumberOfSensors = 10 };
    StMcPxlSensorHitCollection mSensors[kNumberOfSensors];

    ClassDef(StMcPxlLadderHitCollection,1)
};

inline unsigned int StMcPxlLadderHitCollection::numberOfSensors() const {return kNumberOfSensors;}
#endif
