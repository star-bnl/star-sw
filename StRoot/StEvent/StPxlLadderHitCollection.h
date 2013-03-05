/*!
 * \class StPxlLadderHitCollection 
 * \author X. Dong, Jan 2013
 */
/***************************************************************************
 *
 * $Id: StPxlLadderHitCollection.h,v 2.1 2013/03/05 14:40:41 ullrich Exp $
 *
 * Author: X. Dong, Jan 2013
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPxlLadderHitCollection.h,v $
 * Revision 2.1  2013/03/05 14:40:41  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StPxlLadderHitCollection_hh
#define StPxlLadderHitCollection_hh

#include "StObject.h"
#include "StPxlSensorHitCollection.h"

class StPxlLadderHitCollection : public StObject {
public:
    StPxlLadderHitCollection();
    ~StPxlLadderHitCollection();

    unsigned int numberOfHits() const;
    unsigned int numberOfSensors() const;
        
    StPxlSensorHitCollection*       sensor(unsigned int);
    const StPxlSensorHitCollection* sensor(unsigned int) const;
    
private:
    enum { mNumberOfSensors = 10 };
    StPxlSensorHitCollection mSensors[mNumberOfSensors];
    
    ClassDef(StPxlLadderHitCollection,1)
};

inline unsigned int StPxlLadderHitCollection::numberOfSensors() const { return mNumberOfSensors; }

#endif
