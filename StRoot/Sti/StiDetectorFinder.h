/*
 * Allows Storage and retrieval of StiDetectors by name.  Doesn't own
 * the detectors.
 * 
 * Ben Norman, Kent State  (23 Aug 01)
 */

#ifndef STI_DETECTOR_FINDER_H
#define STI_DETECTOR_FINDER_H

#include <map>
using std::map;
#include "StiMapUtilities.h"

class StiDetector;

typedef map<NameMapKey, StiDetector*> detectorMap;
typedef detectorMap::const_iterator detectorIterator;
typedef detectorMap::value_type detectorMapValType;

class StiDetectorFinder{

public:
    friend class nobody; //Avoid compiler warnings for singleton design

    void addDetector(StiDetector *pDetector);
    StiDetector* findDetector(const char *szName);

    static StiDetectorFinder *instance();
    static void kill();

protected:

    detectorMap mDetectorMap;

    StiDetectorFinder(){
      if(sInstance==NULL){ sInstance = this; }
    }

    virtual ~StiDetectorFinder(){
      if(this==sInstance){ sInstance = NULL; }
    }

    static StiDetectorFinder *sInstance;
};

#endif
