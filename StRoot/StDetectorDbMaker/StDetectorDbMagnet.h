#ifndef StDetectorDbMagnet_h
#define StDetectorDbMagnet_h

#include <iostream.h>
#include "StMaker.h"

struct starMagOnl_st;

enum StMagnetPolarity {eUnknownMField, eFullMFieldPolB, eHalfMFieldPolB,
		       eZeroMField, eHalfMFieldPolA, eFullMFieldPolA};

class StDetectorDbMagnet{
public:
    static StDetectorDbMagnet* instance();
    double                     getScaleFactor(unsigned int time=0);
    double                     getMagnetCurrent(unsigned int time=0);
    StMagnetPolarity           getMagneticField(unsigned int time=0);
    unsigned int               getRunNumber();
    friend ostream& operator<<(ostream& os, StDetectorDbMagnet& v);
    // These Fuctions will be public, but should generally be used:
    unsigned int               getNumRows();
    unsigned int               getTimeEntry(unsigned int);
    double                     getMagnetCurrentEntry(unsigned int);
    double                     currentToScaleFactor(double);
    void update(StMaker*);
    friend class nobody; // for virtual ~
    
protected:
    virtual ~StDetectorDbMagnet();
    StDetectorDbMagnet();
    starMagOnl_st * mStarMagOnl; // points to magnet struct
    TTable* mTable; // points to table, need to re-intilize mStarMagOnl every event
    unsigned int mNumRows;
private:
    static StDetectorDbMagnet* sInstance;
};

#endif
