//StiDetectorLayerContainer.h
//M.L. Miller (Yale Software)
//02/02/01

//Container class for StiDetectorLayers
//Is a map, so full STL functionality provided
//Is a map, not multimap, so each key must be unique!

#ifndef StiDetectorLayerContainer_HH
#define StiDetectorLayerContainer_HH

#include <map>
#include "StiMapUtilities.h"

typedef map<DetectorMapKey, StiDetector*, DetectorMapKeyLessThan> detectormap;
typedef detectormap::value_type detectorMapValType;

class StiDetectorLayerContainer : public detectormap
{
public:
    StiDetectorLayerContainer();
    virtual ~StiDetectorLayerContainer();

    virtual void build(const char* buildDirectory); //Temp build from txt file using SCL parser
    void push_back(StiDetector* layer);
    void clearAndDestroy();
    void print() const;
    void print(DetectorMapKey*) const;
    
private:
    DetectorMapKey mkey; //Store one to avoid constructor calls
};

#endif



