// creates detector objects at runtime
//
// 1 aug 01
// Ben Norman (Kent State University)

#ifndef STI_DETECTOR_BUILDER_H
#define STI_DETECTOR_BUILDER_H

#include <map>
#include <vector>
#include "StiMapUtilities.h"

class StiDetector;
class StiMaterial;
class StiShape;

// Set up stl maps for by-name lookup of shapes and materials.
// Not used for placements because they are unique to each detector.
typedef map<NameMapKey, StiMaterial*> materialMap;
typedef materialMap::value_type materialMapValType;

typedef map<NameMapKey, StiShape*> shapeMap;
typedef shapeMap::value_type shapeMapValType;

typedef map<NameMapKey, StiDetector*> detectorMap;
typedef detectorMap::const_iterator detectorIterator;
typedef detectorMap::value_type detectorMapValType;

class StiDetectorBuilder{

public:
    // constructors
    StiDetectorBuilder();
    virtual ~StiDetectorBuilder(); 

    virtual void init() = 0;

    // accessors
    detectorMap getDetectors(){ return mDetectorMap; }

    virtual StiMaterial *findMaterial(const string& szName) const;
    virtual StiShape    *findShape(const string& szName) const;
    virtual StiDetector *findDetector(const string& szName) const;

    // mutators

    // iterators
    bool hasMore() const;
    void fillNext(StiDetector *detector);
    
protected:
    
    virtual void buildMaterials() = 0;
    virtual void buildShapes() = 0;
    virtual void buildDetectors() = 0;

    materialMap         mMaterialMap;
    shapeMap            mShapeMap;
    detectorMap         mDetectorMap;
    detectorIterator    mDetectorIterator;

};

#endif // ifndef STI_DETECTOR_BUILDER_H
