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
typedef map<MaterialMapKey, StiMaterial*> materialMap;
typedef materialMap::value_type materialMapValType;

typedef map<ShapeMapKey, StiShape*> shapeMap;
typedef shapeMap::value_type shapeMapValType;

typedef vector<StiDetector*> detectorVector;
typedef detectorVector::iterator detectorIterator;

class StiDetectorBuilder{

public:
    // constructors
    StiDetectorBuilder();
    virtual ~StiDetectorBuilder(); 

    virtual void init() = 0;

    // accessors
    detectorVector getDetectors(){ return mDetectorVector; }
    
    // mutators

    // iterators
    bool hasMore() const;
    void fillNext(StiDetector *detector);
    
protected:
    
    virtual void buildMaterials() = 0;
    virtual void buildShapes() = 0;
    virtual void buildDetectors() = 0;

    materialMap      mMaterialMap;
    shapeMap         mShapeMap;
    detectorVector   mDetectorVector;
    detectorIterator mDetectorIterator;
};

#endif // ifndef STI_DETECTOR_BUILDER_H
