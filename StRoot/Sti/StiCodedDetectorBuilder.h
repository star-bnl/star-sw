// creates detector objects at runtime
//
// 1 aug 01
// Ben Norman (Kent State University)

#ifndef STI_CODED_DETECTOR_BUILDER_H
#define STI_CODED_DETECTOR_BUILDER_H

#include "StiDetectorBuilder.h"

class StiDetector;
class StiMaterial;
class StiShape;

class StiCodedDetectorBuilder: public StiDetectorBuilder{

public:

    // constructors
    StiCodedDetectorBuilder();
    virtual ~StiCodedDetectorBuilder(); 

    void init();

    // accessors
    
    // mutators

    // iterators
    bool hasMore() const;
    void fillNext(StiDetector *detector);

protected:

    void buildMaterials();
    void buildShapes();
    void buildDetectors();

    StiMaterial *findMaterial(const char *szName) const;
    StiShape *findShape(const char *szName) const;
    
};

#endif // ifndef STI_CODED_DETECTOR_BUILDER_H
