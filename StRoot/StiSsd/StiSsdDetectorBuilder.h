#ifndef StiSsdDetectorBuilder_H
#define StiSsdDetectorBuilder_H

#include "Sti/StiDetectorBuilder.h"

class StiSsdDetectorBuilder : public StiDetectorBuilder
{

public:
    // constructors
    StiSsdDetectorBuilder(bool active);
    virtual ~StiSsdDetectorBuilder(); 
    virtual void buildMaterials();
    virtual void buildShapes();
    virtual void buildDetectors();
    virtual void loadDb();
};

#endif 
