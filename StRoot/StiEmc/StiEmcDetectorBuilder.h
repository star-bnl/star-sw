#ifndef StiEmcDetectorBuilder_H
#define StiEmcDetectorBuilder_H

#include "Sti/StiDetectorBuilder.h"

class StiEmcDetectorBuilder : public StiDetectorBuilder
{

public:
    // constructors
    StiEmcDetectorBuilder();
    virtual ~StiEmcDetectorBuilder(); 
    virtual void loadDb();
    virtual void buildMaterials();
    virtual void buildShapes();
    virtual void buildDetectors();

 protected:

    StiMaterial * _lead;
    StiMaterial * _gas;

    StiShape * _preShower;
    StiShape * _showerMax;
    StiShape * _tower;
};

#endif 
