#ifndef StiEmcDetectorBuilder_H
#define StiEmcDetectorBuilder_H
#include "Sti/StiDetectorBuilder.h"

class StiEmcDetectorBuilder : public StiDetectorBuilder
{
public:
    StiEmcDetectorBuilder(bool active);
    virtual ~StiEmcDetectorBuilder(); 
    virtual void buildDetectors(StMaker&source);
 protected:
    StiMaterial * _lead;
    StiMaterial * _gas;
    StiShape * _preShower;
    StiShape * _showerMax;
    StiShape * _tower;
};
#endif 
