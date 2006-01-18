#ifndef StiIstDetectorBuilder_H
#define StiIstDetectorBuilder_H
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiHitErrorCalculator.h"

class StiIstDetectorBuilder : public StiDetectorBuilder
{
public:
    StiIstDetectorBuilder(bool active, const string & inputFile);
    virtual ~StiIstDetectorBuilder(); 	
    virtual void buildDetectors(StMaker&source);

protected:
    StiMaterial * _gas;
    StiMaterial * _fcMaterial;
    StiDefaultHitErrorCalculator _calculator;
};

#endif 
