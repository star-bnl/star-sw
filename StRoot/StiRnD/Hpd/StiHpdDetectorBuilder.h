#ifndef StiHpdDetectorBuilder_H
#define StiHpdDetectorBuilder_H
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiHitErrorCalculator.h"

class StiHpdDetectorBuilder : public StiDetectorBuilder
{
public:
    StiHpdDetectorBuilder(bool active, const string & inputFile);
    virtual ~StiHpdDetectorBuilder(); 	
    virtual void buildDetectors(StMaker&source);
    virtual void loadDS(TDataSet&);
protected:
    StiMaterial * _gas;
    StiMaterial * _fcMaterial;
    StiDefaultHitErrorCalculator _calculator;
};

#endif 
