#ifndef StiPixelDetectorBuilder_H
#define StiPixelDetectorBuilder_H
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiHitErrorCalculator.h"

class StiPixelDetectorBuilder : public StiDetectorBuilder
{
public:
    StiPixelDetectorBuilder(bool active, const string & inputFile);
    virtual ~StiPixelDetectorBuilder(); 	
    virtual void buildDetectors(StMaker&source);
    double phiForSector(unsigned int iSector) const;
 protected:
    StiMaterial * _gas;
    StiMaterial * _fcMaterial;
    StiDefaultHitErrorCalculator _calculator;
};

inline double StiPixelDetectorBuilder::phiForSector(unsigned int sector) const
{
	int nSectors = 12;
  int offset = nSectors/4;
  double deltaPhi = 2.*M_PI/nSectors;
  double dPhi = (offset - static_cast<int>(sector+1))*deltaPhi;
  return nice(dPhi);  
} 

#endif 
