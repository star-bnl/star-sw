#ifndef StiPixelDetectorBuilder_H
#define StiPixelDetectorBuilder_H

#include "Sti/StiDetectorBuilder.h"
class StiDefaultHitErrorCalculator;

class StiPixelDetectorBuilder : public StiDetectorBuilder
{

public:
    // constructors
    StiPixelDetectorBuilder(bool active);
    virtual ~StiPixelDetectorBuilder(); 	
    virtual void loadDb();
    virtual void buildMaterials();
    virtual void buildShapes();
    virtual void buildDetectors();

    /// returns the azimuthal angle [-pi, pi) for tpc sector [1-24]
    double phiForTpcSector(unsigned int iSector) const;
    
 protected:

    StiMaterial * _gas;
    StiMaterial * _fcMaterial;
		
    StiDefaultHitErrorCalculator * _innerCalc;
};

inline double StiPixelDetectorBuilder::phiForTpcSector(unsigned int sector) const
{
    if(sector<0 || sector>=12)
	{
	    cout << "phiForTpcSector(" << sector << "): invalid sector" << endl;
	    throw runtime_error("StiPixelDetectorBuilder::phiForTpcSector - ERROR - Invalid Sector");
	}
    return phiForSector(sector, 12);
} // phiForTpcSector

#endif 
