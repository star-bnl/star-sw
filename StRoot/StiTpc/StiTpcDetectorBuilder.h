#ifndef StiTpcDetectorBuilder_H
#define StiTpcDetectorBuilder_H

#include "Sti/StiDetectorBuilder.h"
class StTpcCoordinateTransform;
class StTpcPadPlaneI;
class StTpcDimensionsI;
class StTpcCoordinateTransform;
class StiDefaultHitErrorCalculator;

class StiTpcDetectorBuilder : public StiDetectorBuilder
{

public:
    // constructors
    StiTpcDetectorBuilder();
    virtual ~StiTpcDetectorBuilder(); 	
    virtual void loadDb();
    virtual void buildMaterials();
    virtual void buildShapes();
    virtual void buildDetectors();

    /// returns the azimuthal angle [-pi, pi) for tpc sector [1-24]
    double phiForTpcSector(unsigned int iSector) const;
    // radius of tpc padrow (cm) [1-45]
    //double positionForTpcPadrow(unsigned int iPadrow) const;
    // tpc padrow [1-45] for global position
    //unsigned int tpcPadrowForGlobal(const StThreeVector<double> &vec) const;
    // tpc sector [1-45] for global position
    //unsigned int tpcSectorForGlobal(const StThreeVector<double> &vec) const;
    
 protected:

    StiMaterial * _gas;
    StiMaterial * _fcMaterial;    
		
    StTpcCoordinateTransform *_transform;
    StTpcPadPlaneI   * _padPlane; 
    StTpcDimensionsI * _dimensions; 

    StiDefaultHitErrorCalculator * _innerCalc;
    StiDefaultHitErrorCalculator * _outerCalc;
};

inline double StiTpcDetectorBuilder::phiForTpcSector(unsigned int sector) const
{
  if(sector<0 || sector>=_nSectors[0])
    {
      cout << "phiForTpcSector(" << sector << "): invalid sector" << endl;
      throw runtime_error("StiTpcDetectorBuilder::phiForTpcSector - ERROR - Invalid Sector");
    }
  return phiForSector(sector, _nSectors[0]);
} // phiForTpcSector

#endif 
