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
    StiTpcDetectorBuilder(bool active, char* baseName);
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
    int rdoForPadrow(int iPadrow);


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

///Function returns the rdo board number for a given 
///padrow index. 
///Range of map used is 1-45. 
inline int StiTpcDetectorBuilder::rdoForPadrow(int iPadrow)
{
  int iRdo = 0;
  if (iPadrow>0&&iPadrow<=8){
    iRdo = 1;
  }
  else if (iPadrow>8&&iPadrow<=13){
    iRdo = 2;
  }
  else if (iPadrow>13&&iPadrow<=21){
    iRdo = 3;
  }
  else if (iPadrow>21&&iPadrow<=29){
    iRdo = 4;
  }
  else if (iPadrow>29&&iPadrow<=37){
    iRdo = 5;
  }
  else if (iPadrow>37&&iPadrow<=45){
    iRdo = 6;
  }
  return iRdo;
} // rdoForPadrow


#endif 
