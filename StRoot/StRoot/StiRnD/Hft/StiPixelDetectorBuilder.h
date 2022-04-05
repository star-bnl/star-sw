#ifndef StiPixelDetectorBuilder_H
#define StiPixelDetectorBuilder_H
#include "Sti/StiDetectorBuilder.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
class StiPixelDetectorBuilder : public StiDetectorBuilder
{
public:
    StiPixelDetectorBuilder(bool active);
    virtual ~StiPixelDetectorBuilder(); 	
    virtual void buildDetectors(StMaker&source);
    double phiForSector(unsigned int iSector) const;


    void useVMCGeometry();
    void AverageVolume(TGeoPhysicalNode *nodeP);

    /// returns the azimuthal angle [-pi, pi) for tpc sector [1-24]
    double phiForPixelSector(unsigned int iSector) const;
    double radiusForPixelSector(unsigned int iSector) const;
    double psiForPixelSector(unsigned int iSector) const;

 protected:
    StiMaterial * _fcMaterial;
    StiMaterial *_siMat;
    StiMaterial *_hybridMat;
};

inline double StiPixelDetectorBuilder::phiForSector(unsigned int sector) const
{
	int nSectors = 12;
  int offset = nSectors/4;
  double deltaPhi = 2.*M_PI/nSectors;
  double dPhi = (offset - static_cast<int>(sector+1))*deltaPhi;
  return nice(dPhi);  
} 

inline double StiPixelDetectorBuilder::phiForPixelSector(unsigned int sector) const
{
    if(sector>=24)
	{
	    cout << "phiForPixelSector(" << sector << "): invalid sector" << endl;
	    throw runtime_error("StiPixelDetectorBuilder::phiForPixelSector - ERROR - Invalid Sector");
	}
    double phi = -1000.;
    switch (sector)
      {
      case 0:
	phi = 0.;
	break;
      case 3:
	phi = 60.;
	break;
      case 6:
	phi = 120.;
	break;
      case 9:
	phi = 180.;
	break;
      case 12:
	phi = 240.;
	break;
      case 15:
	phi = 300.; 
	break;
      case 1:
	phi = 0. + 20.27;
	break;
      case 4:
	phi = 60. + 20.27;
	break;
      case 7:
	phi = 120. + 20.27;
	break;
      case 10:
	phi = 180. + 20.27;
	break;
      case 13:
	phi = 240. + 20.27;
	break;
      case 16:
	phi = 300. + 20.27; 
	break;
      case 2:
	phi = 0. + 42.62;
	break;
      case 5:
	phi = 60. + 42.62;
	break;
      case 8: 
	phi = 120. + 42.62;
	break;
      case 11:
	phi = 180. + 42.62;
	break;
      case 14:
	phi = 240. + 42.62;
	break;
      case 17:
	phi = 300. + 42.62;
	break;
      case 18:
	phi = 0. + 79.51;
	break;
      case 19:
	phi = 60. + 79.51;
	break;
      case 20:
	phi = 120. + 79.51;
	break;
      case 21:
	phi = 180. + 79.51;
	break;
      case 22:
	phi = 240. + 79.51;
	break;
      case 23:
	phi = 300. + 79.51;
	break;
      }

    return (phi/180.*M_PI);
} // phiForPixelSector

inline double StiPixelDetectorBuilder::radiusForPixelSector(unsigned int sector) const
{
    if(sector<0 || sector>=24)
	{
	    cout << "radiusForPixelSector(" << sector << "): invalid sector" << endl;
	    throw runtime_error("StiPixelDetectorBuilder::radiusForPixelSector - ERROR - Invalid Sector");
	}

    double radius = -1000.;
    switch (sector)
      {
      case 0:
      case 3:
      case 6:
      case 9:
      case 12:
      case 15:
	radius = 5.294; 
	break;
      case 1:
      case 4:
      case 7:
      case 10:
      case 13:
      case 16:
	radius = 4.862; 
	break;
      case 2:
      case 5:
      case 8: 
      case 11:
      case 14:
      case 17:
	radius = 4.391; 
	break;
	break;
      case 18:
      case 19:
      case 20:
      case 21:
      case 22:
      case 23:
	radius = 1.595; 
	break;
      }

    return radius;
} // radiusForPixelSector

inline double StiPixelDetectorBuilder::psiForPixelSector(unsigned int sector) const
{
    if(sector<0 || sector>=24)
	{
	    cout << "psiForPixelSector(" << sector << "): invalid sector" << endl;
	    throw runtime_error("StiPixelDetectorBuilder::psiForPixelSector - ERROR - Invalid Sector");
	}

    double psi = -1000.;
    switch (sector)
      {
      case 0:
      case 3:
      case 6:
      case 9:
      case 12:
      case 15:
	psi = 0.72; 
	break;
      case 1:
      case 4:
      case 7:
      case 10:
      case 13:
      case 16:
	psi = 1.69; 
	break;
      case 2:
      case 5:
      case 8: 
      case 11:
      case 14:
      case 17:
	psi = 2.96; 
	break;
	break;
      case 18:
      case 19:
      case 20:
      case 21:
      case 22:
      case 23:
	psi = 19.85; 
	break;
      }

    return (psi/180.*M_PI);
    //return 0.;
} // psiForPixelSector

#endif 
