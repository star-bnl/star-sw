#ifndef StiTpcDetectorBuilder_H
#define StiTpcDetectorBuilder_H
#include "Sti/StiDetectorBuilder.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
class StTpcPadPlaneI;
class StTpcDimensionsI;

class StiTpcDetectorBuilder : public StiDetectorBuilder
{

public:
    StiTpcDetectorBuilder(bool active, const string & inputFile);
    virtual ~StiTpcDetectorBuilder(); 	
    virtual void buildDetectors(StMaker&s);
    /// returns the azimuthal angle [-pi, pi) for tpc sector [1-24]
    double phiForTpcSector(unsigned int iSector) const;
    double phiForSector(unsigned int iSector,     unsigned int nSectors) const;
    double phiForWestSector(unsigned int iSector, unsigned int nSectors) const;
    double phiForEastSector(unsigned int iSector, unsigned int nSectors) const;
    void         useVMCGeometry();		
    //    virtual void AverageVolume(TGeoPhysicalNode *nodeP);
 protected:
    int rdoForPadrow(int iPadrow);
    StiMaterial * _fcMaterial;    
    StTpcPadPlaneI   * _padPlane; 
    StTpcDimensionsI * _dimensions; 
};

/// Get the azimuthal angle of the given sector
inline double StiTpcDetectorBuilder::phiForTpcSector(unsigned int sector) const
{
  if(sector<0 || sector>=12)
    {
      cout << "phiForTpcSector(" << sector << "): invalid sector" << endl;
      throw runtime_error("StiTpcDetectorBuilder::phiForTpcSector - ERROR - Invalid Sector");
    }
  return phiForSector(sector, 12);
} // phiForTpcSector

/// nSectors is the number of sectors in 360 degrees (one half of the
/// TPC or all of the SVT, for example)
inline double StiTpcDetectorBuilder::phiForSector(unsigned int iSector, 
					unsigned int nSectors) const
{
  if(iSector>=2*nSectors)
    {
      cerr << "StiDetectorBuilder::phiForSector(" << iSector << ", "
	   << nSectors << "):  Error, invalid sector" << endl;
    }
  return (iSector < nSectors) ? 
    phiForWestSector(iSector, nSectors) :
    phiForEastSector(iSector, nSectors);
}

/// returns the reference angle for the given sector number (out of the 
/// given total).  This assumes the star convention where the highest
/// numbered sector is at "12 o'clock", or pi/2, and the sector numbering
/// _decreases_ with increasing phi.  [I guess this must have seemed like
/// a good idea at the time....]
///
/// returns in [-pi, pi)
///
/// nSectors is the number of sectors in the west half of the detector,
/// not both halves.
inline double StiTpcDetectorBuilder::phiForWestSector(unsigned int iSector, 
					    unsigned int nSectors) const
{
  int offset = nSectors/4;
  double deltaPhi = 2.*M_PI/nSectors;
  
  // make phi ~ sector (not -sector) and correct offset
  double dPhi = (offset - static_cast<int>(iSector+1))*deltaPhi;
  return nice(dPhi);  
} // phiForWestSector

/// as above, but numbering _increases_ with increasing phi.
inline double StiTpcDetectorBuilder::phiForEastSector(unsigned int iSector, 
					    unsigned int nSectors) const
{
  int offset = 3*nSectors/4;
  double deltaPhi = 2.*M_PI/nSectors;
  double dPhi = (static_cast<int>(iSector+1) - offset)*deltaPhi;
  return nice(dPhi);  
} // phiForEastSector


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
