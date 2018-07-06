#ifndef StiTpcDetectorBuilder_H
#define StiTpcDetectorBuilder_H

#include <set>

#include "Sti/StiDetectorBuilder.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"



class StiTpcDetectorBuilder : public StiDetectorBuilder
{

public:
    StiTpcDetectorBuilder(bool active) : StiTpcDetectorBuilder(active, false) {}
    StiTpcDetectorBuilder(bool active, bool active_iTpc);
    virtual ~StiTpcDetectorBuilder(); 	
    virtual void buildDetectors(StMaker&s);
    /// returns the azimuthal angle [-pi, pi) for tpc sector [1-24]
    double phiForTpcSector(UInt_t iSector) const;
    double phiForSector(UInt_t iSector,     UInt_t nSectors) const;
    double phiForWestSector(UInt_t iSector, UInt_t nSectors) const;
    double phiForEastSector(UInt_t iSector, UInt_t nSectors) const;
    void   useVMCGeometry();		
    //    virtual void AverageVolume(TGeoPhysicalNode *nodeP);

    static std::pair<int, int>  toStiLayer(const int tpc_sector, const int tpc_padrow);

 protected:
    StiMaterial * _fcMaterial;    

    /// Option to use iTPC hits in Sti tracking. By default hits are not used in Sti tracking
    bool  _active_iTpc = false;

private:

    struct StiLayer
    {
      enum TpcHalf { West = 0, East = 1 };

      StiLayer(int tpc_sector, int tpc_padrow) :
        sti_sector_id(tpc_sector <= 12 ? tpc_sector-1 : 12 - (tpc_sector-12)%12 - 1),
        sti_padrow_id(-1)
        {
          TpcHalf tpc_half_id = (tpc_sector <= 12 ? West : East);
          tpc_sector_id[tpc_half_id] = tpc_sector;
          tpc_padrow_id[tpc_half_id] = tpc_padrow;
        }

               int sti_sector_id;
      mutable  int sti_padrow_id;
      mutable  int tpc_sector_id[2] = {-1, -1}; /// East and/or West if available
      mutable  int tpc_padrow_id[2] = {-1, -1}; /// East and/or West if available

              void update(int tpc_sector, int tpc_padrow) const {
                     TpcHalf tpc_half_id = (tpc_sector <= 12 ? West : East);
                     tpc_sector_id[tpc_half_id] = tpc_sector;
                     tpc_padrow_id[tpc_half_id] = tpc_padrow;
                   }
               int tpc_sector() const { return tpc_sector_id[West] > 0 ? tpc_sector_id[West] : tpc_sector_id[East]; }
               int tpc_padrow() const { return tpc_padrow_id[West] > 0 ? tpc_padrow_id[West] : tpc_padrow_id[East]; }
               int tpc_sector(TpcHalf half) const { return tpc_sector_id[half]; }
              bool operator< (const StiLayer& other) const;
    };

    void fillStiLayersMap();

    StiPlanarShape* constructTpcPadrowShape(StiLayer stiLayer) const;
    StiDetector*    constructTpcPadrowDetector(StiLayer stiLayer, StiPlanarShape* pShape) const;

    static std::set<StiLayer> sStiLayers;
};

/// Get the azimuthal angle of the given sector
inline double StiTpcDetectorBuilder::phiForTpcSector(UInt_t sector) const
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
inline double StiTpcDetectorBuilder::phiForSector(UInt_t iSector, 
					UInt_t nSectors) const
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
inline Double_t StiTpcDetectorBuilder::phiForWestSector(UInt_t iSector, 
					    UInt_t nSectors) const
{
  Int_t offset = nSectors/4;
  Double_t deltaPhi = 2.*M_PI/nSectors;
  
  // make phi ~ sector (not -sector) and correct offset
  Double_t dPhi = (offset - static_cast<Int_t>(iSector+1))*deltaPhi;
  return nice(dPhi);  
} // phiForWestSector

/// as above, but numbering _increases_ with increasing phi.
inline Double_t StiTpcDetectorBuilder::phiForEastSector(UInt_t iSector, 
					    UInt_t nSectors) const
{
  Int_t offset = 3*nSectors/4;
  Double_t deltaPhi = 2.*M_PI/nSectors;
  Double_t dPhi = (static_cast<Int_t>(iSector+1) - offset)*deltaPhi;
  return nice(dPhi);  
} // phiForEastSector

#endif 
