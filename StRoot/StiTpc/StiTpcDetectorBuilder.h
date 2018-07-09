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
    void   useVMCGeometry();		

    static std::pair<int, int>  toStiLayer(const int tpc_sector, const int tpc_padrow);

 protected:

    /// Option to use iTPC hits in Sti tracking. By default hits are not used in Sti tracking
    bool  _active_iTpc = false;

private:

    class StiLayer;

    StiPlanarShape* constructTpcPadrowShape(StiLayer stiLayer) const;
    StiDetector*    constructTpcPadrowDetector(StiLayer stiLayer, StiPlanarShape* pShape) const;

    friend std::ostream& operator<< (std::ostream& os, const StiLayer& stiLayer);

    /// Fills internal container with Sti layers based on information in St_tpcPadConfigC
    static void buildStiLayerMap();

    static std::set<StiLayer> sStiLayers;
};


/**
 * This structure essentially maps physical TPC sector/padrow ID to the
 * corresponding Sti sector/padrow ID
 */
struct StiTpcDetectorBuilder::StiLayer
{
  enum TpcHalf { West = 0, East = 1 };

  StiLayer(int tpc_sector, int tpc_padrow, bool split = false) :
    sti_split_in_half(split),
    sti_sector_id(tpc_sector <= 12 ? tpc_sector-1 : 12 - (tpc_sector-12)%12 - 1),
    sti_padrow_id(0)
    {
      TpcHalf tpc_half_id = (tpc_sector <= 12 ? West : East);
      tpc_sector_id[tpc_half_id] = tpc_sector;
      tpc_padrow_id[tpc_half_id] = tpc_padrow;
    }

         bool sti_split_in_half = false;   ///< Split Sti layer in East and West halves
          int sti_sector_id = -1;
  mutable int sti_padrow_id = -1;
  mutable int tpc_sector_id[2] = {-1, -1}; ///< East and/or West if available
  mutable int tpc_padrow_id[2] = {-1, -1}; ///< East and/or West if available

  void update_tpc_id(int tpc_sector, int tpc_padrow) const {
         TpcHalf tpc_half_id = (tpc_sector <= 12 ? West : East);
         tpc_sector_id[tpc_half_id] = tpc_sector;
         tpc_padrow_id[tpc_half_id] = tpc_padrow;
       }
  int  tpc_sector() const { return tpc_sector_id[West] > 0 ? tpc_sector_id[West] : tpc_sector_id[East]; }
  int  tpc_padrow() const { return tpc_padrow_id[West] > 0 ? tpc_padrow_id[West] : tpc_padrow_id[East]; }
  int  tpc_sector(TpcHalf half) const { return tpc_sector_id[half]; }
  bool represents_only(TpcHalf half) const { return (half == East && tpc_sector_id[West] < 0) ||
                                                    (half == West && tpc_sector_id[East] < 0); }
  double radial_distance() const;

  bool operator< (const StiLayer& other) const;
};


#endif 
