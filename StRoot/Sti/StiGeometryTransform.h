//  17 may 01
//  Ben Norman

#ifndef STI_GEOMETRY_TRANSFORM_HH
#define STI_GEOMETRY_TRANSFORM_HH

#include <map>
#include "StThreeVector.hh"
#include "StThreeVectorD.hh"

//#include "tables/St_svg_config_Table.h"

class StiTrack;
class StiKalmanTrack;
class StiKalmanTrackNode;
class StTrack;
class StGlobalTrack;
class StTpcHit;
class StHit;
class StSvtHit;
class StSsdHit;
class StiHit;
class StTpcCoordinateTransform;
//XX class svg_geom_st;
//XX class svg_shape_st;
class StPrimaryVertex;
class StSvtConfig;
class StPhysicalHelix;
class StTpcHitFilter;
class StSvtConfig;
class StSvtGeometry;
class StiGeometryTransform{
    
public:
    
    typedef map<unsigned int, double> padrow_radius_map;
    typedef padrow_radius_map::value_type padrow_radius_map_ValType;

    friend class nobody;
    
    static StiGeometryTransform* instance();
    static void kill();

    /// phi in [-pi,pi] for tpc sector in [1,24]
    double phiForTpcSector(int sector) const{
      return phiForSector(sector, 12);
    }
    /// radius for center of tpc padrow in [1,45]
    double positionForTpcPadrow(int padrow) const;
    /// center of tpc padrow [1-45] in sector [1-24]
    StThreeVector<double> centerForTpcPadrow(int sector, int padrow) const;
    /// center of svt ladder [1-8], [1-12], [1-16] in barrel [1-3]
    StThreeVector<double> centerForSvtLadder(int barrel, int ladder) const;

    /// tpc sector [1-24] for position
    int sectorForTpcCoords(const StThreeVector<double> &vec) const;
    /// tpc padrow [1-45] for position
    int padrowForTpcCoords(const StThreeVector<double> &vec) const;
    /// svt barrel for position.
    /// barrel 1 == svt layers 1&2
    /// barrel 2 == svt layers 3&4
    /// barrel 3 == svt layers 5&6
    int barrelForSvtCoords(const StThreeVector<double> &vec) const;
    /// svt layer [1-6] and ladder [1-8], [1-12], [1-16] for position
    pair<int, int> layerAndLadderForSvtCoords(
        const StThreeVector<double> &vec) const;

    // accessors
//XX    svg_config_st  getSvgConfig() const { return svgConfig; }
//XX    svg_geom_st   *getSvgGeom() const { return aSvgGeom; }
//XX    svg_shape_st  *getSvgShape() const{ return aSvgShape; }
    StSvtConfig *getSvtConfig() const { return m_pSvtConfig; }
    StSvtGeometry *getSvtGeometry() const { return m_pSvtGeometry; }

    // generic transforms
    /// phi in [-pi,pi] for sector in [1, nSectors] (z>0)
    double phiForWestSector(int iSector, int nSectors) const;
    /// phi in [-pi,pi] for sector in [nSectors + 1, 2*nSectors] (z<0)
    double phiForEastSector(int iSector, int nSectors) const;
    /// phi in [-pi,pi] for sector in [1, 2*nSectors]
    double phiForSector(int iSector, int nSectors) const;
    /// sector in [1, nSectors] for phi
    int westSectorForPhi(double phi, int nSectors) const;
    /// sector in [nSectors + 1, 2*nSectors] for phi
    int eastSectorForPhi(double phi, int nSectors) const;

    ///Set the hit errors in the rotated (StiHit) system defined by rotation of the StHit
    /// error matrix by angle theta about the z-axis.
    void setStiHitError(const StHit* stHit, StiHit* stiHit, double theta);

public:
    /// StTpcHit -> StiHit
    void operator() (const StTpcHit*, StiHit*); 
    /// StiHit -> StTpcHit
    void operator() (const StiHit*, StTpcHit*);
    
    /// StSvtHit -> StiHit
    void operator() (const StSvtHit*, StiHit*);
    /// StiHit -> StSvtHit
    void operator() (const StiHit*, StSvtHit*);
    
    /// StSsdHit -> StiHit
    void operator() (const StSsdHit*, StiHit*);
    /// StiHit -> StSsdHit
    void operator() (const StiHit*, StSsdHit*);
    
    /// StPrimaryVertex -> StiHit
    void operator() (const StPrimaryVertex*, StiHit*);

    /// StiKalmanTrackNode -> StPhysicalHelix
    void operator() (const StiKalmanTrackNode*, StPhysicalHelix*);

    /// StGlobalTrack -> StiKalmanTrack
    void operator() (const StGlobalTrack*, StiKalmanTrack*,
		     unsigned int maxHits=1000, 
                     const StTpcHitFilter* filter=0) const;

    /// Returns reference angle and position of padrow for hit
    pair<double, double> angleAndPosition(const StTpcHit*) const;

protected: 

    StiGeometryTransform();    
    virtual ~StiGeometryTransform();

    static StiGeometryTransform* sinstance;

    // SVT database
    StSvtConfig *m_pSvtConfig;
    StSvtGeometry *m_pSvtGeometry;

    // SSD database tables
//XX    svg_config_st  svgConfig;
//XX    svg_geom_st  *aSvgGeom;
//XX    svg_shape_st *aSvgShape;

    StTpcCoordinateTransform* tpcTransform;

    padrow_radius_map mpadrowradiusmap;

};


#endif
