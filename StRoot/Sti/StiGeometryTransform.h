//  17 may 01
//  Ben Norman

#ifndef STI_GEOMETRY_TRANSFORM_HH
#define STI_GEOMETRY_TRANSFORM_HH

#include <map>
#include "StThreeVector.hh"
#include "StThreeVectorD.hh"

#include "tables/St_svg_config_Table.h"

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
class svg_geom_st;
class svg_shape_st;
class StPrimaryVertex;
class StSvtConfig;
class StHelix;
class StTpcHitFilter;

class StiGeometryTransform{
    
public:
    
    typedef map<unsigned int, double> padrow_radius_map;
    typedef padrow_radius_map::value_type padrow_radius_map_ValType;

    friend class nobody;
    
    static StiGeometryTransform* instance();
    static void kill();

    /// phi in [0,2pi] for tpc sector in [1,24]
    double phiForTpcSector(int sector) const{
      return phiForSector(sector, 12);
    }
    /// radius for center of tpc padrow in [1,45]
    double positionForTpcPadrow(int padrow) const;
    /// center of tpc padrow [1-45] in sector [1-24]
    StThreeVector<double> centerForTpcPadrow(int sector, int padrow) const;
    /// center of svg ladder [1-8], [1-12], [1-16], [1-20] in layer [1-7]
    StThreeVector<double> centerForSvgLadder(int layer, int ladder) const;

    /// tpc sector [1-24] for position
    int sectorForTpcCoords(const StThreeVector<double> &vec) const;
    /// tpc padrow [1-45] for position
    int padrowForTpcCoords(const StThreeVector<double> &vec) const;
    /// svg barrel for position.
    /// barrel 1 == svt layers 1&2
    /// barrel 2 == svt layers 3&4
    /// barrel 3 == svt layers 5&6
    /// barrel 4 == ssd
    int barrelForSvgCoords(const StThreeVector<double> &vec) const;
    /// svg ladder [1-8], [1-12], [1-16], [1-20] for position
    int ladderForSvgCoords(const StThreeVector<double> &vec) const;

    // accessors
    svg_config_st  getSvgConfig() const { return svgConfig; }
    svg_geom_st   *getSvgGeom() const { return aSvgGeom; }
    svg_shape_st  *getSvgShape() const{ return aSvgShape; }

    // generic transforms
    /// phi in [0,2pi] for tpc sector in [1, nSectors]
    double phiForWestSector(int iSector, int nSectors) const;
    /// phi in [0,2pi] for tpc sector in [nSectors + 1, 2*nSectors]
    double phiForEastSector(int iSector, int nSectors) const;
    /// phi in [0,2pi] for tpc sector in [1, 2*nSectors]
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
    void operator() (const StiHit*, StTpcHit*);    //From Sti -> Tpc
    
    /// StSvtHit -> StiHit
    void operator() (const StSvtHit*, StiHit*); //From Svt -> Sti
    /// StiHit -> StSvtHit
    void operator() (const StiHit*, StSvtHit*);  //From Sti -> Svt
    
    /// StSsdHit -> StiHit
    void operator() (const StSsdHit*, StiHit*); //From Ssd -> Sti
    /// StiHit -> StSsdHit
    void operator() (const StiHit*, StSsdHit*);  //From Sti -> Ssd
    
    /// StPrimaryVertex -> StiHit
    void operator() (const StPrimaryVertex*, StiHit*); //From StPrimaryVertex -> StiHit

    /// StiKalmanTrackNode -> StHelix
    void operator() (const StiKalmanTrackNode*, StHelix*);  // from StiTrackNode helix params -> StHelix

    /// StGlobalTrack -> StiKalmanTrack
    void operator() (const StGlobalTrack*, StiKalmanTrack*,
		     unsigned int maxHits=1000, const StTpcHitFilter* filter=0) const;

    /// Returns reference angle and position of padrow for hit
    pair<double, double> angleAndPosition(const StTpcHit*) const;

protected: 

    StiGeometryTransform();    
    virtual ~StiGeometryTransform();

    static StiGeometryTransform* sinstance;

    // SVT & SSD database tables
    svg_config_st  svgConfig;
    svg_geom_st  *aSvgGeom;
    svg_shape_st *aSvgShape;
    
    StTpcCoordinateTransform* tpcTransform;

    padrow_radius_map mpadrowradiusmap;

};


#endif
