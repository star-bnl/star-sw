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
    
    //Global access
    static StiGeometryTransform* instance();
    static void kill();

    // xform routines

    //Local to global
    double phiForTpcSector(int sector) const{
      return phiForSector(sector, 12);
    }
    double positionForTpcPadrow(int padrow) const;
    StThreeVector<double> centerForTpcPadrow(int sector, int padrow) const;
    StThreeVector<double> centerForSvgLadder(int layer, int ladder) const;

    //Global to local
    int sectorForTpcCoords(const StThreeVector<double> &vec) const;
    int padrowForTpcCoords(const StThreeVector<double> &vec) const;
    int layerForSvgCoords(const StThreeVector<double> &vec) const;
    int ladderForSvgCoords(const StThreeVector<double> &vec) const;

    // accessors
    svg_config_st  getSvgConfig() const { return svgConfig; }
    svg_geom_st   *getSvgGeom() const { return aSvgGeom; }
    svg_shape_st  *getSvgShape() const{ return aSvgShape; }

    // generic transforms
    double phiForWestSector(int iSector, int nSectors) const;
    double phiForEastSector(int iSector, int nSectors) const;
    double phiForSector(int iSector, int nSectors) const;
    int westSectorForPhi(double phi, int nSectors) const;
    int eastSectorForPhi(double phi, int nSectors) const;

public:
    //Hit Transform Functors (Transfrom from first argument to second argument)
    //From Tpc -> Sti
    void operator() (const StTpcHit*, StiHit*); 
    void operator() (const StiHit*, StTpcHit*);    //From Sti -> Tpc
    
    void operator() (const StSvtHit*, StiHit*); //From Svt -> Sti
    void operator() (const StiHit*, StSvtHit*);  //From Sti -> Svt
    
    void operator() (const StSsdHit*, StiHit*); //From Ssd -> Sti
    void operator() (const StiHit*, StSsdHit*);  //From Sti -> Ssd
    
    void operator() (const StPrimaryVertex*, StiHit*); //From StPrimaryVertex -> StiHit

    void operator() (const StiKalmanTrackNode*, StHelix*);  // from StiTrackNode helix params -> StHelix

    void operator() (const StGlobalTrack*, StiKalmanTrack*,
		     unsigned int maxHits=1000, const StTpcHitFilter* filter=0) const;

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
