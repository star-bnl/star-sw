//  17 may 01
//  Ben Norman

#ifndef STI_GEOMETRY_TRANSFORM_HH
#define STI_GEOMETRY_TRANSFORM_HH

#include "StThreeVector.hh"
#include "tables/St_svg_config_Table.h"

class StTpcHit;
class StSvtHit;
class StiHit;
class StTpcCoordinateTransform;
class svg_geom_st;
class svg_shape_st;

class StiGeometryTransform{
    
public:
    
    virtual ~StiGeometryTransform();

    //Global access
    static StiGeometryTransform* instance();
    static void kill();

    // xform routines

    //Local to global
    StThreeVector<double> centerForTpcPadrow(int sector, int padrow);
    StThreeVector<double> centerForSvgLadder(int layer, int ladder);

    //Global to local
    int sectorForTpcCoords(const StThreeVector<double> &vec);
    int padrowForTpcCoords(const StThreeVector<double> &vec);
    int layerForSvgCoords(const StThreeVector<double> &vec);
    int ladderForSvgCoords(const StThreeVector<double> &vec);

    // accessors
    svg_config_st  getSvgConfig(){ return svgConfig; }
    svg_geom_st   *getSvgGeom(){ return aSvgGeom; }
    svg_shape_st  *getSvgShape(){ return aSvgShape; }

    // generic transforms
    double phiForWestSector(int iSector, int nSectors);
    double phiForEastSector(int iSector, int nSectors);
    double phiForSector(int iSector, int nSectors);
    int westSectorForPhi(double phi, int nSectors);
    int eastSectorForPhi(double phi, int nSectors);

public:
    //Hit Transform Functors (Transfrom from first argument to second argument)
    void operator() (const StTpcHit*, StiHit*); //From Tpc -> Sti
    void operator() (const StiHit*, StTpcHit*); //From Sti -> Tpc
    
    void operator() (const StSvtHit*, StiHit*); //From Svt -> Sti
    void operator() (const StiHit, StSvtHit*);  //From Sti -> Svt
    
protected: 
    StiGeometryTransform();
    static StiGeometryTransform* sinstance;

    // SVT & SSD database tables
    svg_config_st  svgConfig;
    svg_geom_st  *aSvgGeom;
    svg_shape_st *aSvgShape;

    StTpcCoordinateTransform* tpcTransform;

};

#endif
