//  17 may 01
//  Ben Norman

#ifndef STI_GEOMETRY_TRANSFORM_HH
#define STI_GEOMETRY_TRANSFORM_HH

class StiGeometryTransform{
    
public:
    
    StiGeometryTransform();
    virtual ~StiGeometryTransform();

    // xform routines

    // these return a vector (with z==0) pointing from the origin to 
    // the center of the given padrow or ladder.
    StThreeVector<double> centerForTpcPadrow(int sector, int padrow){
      double radius = 
          gStTpcDb->PadPlaneGeometry()->radialDistanceAtRow(padrow);
      Double_t phi = phiForSector(sector, 12);

      return StThreeVector<double>(radius*cos(phi), radius*sin(phi), 0.);
    }
    // this uses the database convention of 6 svt layers + 1 ssd layer.
    // Every other svt
    // ladder is thus bogus (1,3,5,7 on layer 1, eg), but the xform
    // works regardless
    StThreeVector<double> centerForSvgLadder(int layer, int ladder){
      double radius = svgConfig.layer_radius[layer - 1];
      int nLadders = 2*svgConfig.n_ladder[layer - 1];
      double phi = phiForWestSector(ladder, nLadders);

      return StThreeVector<double>(radius*cos(phi), radius*sin(phi), 0.);
    }

    int sectorForTpcCoords(const StThreeVector<double> &vec){
      return tpcTransform->sectorFromCoordinate(vec);
    }
    int padrowForTpcCoords(const StThreeVector<double> &vec){
      int sector = sectorForTpcCoords(vec);
      return tpcTransform->rowFromLocal(
          tpcTransform->rotateToLocal(vec, sector));
    }
    
    // finds nearest real ladder (as above, could be bogus [electronics instead
    // of wafers]) and layer.
    int layerForSvgCoords(const StThreeVector<double> &vec){
      double minDeltaR = 200.;
      int minLayer = 0;
      for(int layer = 0; layer < 7; layer++){
        if( fabs(svgConfig.layer_radius[layer] - vec.perp()) < minDeltaR){
          minDeltaR = fabs(svgConfig.layer_radius[layer] - vec.perp());
          minLayer = layer;
        }
      }
      return minLayer + 1;
    }
    int ladderForSvgCoords(const StThreeVector<double> &vec){
      int layer = layerForSvgCoords(vec);
      int nLadders = svgConfig.n_ladder[layer - 1];

      return westSectorForPhi(vec.phi(), nLadders);
    }

    // accessors
    svg_config_st  getSvgConfig(){ return svgConfig; }
    svg_geom_st   *getSvgGeom(){ return aSvgGeom; }
    svg_shape_st  *getSvgShape(){ return aSvgShape; }

    // generic transforms
    double phiForWestSector(int iSector, int nSectors);
    double phiForEastSector(int iSector, int nSectors);
    double phiForSector(int iSector, int nSectors){
      if(iSector>nSectors){ return phiForEastSector(iSector, nSectors); }
      else{                 return phiForWestSector(iSector, nSectors); }
    }

    int westSectorForPhi(double phi, int nSectors);
    int eastSectorForPhi(double phi, int nSectors);

protected: 

    // SVT & SSD database tables
    svg_config_st  svgConfig;
    svg_geom_st  *aSvgGeom;
    svg_shape_st *aSvgShape;

    StTpcCoordinateTransform* tpcTransform;

};

#endif
