/***********************************************************************
 *
 * $Id: StSvtCoordinateTransform.hh,v 1.3 2000/08/26 20:37:59 caines Exp $
 *
 * Author: Helen Caines made this on  April 14 2000
 *
 ***********************************************************************
 * Description:
 *
 * Geometrical transformation Routines for:
 * Raw Wafer Coordinate  <-->  Local Coordinate
 *   Local Coordinate  <-->  Global Coordinate
 *
 * These Routines deal positions ONLY!
 *

 ***********************************************************************/
#ifndef ST_COORDINATE_SVTTRANSFORM_HH
#define ST_COORDINATE_SVTTRANSFORM_HH

#include <stdlib.h>
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif
//#include <unistd.h>
#ifndef ST_NO_EXCEPTIONS
//#include <stdexcept>
#endif

// SCL
#include "StGlobals.hh"
#include "tables/St_svg_geom_Table.h"
#include "tables/St_svg_shape_Table.h"
#include "tables/St_srs_srspar_Table.h"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StThreeVector.hh"


#define DEBUG_SVT 0
#define idbsvt if(DEBUG_SVT) cout

class StGlobalCoordinate;
class StSvtLocalCoordinate;
class StSvtWaferCoordinate;

class StSvtCoordinateTransform {
public:

  StSvtCoordinateTransform();
  ~StSvtCoordinateTransform();
  
  //      Raw Data          <-->  Global Coordinate
  void  operator()(const StSvtWaferCoordinate&, StGlobalCoordinate&);
  void  operator()(const StGlobalCoordinate&, StSvtWaferCoordinate&);
  
  //      Raw Data          <--> Svt Local Coordinates
  
  void  operator()(const StSvtLocalCoordinate&, StSvtWaferCoordinate&);
  void  operator()(const StSvtWaferCoordinate&, StSvtLocalCoordinate&);
  
  
  // Svt Local <--> Global
  void  operator()(const StSvtLocalCoordinate&, StGlobalCoordinate&);
  void  operator()(const  StGlobalCoordinate& ,StSvtLocalCoordinate&);
  void  setParamPointers( srs_srspar_st* srspar, svg_geom_st* geom, 
			  svg_shape_st* shape, StSvtHybridCollection* config );
  void  LocaltoGlobal(const StSvtLocalCoordinate&,   StThreeVector<double>& x);
  void  GlobaltoLocal(const StThreeVector<double>& x , StSvtLocalCoordinate&, int Index );
  double CalcDriftLength(double x);
  double UnCalcDriftLength(double x);
  double CalcTransLength(double x);
  double UnCalcTransLength(double x);
  int IsOnWaferZ( const StThreeVector<double>& x, int HardWarePos);
  int IsOnWaferR(const StThreeVector<double>& x, int HardWarePos);
private:
  
  svg_geom_st *mgeom;
  svg_shape_st *mshape;
  srs_srspar_st *mparam;
  StSvtHybridCollection *mconfig;

};

inline void StSvtCoordinateTransform::setParamPointers( srs_srspar_st* param,
							svg_geom_st* geom, 
							svg_shape_st* shape,
							StSvtHybridCollection* config){
  mparam = param;
  mgeom = geom;
  mconfig = config;
  mshape = shape;
}

#endif
 
