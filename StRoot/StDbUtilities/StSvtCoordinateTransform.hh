/***********************************************************************
 *
 * $Id: StSvtCoordinateTransform.hh,v 1.5 2001/02/12 23:42:40 caines Exp $
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

#include "StThreeVector.hh"

#define DEBUG_SVT 0
#define idbsvt if(DEBUG_SVT) cout

class StGlobalCoordinate;
class StSvtLocalCoordinate;
class StSvtWaferCoordinate;
class StSvtConfig;
class svg_geom_st;
class svg_shape_st;
class srs_srspar_st;

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
  void  setParamPointers( srs_srspar_st* srspar, svg_geom_st* geom, svg_shape_st* shape, StSvtConfig* config);
  int  LocaltoGlobal(const StSvtLocalCoordinate&,   StThreeVector<double>& x, int Index);
  int  GlobaltoLocal(const StThreeVector<double>& x , StSvtLocalCoordinate&, int HardWarePos, int Index );
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
  StSvtConfig *mconfig;

};

inline void StSvtCoordinateTransform::setParamPointers( srs_srspar_st* param,
							svg_geom_st* geom, 
							svg_shape_st* shape,
							StSvtConfig* config){
  mparam = param;
  mgeom = geom;
  mconfig = config;
  mshape = shape;
}

#endif
 
