/***********************************************************************
 *
 * $Id: StSvtCoordinateTransform.hh,v 1.8 2002/01/30 14:29:10 caines Exp $
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

#include "tables/St_svg_geom_Table.h"

#ifndef __CINT__
#include "StarCallf77.h"
#define SvtLtoG_ F77_NAME(svtltog,SVTLTOG)
#define SvtGtoL_ F77_NAME(svtgtol,SVTGTOL)
extern "C" {
int type_of_call SvtLtoG_(float *x, float *xp, svg_geom_st *geom, int *index);
int type_of_call SvtGtoL_(float *x, float *xp, svg_geom_st *geom, int *index);
}
#endif
#include "StThreeVector.hh"

#define DEBUG_SVT 0
#define idbsvt if(DEBUG_SVT) cout

class StGlobalCoordinate;
class StSvtLocalCoordinate;
class StSvtWaferCoordinate;
class StSvtConfig;
class StSvtGeometry;
class svg_geom_st;
class svg_shape_st;
class srs_srspar_st;
class StTpcCoordinateTransform;
class StTpcDb;
class StSvtCoordinateTransform {
public:

  StSvtCoordinateTransform();
  StSvtCoordinateTransform(StTpcDb* tpcDbPointer);  
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
  void  setParamPointers( StSvtGeometry* geom, StSvtConfig* config);
  int  LocaltoGlobal(const StSvtLocalCoordinate&,   StThreeVector<double>& x, int Index);
  int  GlobaltoLocal(const StThreeVector<double>& x , StSvtLocalCoordinate&, int HardWarePos, int Index );
  double CalcDriftLength(double x);
  double UnCalcDriftLength(double x);
  double CalcTransLength(double x);
  double UnCalcTransLength(double x);
  int IsOnWaferZ( const StThreeVector<double>& x, int HardWarePos);
  int IsOnWaferR(const StThreeVector<double>& x, int HardWarePos);
  StTpcCoordinateTransform* TpcTransform;
private:
  
  //  svg_geom_st *mgeom;
  //  svg_shape_st *mshape;
  //  srs_srspar_st *mparam;
  StSvtConfig *mconfig;
  StSvtGeometry* mgeom;

};

#endif
 
