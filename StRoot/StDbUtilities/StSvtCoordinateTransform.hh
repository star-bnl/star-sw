/***********************************************************************
 *
 * $Id: StSvtCoordinateTransform.hh,v 1.17 2007/04/13 16:10:34 fisyak Exp $
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

#include "TF1.h"

#include "tables/St_svg_geom_Table.h"

#include "StThreeVector.hh"

#define DEBUG_SVT 0
#define idbsvt if(DEBUG_SVT) cout

class StGlobalCoordinate;
class StSvtLocalCoordinate;
class StSvtWaferCoordinate;
class StSvtConfig;
class StSvtGeometry;
class StSvtHybridCollection;
class StSvtT0;
class svg_geom_st;
class svg_shape_st;
class srs_srspar_st;
class StTpcDb;
class St_svtCorrectionC;
class StSvtCoordinateTransform {
public:

  StSvtCoordinateTransform();
  StSvtCoordinateTransform(StTpcDb* tpcDbPointer);  
  ~StSvtCoordinateTransform();
  
  //      Raw Data          <-->  Global Coordinate
  void  operator()(const StSvtWaferCoordinate&, StGlobalCoordinate&);
  void  operator()(const StGlobalCoordinate&, StSvtWaferCoordinate&, Int_t Id);
  
  //      Raw Data          <--> Svt Local Coordinates
  
  void  operator()(const StSvtLocalCoordinate&, StSvtWaferCoordinate&);
  void  operator()(const StSvtWaferCoordinate&, StSvtLocalCoordinate&);
  
  
  // Svt Local <--> Global
  void  operator()(const StSvtLocalCoordinate&, StGlobalCoordinate&);
  void  operator()(const  StGlobalCoordinate& ,StSvtLocalCoordinate&, Int_t Id);
  void  setParamPointers( srs_srspar_st* srspar, svg_geom_st* geom, svg_shape_st* shape, StSvtConfig* config, StSvtHybridCollection* driftVeloc=NULL, StSvtT0* T0=NULL);
  void  setParamPointers( srs_srspar_st* srspar, svg_geom_st* geom, svg_shape_st* shape, StSvtConfig* config, StSvtHybridCollection* driftVeloc=NULL, StSvtHybridCollection* driftCurve=NULL, StSvtT0* T0=NULL);
  void  setParamPointers( StSvtGeometry* geom, StSvtConfig* config, StSvtHybridCollection* driftVeloc=NULL, StSvtT0* T0=NULL);
  void  setParamPointers( StSvtGeometry* geom, StSvtConfig* config, StSvtHybridCollection* driftVeloc=NULL, StSvtHybridCollection* driftCurve=NULL, StSvtT0* T0=NULL);
  void  setParamPointers( StSvtGeometry* geom, StSvtConfig* config, StSvtHybridCollection* driftVeloc, StSvtHybridCollection* driftCurve, StSvtT0* T0, St_svtCorrectionC*  driftVelCorr);
  void setVelocityScale( double deltaV);
  int  LocaltoGlobal(const StSvtLocalCoordinate&,   StThreeVector<double>& x, int Index);
  int  GlobaltoLocal(const StThreeVector<double>& x , StSvtLocalCoordinate&, int HardWarePos, int Index );
  int IsOnWaferZ( const StThreeVector<double>& x, int HardWarePos);
  int IsOnWaferR(const StThreeVector<double>& x, int HardWarePos);
  void setDriftVelocity();

private:
  
  UInt_t       mFlag;
  StSvtConfig *mconfig;
  StSvtGeometry* mgeom;
  StSvtHybridCollection* mDriftVelocity;
  double mDeltaDriftVelocity;
  StSvtHybridCollection* mDriftCurve;
  StSvtT0* mT0;
  TF1* mPoly9;
  St_svtCorrectionC*  mdriftVelCorr;
};

#endif
 
