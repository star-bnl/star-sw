//  17 may 01
//  ben norman

#include <math.h>
#include "StThreeVector.hh"

#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StTpcDb/StTpcDb.h"
#include "tables/St_svg_geom_Table.h"
#include "tables/St_svg_config_Table.h"
#include "tables/St_svg_shape_Table.h"

#include "StiMaker/StiMaker.h"

#include "StiGeometryTransform.h"

StiGeometryTransform::StiGeometryTransform(){

  // read in svt geometry tables
  St_DataSetIter local(StiMaker::instance()->GetInputDB("svt"));

  svgConfig = 
      dynamic_cast<St_svg_config *>(local("svgpars/config"))->GetTable()[0];  
  aSvgGeom = dynamic_cast<St_svg_geom *>(local("svgpars/geom"))->GetTable();
  aSvgShape = dynamic_cast<St_svg_shape *>(local("svgpars/shape"))->GetTable();

  // instantiate TPC coord x-form
  tpcTransform = new StTpcCoordinateTransform(gStTpcDb);

} // StiGeometryTransform()

StiGeometryTransform::~StiGeometryTransform(){
} // ~StiGeometryTransform

// returns the reference angle for the given sector number (out of the 
// given total).  This assumes the star convention where the highest
// numbered sector is at "12 o'clock", or pi/2, and the sector numbering
// _decreases_ with increasing phi.  [I guess this must have seemed like
// a good idea at the time....]
double StiGeometryTransform::phiForWestSector(int iSector, int nSectors){

  int offset = nSectors/4;
  int minSector = -nSectors/2 + 1;
  double deltaPhi = 2.*M_PI/nSectors;

  // make phi ~ sector (not -sector) and correct offset
  iSector = offset - iSector;
  if(iSector<minSector){ iSector += nSectors; }

  return iSector*deltaPhi;

} // phiForWestSector

// as above, but numbering _increases_ with increasing phi.
double StiGeometryTransform::phiForEastSector(int iSector, int nSectors){

  int offset = nSectors/4;
  int minSector = -nSectors/2 + 1;
  double deltaPhi = 2.*M_PI/nSectors;

  // correct offset
  iSector = iSector - (2*nSectors - offset);
  if(iSector<minSector){ iSector += nSectors; }

  return iSector*deltaPhi;

} // phiForEastSector

int StiGeometryTransform::westSectorForPhi(double phi, int nSectors){

  int offset = nSectors/4;
  double deltaPhi = 2.*M_PI/nSectors;  

  int iSector = 0;
  while(phi > deltaPhi/2.){ phi -= deltaPhi; iSector++; }
  while(phi < deltaPhi/2.){ phi += deltaPhi; iSector--; }

  iSector = offset - iSector;
  if(iSector<1){ iSector += nSectors; }

  return iSector;

} // westSectorForPhi

int StiGeometryTransform::eastSectorForPhi(double phi, int nSectors){

  int offset = nSectors/4;
  double deltaPhi = 2.*M_PI/nSectors;  

  int iSector = 0;
  while(phi > deltaPhi/2.){ phi -= deltaPhi; iSector++; }
  while(phi < deltaPhi/2.){ phi += deltaPhi; iSector--; }

  iSector = iSector + (2*nSectors + offset);
  if(iSector>2*nSectors){ iSector -= nSectors; }

  return iSector;

} // eastSectorForPhi

