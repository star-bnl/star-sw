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

// returns a vector (with z==0) pointing from the origin to  the center of the given padrow or ladder.
StThreeVector<double> StiGeometryTransform::centerForTpcPadrow(int sector, int padrow)
{
    double radius = gStTpcDb->PadPlaneGeometry()->radialDistanceAtRow(padrow);
    double phi = phiForSector(sector, 12);
    
    return StThreeVector<double>(radius*cos(phi), radius*sin(phi), 0.);
}

// this uses the database convention of 6 svt layers + 1 ssd layer.
// Every other svt ladder is thus bogus (1,3,5,7 on layer 1, eg), but the xform  works regardless
StThreeVector<double> StiGeometryTransform::centerForSvgLadder(int layer, int ladder)
{
    double radius = svgConfig.layer_radius[layer - 1];
    int nLadders = 2*svgConfig.n_ladder[layer - 1];
    double phi = phiForWestSector(ladder, nLadders);
    
    return StThreeVector<double>(radius*cos(phi), radius*sin(phi), 0.);
}
    
int StiGeometryTransform::sectorForTpcCoords(const StThreeVector<double> &vec)
{
    return tpcTransform->sectorFromCoordinate(vec);
}

int StiGeometryTransform::padrowForTpcCoords(const StThreeVector<double> &vec)
{
    int sector = sectorForTpcCoords(vec);
    return tpcTransform->rowFromLocal( tpcTransform->rotateToLocal(vec, sector));
}

// finds nearest real ladder (as above, could be bogus [electronics instead  of wafers]) and layer.
int StiGeometryTransform::layerForSvgCoords(const StThreeVector<double> &vec)
{
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

int StiGeometryTransform::ladderForSvgCoords(const StThreeVector<double> &vec)
{
    int layer = layerForSvgCoords(vec);
    int nLadders = svgConfig.n_ladder[layer - 1];    
    return westSectorForPhi(vec.phi(), nLadders);
}
    
double StiGeometryTransform::phiForSector(int iSector, int nSectors)
{
    double phi;
    if(iSector>nSectors){
	phi = phiForEastSector(iSector, nSectors);
    }
    else {
	phi = phiForWestSector(iSector, nSectors);
    }
    if (phi<0.) {
	phi+=2.*M_PI;
    }
    return phi;
}

