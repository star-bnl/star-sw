//  17 may 01
//  ben norman

//Std
#include <math.h>

//SCL
#include "StThreeVector.hh"

//StDb
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StTpcDb/StTpcDb.h"

//Svt Tables
#include "tables/St_svg_geom_Table.h"
#include "tables/St_svg_config_Table.h"
#include "tables/St_svg_shape_Table.h"

//StEvent
#include "StEventTypes.h"

//StiMaker
#include "StiMaker/StiMaker.h"

//Sti
#include "StiHit.h"
#include "StiGeometryTransform.h"

StiGeometryTransform* StiGeometryTransform::sinstance = 0;

double gRefAnleForSector(unsigned int sector);

StiGeometryTransform::StiGeometryTransform()
{
    cout <<"StiGeometryTransform::StiGeometryTransform()"<<endl;

    // read in svt geometry tables
    //cout <<"Read in svt geometry tables: preparing to seg-fualt"<<endl;

    St_DataSetIter local(StiMaker::instance()->GetInputDB("svt"));
    //cout <<"Instantiated local"<<endl;

    svgConfig = 
	dynamic_cast<St_svg_config *>(local("svgpars/config"))->GetTable()[0];
    //cout <<"Instantiated svgConfig"<<endl;

    aSvgGeom = dynamic_cast<St_svg_geom *>(local("svgpars/geom"))->GetTable();
    //cout <<"Instantiated aSvgGeom"<<endl;

    aSvgShape = dynamic_cast<St_svg_shape *>(local("svgpars/shape"))->GetTable();
    //cout <<"Instantiated aSvgShape"<<endl;
    
    // instantiate TPC coord x-form
    //cout <<"We didn't seg-fault!!!!"<<endl;
    //cout <<"instantiate TPC coord x-form"<<endl;
    tpcTransform = new StTpcCoordinateTransform(gStTpcDb);

    //cout <<"Generating Padrow Radius Map"<<endl;
    for (unsigned int padrow=1; padrow<=45; ++padrow) {
	
	//Replace this call!!!!
	double center = gStTpcDb->PadPlaneGeometry()->radialDistanceAtRow(padrow);
	//!!!!!!
	
	mpadrowradiusmap.insert( padrow_radius_map_ValType( padrow, center ) );	
    }
    cout <<"\nPadrow\tRadius"<<endl;
    for (padrow_radius_map::const_iterator it=mpadrowradiusmap.begin(); it!=mpadrowradiusmap.end(); ++it) {
	cout <<(*it).first<<"\t"<<(*it).second<<endl;
    }

    sinstance = this;
    //cout <<"\tLeaving StiGeometryTransform::StiGeometryTransform()"<<endl;
} // StiGeometryTransform()

StiGeometryTransform::~StiGeometryTransform()
{
    cout <<"StiGeometryTransform::~StiGeometryTransform()"<<endl;
    delete tpcTransform;
    tpcTransform = 0;
} // ~StiGeometryTransform

StiGeometryTransform* StiGeometryTransform::instance()
{
    return (sinstance) ? sinstance : new StiGeometryTransform();
}

void StiGeometryTransform::kill()
{
    if (sinstance) {
	delete sinstance;
	sinstance = 0;
    }
}

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

//Hit Translation routines

void StiGeometryTransform::operator() (const StPrimaryVertex* vtx, StiHit* stihit)
{
    //A primary vertex doesn't come from a detector, so it doesn't have a well defined refAngle and centerRadius
    //We'll define these two from global position in cylindrical coordinates
    //refAngle = arctan(global_y / global_x)
    //centerRadius = sqrt (global_x^2 + global_y^2)
    //We'll then say that Sti_x = centerRadius and Sti_y = 0, with Sti_z begin global z, as usual

    const StThreeVectorF& position = vtx->position();
    double pos = sqrt(position.x()*position.x() + position.y()*position.y() );
    double refangle = atan2( position.y(), position.x() );

    if (refangle<0.) refangle+=2.*M_PI;
    
    stihit->setRefangle( refangle );
    stihit->setPosition( pos );
    stihit->setX( pos );
    stihit->setY( 0. );
    stihit->setZ( position.z() );
    
    return;
}

void StiGeometryTransform::operator() (const StTpcHit* tpchit, StiHit* stihit)
{
    //Change if we change numbering scheme
    double refangle = gRefAnleForSector( tpchit->sector() );
    double pos = mpadrowradiusmap[ tpchit->padrow() ];
    stihit->setRefangle( refangle );
    stihit->setPosition( pos );

    //Make Tpc hits
    StGlobalCoordinate gHit( tpchit->position() );
    StTpcLocalSectorCoordinate lsHit;

    //Transform 
    tpcTransform->operator()(gHit, lsHit);

    //Careful, we have to swap z for all hits, and x and y for hits with global z>0

    //Keep z in global coordinates
    stihit->setZ( tpchit->position().z() );

    //Take x -> -x, then swap x for y
    if (tpchit->position().z() >0) {
	stihit->setX( lsHit.position().y() );
	stihit->setY( -1.*lsHit.position().x() );
    }

    //Swap x for y, 
    else {
	stihit->setX( lsHit.position().y() );
	stihit->setY( lsHit.position().x() );
    }

    /*
      cout <<"TpcHit: "<<tpchit->sector()<<" "<<tpchit->padrow()<<" "<<tpchit->position();
      cout <<" GlobHit: "<<gHit.position();
      cout <<" LSHit: "<<lsHit.position()<<" From Sector: "<<lsHit.fromSector()<<" ";
      cout <<" StiHit: "<<stihit->x()<<" "<<stihit->y()<<" "<<stihit->z()<<endl;
    */
    return;
}

double gRefAnleForSector(unsigned int sector)
{
    unsigned int numSectors = 24;
    double tolerance = .00001;
    double beta = (sector > 12) ?(numSectors-sector)*2.*M_PI/(static_cast<double>(numSectors)/2.) :
	sector*2.*M_PI/(static_cast<double>(numSectors)/2.);
    return ( fabs(2.*M_PI - beta) < tolerance ) ? fabs(2.*M_PI-beta) : beta;
}
