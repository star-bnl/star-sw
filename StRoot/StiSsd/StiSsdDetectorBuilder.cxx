/*!
 * \class StiSsdDetectorBuilder
 * \author Christelle Roy
 * \date 02/27/04
*/

#include <stdio.h>
#include <stdexcept>

//Sti Includes
#include "Sti/Base/Messenger.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiPlanarShape.h"
#include "Sti/StiCylindricalShape.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetector.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiHitErrorCalculator.h"
#include "Sti/StiIsActiveFunctor.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "StiSsd/StiSsdIsActiveFunctor.h" 
#include "StiSsd/StiSsdDetectorBuilder.h" 
#include "StSsdUtil/StSsdConfig.hh"
#include "StSsdUtil/StSsdGeometry.hh"
#include "StSsdUtil/StSsdWaferGeometry.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StDbUtilities/StSsdLocalCoordinate.hh"
#include "StDbUtilities/StSsdCoordinateTransform.hh"
#include "StSsdDbMaker/StSsdDbMaker.h"
#include "StSsdDbMaker/St_SsdDb_Reader.hh"
//#include "tables/St_HitError_Table.h"
#include "tables/St_svg_geom_Table.h"

StiSsdDetectorBuilder::StiSsdDetectorBuilder(bool active, const string & inputFile)
  : StiDetectorBuilder("Ssd",active,inputFile)
{
  // Hit error parameters : it is set to 20 microns, in both x and y coordinates 
  _trackingParameters.setName("SsdTrackingParameters");
  _hitCalculator.setName("SsdHitErrors");
  _hitCalculator.set(0.002, 0., 0., 0.002, 0., 0.);
}

StiSsdDetectorBuilder::~StiSsdDetectorBuilder()
{} 


void StiSsdDetectorBuilder::buildDetectors(StMaker & source)
{
  char name[50];  
  int nRows;
  cout << "StiSsdDetectorBuilder::buildDetectors() - I - Started "<<endl;
  load(_inputFile, source);

  St_DataSet *dataSet = NULL;
  dataSet = source.GetDataSet("StSsdConfig");
  if (!dataSet)	throw runtime_error("StiSsdDetectorBuilder::loadDb() -E- dataSet==0 while getting StSsdConfig");
  _config = static_cast<StSsdConfig*>(dataSet->GetObject());
  if (!_config) throw runtime_error("StiSsdDetectorBuilder::loadDb() -E- _config==0");
  
  dataSet = source.GetDataSet("StSsdGeometry");
  if (!dataSet)	throw runtime_error("StiSsdDetectorBuilder::loadDb() -E- dataSet==0 while getting StSsdGeometry");
  _geometry = static_cast<StSsdGeometry*>(dataSet->GetObject());
  if (!_geometry) throw runtime_error("StiSsdDetectorBuilder::loadDb() -E- _geometry==0");
  nRows = _config->getNumberOfBarrels();
  setNRows(nRows);
  cout << "SSD : #Barrel : "<<_config->getNumberOfBarrels()<<endl
       << "  #Ladders      Radius" << endl;
  cout << "      "<<_config->getNumberOfLadders(1) << "   " 
       << _geometry->getBarrelRadius(1) << endl;

  cout << " StiSsdDetectorBuilder::buildMaterials() - I - Started "<<endl;
  /*! buildMaterials : _gasMat is the gas the SSD lives in 
                       _siMat corresponds to Silicon. It remains to be adjust to take into account 
		       all SSD materials (average).
  */
  _gasMat    = add(new StiMaterial("SsdAir", 0.49919,  1., 0.001205, 30420.*0.001205, 5.));
  _siMat     = add(new StiMaterial("SsdSi",14., 28.0855, 2.33, 21.82, 5.));
  cout << " StiSsdDetectorBuilder::buildMaterials() - I - Done "<<endl;
  
  cout << "StiSsdDetectorBuilder::buildShapes() - I - Started" << endl;
  /*! buildShape : SSD has been defined as a PlanarShape 
   */
  unsigned int layer = 0;
  int nWafers = _config->getNumberOfWafers(_config->getNumberOfBarrels());


  _waferShape[layer] = new StiPlanarShape(name,
					  nWafers*_geometry->getWaferLength(),
					  2.*_geometry->getWaferThickness(),
					  _geometry->getWaferWidth() );
  cout <<"SSD : SSD-Wafer Dimensions Length/Thickness/Width = "<< _geometry->getWaferLength() 
	    <<"/"<<_geometry->getWaferThickness()<<"/"<<_geometry->getWaferWidth()<<endl;

  add(_waferShape[layer]);
  cout << "StiSsdDetectorBuilder::buildShapes() - I - Done" << endl;
  
  /*_______________________________________*/
  if (!gStSsdDbMaker)
    throw runtime_error("StiSsdDetectorBuilder::loadDb() - ERROR - gStSsdDbMaker==0");
  St_SsdDb_Reader *pSsdDb_Reader = gStSsdDbMaker->get_SsdDb_Reader();
  if (!pSsdDb_Reader)
    throw runtime_error("StiSsdDetectorBuilder::loadDb() - ERROR - pSsdDb_Reader==0");
  printf("StiSsdDetectorBuilder : Trying to load geometry...\n");
  St_svg_geom* SsdGeom = pSsdDb_Reader->getSvgGeom() ;
  _geometry = pSsdDb_Reader->getGeometry(SsdGeom);
  if (_geometry) 
    printf("StiSsdDetectorBuilder : geometry loaded...\n");
  else
    throw runtime_error("StiSsdDetectorBuilder::loadDb() - ERROR - _geometry==0");
  /*_______________________________________*/


  

  //  StSsdWaferGeometry* waferGeom;
  svg_geom_st *geom = SsdGeom->GetTable();
  
  float fLadderRadius  = _geometry->getBarrelRadius(layer+1);
  float fLayerRadius = fLadderRadius;
  if (fLadderRadius<=0)
    throw runtime_error("StiSsdDetectorBuilder::buildDetectors() - FATAL - fLadderRadius<=0");

  double x,y,z,rc,rn, nx,ny,nz,dx,dy,dz,yOff;
  float fLadderPhi;
  float phiC, phiN, dPhi;
  int firstWaferPerLadderIndex = 216 ;
  int nSectors = _config->getNumberOfLadders(1);
  setNSectors(layer,nSectors); 
  

  /*! Placement of Ssd Modules is currently done by reading the geom.C table. 
    Ladders are placed according to the coordinates of its first module number 	  
    int idwafer = 7*1000+wafer*100+ladder;      	
    ----> ladder # 1  ===> module 7101 
    ----> ladder # 20 ===> module 7120
  */

  /*! ladder_present[20] has two sets of parameters (Ladder off/on = 0/1) 
    All set to 1 correspond to Year2a-Siumlation or Future-Year2005
    Presence of 0 parameters indicate Y2004 setup
   */
    int ladder_present[20]={1,1,1,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,1,1} ;
  // int ladder_present[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1} ;


  for(unsigned int ladder = 0; ladder < nSectors; ladder++)
    {
      if (ladder_present[ladder]==1) 
	{
	  firstWaferPerLadderIndex = 216 + ladder*_config->getNumberOfWafers(1);	  
	  x = geom[firstWaferPerLadderIndex].x[0];
	  y = geom[firstWaferPerLadderIndex].x[1];
	  z = geom[firstWaferPerLadderIndex].x[2];
	  nx = geom[firstWaferPerLadderIndex].n[0];
	  ny = geom[firstWaferPerLadderIndex].n[1];
	  nz = geom[firstWaferPerLadderIndex].n[2];
	  dx = geom[firstWaferPerLadderIndex].d[0];
	  dy = geom[firstWaferPerLadderIndex].d[1];
	  dz = geom[firstWaferPerLadderIndex].d[2];
	  cout<<"___________________________________________________________________________________"<<endl;
	  cout <<" ladder/index = "<<ladder<<"/"<<firstWaferPerLadderIndex<<" Det Nber = "<<geom[firstWaferPerLadderIndex].id<<" x:"<<x<<" y:"<<y<<" z:"<<z<<endl;
	  rc = sqrt(x*x+y*y);
	  rn = x*nx+y*ny;
	  dPhi = acos((x*nx+y*ny)/rc);
	  phiC = fLadderPhi = atan2(y,x);
	  phiN = atan2(ny,nx);
	  dPhi = phiC-phiN;
	  yOff = sqrt(rc*rc-rn*rn);
	  //	  cout << "  phiN:"<<180*phiN/3.1415927<< " phiC:"<< 180*phiC/3.1415927<< " dPhi:"<<180*dPhi/3.1415927<<"  yOff:"<<yOff<<endl;
	  StiPlacement *pPlacement = new StiPlacement;
	  pPlacement->setZcenter(0.);
	  pPlacement->setLayerRadius(fLayerRadius);
	  pPlacement->setRegion(StiPlacement::kMidRapidity);
	  pPlacement->setCenterRep(phiForSsdLadder(ladder), fLayerRadius, -dPhi); 
	  sprintf(name, "Ssd/Layer_%d/Ladder_%d/Wafers", layer, ladder);
	  
	  StiDetector *pLadder = _detectorFactory->getInstance();
	  pLadder->setName(name);
	  pLadder->setIsOn(true);
	  pLadder->setIsActive(new StiIsActiveFunctor(_active));
	  pLadder->setIsContinuousMedium(true);
	  pLadder->setIsDiscreteScatterer(true);
	  pLadder->setGas(_gasMat);
	  pLadder->setMaterial(_siMat);
	  pLadder->setShape(_waferShape[layer]);
	  pLadder->setPlacement(pPlacement); 
	  pLadder->setHitErrorCalculator(&_hitCalculator);
	  add(layer,ladder,pLadder);
	}
    } // ladder
}

double StiSsdDetectorBuilder::phiForSsdLadder(unsigned int ladder) const
{
  double angle;
  switch (ladder)
    {
//  Case Year2004 
    
/*
  case 0:  angle =   90.0; break;  //ladder 1
  case 1:  angle =  108.3; break;  //ladder 2
  case 2:  angle =  130.0; break;  //ladder 3
  case 8:  angle =  230.0; break;  //ladder 9
  case 9:  angle =  251.7; break;  //ladder 10
  case 10: angle =  270.0; break;  //ladder 11
  case 11: angle =  288.3; break;  //ladder 12
  case 12: angle =  310.0; break;  //ladder 13
  case 18: angle =   50.0; break;  //ladder 19
  case 19: angle =   71.7; break;  //ladder 20
*/

//  Case Future-Year2005 (Ladder 0 à Phi = 90 deg)
  
    case 0:  angle =   90.0; break; //ladder 1
    case 1:  angle =  108.3; break; //ladder 2
    case 2:  angle =  126.6; break; //ladder 3
    case 3:  angle =  144.4; break; //ladder 4
    case 4:  angle =  162.2; break; //ladder 5
    case 5:  angle =  180.0; break; //ladder 6
    case 6:  angle =  197.8; break; //ladder 7
    case 7:  angle =  215.6; break; //ladder 8
    case 8:  angle =  233.4; break; //ladder 9
    case 9:  angle =  251.7; break; //ladder 10
    case 10: angle =  270.0; break; //ladder 11
    case 11: angle =  288.3; break; //ladder 12
    case 12: angle =  306.6; break; //ladder 13
    case 13: angle =  324.4; break; //ladder 14
    case 14: angle =  342.2; break; //ladder 15
    case 15: angle =    0.0; break; //ladder 16
    case 16: angle =   17.8; break; //ladder 17
    case 17: angle =   35.6; break; //ladder 18
    case 18: angle =   53.4; break; //ladder 19
    case 19: angle =   71.7; break; //ladder 20

    default: throw runtime_error("StiSsdDetectorBuilder::phiForSsdLadder() -E- Arg out of bound");
    };
  //  cout <<" Ladder = "<<ladder<<" a Phi (deg/rad) ="<< angle << "/" <<angle*M_PI/180.<<endl;
  return  angle*M_PI/180.;
} // phiForSsdLadder


// double StiSsdDetectorBuilder::radiusForSsdLadder(unsigned int ladder) const
// {
//   double radius;
//   switch (ladder)
//     {
    
//     case 0:  radius =  23.174; break; //ladder 1
//     case 1:  radius =  22.8; break; //ladder 2
//     case 2:  radius =  22.46; break; //ladder 3
//     case 3:  radius =  30.; break; //ladder 4
//     case 4:  radius =  30.; break; //ladder 5
//     case 5:  radius =  30.; break; //ladder 6
//     case 6:  radius =  30.; break; //ladder 7
//     case 7:  radius =  30.; break; //ladder 8
//     case 8:  radius =  24.6; break; //ladder 9
//     case 9:  radius =  22.8; break; //ladder 10
//     case 10: radius =  23.174; break; //ladder 11
//     case 11: radius =  22.8; break; //ladder 12
//     case 12: radius =  24.6; break; //ladder 13
//     case 13: radius =  30.; break; //ladder 14
//     case 14: radius =  30.; break; //ladder 15
//     case 15: radius =  30.; break; //ladder 16
//     case 16: radius =  30.; break; //ladder 17
//     case 17: radius =  30.; break; //ladder 18
//     case 18: radius =  24.6; break; //ladder 19
//     case 19: radius =  22.8; break; //ladder 20
//     default: throw runtime_error("StiSsdDetectorBuilder::radiusForSsdLadder() -E- Arg out of bound");
//     };
//   return  radius;
// } 

void StiSsdDetectorBuilder::setDefaults()
{
  cout << "StiSsdDetectorBuilder::setDefaults() -I- Started" << endl;
  _trackingParameters.setMaxChi2ForSelection(5.);
  _trackingParameters.setMinSearchWindow(1.);
  _trackingParameters.setMaxSearchWindow(4.);
  _trackingParameters.setSearchWindowScaling(10.);
  _hitCalculator.set(0.002, 0., 0., 0.002, 0., 0.); //!< Hit error parameters set to 20 microns in x and y directions
  cout << _trackingParameters << endl;
  cout << _hitCalculator <<endl;
  cout << "StiSsdDetectorBuilder::setDefaults() -I- Done" << endl;
}
