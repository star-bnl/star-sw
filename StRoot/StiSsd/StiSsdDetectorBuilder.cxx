/*!
 * \class StiSsdDetectorBuilder
 * \author Christelle Roy
 * \date 02/27/04
*/

#include <stdio.h>
#include <stdexcept>
#include "StMessMgr.h"
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
#include "StSsdDbMaker/StSsdDbMaker.h"
#include "StSsdDbMaker/St_SsdDb_Reader.hh"

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
  int nRows = 1 ;
  gMessMgr->Info() << "StiSsdDetectorBuilder::buildDetectors() - I - Started "<<endm;
  load(_inputFile, source);

  setNRows(nRows);

  /*_______________________________________*/
  if (!gStSsdDbMaker)
    throw runtime_error("StiSsdDetectorBuilder::loadDb() - ERROR - gStSsdDbMaker==0");
  St_SsdDb_Reader *pSsdDb_Reader = gStSsdDbMaker->get_SsdDb_Reader();
  if (!pSsdDb_Reader)
    throw runtime_error("StiSsdDetectorBuilder::loadDb() - ERROR - pSsdDb_Reader==0");
  /*_______________________________________*/
  //  St_ssdWafersPosition*  wafersPosition = pSsdDb_Reader->getWafersPosition() ;
  ssdWafersPosition_st*  wafersPosition = pSsdDb_Reader->getWafersPosition() ;
  _geometry = pSsdDb_Reader->getGeometry(wafersPosition);
  if (_geometry) 
    gMessMgr->Info()<< "StiSsdDetectorBuilder : SSD Geometry loaded..."<<endm;
  else
    throw runtime_error("StiSsdDetectorBuilder::loadDb() - ERROR - _geometry==0");
  /*_______________________________________*/
  //  St_ssdConfiguration*  ssdConfiguration = pSsdDb_Reader->getSsdConfiguration() ;
  ssdConfiguration_st*  ssdConfiguration = pSsdDb_Reader->getSsdConfiguration() ;
  _config = pSsdDb_Reader->getConfiguration(ssdConfiguration);
  if (_config) 
    gMessMgr->Info() <<"StiSsdDetectorBuilder : SSD Configuration loaded..." << endm;
  else
    throw runtime_error("StiSsdDetectorBuilder::loadDb() - ERROR - _config==0");
  /*_______________________________________*/

  //  St_ssdDimensions*  ssdDimensions = pSsdDb_Reader->getSsdDimensions() ;
  ssdDimensions_st*  ssdDimensions = pSsdDb_Reader->getSsdDimensions() ;
  _dimensions = pSsdDb_Reader->getDimensions(ssdDimensions);
  if (_dimensions) 
    gMessMgr->Info() <<"StiSsdDetectorBuilder : SSD Dimensions set..." << endm;
  else
    throw runtime_error("StiSsdDetectorBuilder::loadDb() - ERROR - _dimensions==0");
  /*_______________________________________*/
  
  
  /*! buildMaterials : _gasMat is the gas the SSD lives in 
                       _siMat corresponds to Silicon Wafers. 
		       _hybridMat corresponds to Hybrids.
		       all SSD materials (average).
  */
  gMessMgr->Info() << "StiSsdDetectorBuilder::buildMaterials() - I - Started "<<endm;
  _gasMat     = add(new StiMaterial("SsdAir", 0.49919,  1., 0.001205, 30420.*0.001205, 5.));
  _siMat      = add(new StiMaterial("SsdSi",14., 28.0855, 2.33, 21.82, 5.));
  _hybridMat  = add(new StiMaterial("SsdHyb",14., 28.0855, 2.33, 21.82, 5.));
  gMessMgr->Info() << "StiSsdDetectorBuilder::buildMaterials() - I - Done "<<endm;  

  /*! buildShape : SSD has been defined as a PlanarShape 
    Hybrids are defined as a PlanarShape, positionned on the Silicon Wafers; the thickness of 
    the hybrids corresponds to 0.75 % of X0. 
   */
  unsigned int layer = 0;
  int nWafers;
  gMessMgr->Info() << "StiSsdDetectorBuilder::buildShapes() - I - Started" << endm;

  nWafers = _config->getNumberOfWafers();
  _waferShape[layer] = new StiPlanarShape(name,
   					  nWafers*_dimensions->getWaferWidth(),
					  _dimensions->getWaferThickness(),
					  _dimensions->getWaferLength() );
  gMessMgr->Info() <<"SSD : Wafer dimensions = "<< _dimensions->getWaferLength() 
		   <<"/"<<_dimensions->getWaferThickness()<<"/"<<_dimensions->getWaferWidth()<<endm;
  add(_waferShape[layer]);

  _hybridShape[layer] = new StiPlanarShape(name,nWafers*_dimensions->getWaferWidth(),
					  0.07,_dimensions->getWaferLength() );
  add(_hybridShape[layer]);  

  gMessMgr->Info() << "StiSsdDetectorBuilder::buildShapes() - I - Done" << endm;



  double x,y,z,rc,rn, nx,ny,nz,dx,dy,dz,yOff;
  float fLadderPhi;
  float phiC, phiN, dPhi;
  setNSectors(layer,_config->getTotalNumberOfLadders()); 

  /*! Placement of Ssd Modules is currently done by reading the geom.C table. 
    Ladders are placed according to the coordinates of its first module number 	  
    int idwafer = 7*1000+wafer*100+ladder;      	
    ----> ladder # 1  ===> module 7101 
    ----> ladder # 20 ===> module 7120
  */

  StSsdWaferGeometry* waferGeom;
  for(unsigned int ladder = 0; ladder < _config->getTotalNumberOfLadders(); ladder++)
    {
      if (_config->getLadderIsActive(ladder+1) != 0)
	{
	  cout<<" Ladder = "<<ladder<<" is active"<<endl;
	  for (unsigned int firstWaferPerLadderIndex = 0; firstWaferPerLadderIndex < 160;firstWaferPerLadderIndex++ )
	    {
	      waferGeom = (StSsdWaferGeometry*) _geometry->at(firstWaferPerLadderIndex);
	      if(waferGeom->getID() == (7100 + (ladder+1)))
		{
		  cout<<" Wafer Geom Id = "<<waferGeom->getID()<<" pour ladder "<<ladder +1 <<endl;
		  x  = waferGeom->x(0);
		  y  = waferGeom->x(1);
		  z  = waferGeom->x(2);
		  nx = waferGeom->n(0);
		  ny = waferGeom->n(1);
		  nz = waferGeom->n(2);
		  dx = waferGeom->d(0);
		  dy = waferGeom->d(1);
		  dz = waferGeom->d(2);
		  cout <<" ladder/index = "<<ladder<<"/"<<firstWaferPerLadderIndex<<" Det Nber = "<<waferGeom->getID()<<" x:"<<x<<" y:"<<y<<" z:"<<z<<endl;
		  rc = sqrt(x*x+y*y);
		  rn = x*nx+y*ny;
		  dPhi = acos((x*nx+y*ny)/rc);
		  phiC = fLadderPhi = atan2(y,x);
		  phiN = atan2(ny,nx);
		  dPhi = phiC-phiN;
		  yOff = sqrt(rc*rc-rn*rn);

		  StiPlacement *pPlacement = new StiPlacement;
		  pPlacement->setZcenter(0.);
		  pPlacement->setLayerRadius(radiusForSsdLadder(ladder));
		  pPlacement->setRegion(StiPlacement::kMidRapidity);
		  pPlacement->setCenterRep(phiForSsdLadder(ladder), radiusForSsdLadder(ladder), -dPhi); 
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

		  StiPlacement *phybPlacement = new StiPlacement;
		  phybPlacement->setZcenter(0.);
		  phybPlacement->setLayerRadius(radiusForSsdLadder(ladder)+0.15);
		  phybPlacement->setRegion(StiPlacement::kMidRapidity);

		  StiDetector *pHybrid = _detectorFactory->getInstance();
		  pHybrid->setName(name);
		  pHybrid->setIsOn(true);
		  pHybrid->setIsActive(new StiNeverActiveFunctor);
		  pHybrid->setIsContinuousMedium(true);
		  pHybrid->setIsDiscreteScatterer(true);
		  pHybrid->setGas(_gasMat);
		  pHybrid->setMaterial(_hybridMat);
		  pHybrid->setShape(_hybridShape[layer]);
		  pHybrid->setPlacement(phybPlacement); 
		  add(pHybrid);



		}
	    }
	}
    }
}

float StiSsdDetectorBuilder::phiForSsdLadder(unsigned int ladder) const
{
  float angle;
  switch (ladder)
    {
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
  return  angle*M_PI/180.;
} 


float StiSsdDetectorBuilder::radiusForSsdLadder(unsigned int ladder) const
{
  float radius;
  switch (ladder)
    {
    case 0:  radius =  23.174; break; //ladder 1
    case 1:  radius =  22.8;   break; //ladder 2
    case 2:  radius =  22.46;  break; //ladder 3
    case 3:  radius =  30.;    break; //ladder 4
    case 4:  radius =  30.;    break; //ladder 5
    case 5:  radius =  30.;    break; //ladder 6
    case 6:  radius =  30.;    break; //ladder 7
    case 7:  radius =  30.;    break; //ladder 8
    case 8:  radius =  24.6;   break; //ladder 9
    case 9:  radius =  22.8;   break; //ladder 10
    case 10: radius =  23.174; break; //ladder 11
    case 11: radius =  22.8;   break; //ladder 12
    case 12: radius =  24.6;   break; //ladder 13
    case 13: radius =  30.;    break; //ladder 14
    case 14: radius =  30.;    break; //ladder 15
    case 15: radius =  30.;    break; //ladder 16
    case 16: radius =  30.;    break; //ladder 17
    case 17: radius =  30.;    break; //ladder 18
    case 18: radius =  24.6;   break; //ladder 19
    case 19: radius =  22.8;   break; //ladder 20
    default: throw runtime_error("StiSsdDetectorBuilder::radiusForSsdLadder() -E- Arg out of bound");
    };
  return  radius;
} 

void StiSsdDetectorBuilder::setDefaults()
{
  cout << "StiSsdDetectorBuilder::setDefaults() -I- Started" << endl;
  _trackingParameters.setMaxChi2ForSelection(5.);
  _trackingParameters.setMinSearchWindow(1.);
  _trackingParameters.setMaxSearchWindow(4.);
  _trackingParameters.setSearchWindowScaling(10.);
  _hitCalculator.set(0.002, 0., 0., 0.002, 0., 0.);  //!< Hit error parameters set to 20 um in x&y directions
  cout << _trackingParameters << endl;
  cout << _hitCalculator <<endl;
  cout << "StiSsdDetectorBuilder::setDefaults() -I- Done" << endl;
}
