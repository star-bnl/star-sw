/*!
 * \class StiSsdDetectorBuilder
 * \author Christelle Roy
 * \date 02/27/04
 */

#include <stdio.h>
#include <map>
using namespace std;
#include <stdexcept>
#include "StMessMgr.h"
#include "StThreeVectorD.hh"

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
  return;
    cout <<" -1-"<<endl;
    char name[50];  
    int nRows = 1 ;
    //gMessMgr->Info() << "StiSsdDetectorBuilder::buildDetectors() - I - Started "<<endm;
    //load(_inputFile, source);

    setNRows(nRows);
    cout <<" -2-"<<endl;

    //assert(gStSsdDbMaker);
    if (!gStSsdDbMaker) {
	throw runtime_error("StiSsdDetectorBuilder::loadDb() - ERROR - gStSsdDbMaker==0");
    }
      cout <<" -3-"<<endl;

    St_SsdDb_Reader *pSsdDb_Reader = gStSsdDbMaker->get_SsdDb_Reader();
    //assert(pSsdDb_Reader);
    if (!pSsdDb_Reader)
	throw runtime_error("StiSsdDetectorBuilder::loadDb() - ERROR - pSsdDb_Reader==0");
    cout <<" -4-"<<endl;

    //St_ssdWafersPosition*  wafersPosition = pSsdDb_Reader->getWafersPosition() ;
    ssdWafersPosition_st*  wafersPosition = pSsdDb_Reader->getWafersPosition() ;
    _geometry = pSsdDb_Reader->getGeometry(wafersPosition);
    //assert(pSsdDb_Reader);
    if (_geometry) 
	//gMessMgr->Info()<< "StiSsdDetectorBuilder : SSD Geometry loaded..."<<endm;
      cout<< "StiSsdDetectorBuilder : SSD Geometry loaded..."<<endl;
    else
	throw runtime_error("StiSsdDetectorBuilder::loadDb() - ERROR - _geometry==0");
      cout <<" -5-"<<endl;

    //St_ssdConfiguration*  ssdConfiguration = pSsdDb_Reader->getSsdConfiguration() ;
    ssdConfiguration_st*  ssdConfiguration = pSsdDb_Reader->getSsdConfiguration() ;
    _config = pSsdDb_Reader->getConfiguration(ssdConfiguration);
    //assert(_config);
    if (_config) 
	//gMessMgr->Info() <<"StiSsdDetectorBuilder : SSD Configuration loaded..." << endm;
      cout <<"StiSsdDetectorBuilder : SSD Configuration loaded..." << endl;
    else
	throw runtime_error("StiSsdDetectorBuilder::loadDb() - ERROR - _config==0");
    cout <<" -6-"<<endl;

  
    //St_ssdDimensions*  ssdDimensions = pSsdDb_Reader->getSsdDimensions() ;
    ssdDimensions_st*  ssdDimensions = pSsdDb_Reader->getSsdDimensions() ;
    _dimensions = pSsdDb_Reader->getDimensions(ssdDimensions);
    //assert(_dimensions);
    if (_dimensions) 
	//gMessMgr->Info() <<"StiSsdDetectorBuilder : SSD Dimensions set..." << endm;
      gMessMgr->Info() <<"StiSsdDetectorBuilder : SSD Dimensions set..." << endl;
    else
	throw runtime_error("StiSsdDetectorBuilder::loadDb() - ERROR - _dimensions==0");

      cout <<" -7-"<<endl;

  
    /*! buildMaterials : _gasMat is the gas the SSD lives in 
      _siMat corresponds to Silicon Wafers. 
      _hybridMat corresponds to Hybrids.
      all SSD materials (average).
    */
    //gMessMgr->Info() << "StiSsdDetectorBuilder::buildMaterials() - I - Started "<<endm;
    _gasMat     = add(new StiMaterial("SsdAir", 0.49919,  1., 0.001205, 30420.*0.001205, 5.));
    _siMat      = add(new StiMaterial("SsdSi",14., 28.0855, 2.33, 21.82, 5.));
    _hybridMat  = add(new StiMaterial("SsdHyb",14., 28.0855, 2.33, 21.82, 5.));
    //gMessMgr->Info() << "StiSsdDetectorBuilder::buildMaterials() - I - Done "<<endm;  
    cout << "StiSsdDetectorBuilder::buildMaterials() - I - Done "<<endl;  
    cout <<" -8-"<<endl;
    /*! buildShape : SSD has been defined as a PlanarShape 
      Hybrids are defined as a PlanarShape, positionned on the Silicon Wafers; the thickness of 
      the hybrids corresponds to 0.75 % of X0. 
    */
    unsigned int layer = 0;
    int nWafers;
    //gMessMgr->Info() << "StiSsdDetectorBuilder::buildShapes() - I - Started" << endm;
    cout  << "StiSsdDetectorBuilder::buildShapes() - I - Started" << endl;

    nWafers = _config->getNumberOfWafers();
    _waferShape[layer] = new StiPlanarShape(name,
					    //nWafers*_dimensions->getWaferWidth(), 
					    //nWafers*_dimensions->getWaferWidth(), //too big, it wants halfDepth, not depth
					    nWafers*_dimensions->getWaferWidth()/2., //too big, it wants halfDepth, not depth
					    _dimensions->getWaferThickness(),
					    //_dimensions->getWaferLength() );
					    _dimensions->getWaferLength()/2. ); //too big? MLM 9/1/04
    cout <<" -9-"<<endl;
    //gMessMgr->Info() <<"SSD : Wafer dimensions = "<< _dimensions->getWaferLength() 
    //	     <<"/"<<_dimensions->getWaferThickness()<<"/"<<_dimensions->getWaferWidth()<<endm;
    add(_waferShape[layer]);
    cout <<" -10-"<<endl;

    _hybridShape[layer] = new StiPlanarShape(name,nWafers*_dimensions->getWaferWidth(),
					     0.07,_dimensions->getWaferLength() );
    add(_hybridShape[layer]);  
    cout <<" -11-"<<endl;

    //gMessMgr->Info() << "StiSsdDetectorBuilder::buildShapes() - I - Done" << endm;


    setNSectors(layer,_config->getTotalNumberOfLadders()); 
    cout <<" -12-"<<endl;

    /*! Placement of Ssd Modules is currently done by reading the geom.C table. 
      Ladders are placed according to the coordinates of its first module number 	  
      int idwafer = 7*1000+wafer*100+ladder;      	
      ----> ladder # 1  ===> module 7101 
      ----> ladder # 20 ===> module 7120
    */

    //this is a crude hack by MLM, 8/31/04, but the ssd wafers are in place, so they're not going to change.
    //_IF_ the mapping changes, we're screwed from this line on...


    //ok, let's see what in the container, once and for all... (Brute force...)
    //typedef map<int,int> IntMap;
    //IntMap mymap;

    int ind=0;
    for (VecTObjIter it=_geometry->begin(); it!=_geometry->end(); ++it) {
	cout <<"look at index:\t"<<ind<<endl;
	
	StSsdWaferGeometry* waferGeom = dynamic_cast<StSsdWaferGeometry*>( *it );
	//assert(waferGeom);
	//mymap[waferGeom->getID()] = ind;
	++ind;
	cout <<" -13 recur-"<<endl;

	for (int ladder=1; ladder<=20; ++ladder) {	    
	    if(waferGeom->getID() == (7100 + (ladder))) { //this gives us the first wafer on the ladder
		//int wafer = waferGeom->getID();
		    cout <<" -14 rec-"<<endl;

		StThreeVectorD centerVector(waferGeom->x(0), waferGeom->x(1), waferGeom->x(2) );
		StThreeVectorD normalVector(waferGeom->n(0), waferGeom->n(1), waferGeom->n(2) );
	    
		double phi = centerVector.phi();
		double r = centerVector.perp();
		/* let's assume everything is non-tilted in the new design
		  double tilt = normalVector.phi() - phi;
		  
		  double psi = phi + tilt;
		  double dy = r*sin(psi-phi);
		  double rprime = sqrt(r*r - dy*dy);
		*/
	
		cout <<"Det Nber = "<<waferGeom->getID()<<"\tcv\t:"<<centerVector<<"\tphi:\t"<<phi<<"\tr:\t"<<r<<endl;
	
		StiPlacement *pPlacement = new StiPlacement;
    cout <<" -15-"<<endl;
		pPlacement->setZcenter(0.);
		//pPlacement->setLayerRadius(centerVector.perp()); //this is only used for ordering in detector container...
		pPlacement->setLayerRadius(r); //this is only used for ordering in detector container...
		pPlacement->setLayerAngle(phi); //this is only used for ordering in detector container...
		pPlacement->setRegion(StiPlacement::kMidRapidity);
		//pPlacement->setNormalRep(psi, rprime, dy); //this should work
		pPlacement->setNormalRep(phi, r, 0.);  //but we have to use this to fix ladders 20 and 12
		sprintf(name, "Ssd/Layer_%d/Ladder_%d/Wafers", layer, ladder);
	    cout <<" -16-"<<endl;

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
		cout <<" -17-"<<endl;
		add(layer,ladder-1,pLadder); 
	    cout <<" -18-"<<endl;

		/*StiDetector *pHybrid = _detectorFactory->getInstance();
		pHybrid->setName(name);
		pHybrid->setIsOn(true);
		pHybrid->setIsActive(new StiNeverActiveFunctor);
		pHybrid->setIsContinuousMedium(true);
		pHybrid->setIsDiscreteScatterer(true);
		pHybrid->setGas(_gasMat);
		pHybrid->setMaterial(_hybridMat);
		pHybrid->setShape(_hybridShape[layer]);
		pHybrid->setPlacement(pPlacement); 
		add(pHybrid);*/
	    }
	}
    }

    /*
      cout <<"\ncontents of map:"<<endl;
      for (IntMap::iterator it=mymap.begin(); it!=mymap.end();++it) {
	cout <<(*it).first<<"\t"<<(*it).second<<endl;
	}
    */
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
	case 1:  radius =  22.8;   break; //ladder 2.
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
