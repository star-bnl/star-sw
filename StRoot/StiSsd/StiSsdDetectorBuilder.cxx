// $Id: StiSsdDetectorBuilder.cxx,v 1.23 2006/06/28 18:51:46 fisyak Exp $
// 
// $Log: StiSsdDetectorBuilder.cxx,v $
// Revision 1.23  2006/06/28 18:51:46  fisyak
// Add loading of tracking and hit error parameters from DB
//
// Revision 1.22  2006/05/31 04:00:02  fisyak
// remove SSD ladder mother volume
//
// Revision 1.21  2005/06/21 16:35:01  lmartin
// DetectorBuilder updated with the correct methods from StSsdUtil
//
// Revision 1.20  2005/06/21 15:31:47  lmartin
// CVS tags added
//
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
#include "Sti/StiElossCalculator.h"
#include "StSsdUtil/StSsdConfig.hh"
#include "StSsdUtil/StSsdGeometry.hh"
#include "StSsdUtil/StSsdWaferGeometry.hh"
#include "StSsdDbMaker/StSsdDbMaker.h"
#include "StSsdDbMaker/St_SsdDb_Reader.hh"

StiSsdDetectorBuilder::StiSsdDetectorBuilder(bool active, const string & inputFile)
    : StiDetectorBuilder("Ssd",active,inputFile), _siMat(0), _hybridMat(0)
{
    // Hit error parameters : it is set to 20 microns, in both x and y coordinates 
    _trackingParameters.setName("ssdTrackingParameters");
    _hitCalculator.setName("ssdHitError");
    _hitCalculator.set(0.002, 0., 0., 0.002, 0., 0.);
}

StiSsdDetectorBuilder::~StiSsdDetectorBuilder()
{} 


void StiSsdDetectorBuilder::buildDetectors(StMaker & source)
{
#ifdef __SsdInChain__
    char name[50];  
    int nRows = 1 ;
    gMessMgr->Info() << "StiSsdDetectorBuilder::buildDetectors() - I - Started "<<endm;
    load(_inputFile, source);
    
    setNRows(nRows);
    if (StiVMCToolKit::GetVMC()) {useVMCGeometry();}

    if (!gStSsdDbMaker) {
	throw runtime_error("StiSsdDetectorBuilder::loadDb() - ERROR - gStSsdDbMaker==0");
    }
    St_SsdDb_Reader *pSsdDb_Reader = gStSsdDbMaker->get_SsdDb_Reader();

    if (!pSsdDb_Reader)
	throw runtime_error("StiSsdDetectorBuilder::loadDb() - ERROR - pSsdDb_Reader==0");

    _geometry = pSsdDb_Reader->getGeometry();

    if (_geometry) 
      gMessMgr->Info()<< "StiSsdDetectorBuilder : SSD Geometry loaded..."<<endm;
    else
	throw runtime_error("StiSsdDetectorBuilder::loadDb() - ERROR - _geometry==0");

    _config = _geometry->getSsdConfig();
    if (_config) 
      gMessMgr->Info() <<"StiSsdDetectorBuilder : SSD Configuration loaded..." << endm;
    else
	throw runtime_error("StiSsdDetectorBuilder::loadDb() - ERROR - _config==0");

    /*! buildMaterials : _gasMat is the gas the SSD lives in 
      _siMat corresponds to Silicon Wafers. 
      _hybridMat corresponds to Hybrids.
      all SSD materials (average).
    */
    //gMessMgr->Info() << "StiSsdDetectorBuilder::buildMaterials() - I - Started "<<endm;
    if (! _gasMat)
      _gasMat     = add(new StiMaterial("SsdAir",7.3, 14.61, 0.001205, 30420.*0.001205, 7.3*12.e-9));
    if (! _siMat)
      _siMat      = add(new StiMaterial("SsdSi",14., 28.0855, 2.33, 21.82, 14.*12.*1e-9));
    if (! _hybridMat)
    _hybridMat  = add(new StiMaterial("SsdHyb",14., 28.0855, 2.33, 21.82, 14.*12.*1e-9));

    double ionization = _siMat->getIonization();
    //const static double I2Ar = (15.8*18) * (15.8*18) * 1e-18; // GeV**2
    StiElossCalculator * siElossCalculator = new StiElossCalculator(_siMat->getZOverA(), ionization*ionization, _siMat->getA(), _siMat->getZ(), _siMat->getDensity());

    //gMessMgr->Info() << "StiSsdDetectorBuilder::buildMaterials() - I - Done "<<endm;  
    cout << "StiSsdDetectorBuilder::buildMaterials() - I - Done "<<endl;  
    /*! buildShape : SSD has been defined as a PlanarShape 
      Hybrids are defined as a PlanarShape, positionned on the Silicon Wafers; the thickness of 
      the hybrids corresponds to 0.75 % of X0. 
    */
    unsigned int layer = 0;
    int nWafers;
    //gMessMgr->Info() << "StiSsdDetectorBuilder::buildShapes() - I - Started" << endm;
    nWafers = _config->getNumberOfWafers();
    _waferShape[layer] = new StiPlanarShape(name,
					    nWafers*_geometry->getWaferWidth(), 
					    _geometry->getWaferThickness(),
					    _geometry->getWaferLength() );
    add(_waferShape[layer]);

    _hybridShape[layer] = new StiPlanarShape(name,nWafers*_geometry->getWaferWidth(),
					     0.07,_geometry->getWaferLength() );
    add(_hybridShape[layer]);  
    //gMessMgr->Info() << "StiSsdDetectorBuilder::buildShapes() - I - Done" << endm;

    setNSectors(layer,_config->getTotalNumberOfLadders()); 
    /*! Placement of Ssd Modules is currently done by reading the geom.C table. 
      Ladders are placed according to the coordinates of its first module number 	  
      int idwafer = 7*1000+wafer*100+ladder;      	
      ----> ladder # 1  ===> module 7101 
      ----> ladder # 20 ===> module 7120
    */
    int ind=0;
    for (VecTObjIter it=_geometry->begin(); it!=_geometry->end(); ++it) {
	StSsdWaferGeometry* waferGeom = dynamic_cast<StSsdWaferGeometry*>( *it );
	//assert(waferGeom);
	++ind;
	for (int ladder=1; ladder<=20; ++ladder) {	    
	    if(waferGeom->getID() == (7100 + (ladder))) { //this gives us the first wafer on the ladder
		StThreeVectorD centerVector(waferGeom->x(0), waferGeom->x(1), waferGeom->x(2) );
		StThreeVectorD normalVector(waferGeom->n(0), waferGeom->n(1), waferGeom->n(2) );
		double phi = centerVector.phi();
		double r = centerVector.perp();
		cout <<"Det Nber = "<<waferGeom->getID()<<"\tcv\t:"<<centerVector<<"\tphi:\t"<<phi<<"\tr:\t"<<r<<endl;
		StiPlacement *pPlacement = new StiPlacement;
		pPlacement->setZcenter(0.);
		pPlacement->setLayerRadius(r); //this is only used for ordering in detector container...
		pPlacement->setLayerAngle(phi); //this is only used for ordering in detector container...
		pPlacement->setRegion(StiPlacement::kMidRapidity);
		pPlacement->setNormalRep(phi, r, 0.);  //but we have to use this to fix ladders 20 and 12
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
		pLadder->setKey(1,0);
		pLadder->setKey(2,ladder-1);
		pLadder->setElossCalculator(siElossCalculator);
		add(layer,ladder-1,pLadder); 
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
		pHybrid->setElossCalculator(siElossCalculator);
		add(pHybrid);*/
	    }
	}
    }
#endif
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

void StiSsdDetectorBuilder::loadDS(TDataSet& ds)
{
	cout << "StiSsdDetectorBuilder::loadDS(TDataSet* ds) -I- Started" << endl;
	_trackingParameters.loadDS(ds);
	_hitCalculator.loadDS(ds);
	cout << "StiSsdDetectorBuilder::loadDS(TDataSet* ds) -I- Done" << endl;
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
//________________________________________________________________________________
void StiSsdDetectorBuilder::useVMCGeometry() {
#ifdef __SsdInChain__
  cout << "StiSsdDetectorBuilder::buildDetectors() -I- Use VMC geometry" << endl;
  SetCurrentDetectorBuilder(this);
  struct Material_t {
    Char_t *name;
    StiMaterial    **p;
  };
  Material_t map[] = {
    {"AIR", &_gasMat},
    {"SILICON", &_siMat},
    {"SILICON", &_hybridMat}
  };
  Int_t M = sizeof(map)/sizeof(Material_t);
  for (Int_t i = 0; i < M; i++) {
    const TGeoMaterial *mat =  gGeoManager->GetMaterial(map[i].name); 
    if (! mat) continue;
    Double_t PotI = StiVMCToolKit::GetPotI(mat);
    *map[i].p = add(new StiMaterial(mat->GetName(),
				    mat->GetZ(),
				    mat->GetA(),
				    mat->GetDensity(),
				    mat->GetDensity()*mat->GetRadLen(),
				    PotI));
  }
  const VolumeMap_t SsdVolumes[] = { 
  // SSD
  //  {"SFMO", "the mother of all Silicon Strip Detector volumes","HALL_1/CAVE_1/SVTT_1/SFMO_1","",""},
  {"SCMP","SSD mounting plate inserted in the cone","HALL_1/CAVE_1/SVTT_1/SFMO_1/SCMP_1-8","",""},
  {"SCVM","SSD V-shape mouting piece","HALL_1/CAVE_1/SVTT_1/SFMO_1/SCVM_1-8/*","",""},
  {"SSLT","the linking (sector to the cone) tube","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSLT_1-8","",""},
  {"SSLB","the linking (sector to the cone)","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSLB_1-8","",""},
  {"SSRS","the side of the small rib","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSRS_1-4","",""},
  {"SSRT","the top of the side rib","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSRT_1-4","",""},
  {"SSSS","Side parts of the small sectors","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSSS_1-4","",""},
  {"SSST","Top parts of the small sectors","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSST_1-4","",""},
  //  {"SFLM","the mother of the ladder","HALL_1/CAVE_1/SVTT_1/SFMO_1/SFLM_1-20/*","",""}, 
  {"SFSM","the structure mother volume","HALL_1/CAVE_1/SVTT_1/SFMO_1/SFLM_1-20/SFSM_1/*","",""}
  //  {"SFDM","the detectors and adcs mother volume","HALL_1/CAVE_1/SVTT_1/SFMO_1/SFLM_1-20/SFDM_1/*","",""}
  //  {"SFSD","the strip detector",                  "HALL_1/CAVE_1/SVTT_1/SFMO_1/SFLM_1-20/SFDM_1/SFSW_1-16/SFSD_1","ssd",""},// <+++
  };
  Int_t NoSsdVols = sizeof(SsdVolumes)/sizeof(VolumeMap_t);
  TString pathT("HALL_1/CAVE_1/SVTT_1/SFMO_1");
  TString path("");
  for (Int_t i = 0; i < NoSsdVols; i++) {
    gGeoManager->RestoreMasterVolume(); 
    gGeoManager->CdTop();
    gGeoManager->cd(pathT); path = pathT;
    TGeoNode *nodeT = gGeoManager->GetCurrentNode();
    if (! nodeT) continue;;
    StiVMCToolKit::LoopOverNodes(nodeT, path, SsdVolumes[i].name, MakeAverageVolume);
  }
#endif
}
