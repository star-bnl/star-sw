// $Id: StiSsdDetectorBuilder.cxx,v 1.26 2006/10/17 20:18:05 fisyak Exp $
// 
// $Log: StiSsdDetectorBuilder.cxx,v $
// Revision 1.26  2006/10/17 20:18:05  fisyak
// Add handle when SVTT mother volume is missing
//
// Revision 1.25  2006/10/16 20:31:17  fisyak
// Clean dependencies from Sti useless classes
//
// Revision 1.24  2006/10/09 15:47:59  fisyak
// use Normal represantation, remove StiDedxCalculator
//
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
#include <assert.h>
#include <map>
using namespace std;
#include <stdexcept>
#include "StMessMgr.h"
#include "StThreeVectorD.hh"
#include "tables/St_ssdDimensions_Table.h"
#include "tables/St_ssdConfiguration_Table.h"
#include "tables/St_ssdWafersPosition_Table.h"

#include "Sti/Base/Factory.h"
#include "Sti/StiPlanarShape.h"
#include "Sti/StiCylindricalShape.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetector.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiElossCalculator.h"
#include "Sti/StiHitErrorCalculator.h"
#include "Sti/StiIsActiveFunctor.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "StiSsd/StiSsdIsActiveFunctor.h" 
#include "StiSsd/StiSsdDetectorBuilder.h" 


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
    char name[50];  
    int nRows = 1 ;
    assert(StiVMCToolKit::GetVMC());
    gMessMgr->Info() << "StiSsdDetectorBuilder::buildDetectors() - I - Started "<<endm;
    load(_inputFile, source);
    
    setNRows(nRows);
    TDataSet *dbSet = source.GetDataBase("Geometry/ssd");
    assert( dbSet);
    St_ssdDimensions *_dimensions = (St_ssdDimensions *) dbSet->Find("ssdDimensions");
    if (_dimensions) 
      gMessMgr->Info()<< "StiSsdDetectorBuilder : SSD Dimensions loaded..."<<endm;
    else
	throw runtime_error("StiSsdDetectorBuilder::loadDb() - ERROR - _dimensions==0");
    ssdDimensions_st *dimensions = _dimensions->GetTable();
    St_ssdConfiguration *_config = (St_ssdConfiguration *) dbSet->Find("ssdConfiguration");
    if (_config) 
      gMessMgr->Info() <<"StiSsdDetectorBuilder : SSD Configuration loaded..." << endm;
    else
	throw runtime_error("StiSsdDetectorBuilder::loadDb() - ERROR - _config==0");
    ssdConfiguration_st *config = _config->GetTable();

    St_ssdWafersPosition *_wafers = (St_ssdWafersPosition *) dbSet->Find("ssdWafersPosition");
    if (_wafers) 
      gMessMgr->Info() <<"StiSsdDetectorBuilder : SSD Wafers position loaded..." << endm;
    else
	throw runtime_error("StiSsdDetectorBuilder::loadDb() - ERROR - _wafers==0");
    ssdWafersPosition_st *wafers = _wafers->GetTable();
          
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
    Int_t nWafers = config->nMaxWafers/config->nMaxLadders;
    StiPlanarShape *ladderShape = new StiPlanarShape(name,
						     nWafers*dimensions->waferHalfWidth, 
						     dimensions->waferHalfThickness,
						     dimensions->waferHalfLength );
    add(ladderShape);
    Int_t layer = 0;
    setNSectors(layer,config->nMaxLadders); 
    /*! Placement of Ssd Modules is currently done by reading the geom.C table. 
      Ladders are placed according to the coordinates of its first module number 	  
      int idwafer = 7*1000+wafer*100+ladder;      	
      ----> ladder # 1  ===> module 7101 
      ----> ladder # 20 ===> module 7120
    */
    Int_t N = _wafers->GetNRows();
    for (Int_t i = 0; i < N; i++, wafers++) {
      Int_t ladder = wafers->id%100;
      Int_t Layer  = wafers->id/1000; if (Layer > 7) Layer = 7;
      Int_t Wafer  = (wafers->id - 1000*Layer)/100;
      if (Wafer != 1) continue;//this gives us the first wafer on the ladder
      StThreeVectorD centerVector(wafers->centerPosition[0],wafers->centerPosition[1],wafers->centerPosition[2]);
      StThreeVectorD normalVector(wafers->normalDirection[0],wafers->normalDirection[1],wafers->normalDirection[2]);
      Double_t prod = centerVector*normalVector;
      if (prod < 0) normalVector *= -1;
      double phi  = centerVector.phi();
      double phiD = normalVector.phi();
      double r = centerVector.perp();
      cout <<"Det Nber = "<<wafers->id<<"\tcv\t:"<<centerVector<<"\tphi:\t"<<phi<<"\tr:\t"<<r<<endl;
      StiPlacement *pPlacement = new StiPlacement;
      pPlacement->setZcenter(0.);
      pPlacement->setLayerRadius(r); //this is only used for ordering in detector container...
      pPlacement->setLayerAngle(phi); //this is only used for ordering in detector container...
      pPlacement->setRegion(StiPlacement::kMidRapidity);
      //		pPlacement->setNormalRep(phi, r, 0.);  //but we have to use this to fix ladders 20 and 12
      pPlacement->setNormalRep(phiD, r*TMath::Cos(phi-phiD), r*TMath::Sin(phi-phiD)); 
      sprintf(name, "Ssd/Layer_%d/Ladder_%d/Wafers", layer, ladder);
      StiDetector *pLadder = _detectorFactory->getInstance();
      pLadder->setName(name);
      pLadder->setIsOn(true);
      pLadder->setIsActive(new StiIsActiveFunctor(_active));
      pLadder->setIsContinuousMedium(true);
      pLadder->setIsDiscreteScatterer(true);
      pLadder->setGas(_gasMat);
      pLadder->setMaterial(_siMat);
      pLadder->setShape(ladderShape);
      pLadder->setPlacement(pPlacement); 
      pLadder->setHitErrorCalculator(&_hitCalculator);
      pLadder->setKey(1,0);
      pLadder->setKey(2,ladder-1);
      pLadder->setElossCalculator(siElossCalculator);
      add(layer,ladder-1,pLadder); 
    }
    useVMCGeometry();
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
  gGeoManager->RestoreMasterVolume(); 
  gGeoManager->CdTop();
  // Check that SVTT_1/SFMO_1 exist
  if (! gGeoManager->cd(pathT)) {
    pathT = "HALL_1/CAVE_1/SFMO_1";
  }
  TString path("");
  for (Int_t i = 0; i < NoSsdVols; i++) {
    gGeoManager->RestoreMasterVolume(); 
    gGeoManager->CdTop();
    if (gGeoManager->cd(pathT)) {
      path = pathT;
      TGeoNode *nodeT = gGeoManager->GetCurrentNode();
      if (! nodeT) continue;;
      StiVMCToolKit::LoopOverNodes(nodeT, path, SsdVolumes[i].name, MakeAverageVolume);
    } else gMessMgr->Info() << "StiSsdDetectorBuilder::useVMCGeometry skip node " << pathT.Data() << endm;
  }
}
//________________________________________________________________________________
ssdWafersPosition_st *StiSsdDetectorBuilder::ssdWafersPosition(Int_t Id, St_ssdWafersPosition *wafers) {
  Int_t N = wafers->GetNRows();
  ssdWafersPosition_st *wafer = wafers->GetTable();
  for (Int_t i = 0; i < N; i++, wafer++) if (Id ==  wafer->id) return wafer;
  return 0;
}
