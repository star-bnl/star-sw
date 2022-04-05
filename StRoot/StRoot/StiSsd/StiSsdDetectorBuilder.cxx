// $Id: StiSsdDetectorBuilder.cxx,v 1.40 2015/02/03 10:21:17 smirnovd Exp $
// 
// $Log: StiSsdDetectorBuilder.cxx,v $
// Revision 1.40  2015/02/03 10:21:17  smirnovd
// Check for valid gGeoManager in buildDetectors instead of constructor
//
// This is a fixup of the change committed on 2015-01-30 16:33:59 (8b14dfaf)
// The detector builder requires a valid TGeoManager object to build detector
// geometries. However in the current StiMaker gGeoManager is not available when
// detector builder is created. It becomes available just before the
// ::buildDetectors() is called in StiMasterDetectorBuilder::build()
//
// Revision 1.39  2015/01/30 21:34:00  smirnovd
// StiXxxDetectorBuilder: Added a check for valid global object of TGeoManager. The detector builder is required one and cannot proceed if one does not exist
//
// Revision 1.38  2014/12/02 23:25:54  smirnovd
// StiXxxDetectorBuilder: Removed deprecated calls to dummy methods
//
// Revision 1.37  2014/08/22 17:52:18  perev
// StiEloss calculator creates in material now
//
// Revision 1.36  2013/03/22 23:58:51  fisyak
// Remove name[50]
//
// Revision 1.35  2011/04/22 22:00:39  fisyak
// warn off
//
// Revision 1.34  2010/08/25 21:57:42  fisyak
// Get rid off access to specfic detector tracking parameters which usage has been  disable since 2008/06/11
//
// Revision 1.33  2009/03/16 13:50:15  fisyak
// Move out all Sti Chairs into StDetectorDb
//
// Revision 1.32  2008/08/12 20:54:40  fisyak
// If StSsdBarrel does not exist then take SSD as dead material from whatever exist in GEANT
//
// Revision 1.31  2008/06/12 16:36:55  fisyak
// Remove all SSD endcap volumes
//
// Revision 1.30  2008/06/11 22:04:39  fisyak
// Add dead material
//
// Revision 1.29  2008/04/03 20:04:22  fisyak
// Straighten out DB access via chairs
//
// Revision 1.28  2007/07/12 20:39:13  fisyak
// Remove default errors for SSD
//
// Revision 1.27  2007/03/21 17:53:38  fisyak
// make use for new StSsdBarrel
//
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
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
#include "Sti/StiIsActiveFunctor.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "StiSsd/StiSsdIsActiveFunctor.h" 
#include "StiSsd/StiSsdDetectorBuilder.h" 
#include "StSsdUtil/StSsdBarrel.hh"
#include "StDetectorDbMaker/StiSsdHitErrorCalculator.h"
StiSsdDetectorBuilder::StiSsdDetectorBuilder(bool active)
    : StiDetectorBuilder("Ssd",active), _siMat(0), _hybridMat(0)
{
    // Hit error parameters : it is set to 20 microns, in both x and y coordinates 
}

StiSsdDetectorBuilder::~StiSsdDetectorBuilder()
{} 


void StiSsdDetectorBuilder::buildDetectors(StMaker & source)
{
   if (!gGeoManager)
      throw runtime_error("StiSsdDetectorBuilder::StiSsdDetectorBuilder() "
         "- Cannot build Sti geometry due to missing global object of TGeoManager class. "
         "Make sure STAR geometry is properly loaded with BFC AgML option");

    gMessMgr->Info() << "StiSsdDetectorBuilder::buildDetectors() - I - Started "<<endm;
    StSsdBarrel *mySsd = StSsdBarrel::Instance();
    if (! mySsd) {// no active SSD
      gMessMgr->Info() << "StiSsdDetectorBuilder::buildDetectors() - I - there is no SSD barrel - take whatever exist in GEANT" << endm;
      StiVMCToolKit::GetVMC();
      return;
    }
    int nRows = 1 ;
    setNRows(nRows);
    if (! _gasMat)
      _gasMat     = add(new StiMaterial("SsdAir",7.3, 14.61, 0.001205, 30420.*0.001205, 7.3*12.e-9));
    if (! _siMat)
      _siMat      = add(new StiMaterial("SsdSi",14., 28.0855, 2.33, 21.82, 14.*12.*1e-9));
    if (! _hybridMat)
    _hybridMat  = add(new StiMaterial("SsdHyb",14., 28.0855, 2.33, 21.82, 14.*12.*1e-9));

    //gMessMgr->Info() << "StiSsdDetectorBuilder::buildMaterials() - I - Done "<<endm;  
    cout << "StiSsdDetectorBuilder::buildMaterials() - I - Done "<<endl;  
    ssdDimensions_st *dimensions = mySsd->getDimensions();
    Int_t NL = mySsd->getNumberOfLadders();
    Int_t NW = mySsd->getNWaferPerLadder();
    StSsdLadder *Ladder = mySsd->getLadder(0);
    assert(Ladder);
    StSsdWafer *Wafer1 = Ladder->getWafer(0);
    StSsdWafer *Wafer2 = Ladder->getWafer(NW-1);
    assert(Wafer1 && Wafer2);
    Double_t width = TMath::Abs(Wafer1->x(2) - Wafer2->x(2))/2. + 2;
    StiPlanarShape *ladderShape = new StiPlanarShape("SsdLadder",
						     width,
						     0.34, // increas by a factor ~10 2*dimensions->waferHalfThickness,
						     dimensions->waferHalfLength );
    add(ladderShape);
    Int_t layer = 0;
    setNSectors(layer,NL); 
    /*! Placement of Ssd Modules is currently done by reading the geom.C table. 
      Ladders are placed according to the coordinates of its first module number 	  
      int idwafer = 7*1000+wafer*100+ladder;      	
      ----> ladder # 1  ===> module 7101 
      ----> ladder # 20 ===> module 7120
    */
    for (Int_t ladder = 0; ladder < NL; ladder++) {
      Ladder = mySsd->getLadder(ladder);
      if (! Ladder) continue;
      Wafer1 = Ladder->getWafer(0);
      Wafer2 = Ladder->getWafer(NW-1);
      if (! Wafer1 || ! Wafer2) continue; 
      StThreeVectorD centerVector1(Wafer1->x(0),Wafer1->x(1),Wafer1->x(2));
      StThreeVectorD normalVector1(Wafer1->n(0),Wafer1->n(1),Wafer1->n(2));
      StThreeVectorD centerVector2(Wafer2->x(0),Wafer2->x(1),Wafer2->x(2));
      StThreeVectorD normalVector2(Wafer2->n(0),Wafer2->n(1),Wafer2->n(2));
      StThreeVectorD centerVector = centerVector1 + centerVector2; centerVector *= 0.5;
      StThreeVectorD normalVector = normalVector1 + normalVector2; normalVector *= 0.5;
      Double_t prod = centerVector*normalVector;
      if (prod < 0) normalVector *= -1;
      double phi  = centerVector.phi();
      double phiD = normalVector.phi();
      double r = centerVector.perp();
      cout <<"Det Id = "<<Wafer1->getId()<<"\tcv\t:"<<centerVector<<"\tphi:\t"<<phi<<"\tr:\t"<<r<<"\tz:\t" << centerVector.z()<< endl;
      StiPlacement *pPlacement = new StiPlacement;
      pPlacement->setZcenter(centerVector.z());
      pPlacement->setLayerRadius(r); //this is only used for ordering in detector container...
      pPlacement->setLayerAngle(phi); //this is only used for ordering in detector container...
      pPlacement->setRegion(StiPlacement::kMidRapidity);
      //		pPlacement->setNormalRep(phi, r, 0.);  //but we have to use this to fix ladders 20 and 12
      pPlacement->setNormalRep(phiD, r*TMath::Cos(phi-phiD), r*TMath::Sin(phi-phiD)); 
      StiDetector *pLadder = _detectorFactory->getInstance();
      pLadder->setName(Form("Ssd/Layer_%d/Ladder_%d/Wafers", layer, ladder));
      pLadder->setIsActive(new StiIsActiveFunctor(_active));
      pLadder->setGas(_gasMat);
      pLadder->setMaterial(_siMat);
      pLadder->setShape(ladderShape);
      pLadder->setPlacement(pPlacement); 
      pLadder->setHitErrorCalculator(StiSsdHitErrorCalculator::instance());
      pLadder->setKey(1,0);
      pLadder->setKey(2,ladder-1);
      add(layer,ladder,pLadder); 
    }
    useVMCGeometry();
}
//________________________________________________________________________________
void StiSsdDetectorBuilder::useVMCGeometry() {
  cout << "StiSsdDetectorBuilder::buildDetectors() -I- Use VMC geometry" << endl;
  SetCurrentDetectorBuilder(this);
  struct Material_t {
    const Char_t *name;
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
    //  {"SFMO", "the mother of all Silicon Strip Detector volumes","HALL_1/CAVE_1/SVTT_1/SFMO_1","",""}, // 17.466824 [kg]
    //    {"SCMP","SSD mounting plate inserted in the cone","HALL_1/CAVE_1/SVTT_1/SFMO_1/SCMP_1-8","",""},      //  0.024494 [kg]
    //    {"SCVM","SSD V-shape mouting piece","HALL_1/CAVE_1/SVTT_1/SFMO_1/SCVM_1-8/*","",""},                  //  0.057931 [kg]
    //    {"SSLT","the linking (sector to the cone) tube","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSLT_1-8","",""},        //  0.027415 [kg]
    //    {"SSLB","the linking (sector to the cone)","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSLB_1-8","",""},             //  0.073710 [kg]
    //    {"SSRS","the side of the small rib","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSRS_1-4","",""},                    //  0.462138 [kg]
    //    {"SSRT","the top of the side rib","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSRT_1-4","",""},                      //  0.172237 [kg]
    //    {"SSSS","Side parts of the small sectors","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSSS_1-4","",""},              //  0.462138 [kg]
    //    {"SSST","Top parts of the small sectors","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSST_1-4","",""},               //  0.172237 [kg]
    {"SFLM","the mother of the ladder","HALL_1/CAVE_1/SVTT_1/SFMO_1/SFLM_1-20/*","",""}              //  0.546171 [kg]
    //  {"SFSM","the structure mother volume","HALL_1/CAVE_1/SVTT_1/SFMO_1/SFLM_1-20/SFSM_1/*","",""}     //   0.451003 [kg]
    //  {"SFDM","the detectors and adcs mother volume","HALL_1/CAVE_1/SVTT_1/SFMO_1/SFLM_1-20/SFDM_1/*","",""} // 0.095168 [kg]
    // {"SFSD","the strip detector",                  "HALL_1/CAVE_1/SVTT_1/SFMO_1/SFLM_1-20/SFDM_1/SFSW_1-16/SFSD_1","ssd",""}// 0.002041 [kg]
  };
  Int_t NoSsdVols = sizeof(SsdVolumes)/sizeof(VolumeMap_t);
  TString pathT("HALL_1/CAVE_1");
  gGeoManager->RestoreMasterVolume(); 
  gGeoManager->CdTop();
  // Check that SVTT_1/SFMO_1 exist
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
