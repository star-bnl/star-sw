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
#include "StiSsd/StiSstDetectorBuilder.h"
#include "StSsdUtil/StSsdBarrel.hh"
#include "StDetectorDbMaker/StiSsdHitErrorCalculator.h"


StiSstDetectorBuilder::StiSstDetectorBuilder(bool active)
   : StiDetectorBuilder("Ssd", active), _siMat(0), _hybridMat(0)
{
   // Hit error parameters : it is set to 20 microns, in both x and y coordinates
}

StiSstDetectorBuilder::~StiSstDetectorBuilder()
{}


/** Build the SST detector components. */
void StiSstDetectorBuilder::buildDetectors(StMaker &source)
{
   gMessMgr->Info() << "StiSstDetectorBuilder::buildDetectors() - I - Started " << endm;
   //StSsdBarrel *mySsd = StSsdBarrel::Instance();
   mySsd = StSsdBarrel::Instance();

   if (! mySsd) {// no active SSD
      gMessMgr->Info() << "StiSstDetectorBuilder::buildDetectors() - I - there is no SSD barrel - take whatever exist in GEANT" << endm;
      StiVMCToolKit::GetVMC();
      return;
   }

   setNRows(1);

   SetCurrentDetectorBuilder(this);

   // Gas material must be defined. Here we use air properties
   _gasMat = add(new StiMaterial("PixelAir", 7.3, 14.61, 0.001205, 30420.*0.001205, 7.3 * 12.e-9));

   if (StiVMCToolKit::GetVMC()) {
      useVMCGeometry();
      buildInactiveVolumes();
   }
}


/** Builds the sensors of the SST detector. */
void StiSstDetectorBuilder::useVMCGeometry()
{
   cout << "StiSstDetectorBuilder::buildDetectors() -I- Use VMC geometry" << endl;

   //build the material
   struct Material_t {
      const Char_t *name;
      StiMaterial    **p;
   };

   Material_t map[] = {
      {"AIR", &_gasMat},
      {"SILICON", &_siMat},
      {"SILICON", &_hybridMat}
   };

   Int_t M = sizeof(map) / sizeof(Material_t);

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


   cout << "StiSstDetectorBuilder::buildMaterials() - I - Done " << endl;

   ssdDimensions_st *dimensions = mySsd->getDimensions();
   Int_t NL = mySsd->getNumberOfLadders();
   Int_t NW = mySsd->getNWaferPerLadder();

   // use 1 ladder to get the dimension
   StSsdLadder *ladder = mySsd->getLadder(0);
   assert(ladder);
   StSsdWafer *wafer1 = ladder->getWafer(0);
   StSsdWafer *wafer2 = ladder->getWafer(NW - 1);
   assert(wafer1 && wafer2);
   Double_t width = TMath::Abs(wafer1->x(2) - wafer2->x(2)) / 2. + 2;
   StiPlanarShape *ladderShape = new StiPlanarShape("SsdLadder",
         width,
         2*dimensions->waferHalfThickness,
         dimensions->waferHalfLength );
   add(ladderShape);

   Int_t layer = 0;
   setNSectors(layer, NL);

   /*! Placement of Ssd Modules is currently done by reading the geom.C table.
     Ladders are placed according to the coordinates of its first module number
     int idwafer = 7*1000+wafer*100+ladder;
     ----> ladder # 1  ===> module 7101
     ----> ladder # 20 ===> module 7120
   */
   for (Int_t iLadder = 0; iLadder < NL; iLadder++) {
      ladder = mySsd->getLadder(iLadder);

      if (! ladder) continue;

      wafer1 = ladder->getWafer(0);
      wafer2 = ladder->getWafer(NW - 1);

      if (! wafer1 || ! wafer2) continue;

      StThreeVectorD centerVector1(wafer1->x(0), wafer1->x(1), wafer1->x(2));
      StThreeVectorD normalVector1(wafer1->n(0), wafer1->n(1), wafer1->n(2));
      StThreeVectorD centerVector2(wafer2->x(0), wafer2->x(1), wafer2->x(2));
      StThreeVectorD normalVector2(wafer2->n(0), wafer2->n(1), wafer2->n(2));
      StThreeVectorD centerVector = centerVector1 + centerVector2; centerVector *= 0.5;
      StThreeVectorD normalVector = normalVector1 + normalVector2; normalVector *= 0.5;
      Double_t prod = centerVector * normalVector;

      if (prod < 0) normalVector *= -1;

      double phi  = centerVector.phi();
      double phiD = normalVector.phi();
      double r = centerVector.perp();
      cout << "Det Id = " << wafer1->getId() << "\tcv\t:" << centerVector << "\tphi:\t" << phi << "\tr:\t" << r << "\tz:\t" << centerVector.z() << endl;
      StiPlacement *pPlacement = new StiPlacement;
      pPlacement->setZcenter(centerVector.z());
      // centerVector.z can be different than with this definition
      // issue ?
      pPlacement->setLayerRadius(r);  //this is only used for ordering in detector container...
      pPlacement->setLayerAngle(phi); //this is only used for ordering in detector container...
      pPlacement->setRegion(StiPlacement::kMidRapidity);
      //pPlacement->setNormalRep(phi, r, 0.);  //but we have to use this to fix ladders 20 and 12
      pPlacement->setNormalRep(phiD, r * TMath::Cos(phi - phiD), r * TMath::Sin(phi - phiD));

      //Build final detector object
      StiDetector *pLadder = _detectorFactory->getInstance();
      pLadder->setName(Form("Ssd/Layer_%d/Ladder_%d/Wafers", layer, iLadder));
      pLadder->setIsOn(kTRUE);
      pLadder->setIsActive(new StiIsActiveFunctor(_active));
      pLadder->setIsContinuousMedium(false);
      pLadder->setIsDiscreteScatterer(true);
      pLadder->setGas(GetCurrentDetectorBuilder()->getGasMat());

      if (!pLadder->getGas()) LOG_INFO << "gas not there!" << endm;

      //pLadder->setGas(_gasMat);
      pLadder->setMaterial(_siMat);
      pLadder->setShape(ladderShape);
      pLadder->setPlacement(pPlacement);
      pLadder->setHitErrorCalculator(StiSsdHitErrorCalculator::instance());
      pLadder->setKey(1, 0);
      pLadder->setKey(2, iLadder - 1);
      add(layer, iLadder, pLadder);
   }
}


/**
 * Creates a crude approximation of the SST detector. The geometry is modeled with a single tube
 * using the dimensions and other physical properties of the IST mother volume defined in the ROOT
 * TGeo geometry.
 */
void StiSstDetectorBuilder::buildInactiveVolumes()
{
   // Build average inactive volumes
   const VolumeMap_t volumes[] = {
      {"SFLM", "the mother of the ladder support volume", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/SFMO_1", "", ""},
      {"SFDM", "the mother of the detector volume", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/SFMO_1", "", ""}
   };

   // Build the volume map and loop over all found volumes
   Int_t nVolumes = sizeof(volumes) / sizeof(VolumeMap_t);
   gGeoManager->RestoreMasterVolume();
   gGeoManager->CdTop();

   for (Int_t i = 0; i < nVolumes; i++) {

      if ( !gGeoManager->cd(volumes[i].path) ) {
         Warning("buildInactiveVolumes()", "Cannot find path to %s node. Skipping to next node...", volumes[i].name);
         continue;
      }

      TGeoNode *geoNode = gGeoManager->GetCurrentNode();

      if (!geoNode) continue;

      LOG_DEBUG << "Current node : " << i << "/" << nVolumes << " path is : " << volumes[i].name << endm;
      LOG_DEBUG << "Number of daughters : " << geoNode->GetNdaughters() << " weight : " << geoNode->GetVolume()->Weight() << endm;

      StiVMCToolKit::LoopOverNodes(geoNode, volumes[i].path, volumes[i].name, MakeAverageVolume);

      // Access last added volume
      int row = getNRows() - 1;
      int sector = 0;

      // Make Sti detector active, i.e. use it in tracking. XXX:ds: Not sure it has any effect but should not hurt
      StiDetector *stiDetector = getDetector(row, sector);
      stiDetector->setIsOn(true);

      // Modify dimensions of the mother volume
      if (string(volumes[i].name) == string("SFMO1")) {
         StiCylindricalShape *stiShape = (StiCylindricalShape *) stiDetector->getShape();
         stiShape->setThickness(stiShape->getThickness()/2);
      }
   }
}


ssdWafersPosition_st *StiSstDetectorBuilder::ssdWafersPosition(Int_t Id, St_ssdWafersPosition *wafers)
{
   Int_t N = wafers->GetNRows();
   ssdWafersPosition_st *wafer = wafers->GetTable();

   for (Int_t i = 0; i < N; i++, wafer++) if (Id ==  wafer->id) return wafer;

   return 0;
}
