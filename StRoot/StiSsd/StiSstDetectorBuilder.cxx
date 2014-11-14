#include <stdio.h>
#include <assert.h>
#include <map>

#include "TVector3.h"

using namespace std;
#include <stdexcept>
#include "StMessMgr.h"
#include "StThreeVectorD.hh"
#include "tables/St_ssdDimensions_Table.h"
#include "tables/St_ssdConfiguration_Table.h"
#include "tables/St_ssdWafersPosition_Table.h"

#include "StEvent/StEnumerations.h"
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
#include "Sti/StiNeverActiveFunctor.h"
#include "StiSsd/StiSsdIsActiveFunctor.h"
#include "StSsdUtil/StSstConsts.h"
#include "StDetectorDbMaker/StiSsdHitErrorCalculator.h"


StiSstDetectorBuilder::StiSstDetectorBuilder(bool active)
   : StiDetectorBuilder("Ssd", active)
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
   const TGeoMaterial* geoMat = gGeoManager->GetMaterial("AIR");

   _gasMat = geoMat ? add(new StiMaterial(geoMat->GetName(), geoMat->GetZ(), geoMat->GetA(), geoMat->GetDensity(), geoMat->GetDensity()*geoMat->GetRadLen()))
                    : add(new StiMaterial("AIR", 7.3, 14.61, 0.001205, 30420.*0.001205, 7.3 * 12.e-9));

   if (StiVMCToolKit::GetVMC()) {
      useVMCGeometry();
      buildInactiveVolumes();
   }
}


/** Builds the sensors of the SST detector. */
void StiSstDetectorBuilder::useVMCGeometry()
{
   cout << "StiSstDetectorBuilder::buildDetectors() -I- Use VMC geometry" << endl;

   // Define silicon material used in manual construction of sensitive layers in this builder
   const TGeoMaterial* geoMat = gGeoManager->GetMaterial("SILICON");

   StiMaterial* silicon = geoMat ? add(new StiMaterial(geoMat->GetName(), geoMat->GetZ(), geoMat->GetA(), geoMat->GetDensity(), geoMat->GetDensity()*geoMat->GetRadLen()))
                                 : add(new StiMaterial("SILICON", 14, 28.0855, 2.33, 21.82, 14.*12.*1e-9) );

   // Build active sti volumes for SST sensors
   int iSensor = floor(kSstNumSensorsPerLadder/2);

   for (int iLadder = 1; iLadder <= kSstNumLadders; ++iLadder)
   {
      ostringstream geoPath;
      geoPath << "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/SFMO_1/SFLM_" << iLadder << "/SFSW_" << iSensor << "/SFSL_1/SFSD_1";

      bool isAvail = gGeoManager->cd(geoPath.str().c_str());

      if (!isAvail) {
         Warning("useVMCGeometry()", "Cannot find path to SFSD (SST sensitive) node. Skipping to next ladder...");
         continue;
      }

      TGeoVolume* sensorVol = gGeoManager->GetCurrentNode()->GetVolume();
      TGeoMatrix* sensorMatrix = gGeoManager->MakePhysicalNode(geoPath.str().c_str())->GetMatrix();

      if (!sensorMatrix) {
         Warning("useVMCGeometry()", "Could not get SST sensor position matrix. Skipping to next ladder...");
         continue;
      }

      // Build global rotation for the sensor
      TGeoRotation sensorRot(*sensorMatrix);

      TGeoBBox *sensorBBox = (TGeoBBox*) sensorVol->GetShape();

      LOG_DEBUG << "Weight/Daughters/Material/A/Z : " << sensorVol->Weight() << "/"
                << sensorVol->GetNdaughters() << "/" << sensorVol->GetMaterial()->GetName() << "/"
                << sensorVol->GetMaterial()->GetA() << "/" << sensorVol->GetMaterial()->GetZ() << endm
                << "DZ/DY/DX : " << sensorBBox->GetDZ() << "/" << sensorBBox->GetDY() << "/" << sensorBBox->GetDX() << endm;

      // Convert center of the sensor geobox to coordinates in the global coordinate system
      double sensorXyzLocal[3]  = {};
      double sensorXyzGlobal[3] = {};

      sensorMatrix->LocalToMaster(sensorXyzLocal, sensorXyzGlobal);

      TVector3 sensorVec(sensorXyzGlobal);

      // XXX:ds: Need to verify the constant for sensor spacing
      double sensorLength = kSstNumSensorsPerLadder * (sensorBBox->GetDZ() + 0.02); // halfDepth + 0.02 ~= (dead edge + sensor gap)/2
      StiShape *stiShape = new StiPlanarShape(geoPath.str().c_str(), sensorLength, 2*sensorBBox->GetDY(), sensorBBox->GetDX());

      add(stiShape);

      Double_t phi  = sensorVec.Phi();
      Double_t phiD = sensorRot.GetPhiRotation() / 180 * M_PI;
      Double_t r    = sensorVec.Perp(); // Ignore the z component if any
      double normVecMag = fabs(r * sin(phi - phiD));
      TVector3 normVec(cos(phiD + M_PI_2), sin(phiD + M_PI_2), 0);

      if (normVec.Dot(sensorVec) < 0) normVec *= -normVecMag;
      else                            normVec *=  normVecMag;

      // Volume positioning
      StiPlacement *pPlacement = new StiPlacement();

      pPlacement->setZcenter(0);
      pPlacement->setLayerRadius(r);
      pPlacement->setLayerAngle(phi);
      pPlacement->setRegion(StiPlacement::kMidRapidity);

      double centerOrient = sensorVec.Phi() - normVec.Phi();
      pPlacement->setNormalRep(normVec.Phi(), normVecMag, r * sin(centerOrient));

      // Build final detector object
      StiDetector *stiDetector = getDetectorFactory()->getInstance();

      stiDetector->setName(geoPath.str().c_str());
      stiDetector->setIsOn(true);

      if (_active) { stiDetector->setIsActive(new StiSsdIsActiveFunctor);}
      else         { stiDetector->setIsActive(new StiNeverActiveFunctor);}

      stiDetector->setIsContinuousMedium(false); // true for gases
      stiDetector->setIsDiscreteScatterer(true); // true for anything other than gas
      stiDetector->setGroupId(kSsdId);
      stiDetector->setShape(stiShape);
      stiDetector->setPlacement(pPlacement);
      stiDetector->setGas(GetCurrentDetectorBuilder()->getGasMat());
      stiDetector->setMaterial(silicon);
      stiDetector->setHitErrorCalculator(StiSsdHitErrorCalculator::instance());

      stiDetector->setKey(1, 0);
      stiDetector->setKey(2, iLadder-1);
      add(0, iLadder, stiDetector);

      // Whole bunch of debugging information
      Float_t rad2deg = 180.0 / 3.1415927;
      LOG_DEBUG << "===>NEW:SST:stiDetector:Name             = " << stiDetector->getName()                     << endm
                << "===>NEW:SST:pPlacement:NormalRefAngle    = " << pPlacement->getNormalRefAngle()*rad2deg    << endm
                << "===>NEW:SST:pPlacement:NormalRadius      = " << pPlacement->getNormalRadius()              << endm
                << "===>NEW:SST:pPlacement:NormalYoffset     = " << pPlacement->getNormalYoffset()             << endm
                << "===>NEW:SST:pPlacement:CenterRefAngle    = " << pPlacement->getCenterRefAngle()*rad2deg    << endm
                << "===>NEW:SST:pPlacement:CenterRadius      = " << pPlacement->getCenterRadius()              << endm
                << "===>NEW:SST:pPlacement:CenterOrientation = " << pPlacement->getCenterOrientation()*rad2deg << endm
                << "===>NEW:SST:pPlacement:LayerRadius       = " << pPlacement->getLayerRadius()               << endm
                << "===>NEW:SST:pPlacement:LayerAngle        = " << pPlacement->getLayerAngle()*rad2deg        << endm
                << "===>NEW:SST:pPlacement:Zcenter           = " << pPlacement->getZcenter()                   << endm
                << "===>NEW:SST:stiDetector:Ladder           = " << iLadder                                    << endm
                << "===>NEW:SST:stiDetector:sensor           = " << iSensor                                    << endm
                << "===>NEW:SST:stiDetector:Active?          = " << stiDetector->isActive()                    << endm;
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


/**
 * Manually modify the SST mother volume by splitting it into three tubes.
 */
void StiSstDetectorBuilder::segmentSFMOVolume(StiDetector* stiSFMO)
{
   StiCylindricalShape* stiSFMOShape = (StiCylindricalShape*) stiSFMO->getShape();

   stiSFMOShape->setThickness(0.75*stiSFMOShape->getThickness());

   // Copy original shape (before its depth is modified) to a new one for the end SFMO tubes
   StiCylindricalShape* stiSFMOEndShape = new StiCylindricalShape(*stiSFMOShape);
   stiSFMOEndShape->setName(stiSFMOEndShape->getName() + "_end");
   stiSFMOEndShape->setHalfDepth(8.25); // 8.25 cm = (50.5-34)/2 cm

   add(stiSFMOEndShape);

   stiSFMOShape->setHalfDepth(34); // 34 cm

   // Create a shalow copy for end tube 1
   StiDetector* stiSFMOEnd = new StiDetector(*stiSFMO);

   StiPlacement* stiSFMOEndPlacement = new StiPlacement(*stiSFMOEnd->getPlacement());
   stiSFMOEndPlacement->setZcenter(stiSFMOShape->getHalfDepth() + stiSFMOEndShape->getHalfDepth());

   stiSFMOEnd->setShape(stiSFMOEndShape);
   stiSFMOEnd->setPlacement(stiSFMOEndPlacement);

   add(getNRows(), 0, stiSFMOEnd);

   // Create a shalow copy for end tube 2
   StiDetector* stiSFMOEnd2 = new StiDetector(*stiSFMOEnd);

   stiSFMOEndPlacement = new StiPlacement(*stiSFMOEnd2->getPlacement());
   stiSFMOEndPlacement->setZcenter(-stiSFMOShape->getHalfDepth() - stiSFMOEndShape->getHalfDepth());

   stiSFMOEnd2->setShape(stiSFMOEndShape);
   stiSFMOEnd2->setPlacement(stiSFMOEndPlacement);

   add(getNRows(), 0, stiSFMOEnd2);
}


ssdWafersPosition_st *StiSstDetectorBuilder::ssdWafersPosition(Int_t Id, St_ssdWafersPosition *wafers)
{
   Int_t N = wafers->GetNRows();
   ssdWafersPosition_st *wafer = wafers->GetTable();

   for (Int_t i = 0; i < N; i++, wafer++) if (Id ==  wafer->id) return wafer;

   return 0;
}
