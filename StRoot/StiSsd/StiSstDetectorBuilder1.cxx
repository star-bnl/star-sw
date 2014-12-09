#include <assert.h>
#include <sstream>
#include <string>

#include "TVector3.h"

#include "StMessMgr.h"
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
#include "StiSsd/StiSstDetectorBuilder1.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "StiSsd/StiSsdIsActiveFunctor.h"
#include "StSsdUtil/StSstConsts.h"
#include "StDetectorDbMaker/StiSsdHitErrorCalculator.h"


StiSstDetectorBuilder1::StiSstDetectorBuilder1(bool active, bool buildIdealGeom)
   : StiDetectorBuilder("Ssd", active), mBuildIdealGeom(buildIdealGeom), mSstDb(0)
{
}


StiSstDetectorBuilder1::~StiSstDetectorBuilder1()
{}


/** Build the SST detector components. */
void StiSstDetectorBuilder1::buildDetectors(StMaker &source)
{
   LOG_INFO << "StiSstDetectorBuilder1::buildDetectors() - I - Started " << endm;
   //StSsdBarrel *mySsd = StSsdBarrel::Instance();

   if (!mBuildIdealGeom) {

      if (!gStSstDbMaker) {
         LOG_ERROR << "StiSstDetectorBuilder1::buildDetectors: SST geometry was requested from "
            "DB but no StSstDb object found. Check for sstDb option in BFC chain" << endm;
         exit(EXIT_FAILURE);
      }

      mSstDb = (StSstDbMaker*) gStSstDbMaker;
      assert(mSstDb);

      LOG_INFO << "StiSstDetectorBuilder1::buildDetectors: Will build SST geometry from DB tables" << endm;
   }

   setNRows(1);

   SetCurrentDetectorBuilder(this);

   // Gas material must be defined. Here we use air properties
   const TGeoMaterial* geoMat = gGeoManager->GetMaterial("AIR");

   _gasMat = geoMat ? add(new StiMaterial(geoMat->GetName(), geoMat->GetZ(), geoMat->GetA(), geoMat->GetDensity(), geoMat->GetRadLen()))
                    : add(new StiMaterial("AIR", 7.3, 14.61, 0.001205, 30420.));

   if (StiVMCToolKit::GetVMC()) {
      useVMCGeometry();
      buildInactiveVolumes();
   }
}


/**
 * Builds the sensors of the SST detector.
 *
 * \author Dmitri Smirnov, BNL
 */
void StiSstDetectorBuilder1::useVMCGeometry()
{
   LOG_INFO << "StiSstDetectorBuilder1::useVMCGeometry() -I- Use VMC geometry" << endm;

   // Define silicon material used in manual construction of sensitive layers in this builder
   const TGeoMaterial* geoMat = gGeoManager->GetMaterial("SILICON");

   StiMaterial* silicon = geoMat ? add(new StiMaterial(geoMat->GetName(), geoMat->GetZ(), geoMat->GetA(), geoMat->GetDensity(), geoMat->GetRadLen()))
                                 : add(new StiMaterial("SILICON", 14, 28.0855, 2.33, 9.36) );

   // Build active sti volumes for SST sensors
   int iSensor = floor(kSstNumSensorsPerLadder/2);

   for (int iLadder = 1; iLadder <= kSstNumLadders; ++iLadder)
   {
      std::ostringstream geoPath;
      geoPath << "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/SFMO_1/SFLM_" << iLadder << "/SFSW_" << iSensor << "/SFSL_1/SFSD_1";

      bool isAvail = gGeoManager->cd(geoPath.str().c_str());

      if (!isAvail) {
         Warning("useVMCGeometry()", "Cannot find path to SFSD (SST sensitive) node. Skipping to next ladder...");
         continue;
      }

      TGeoVolume* sensorVol = gGeoManager->GetCurrentNode()->GetVolume();
      TGeoMatrix* sensorMatrix = 0;

      if (mBuildIdealGeom) {
         sensorMatrix = gGeoManager->MakePhysicalNode(geoPath.str().c_str())->GetMatrix();
      } else {
         sensorMatrix = (TGeoMatrix*) mSstDb->getHMatrixSensorOnGlobal(iLadder, iSensor);
      }

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
 * using the dimensions and other physical properties of the SST mother volume defined in the ROOT
 * TGeo geometry.
 *
 * \author Dmitri Smirnov
 */
void StiSstDetectorBuilder1::buildInactiveVolumes()
{
   // Build average inactive volumes
   const VolumeMap_t volumes[] = {
      {"SFMO", "Mother of the SST detector", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1", "", ""}
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

      // Manually modify dimensions of the mother volume
      if (std::string(volumes[i].name) == std::string("SFMO"))
         segmentSFMOVolume(stiDetector);
   }
}


/**
 * Manually modify the SST mother volume by splitting it into three tubes.
 *
 * The original SFMO volume has the following dimensions:
 *
 * Inner radius: 22.0 cm
 * Outer radius: 27.0 cm
 * Z edges at -50.5 +50.5 cm
 *
 * In Sti we split it into three tubes
 *
 * Inner radius: 23.25 cm
 * Outer radius: 27.0 cm
 * Z edges at -50.5, -34.0, +34.0, and +50.5 cm
 *
 * Note: The thickness is scaled by 0.75 leaving the outter radius the same.
 * This is done to avoid an overlap of the cylinders with the sensitive layers.
 *
 * The following material properties are calculated for the case when the inner
 * radius is 22 cm:
 *
 * Cylinder 0: Z=7.63150  A=15.2791  density=0.291833  X0=24675.4  weight=3706.24
 * Cylinder 1: Z=7.32555  A=14.6629  density=0.064873  X0=29477.9  weight=3395.38
 * Cylinder 2: Z=7.63471  A=15.2856  density=0.293828  X0=24633.0  weight=3731.59
 *
 * Note: The density is recalculated for the case when the inner radius is
 * increased.
 *
 * \author Dmitri Smirnov, BNL
 */
void StiSstDetectorBuilder1::segmentSFMOVolume(StiDetector* stiSFMO)
{
   StiCylindricalShape* stiSFMOShape = (StiCylindricalShape*) stiSFMO->getShape();

   // Reduce the thickness of the tube to avoid overlapping with the sensitive layers
   stiSFMOShape->setThickness(0.75*stiSFMOShape->getThickness());

   // Copy original shape (before its depth is modified) to a new one for the end SFMO tubes
   StiCylindricalShape* stiSFMOEndShape = new StiCylindricalShape(*stiSFMOShape);
   stiSFMOEndShape->setName(stiSFMOEndShape->getName() + "_end");
   stiSFMOEndShape->setHalfDepth(8.25); // 8.25 cm = (50.5-34)/2 cm

   add(stiSFMOEndShape);

   stiSFMOShape->setHalfDepth(34); // 34 cm

   // Redefine the material of the central tube
   StiMaterial* stiSFMOMaterial = stiSFMO->getMaterial();
   // The parameters provided by Jason W.: name, Z, A, density, X0
   stiSFMOMaterial->set(stiSFMOMaterial->getName(), 7.33, 14.66, 0.0843, 29480);

   // Define the material for the end tubes by creating a shalow copy from the original SFMO material
   StiMaterial* stiSFMOEndMaterial = new StiMaterial(*stiSFMOMaterial);
   stiSFMOEndMaterial->set(stiSFMOMaterial->getName() + "_end", 7.63, 15.29, 0.380, 24650);

   add(stiSFMOEndMaterial);

   // Create a shalow copy for end tube 1
   StiDetector* stiSFMOEnd = new StiDetector(*stiSFMO);

   StiPlacement* stiSFMOEndPlacement = new StiPlacement(*stiSFMOEnd->getPlacement());
   stiSFMOEndPlacement->setZcenter(stiSFMOShape->getHalfDepth() + stiSFMOEndShape->getHalfDepth());

   stiSFMOEnd->setName(stiSFMO->getName() + "_end1");
   stiSFMOEnd->setShape(stiSFMOEndShape);
   stiSFMOEnd->setPlacement(stiSFMOEndPlacement);
   stiSFMOEnd->setMaterial(stiSFMOEndMaterial);

   add(getNRows(), 0, stiSFMOEnd);

   // Create a shalow copy for end tube 2
   StiDetector* stiSFMOEnd2 = new StiDetector(*stiSFMOEnd);

   stiSFMOEndPlacement = new StiPlacement(*stiSFMOEnd2->getPlacement());
   stiSFMOEndPlacement->setZcenter(-stiSFMOShape->getHalfDepth() - stiSFMOEndShape->getHalfDepth());

   stiSFMOEnd2->setName(stiSFMO->getName() + "_end2");
   stiSFMOEnd2->setShape(stiSFMOEndShape);
   stiSFMOEnd2->setPlacement(stiSFMOEndPlacement);
   stiSFMOEnd2->setMaterial(stiSFMOEndMaterial);

   add(getNRows(), 0, stiSFMOEnd2);
}
