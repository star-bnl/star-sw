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
#include "StiSsd/StiSstDetectorBuilder.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "StiSsd/StiSsdIsActiveFunctor.h"
#include "StSsdUtil/StSstConsts.h"
#include "StDetectorDbMaker/StiSsdHitErrorCalculator.h"


StiSstDetectorBuilder::StiSstDetectorBuilder(bool active, bool buildIdealGeom)
   : StiDetectorBuilder("Ssd", active), mBuildIdealGeom(buildIdealGeom), mSstDb(0)
{
}


StiSstDetectorBuilder::~StiSstDetectorBuilder()
{}


/** Build the SST detector components. */
void StiSstDetectorBuilder::buildDetectors(StMaker &source)
{
   LOG_INFO << "StiSstDetectorBuilder::buildDetectors() - I - Started " << endm;
   //StSsdBarrel *mySsd = StSsdBarrel::Instance();

   if (!mBuildIdealGeom) {

      if (!gStSstDbMaker) {
         LOG_ERROR << "StiSstDetectorBuilder::buildDetectors: SST geometry was requested from "
            "DB but no StSstDb object found. Check for sstDb option in BFC chain" << endm;
         exit(EXIT_FAILURE);
      }

      mSstDb = (StSstDbMaker*) gStSstDbMaker;
      assert(mSstDb);

      LOG_INFO << "StiSstDetectorBuilder::buildDetectors: Will build SST geometry from DB tables" << endm;
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
void StiSstDetectorBuilder::useVMCGeometry()
{
   LOG_INFO << "StiSstDetectorBuilder::useVMCGeometry() -I- Use VMC geometry" << endm;

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

      if (_active) { stiDetector->setIsActive(new StiSsdIsActiveFunctor);}
      else         { stiDetector->setIsActive(new StiNeverActiveFunctor);}

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
void StiSstDetectorBuilder::buildInactiveVolumes()
{
}
