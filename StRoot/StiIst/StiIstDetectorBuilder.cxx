#include <assert.h>
#include <sstream>
#include <string>

#include "TGeoVolume.h"
#include "TGeoMatrix.h"
#include "TVector3.h"

#include "StMessMgr.h"
#include "StThreeVectorD.hh"

#include "Sti/Base/Factory.h"
#include "Sti/StiPlanarShape.h"
#include "Sti/StiCylindricalShape.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetector.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiIsActiveFunctor.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "StiIst/StiIstIsActiveFunctor.h"
#include "StiIst/StiIstDetectorBuilder.h"
#include "tables/St_HitError_Table.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEnumerations.h"
#include "StEvent/StEventTypes.h"
#include "StDetectorDbMaker/StiIst1HitErrorCalculator.h"
#include "StIstDbMaker/StIstDb.h"
#include "StIstUtil/StIstConsts.h"
#include "StBFChain/StBFChain.h"


using namespace StIstConsts;


StiIstDetectorBuilder::StiIstDetectorBuilder(bool active, bool buildIdealGeom) :
   StiDetectorBuilder("Ist", active), mBuildIdealGeom(buildIdealGeom), mIstDb(0)
{
   setGroupId(kIstId);
}


void StiIstDetectorBuilder::buildDetectors(StMaker &source)
{
   LOG_INFO << "StiIstDetectorBuilder::buildDetectors() -I- Started " << endm;

   SetCurrentDetectorBuilder(this);

   if (!mBuildIdealGeom) {
      TObjectSet *istDbDataSet = (TObjectSet *) source.GetDataSet("ist_db");

      if (!istDbDataSet) {
         LOG_ERROR << "StiIstDetectorBuilder::buildDetectors: IST geometry was requested from "
                   "DB but no StIstDb object found. Check for istDb option in BFC chain" << endm;
         exit(EXIT_FAILURE);
      }

      mIstDb = (StIstDb *) istDbDataSet->GetObject();
      assert(mIstDb);
      LOG_INFO << "StiIstDetectorBuilder::buildDetectors: Will build IST geometry from DB tables" << endm;
   }

   // Gas material must be defined. Here we use air properties
   const TGeoMaterial* geoMat = gGeoManager->GetMaterial("AIR");

   _gasMat = geoMat ? add(new StiMaterial(geoMat->GetName(), geoMat->GetZ(), geoMat->GetA(), geoMat->GetDensity(), geoMat->GetRadLen()))
                    : add(new StiMaterial("AIR", 7.3, 14.61, 0.001205, 30420.));

   if (StiVMCToolKit::GetVMC()) {
      useVMCGeometry();
      buildInactiveVolumes();
   }
}


void StiIstDetectorBuilder::useVMCGeometry()
{
   LOG_INFO << "StiIstDetectorBuilder::buildDetectors() -I- Use VMC geometry" << endm;

   // Define silicon material used in manual construction of sensitive layers in this builder
   const TGeoMaterial* geoMat = gGeoManager->GetMaterial("SILICON");

   StiMaterial* silicon = geoMat ? add(new StiMaterial(geoMat->GetName(), geoMat->GetZ(), geoMat->GetA(), geoMat->GetDensity(), geoMat->GetRadLen()))
                                 : add(new StiMaterial("SILICON", 14, 28.0855, 2.33, 9.36) );

   // Build active sti volumes for SST sensors
   int stiRow = getNRows(); // Put all sensitive volumes in the same (and next available) Sti row
   // Use the "middle" sensor on the ladder to extract alignment corrections from DB
   int iSensor = floor(kIstNumSensorsPerLadder/2);

   for (int iLadder = 1; iLadder < kIstNumLadders; ++iLadder)
   {
      std::ostringstream geoPath;
      geoPath << "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1/IBAM_" << iLadder << "/IBLM_" << iSensor << "/IBSS_1";

      bool isAvail = gGeoManager->cd(geoPath.str().c_str());

      if (!isAvail) {
         Warning("useVMCGeometry()", "Cannot find path to IBSS (IST sensitive) node. Skipping to next ladder...");
         continue;
      }

      TGeoVolume* sensorVol = gGeoManager->GetCurrentNode()->GetVolume();
      TGeoMatrix* sensorMatrix = 0;

      if (mBuildIdealGeom) {
         sensorMatrix = gGeoManager->MakePhysicalNode(geoPath.str().c_str())->GetMatrix();
      } else {
         sensorMatrix = (TGeoMatrix*) mIstDb->getHMatrixSensorOnGlobal(iLadder, iSensor);
      }

      if (!sensorMatrix) {
         Warning("useVMCGeometry()", "Could not get IST sensor position matrix. Skipping to next ladder...");
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

      //IBSS shape : DX =1.9008cm ; DY = .015cm ; DZ = 3.765 cm
      double sensorLength = kIstNumSensorsPerLadder * (sensorBBox->GetDZ() + 0.10); // halfDepth + deadedge 0.16/2 + sensor gap 0.04/2
      StiShape *stiShape  = new StiPlanarShape(geoPath.str().c_str(), sensorLength, 2 * sensorBBox->GetDY(), sensorBBox->GetDX());

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
      stiDetector->setShape(stiShape);
      stiDetector->setPlacement(pPlacement);
      stiDetector->setGas(GetCurrentDetectorBuilder()->getGasMat());
      stiDetector->setMaterial(silicon);
      stiDetector->setHitErrorCalculator(StiIst1HitErrorCalculator::instance());

      if (_active) {  stiDetector->setIsActive(new StiIstIsActiveFunctor);}
      else         {  stiDetector->setIsActive(new StiNeverActiveFunctor);}

      // Adding detector, note that no keys are set in IST!
      add(stiRow, iLadder-1, stiDetector);

      // Whole bunch of debugging information
      Float_t rad2deg = 180.0 / 3.1415927;
      LOG_DEBUG << "===>NEW:IST:stiDetector:Name             = " << stiDetector->getName()                     << endm
                << "===>NEW:IST:pPlacement:NormalRefAngle    = " << pPlacement->getNormalRefAngle()*rad2deg    << endm
                << "===>NEW:IST:pPlacement:NormalRadius      = " << pPlacement->getNormalRadius()              << endm
                << "===>NEW:IST:pPlacement:NormalYoffset     = " << pPlacement->getNormalYoffset()             << endm
                << "===>NEW:IST:pPlacement:CenterRefAngle    = " << pPlacement->getCenterRefAngle()*rad2deg    << endm
                << "===>NEW:IST:pPlacement:CenterRadius      = " << pPlacement->getCenterRadius()              << endm
                << "===>NEW:IST:pPlacement:CenterOrientation = " << pPlacement->getCenterOrientation()*rad2deg << endm
                << "===>NEW:IST:pPlacement:LayerRadius       = " << pPlacement->getLayerRadius()               << endm
                << "===>NEW:IST:pPlacement:LayerAngle        = " << pPlacement->getLayerAngle()*rad2deg        << endm
                << "===>NEW:IST:pPlacement:Zcenter           = " << pPlacement->getZcenter()                   << endm
                << "===>NEW:IST:stiDetector:Ladder           = " << iLadder                                    << endm
                << "===>NEW:IST:stiDetector:sensor           = " << iSensor                                    << endm
                << "===>NEW:IST:stiDetector:Active?          = " << stiDetector->isActive()                    << endm;
   }
}


/**
 * Creates a crude approximation of the IST detector. The geometry is modeled
 * with a single tube using the dimensions and other physical properties of the
 * IST mother volume defined in the ROOT TGeo geometry.
 */
void StiIstDetectorBuilder::buildInactiveVolumes()
{
}
