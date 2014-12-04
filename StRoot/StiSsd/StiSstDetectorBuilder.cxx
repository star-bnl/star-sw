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
   setGroupId(kSsdId);
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
   // StiCylindricalShape(const string &name, float halfDepth_, float thickness_, float outerRadius_, float openingAngle_)
   StiCylindricalShape* sfmoCntrInnShape = new StiCylindricalShape("SFMO_CNTR_INN", fabs(34.25+34.25)/2., 23.5-23.2, 23.5, 2*M_PI);
   StiCylindricalShape* sfmoCntrMidShape = new StiCylindricalShape("SFMO_CNTR_MID", fabs(34.25+34.25)/2., 25.8-23.5, 25.8, 2*M_PI);
   StiCylindricalShape* sfmoCntrOutShape = new StiCylindricalShape("SFMO_CNTR_OUT", fabs(34.25+34.25)/2., 27.0-25.8, 27.0, 2*M_PI);

   StiCylindricalShape* sfmoLeftInnShape = new StiCylindricalShape("SFMO_LEFT_INN", fabs(51.50-34.25)/2., 23.5-22.2, 23.5, 2*M_PI);
   StiCylindricalShape* sfmoLeftMidShape = new StiCylindricalShape("SFMO_LEFT_MID", fabs(49.50-34.25)/2., 25.0-23.5, 25.0, 2*M_PI);
   StiCylindricalShape* sfmoLeftOutShape = new StiCylindricalShape("SFMO_LEFT_OUT", fabs(49.50-34.25)/2., 26.5-25.0, 26.5, 2*M_PI);

   StiCylindricalShape* sfmoRghtInnShape = new StiCylindricalShape("SFMO_RGHT_INN", fabs(51.50-34.25)/2., 23.5-22.2, 23.5, 2*M_PI);
   StiCylindricalShape* sfmoRghtMidShape = new StiCylindricalShape("SFMO_RGHT_MID", fabs(49.50-34.25)/2., 25.0-23.5, 25.0, 2*M_PI);
   StiCylindricalShape* sfmoRghtOutShape = new StiCylindricalShape("SFMO_RGHT_OUT", fabs(49.50-34.25)/2., 26.5-25.0, 26.5, 2*M_PI);

   // StiPlacement(float normRefAngle, float normRadius, float normYOffset, float centralZ, StiRegion region)
   StiPlacement* sfmoCntrInnPlacement = new StiPlacement(0, (23.5+23.2)/2., 0, (-34.25+34.25)/2.);
   StiPlacement* sfmoCntrMidPlacement = new StiPlacement(0, (25.8+23.5)/2., 0, (-34.25+34.25)/2.);
   StiPlacement* sfmoCntrOutPlacement = new StiPlacement(0, (27.0+25.8)/2., 0, (-34.25+34.25)/2.);

   StiPlacement* sfmoLeftInnPlacement = new StiPlacement(0, (23.5+22.2)/2., 0, (-51.50-34.25)/2.);
   StiPlacement* sfmoLeftMidPlacement = new StiPlacement(0, (25.0+23.5)/2., 0, (-49.50-34.25)/2.);
   StiPlacement* sfmoLeftOutPlacement = new StiPlacement(0, (26.5+25.0)/2., 0, (-49.50-34.25)/2.);

   StiPlacement* sfmoRghtInnPlacement = new StiPlacement(0, (23.5+22.2)/2., 0, (+51.50+34.25)/2.);
   StiPlacement* sfmoRghtMidPlacement = new StiPlacement(0, (25.0+23.5)/2., 0, (+49.50+34.25)/2.);
   StiPlacement* sfmoRghtOutPlacement = new StiPlacement(0, (26.5+25.0)/2., 0, (+49.50+34.25)/2.);

   // StiMaterial(const string &name, double z, double a, double density, double X0)
   StiMaterial* sfmoCntrInnMaterial = new StiMaterial("SFMO_CNTR_INN", 7.38471, 14.7875, 0.7536000, 28128.1);
   StiMaterial* sfmoCntrMidMaterial = new StiMaterial("SFMO_CNTR_MID", 7.29364, 14.5971, 0.0147153, 30146.9);
   StiMaterial* sfmoCntrOutMaterial = new StiMaterial("SFMO_CNTR_OUT", 7.27831, 14.5655, 0.0372666, 29681.4);

   StiMaterial* sfmoLeftInnMaterial = new StiMaterial("SFMO_LEFT_INN", 7.90551, 15.8598, 0.5131150, 21711.5);
   StiMaterial* sfmoLeftMidMaterial = new StiMaterial("SFMO_LEFT_MID", 7.67447, 15.3544, 0.3013760, 23510.2);
   StiMaterial* sfmoLeftOutMaterial = new StiMaterial("SFMO_LEFT_OUT", 7.52669, 15.0602, 0.2235500, 25904.3);

   StiMaterial* sfmoRghtInnMaterial = new StiMaterial("SFMO_RGHT_INN", 7.92641, 15.9019, 0.5298270, 21402.6);
   StiMaterial* sfmoRghtMidMaterial = new StiMaterial("SFMO_RGHT_MID", 7.66531, 15.3361, 0.2953080, 23582.3);
   StiMaterial* sfmoRghtOutMaterial = new StiMaterial("SFMO_RGHT_OUT", 7.53069, 15.0682, 0.2273420, 25831.5);

   // Define a prefix for detector name for consistency with automatic TGeo to Sti conversion
   std::string pfx("/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/");

   StiDetector* stiDetector = getDetectorFactory()->getInstance();
   setDetectorProperties(stiDetector, pfx+"SFMO_CNTR_INN", new StiNeverActiveFunctor, sfmoCntrInnShape, sfmoCntrInnPlacement, getGasMat(), sfmoCntrInnMaterial);
   add(getNRows(), 0, stiDetector);

   stiDetector = getDetectorFactory()->getInstance();
   setDetectorProperties(stiDetector, pfx+"SFMO_CNTR_MID", new StiNeverActiveFunctor, sfmoCntrMidShape, sfmoCntrMidPlacement, getGasMat(), sfmoCntrMidMaterial);
   add(getNRows(), 0, stiDetector);

   stiDetector = getDetectorFactory()->getInstance();
   setDetectorProperties(stiDetector, pfx+"SFMO_CNTR_OUT", new StiNeverActiveFunctor, sfmoCntrOutShape, sfmoCntrOutPlacement, getGasMat(), sfmoCntrOutMaterial);
   add(getNRows(), 0, stiDetector);


   stiDetector = getDetectorFactory()->getInstance();
   setDetectorProperties(stiDetector, pfx+"SFMO_LEFT_INN", new StiNeverActiveFunctor, sfmoLeftInnShape, sfmoLeftInnPlacement, getGasMat(), sfmoLeftInnMaterial);
   add(getNRows(), 0, stiDetector);

   stiDetector = getDetectorFactory()->getInstance();
   setDetectorProperties(stiDetector, pfx+"SFMO_LEFT_MID", new StiNeverActiveFunctor, sfmoLeftMidShape, sfmoLeftMidPlacement, getGasMat(), sfmoLeftMidMaterial);
   add(getNRows(), 0, stiDetector);

   stiDetector = getDetectorFactory()->getInstance();
   setDetectorProperties(stiDetector, pfx+"SFMO_LEFT_OUT", new StiNeverActiveFunctor, sfmoLeftOutShape, sfmoLeftOutPlacement, getGasMat(), sfmoLeftOutMaterial);
   add(getNRows(), 0, stiDetector);


   stiDetector = getDetectorFactory()->getInstance();
   setDetectorProperties(stiDetector, pfx+"SFMO_RGHT_INN", new StiNeverActiveFunctor, sfmoRghtInnShape, sfmoRghtInnPlacement, getGasMat(), sfmoRghtInnMaterial);
   add(getNRows(), 0, stiDetector);

   stiDetector = getDetectorFactory()->getInstance();
   setDetectorProperties(stiDetector, pfx+"SFMO_RGHT_MID", new StiNeverActiveFunctor, sfmoRghtMidShape, sfmoRghtMidPlacement, getGasMat(), sfmoRghtMidMaterial);
   add(getNRows(), 0, stiDetector);

   stiDetector = getDetectorFactory()->getInstance();
   setDetectorProperties(stiDetector, pfx+"SFMO_RGHT_OUT", new StiNeverActiveFunctor, sfmoRghtOutShape, sfmoRghtOutPlacement, getGasMat(), sfmoRghtOutMaterial);
   add(getNRows(), 0, stiDetector);
}


void StiSstDetectorBuilder::setDetectorProperties(StiDetector* detector, std::string name, StiIsActiveFunctor* activeFunctor, StiShape* shape, StiPlacement* placement, StiMaterial* gas, StiMaterial* material)
{
   if (!detector) return;

   detector->setName(name.c_str());
   detector->setIsActive(activeFunctor);
   detector->setShape(shape);
   detector->setPlacement(placement);
   detector->setGas(gas);
   detector->setMaterial(material);
}
