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

   for (int iLadder = 1; iLadder <= kIstNumLadders; ++iLadder)
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
   // Prepare shapes for each ladder
   StiShape *digiBoardShape = new StiPlanarShape("IBAM_DIGI_BOARD",  5.830, 2*0.0823, 1.9355);
   StiShape *connectorShape = new StiPlanarShape("IBAM_CONNECTOR",   1.125, 2*0.3050, 3.0500);
   StiShape *cfBackingShape = new StiPlanarShape("IBAM_CF_BACKING", 27.800, 2*0.3050, 3.0500); // Carbon fiber backing

   // StiMaterial(const string &name, double z, double a, double density, double X0)
   StiMaterial* digiBoardMaterial     = new StiMaterial("IBAM_DIGI_BOARD",      9.01, 18.01,      1.70,   18.2086);
   StiMaterial* alumConnectorMaterial = new StiMaterial("IBAM_ALUM_CONNECTOR", 13.00, 26.98, 1.05*2.70,    8.8751);
   StiMaterial* gtenConnectorMaterial = new StiMaterial("IBAM_GTEN_CONNECTOR",  9.01, 18.02, 1.10*1.70,   18.2086);
   StiMaterial* cfBackingMaterial     = new StiMaterial("IBAM_CF_BACKING",      6.34, 12.71,      0.19, 1096.0000);

   // Volumes offsets
   TVector3 digiBoardOffset    (2.11591, -2.087862, -25.4911);
   TVector3 alumConnectorOffset(0.47779, -0.352065, -29.3221);
   TVector3 gtenConnectorOffset(0.47779, -0.352065,  28.8080);
   TVector3 cfBackingOffset    (0.47779, -0.352065,  -0.5141);

   // We use position of sensitive IST volumes to place new inactive volumes
   int stiRow = getNRows(); // Put volumes in the same (and next available) Sti row
   // Use the "middle" sensor on the ladder to extract alignment corrections from DB
   int iSensor = floor(kIstNumSensorsPerLadder/2);

   for (int iLadder = 1; iLadder <= kIstNumLadders; ++iLadder)
   {
      std::ostringstream geoPath, ssPfx;
      geoPath << "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1/IBAM_" << iLadder << "/IBLM_" << iSensor << "/IBSS_1";
      ssPfx   << "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1/IBAM_" << iLadder << "/";

      // Save first part of geoPath to reuse in new detector names
      std::string pfx(ssPfx.str());

      bool isAvail = gGeoManager->cd(geoPath.str().c_str());

      if (!isAvail) {
         Warning("useVMCGeometry()", "Cannot find path to IBSS (IST sensitive) node. Skipping to next ladder...");
         continue;
      }

      TGeoMatrix* transMatrix = 0;

      if (mBuildIdealGeom) {
         transMatrix = gGeoManager->MakePhysicalNode(geoPath.str().c_str())->GetMatrix();
      } else {
         transMatrix = (TGeoMatrix*) mIstDb->getHMatrixSensorOnGlobal(iLadder, iSensor);
      }

      if (!transMatrix) {
         Warning("useVMCGeometry()", "Could not get IST sensor position matrix. Skipping to next ladder...");
         continue;
      }

      StiPlacement *cfBackingPlacement = createPlacement(*transMatrix, cfBackingOffset);
      StiPlacement *alumConnectorPlacement = createPlacement(*transMatrix, alumConnectorOffset);
      StiPlacement *gtenConnectorPlacement = createPlacement(*transMatrix, gtenConnectorOffset);
      StiPlacement *digiBoardPlacement = createPlacement(*transMatrix, digiBoardOffset);

      StiDetector *stiDetector = getDetectorFactory()->getInstance();
      setDetectorProperties(stiDetector, pfx+"IBAM_CF_BACKING", new StiNeverActiveFunctor, cfBackingShape, cfBackingPlacement, getGasMat(), cfBackingMaterial);
      add(stiRow, iLadder-1, stiDetector);

      stiDetector = getDetectorFactory()->getInstance();
      setDetectorProperties(stiDetector, pfx+"IBAM_ALUM_CONNECTOR", new StiNeverActiveFunctor, connectorShape, alumConnectorPlacement, getGasMat(), alumConnectorMaterial);
      add(stiRow+1, iLadder-1, stiDetector);

      stiDetector = getDetectorFactory()->getInstance();
      setDetectorProperties(stiDetector, pfx+"IBAM_GTEN_CONNECTOR", new StiNeverActiveFunctor, connectorShape, gtenConnectorPlacement, getGasMat(), gtenConnectorMaterial);
      add(stiRow+2, iLadder-1, stiDetector);

      stiDetector = getDetectorFactory()->getInstance();
      setDetectorProperties(stiDetector, pfx+"IBAM_DIGI_BOARD", new StiNeverActiveFunctor, digiBoardShape, digiBoardPlacement, getGasMat(), digiBoardMaterial);
      add(stiRow+3, iLadder-1, stiDetector);
   }

   std::string pfx("/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1/");

   // Implement plastic brackets as a thin cylinder on east and west sides
   StiCylindricalShape* ibamBracketShape = new StiCylindricalShape("IBAM_BRACKET", 0.635, 0.2, 12, 2*M_PI);
   StiPlacement* ibamBracketEastPlacement = new StiPlacement(0, 11.9, 0,  29.34);
   StiPlacement* ibamBracketWestPlacement = new StiPlacement(0, 11.9, 0, -29.6);
   StiMaterial* ibamBracketMaterial = new StiMaterial("IBAM_BRACKET", 6.089, 12.149, 24*0.2601, 160);

   StiDetector* stiDetector = getDetectorFactory()->getInstance();
   setDetectorProperties(stiDetector, pfx+"IBAM_BRACKET_EAST", new StiNeverActiveFunctor, ibamBracketShape, ibamBracketEastPlacement, getGasMat(), ibamBracketMaterial);
   add(getNRows(), 0, stiDetector);

   stiDetector = getDetectorFactory()->getInstance();
   setDetectorProperties(stiDetector, pfx+"IBAM_BRACKET_WEST", new StiNeverActiveFunctor, ibamBracketShape, ibamBracketWestPlacement, getGasMat(), ibamBracketMaterial);
   add(getNRows(), 0, stiDetector);

   // Implement cooling line and cablings in transition area
   StiCylindricalShape* icctShape = new StiCylindricalShape("ICCT", 4.35720/2., 22.384-19.115, 22.384, 2*M_PI);
   StiPlacement* icctPlacement = new StiPlacement(0, (22.384+19.115)/2., 0, -57.4);
   StiMaterial* icctMaterial = new StiMaterial("ICCT", 26.8, 56.9, 0.673, 17.9);

   stiDetector = getDetectorFactory()->getInstance();
   setDetectorProperties(stiDetector, pfx+"ICCT", new StiNeverActiveFunctor, icctShape, icctPlacement, getGasMat(), icctMaterial);
   add(getNRows(), 0, stiDetector);
}


void StiIstDetectorBuilder::setDetectorProperties(StiDetector* detector, std::string name, StiIsActiveFunctor* activeFunctor, StiShape* shape, StiPlacement* placement, StiMaterial* gas, StiMaterial* material)
{
   if (!detector) return;

   detector->setName(name.c_str());
   detector->setIsActive(activeFunctor);
   detector->setShape(shape);
   detector->setPlacement(placement);
   detector->setGas(gas);
   detector->setMaterial(material);
}


StiPlacement* StiIstDetectorBuilder::createPlacement(const TGeoMatrix& transMatrix, const TVector3& localCenterOffset, const TVector3& normal)
{
   // Convert center of the geobox to coordinates in the global coordinate system
   double sensorXyzLocal[3]  = {localCenterOffset.X(), localCenterOffset.Y(), localCenterOffset.Z()};
   double sensorXyzGlobal[3] = {};

   double normalXyzLocal[3]  = {normal.X() + localCenterOffset.X(), normal.Y() + localCenterOffset.Y(), normal.Z() + localCenterOffset.Z()};
   double normalXyzGlobal[3] = {};

   transMatrix.LocalToMaster(sensorXyzLocal, sensorXyzGlobal);
   transMatrix.LocalToMaster(normalXyzLocal, normalXyzGlobal);

   TVector3 centralVec(sensorXyzGlobal);
   TVector3 normalVec(normalXyzGlobal);

   normalVec -= centralVec;

   if (normalVec.Dot(centralVec) < 0) normalVec *= -1;

   double deltaPhi = centralVec.DeltaPhi(normalVec);
   double normalVecMag = fabs(centralVec.Perp() * cos(deltaPhi));

   return new StiPlacement(normalVec.Phi(), normalVecMag, centralVec.Perp()*sin(deltaPhi), localCenterOffset.Z());
}
