/* $Id: StiPxlDetectorBuilder.cxx,v 1.43 2014/03/28 19:47:58 smirnovd Exp $ */

#include <stdio.h>
#include <stdexcept>

#include "TDataSetIter.h"
#include "THashList.h"
#include "TGeoVolume.h"
#include "TGeoMatrix.h"
#include "TVector3.h"

#include "Sti/StiPlanarShape.h"
#include "Sti/StiCylindricalShape.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetector.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "Sti/StiElossCalculator.h"
#include "StiPxlDetectorBuilder.h"
#include "StiPxlIsActiveFunctor.h"
#include "StiPxlHitErrorCalculator.h"
#include "tables/St_HitError_Table.h"
#include "StEvent.h"
#include "StEventTypes.h"
#include "StPxlDbMaker/StPxlDb.h"
#include "StPxlDbMaker/StPxlDbMaker.h"
#include "StPxlUtil/StPxlConstants.h"
#include "StBFChain/StBFChain.h"

using namespace std;


/**
 * Parameterized hit error calculator.  Given a track (dip, cross, pt, etc)
 * returns average error once you actually want to do tracking, the results
 * depend strongly on the numbers below.
 *
   numbering should be the following :
   hardware : sector ladder   ITTF : layer  ladder
   1      1                          1      0
   1      2                          1      1
   1      3                          1      2
   1      4                          0      0

   2      1                          1      3
   2      2                          1      4
   2      3                          1      5
   2      4                          0      1
   (...)
   10     1                          1     27
   10     2                          1     28
   10     3                          1     29
   10     4                          0     9
 */
StiPxlDetectorBuilder::StiPxlDetectorBuilder(bool active, const string &inputFile) :
   StiDetectorBuilder("Pixel", active, inputFile), mSiMaterial(0), mHybridMaterial(0), mPxlDb(0),
   mUseDbGeom(false), mGeomDebug(kNoDebug)
{
   StBFChain *chain = (StBFChain *) StMaker::GetChain();

   if (chain) {
      Info("StiPxlDetectorBuilder", "Valid global pointer to chain");

      if ( chain->GetOption("StiPxlSimpleBox") )
         mGeomDebug = kSimpleBoxes;
      else if ( chain->GetOption("StiPxlSimpleTube") )
         mGeomDebug = kSimpleTube;
   }
}


/** Build the pixel detector components. */
void StiPxlDetectorBuilder::buildDetectors(StMaker &source)
{
   LOG_INFO << "StiPxlDetectorBuilder::buildDetectors() -I- Started" << endm;

   // 2 real rows, but we have detector elements and support elements.
   setNRows(2);

   SetCurrentDetectorBuilder(this);

   TObjectSet *pxlDbDataSet = (TObjectSet*) source.GetDataSet("pxlDb");
   if (pxlDbDataSet) {
      mPxlDb = (StPxlDb*) pxlDbDataSet->GetObject();
      LOG_INFO << "buildDetectors : initialize geometry with DB tables" << endm;
   }

   if(mUseDbGeom && !mPxlDb) {
      LOG_ERROR << "buildDetectors : no pxlDb for Geometry" << endm;
      return;
   }

   if (StiVMCToolKit::GetVMC()) {

      switch (mGeomDebug) {
      case kSimpleBoxes:
         buildSimpleBoxes();
         break;
      case kSimpleTube:
         buildSimpleTube();
         break;
      default:
         useVMCGeometry();
         buildInactiveVolumes();
      }
   }
}


void StiPxlDetectorBuilder::useVMCGeometry()
{
   LOG_INFO << "StiPxlDetectorBuilder::useVMCGeometry() -I- Use VMC geometry" << endm;

   // Get Materials
   struct Material_t {
      const Char_t *name;
      StiMaterial    **p;
   };

   _gasMat    = add(new StiMaterial("PixelAir", 7.3,   14.61,     0.001205, 30420.*0.001205, 7.3 * 12.e-9));
   mSiMaterial = add(new StiMaterial("PixelSi",  14.,  28.0855,   2.33,     21.82,           14.*12.*1e-9) );
   mHybridMaterial = add(new StiMaterial("PixelHyb", 14.,  28.0855,   2.33,     21.82,           14.*12.*1e-9) );

   Material_t map[] = {
      {"AIR", &_gasMat},
      {"SILICON", &mSiMaterial},
      {"SILICON", &mHybridMaterial}
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

   double ionization = mSiMaterial->getIonization();

   StiElossCalculator *ElossCalculator = new StiElossCalculator(mSiMaterial->getZOverA(),
         ionization * ionization,
         mSiMaterial->getA(), mSiMaterial->getZ(), mSiMaterial->getDensity());

   // Build active sti volumes for pixel sensors
   for (int iSector = 1; iSector <= kNumberOfPxlSectors; ++iSector)
   {
      for (int iLadder = 1; iLadder <= kNumberOfPxlLaddersPerSector; ++iLadder)
      {
         for (int iSensor = 1; iSensor <= kNumberOfPxlSensorsPerLadder; iSensor++)
         {
            ostringstream geoPath;
            geoPath << "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1/PXLA_" << iSector << "/LADR_" << iLadder << "/PXSI_" << iSensor << "/PLAC_1";

            bool isAvail = gGeoManager->cd(geoPath.str().c_str());

            if (!isAvail) {
               Error("useVMCGeometry()", "Cannot find path to PLAC (pixel sensitive) node. Skipping to next node...");
               continue;
            }

            TGeoVolume* sensorVol = gGeoManager->GetCurrentNode()->GetVolume();
            TGeoMatrix* sensorMatrix = 0;

            if (mUseDbGeom) {
               sensorMatrix = (TGeoMatrix*) mPxlDb->geoHMatrixSensorOnGlobal(iSector, iLadder, iSensor);
            } else {
               sensorMatrix = gGeoManager->GetCurrentMatrix();
            }

            if (!sensorMatrix) {
               Error("useVMCGeometry()", "Could not get pixel sensor position matrix. Skipping to next pixel sensor volume");
               continue;
            }

            // Convert origin (0, 0, 0) of the sensor geobox to coordinates in the global coordinate system
            double sensorXyzLocal[3]  = {};
            double sensorXyzGlobal[3] = {};

            sensorMatrix->LocalToMaster(sensorXyzLocal, sensorXyzGlobal);

            TVector3 sensorVec(sensorXyzGlobal);

            // Build global rotation for the sensor
            TGeoRotation sensorRot(*sensorMatrix);

            TGeoBBox *sensorBBox = (TGeoBBox*) sensorVol->GetShape();

            char name[50];
            sprintf(name, "Pixel/Sector_%d/Ladder_%d/Sensor_%d", iSector, iLadder, iSensor);
            LOG_DEBUG << " weigh/daughters/Material/A/Z : " << sensorVol->Weight() << " "
                      << sensorVol->GetNdaughters() << " " << sensorVol->GetMaterial()->GetName() << " "
                      << sensorVol->GetMaterial()->GetA() << " " << sensorVol->GetMaterial()->GetZ() << endm;
            LOG_DEBUG << " DZ/DY/DX : " << sensorBBox->GetDZ() << "/" << sensorBBox->GetDY() << "/" << sensorBBox->GetDX() << endm;

            // Create new Sti shape based on the sensor geometry
            StiShape *stiShape = new StiPlanarShape(name, sensorBBox->GetDZ(), sensorBBox->GetDY(), sensorBBox->GetDX());

            add(stiShape);

            Double_t phi  = sensorVec.Phi();
            Double_t phiD = sensorRot.GetPhiRotation()/180*M_PI;
            Double_t r    = sensorVec.Perp();
            double normVecMag = fabs(r*sin(phi - phiD));

            TVector3 normVec(cos(phiD + M_PI_2), sin(phiD + M_PI_2), 0);

            if (normVec.Dot(sensorVec) < 0) normVec *= -normVecMag;
            else                            normVec *=  normVecMag;

            // Volume positioning
            StiPlacement *pPlacement = new StiPlacement();

            pPlacement->setZcenter(sensorVec.Z());
            pPlacement->setLayerRadius(r);
            pPlacement->setLayerAngle(phi);
            pPlacement->setRegion(StiPlacement::kMidRapidity);
            double centerOrient = sensorVec.Phi() - normVec.Phi();
            pPlacement->setNormalRep(normVec.Phi(), normVecMag, r*sin(centerOrient));

            // Build final detector object
            StiDetector *stiDetector = getDetectorFactory()->getInstance();

            if ( !stiDetector ) {
               Error("useVMCGeometry()", "Failed to create a valid Sti detector. Skipping to next pixel sensor volume");
               continue;
            }

            stiDetector->setName(name);
            stiDetector->setIsOn(kTRUE);
            stiDetector->setIsActive(new StiPxlIsActiveFunctor);
            stiDetector->setIsContinuousMedium(false); // XXX:ds: should it be true?
            stiDetector->setIsDiscreteScatterer(true); // false?
            stiDetector->setGroupId(kPxlId);
            stiDetector->setShape(stiShape);
            stiDetector->setPlacement(pPlacement);
            stiDetector->setGas(GetCurrentDetectorBuilder()->getGasMat());
            stiDetector->setMaterial(mSiMaterial);
            stiDetector->setElossCalculator(ElossCalculator);
            stiDetector->setHitErrorCalculator(StiPxlHitErrorCalculator::instance());

            int stiRow    = 0;
            int stiSensor = 0;

            // Add created sti pixel detector to the system
            // The numbering is:
            // ladder = 0-1- ...9 for inner layer --> stiRow =0
            // ladder = 0-1-2 for sector 0 of outer layer, then 3-4-5 for the second sector until 29 for the last sectro
            // ladder=1 is the inner ladder
            if (iLadder == 1) {
               stiRow = 0 ;
               stiSensor = (iSector-1) * kNumberOfPxlSensorsPerLadder + iSensor-1;
            } else {
               stiRow = 1;
               stiSensor = (iSector-1) * (kNumberOfPxlLaddersPerSector-1) * kNumberOfPxlSensorsPerLadder
                         + (iLadder-2) * kNumberOfPxlSensorsPerLadder + iSensor-1;
            }

            stiDetector->setKey(1, stiRow);
            stiDetector->setKey(2, stiSensor);
            add(stiRow, stiSensor, stiDetector);

            // Whole bunch of debugging information
            Float_t rad2deg = 180.0 / 3.1415927;
            LOG_DEBUG << "===>NEW:PIXEL:stiDetector:Name               = " << stiDetector->getName()                               << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pPlacement:NormalRefAngle    = " << pPlacement->getNormalRefAngle()*rad2deg    << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pPlacement:NormalRadius      = " << pPlacement->getNormalRadius()              << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pPlacement:NormalYoffset     = " << pPlacement->getNormalYoffset()             << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pPlacement:CenterRefAngle    = " << pPlacement->getCenterRefAngle()*rad2deg    << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pPlacement:CenterRadius      = " << pPlacement->getCenterRadius()              << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pPlacement:CenterOrientation = " << pPlacement->getCenterOrientation()*rad2deg << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pPlacement:LayerRadius       = " << pPlacement->getLayerRadius()               << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pPlacement:LayerAngle        = " << pPlacement->getLayerAngle()*rad2deg        << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pPlacement:Zcenter           = " << pPlacement->getZcenter()                   << endm;
            LOG_DEBUG << "===>NEW:PIXEL:stiDetector:sector             = " << iSector                                    << endm;
            LOG_DEBUG << "===>NEW:PIXEL:stiDetector:Ladder             = " << iLadder                                    << endm;
            LOG_DEBUG << "===>NEW:PIXEL:stiDetector:sensor             = " << iSensor                                    << endm;
            LOG_DEBUG << "===>NEW:PIXEL:stiDetector:stiRow/stiSensor (ITTF)  = " << stiRow << " / " << stiSensor                     << endm;
            LOG_DEBUG << "===>NEW:PIXEL:stiDetector:Active?            = " << stiDetector->isActive()                              << endm;
         }
      }
   }
}


/** Creates inactive sti volumes for the pixel support material. */
void StiPxlDetectorBuilder::buildInactiveVolumes()
{
   // Build average inactive volumes
   const VolumeMap_t pxlVolumes[] = {
      {"DTUH1", "Dtube part of pixel support",  "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/DTUH_1", "", ""},
      {"DTUH2", "Dtube part of pixel support",  "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/DTUH_2", "", ""},
      {"PSHA1", "Detail in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_1", "", ""},
      {"PSHC1", "Detail in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_1", "", ""},
      {"PSHE1", "Detail in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_1", "", ""},
      {"PSHG1", "Detail in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_1", "", ""},
      {"PSAL1", "Detail in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_1", "", ""},
      {"PSAK1", "Detail in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_1", "", ""},
      {"PSCL1", "Detail in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_1", "", ""},
      {"PSCK1", "Detail in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_1", "", ""},
      {"PSAB1", "Detail in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_1", "", ""},
      {"PSAE1", "Detail in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_1", "", ""},
      {"PSMD1", "Detail in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_1", "", ""},
      {"PSHA2", "Detail in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_2", "", ""},
      {"PSHC2", "Detail in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_2", "", ""},
      {"PSHE2", "Detail in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_2", "", ""},
      {"PSHG2", "Detail in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_2", "", ""},
      {"PSAL2", "Long tube in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_2", "", ""},
      {"PSAK2", "Short tube in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_2", "", ""},
      {"PSCL2", "Plane in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_2", "", ""},
      {"PSCK2", "Detail in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_2", "", ""},
      {"PSAB2", "Detail in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_2", "", ""},
      {"PSAE2", "Detail in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_2", "", ""},
      {"PSMD2", "Detail in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_2", "", ""}
   };

   int nPxlVolumes = sizeof(pxlVolumes) / sizeof(VolumeMap_t);
   LOG_DEBUG << " # of volume(s) : " << nPxlVolumes << endm;

   for (int i = 0; i < nPxlVolumes; i++) {
      TString path(pxlVolumes[i].path);

      gGeoManager->cd(path);
      TGeoNode *geoNode = gGeoManager->GetCurrentNode();

      if (!geoNode) continue;

      LOG_DEBUG << "Current node : " << i << "/" << nPxlVolumes << " path is : " << pxlVolumes[i].name << endm;
      LOG_DEBUG << "Number of daughters : " << geoNode->GetNdaughters() << " weight : " << geoNode->GetVolume()->Weight() << endm;
      StiVMCToolKit::LoopOverNodes(geoNode, path, pxlVolumes[i].name, MakeAverageVolume);
   }
}


/** Creates inactive sti volumes for the pixel support material. */
void StiPxlDetectorBuilder::buildSimpleBoxes()
{
   // Build average inactive volumes
   const VolumeMap_t pxlVolumes[] = {
      {"PSAL1", "Long tube in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_1", "", ""},
      {"PSAL2", "Long tube in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSUP_2", "", ""}
   };

   int nPxlVolumes = sizeof(pxlVolumes) / sizeof(VolumeMap_t);
   LOG_DEBUG << " # of volume(s) : " << nPxlVolumes << endm;

   for (int i = 0; i < nPxlVolumes; i++) {
      TString path(pxlVolumes[i].path);

      gGeoManager->cd(path);
      TGeoNode *geoNode = gGeoManager->GetCurrentNode();

      if (!geoNode) continue;

      LOG_DEBUG << "Current node : " << i << "/" << nPxlVolumes << " path is : " << pxlVolumes[i].name << endm;
      LOG_DEBUG << "Number of daughters : " << geoNode->GetNdaughters() << " weight : " << geoNode->GetVolume()->Weight() << endm;
      StiVMCToolKit::LoopOverNodes(geoNode, path, pxlVolumes[i].name, MakeAverageVolume);

      // Access just added volume
      int row = getNRows() - 1;
      int sector = 0;
      StiDetector *stiDetector = getDetector(row, sector);
      StiMaterial *mat = stiDetector->getMaterial();
      mat->set(mat->getName(), mat->getZ(), mat->getA(), mat->getDensity()*10, mat->getRadLength(), mat->getIonization());
   }
}


/** Creates inactive sti volumes for the pixel support material. */
void StiPxlDetectorBuilder::buildSimpleTube()
{
   // Build average inactive volumes
   const VolumeMap_t pxlVolumes[] = {
      {"DTUH1", "Dtube part of pixel support",  "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/DTUH_1", "", ""},
      {"DTUH2", "Dtube part of pixel support",  "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/DTUH_2", "", ""}
   };

   int nPxlVolumes = sizeof(pxlVolumes) / sizeof(VolumeMap_t);
   LOG_DEBUG << " # of volume(s) : " << nPxlVolumes << endm;

   for (int i = 0; i < nPxlVolumes; i++) {
      TString path(pxlVolumes[i].path);

      gGeoManager->cd(path);
      TGeoNode *geoNode = gGeoManager->GetCurrentNode();

      if (!geoNode) continue;

      LOG_DEBUG << "Current node : " << i << "/" << nPxlVolumes << " path is : " << pxlVolumes[i].name << endm;
      LOG_DEBUG << "Number of daughters : " << geoNode->GetNdaughters() << " weight : " << geoNode->GetVolume()->Weight() << endm;
      StiVMCToolKit::LoopOverNodes(geoNode, path, pxlVolumes[i].name, MakeAverageVolume);

      // Access just added volume
      int row = getNRows() - 1;
      int sector = 0;
      StiDetector *stiDetector = getDetector(row, sector);
      StiMaterial *mat = stiDetector->getMaterial();
      mat->set(mat->getName(), mat->getZ(), mat->getA(), mat->getDensity()*10, mat->getRadLength(), mat->getIonization());
   }
}
