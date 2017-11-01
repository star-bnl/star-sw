/* $Id: StiPxlDetectorBuilder.cxx,v 1.118 2017/11/01 21:13:14 smirnovd Exp $ */

#include <assert.h>
#include <sstream>
#include <string>

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
#include "StiPxl/StiPxlDetectorBuilder.h"
#include "StiPxl/StiPxlIsActiveFunctor.h"
#include "StiPxl/StiPxlHitErrorCalculator.h"
#include "tables/St_HitError_Table.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"
#include "StPxlDbMaker/StPxlDb.h"
#include "StPxlDbMaker/StPxlDbMaker.h"
#include "StPxlUtil/StPxlConstants.h"
#include "StBFChain/StBFChain.h"



/**
 * Builds an object to direct the construction of Sti detectors/volumes.
 *
 * \param active   Set to true when accounting for hits in active volumes or
 * false otherwise
 *
 * \param buildIdealGeom  Set to true (default) to ignore volume position
 * transformation stored in the survey DB tables
 */
StiPxlDetectorBuilder::StiPxlDetectorBuilder(bool active, bool buildIdealGeom) :
   StiDetectorBuilder("Pixel", active), mBuildIdealGeom(buildIdealGeom), mPxlDb(0)
{
   setGroupId(kPxlId);
}


/**
 * Creates all Sti volumes of the PXL detector.
 */
void StiPxlDetectorBuilder::buildDetectors(StMaker &source)
{
   if (!gGeoManager)
      throw runtime_error("StiPxlDetectorBuilder::StiPxlDetectorBuilder() "
         "- Cannot build Sti geometry due to missing global object of TGeoManager class. "
         "Make sure STAR geometry is properly loaded with BFC AgML option");

   SetCurrentDetectorBuilder(this);

   // Access the (survey) geometry if requested by the user
   if (!mBuildIdealGeom) {
      TObjectSet *pxlDbDataSet = (TObjectSet*) source.GetDataSet("pxl_db");

      if (!pxlDbDataSet) {
         LOG_ERROR << "StiPxlDetectorBuilder::buildDetectors() - PXL geometry was requested from "
            "DB but no StPxlDb object found. Check for pxlDb option in BFC chain" << endm;
         exit(EXIT_FAILURE);
      }

      mPxlDb = (StPxlDb*) pxlDbDataSet->GetObject();
      assert(mPxlDb);

      LOG_INFO << "StiPxlDetectorBuilder::buildDetectors() - Will build PXL geometry from DB tables" << endm;
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


/**
 * Builds active Sti volumes by creating Sti shapes corresponding to the sensors
 * of the PXL detector. The created Sti detectors are positioned using either
 * the ideal (ROOT's TGeo gGeoManager) or missaligned geometry from the STAR
 * database via object of the StPxlDb class.
 */
void StiPxlDetectorBuilder::useVMCGeometry()
{
   // Define silicon material used in manual construction of sensitive layers in this builder
   const TGeoMaterial* geoMat = gGeoManager->GetMaterial("SILICON");

   StiMaterial* silicon = geoMat ? add(new StiMaterial(geoMat->GetName(), geoMat->GetZ(), geoMat->GetA(), geoMat->GetDensity(), geoMat->GetRadLen()))
                                 : add(new StiMaterial("SILICON", 14, 28.0855, 2.33, 9.36) );

   // Use the "middle" sensor on the ladder to extract alignment corrections from DB
   int iSensor = floor(kNumberOfPxlSensorsPerLadder/2);

   for (int iSector = 1; iSector <= kNumberOfPxlSectors; ++iSector)
   {
      for (int iLadder = 1; iLadder <= kNumberOfPxlLaddersPerSector; ++iLadder)
      {
         std::string geoPath( formTGeoPath(iSector, iLadder, iSensor) );

         if ( geoPath.empty() ) {
            LOG_WARN << "StiPxlDetectorBuilder::useVMCGeometry() - Cannot find path to PLAC (PXL sensitive) node. Skipping to next ladder..." << endm;
            continue;
         }

         TGeoVolume* sensorVol = gGeoManager->GetCurrentNode()->GetVolume();
         TGeoHMatrix sensorMatrix( *gGeoManager->MakePhysicalNode(geoPath.c_str())->GetMatrix() );

         // Temporarily save the translation for this sensor in Z so, we can center
         // the newly built sensors at Z=0 (in ideal geometry) later
         double idealOffsetZ = sensorMatrix.GetTranslation()[2];

         if (!mBuildIdealGeom) {
            const TGeoHMatrix* sensorMatrixDb = mPxlDb->geoHMatrixSensorOnGlobal(iSector, iLadder, iSensor);

            if (!sensorMatrixDb) {
               LOG_WARN << "StiPxlDetectorBuilder::useVMCGeometry() - Cannot get PXL sensor position matrix. Skipping to next ladder..." << endm;
               continue;
            }

            sensorMatrix = *sensorMatrixDb;
         }

         // Update the global translation in Z so that the new volumes are centered at Z=0
         sensorMatrix.SetDz(sensorMatrix.GetTranslation()[2] - idealOffsetZ);

         TGeoBBox *sensorBBox = (TGeoBBox*) sensorVol->GetShape();

         // Split the ladder in two halves
         for (int iLadderHalf = 1; iLadderHalf <= 2; iLadderHalf++) {
            // Create new Sti shape based on the sensor geometry
            std::string halfLadderName(geoPath + (iLadderHalf == 1 ? "_HALF1" : "_HALF2") );
            double sensorLength = kNumberOfPxlSensorsPerLadder * (sensorBBox->GetDZ() + 0.02); // halfDepth + 0.02 ~= (dead edge + sensor gap)/2
            StiShape *stiShape = new StiPlanarShape(halfLadderName.c_str(), sensorLength, 2*sensorBBox->GetDY(), sensorBBox->GetDX()/2);

            TVector3 offset((iLadderHalf == 1 ? -sensorBBox->GetDX()/2 : sensorBBox->GetDX()/2), 0, 0);
            StiPlacement *pPlacement= new StiPlacement(sensorMatrix, offset);

            // Build final detector object
            StiDetector *stiDetector = getDetectorFactory()->getInstance();
            StiIsActiveFunctor* isActive = _active ?  new StiPxlIsActiveFunctor :
               static_cast<StiIsActiveFunctor*>(new StiNeverActiveFunctor);

            stiDetector->setProperties(halfLadderName, isActive, stiShape, pPlacement, getGasMat(), silicon);
            stiDetector->setHitErrorCalculator(StiPxlHitErrorCalculator::instance());

            // Convert geo sensor id to Sti indices. We do not check the validity
            // of returned values because the input is valid a priori in this case
            int stiRow, stiSensor;
            convertSensor2StiId(iSector, iLadder, iLadderHalf, stiRow, stiSensor);

            // Add created sensitive PXL layer to Sti
            add(stiRow, stiSensor, stiDetector);
         }
      }
   }
}


/**
 * Returns the active StiDetector corresponding to a sensitive layer in PXL. The
 * StiDetector is normally created by this StiDetectorBuilder and identified by
 * its sector, ladder, and sesortHalf id-s. An active volume can have hits
 * associated with it. The ladder id is expected to follow the human friendly
 * numbering scheme, i.e.
 *
 * <pre>
 * 1 <= sector <= kNumberOfPxlSectors
 * 1 <= ladder <= kNumberOfPxlLaddersPerSector
 * 1 <= sensorHalf <= 2
 * </pre>
 */
const StiDetector* StiPxlDetectorBuilder::getActiveDetector(int sector, int ladder, int sensorHalf) const
{
   int stiRow, stiSensor;
   convertSensor2StiId(sector, ladder, sensorHalf, stiRow, stiSensor);

   return stiRow < 0 ? 0 : getDetector(stiRow, stiSensor);
}


/**
 * Returns a full path to the pixel sensor placed in a predifined location in
 * the detector's ROOT geometry. An empty string is returned if the sensor not
 * found in the geometry hierarchy (via gGeoManager).
 */
std::string StiPxlDetectorBuilder::formTGeoPath(int sector, int ladder, int sensor)
{
   const std::string tgeoPathToMother("/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1");

   std::ostringstream geoPath;

   geoPath << tgeoPathToMother << "/PXLA_" << sector
                               << "/LADR_" << ladder
                               << "/PXSI_" << sensor
                               << "/PLAC_1";

   bool found = gGeoManager->cd( geoPath.str().c_str() );

   // Look for ladders staged in the mother volume
   if ( !found )
   {
      geoPath.str("");
      geoPath << tgeoPathToMother << "/LADR_" << 4*(sector - 1) + ladder
                                  << "/PXSI_" << sensor
                                  << "/PLAC_1";
      found = gGeoManager->cd( geoPath.str().c_str() );
   }

   // Look for sensors staged in the mother volume
   if ( !found )
   {
      geoPath.str("");
      geoPath << tgeoPathToMother << "/PXSI_" << 40*(sector - 1) + 10*(ladder - 1) + sensor
                                  << "/PLAC_1";
      found = gGeoManager->cd( geoPath.str().c_str() );
   }

   return found ? geoPath.str() : "";
}


/** Creates inactive sti volumes for the PXL support material. */
void StiPxlDetectorBuilder::buildInactiveVolumes()
{
   // Build average inactive volumes
   const VolumeMap_t pxlVolumes[] = {
      {"DTUH", "Dtube part of pixel support",  "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1", "", ""},

      // Planar components of the pixel sector support .../PSUP_\d/...
      {"PSHA", "Detail in half pixel support",     "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1", "", ""},
      {"PSHC", "Detail in half pixel support",     "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1", "", ""},
      {"PSHE", "Detail in half pixel support",     "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1", "", ""},
      {"PSHG", "Detail in half pixel support",     "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1", "", ""},
      {"PSAL", "Long tube in half pixel support",  "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1", "", ""},
      {"PSAK", "Short tube in half pixel support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1", "", ""},
      {"PSCL", "Plane in half pixel support",      "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1", "", ""},
      {"PSCK", "Detail in half pixel support",     "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1", "", ""},
      {"PSAB", "Detail in half pixel support",     "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1", "", ""},
      {"PSAE", "Detail in half pixel support",     "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1", "", ""},
      {"PSMD", "Detail in half pixel support",     "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1", "", ""},

      // The following are the largest planar components of the central pixel
      // sector support .../PXLA_\d/...
      {"PXRB", "Pixel sector support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1", "", ""},
      {"PXTR", "Pixel sector support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1", "", ""},
      {"PXTM", "Pixel sector support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1", "", ""},
      {"PXTL", "Pixel sector support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1", "", ""},
      {"PXLB", "Pixel sector support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1", "", ""},
      {"PXIB", "Pixel sector support", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1", "", ""},

      // Inactive material close to silicon layers
      { "DRIV", "Driver Board",         "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1", "", ""},
      { "GLUA", "Glu layer A",          "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1", "", ""},
      { "GLUB", "Glu layer B",          "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1", "", ""},
      { "GLUC", "Glu layer C",          "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1", "", ""},
      { "ALCA", "Aluminium cable",      "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1", "", ""},
      { "CFBK", "Carbon Fiber Backing", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1", "", ""},

      // These are the components of the pixel support tube (PSTM)
      { "APTS1", "Tube shell", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSTM_1/APTS_1", "", ""},
      { "PITN1", "Pixel insertion TubeNaked", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PSTM_1/PITN_1", "", ""}
   };

   int nPxlVolumes = sizeof(pxlVolumes) / sizeof(VolumeMap_t);

   for (int i = 0; i < nPxlVolumes; i++) {

      if (! gGeoManager->cd(pxlVolumes[i].path) ) {
         LOG_WARN << "StiPxlDetectorBuilder::buildInactiveVolumes() - Cannot find path to '"
                  << pxlVolumes[i].name << "' node. Skipping to next node..." << endm;
         continue;
      }

      TGeoNode *geoNode = gGeoManager->GetCurrentNode();

      if (!geoNode) continue;

      StiVMCToolKit::LoopOverNodes(geoNode, pxlVolumes[i].path, pxlVolumes[i].name, MakeAverageVolume);
   }
}


/**
 * Convert natural sensor id (sector/ladder/sensorHalf) to Sti indices stiRow
 * and stiSensor. If the input values are not within valid ranges unphisical
 * (i.e. negative) Sti indices returned.
 *
 * The numbering is:
 *
 * <pre>
 * sector ladder sensorHalf -> stiRow stiSensor
 *
 * 1      1      1          -> 0      0
 * 1      1      2          -> 1      0
 * 1      2      1          -> 2      0
 * 1      2      2          -> 3      0
 * 1      3      1          -> 2      1
 * 1      3      2          -> 3      1
 * 1      4      1          -> 2      2
 * 1      4      2          -> 3      2
 *
 * 2      1      1          -> 0      1
 * 2      1      2          -> 1      1
 * 2      2      1          -> 2      3
 * 2      2      2          -> 3      3
 * 2      3      1          -> 2      4
 * 2      3      2          -> 3      4
 * 2      4      1          -> 2      5
 * 2      4      2          -> 3      5
 *
 * ...
 *
 * </pre>
 */
void StiPxlDetectorBuilder::convertSensor2StiId(int sector, int ladder, int sensorHalf, int& stiRow, int& stiSensor)
{
   // Check validity of input values
   if (sector < 1 || sector > kNumberOfPxlSectors ||
       ladder < 1 || ladder > kNumberOfPxlLaddersPerSector ||
       sensorHalf < 1 || sensorHalf > 2)
   {
      stiRow = stiSensor = -1;
      return;
   }

   if (ladder == 1) {
      stiRow = sensorHalf == 1 ? 0 : 1;
      stiSensor = (sector - 1);
   } else { // ladder = 2, 3, 4
      stiRow = sensorHalf == 1 ? 2 : 3;
      stiSensor = (sector - 1) * (kNumberOfPxlLaddersPerSector - 1) + (ladder - 2);
   }
}
