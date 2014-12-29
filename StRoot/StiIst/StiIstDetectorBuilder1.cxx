#include <assert.h>
#include <sstream>
#include <string>

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
#include "StiIst/StiIstDetectorBuilder1.h"
#include "TDataSetIter.h"
#include "THashList.h"
#include "TString.h"
#include "tables/St_HitError_Table.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEnumerations.h"
#include "StEvent/StEventTypes.h"
#include "StDetectorDbMaker/StiIst1HitErrorCalculator.h"
#include "StIstDbMaker/StIstDb.h"
#include "StIstUtil/StIstConsts.h"
#include "StBFChain/StBFChain.h"


using namespace StIstConsts;


StiIstDetectorBuilder1::StiIstDetectorBuilder1(bool active, bool buildIdealGeom) :
   StiIstDetectorBuilder(active, buildIdealGeom)
{
}


/**
 * Creates a crude approximation of the IST detector. The geometry is modeled
 * with a single tube using the dimensions and other physical properties of the
 * IST mother volume defined in the ROOT TGeo geometry.
 */
void StiIstDetectorBuilder1::buildInactiveVolumes()
{
   // Build average inactive volumes
   const VolumeMap_t volumes[] = {
      {"IHTC", "Top Kapton hybrid east box volume",                   "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""}, //kapton hybrid
      {"IHTH", "Top Kapton hybrid west box volume",                   "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IHBC", "Bottom Kapton hybrid east volume",                    "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IHBH", "Bottom Kapton hybrid west volume",                    "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      //{"IBHC", "North Kapton hybrid edge tub volume",               "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
      {"ICFC", "carbon foam east box volume",                         "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""}, //carbon foam
      {"ICFD", "carbon foam west box volume",                         "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICCU", "CFRPMix honeycomb east box volume",                   "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""}, //carbon honeycomb
      {"ICCD", "CFRPMix honeycomb west box volume",                   "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISTC", "top carbon fiber skin east box volume",               "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""}, //top carbon skin
      {"ISTH", "top carbon fiber skin west box volume",               "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISBC", "bottom carbon fiber skin east box volume",            "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""}, //bottom carbon skin
      {"ISBH", "bottom carbon fiber skin west box volume",            "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IECE", "east aluminum end-cap box volume",                    "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""}, //east AL end-cap
      {"IECW", "west PEEK CF 30 end-cap box volume",                  "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""}, //west carbon end-cap
      {"ICTC", "middle aluminum cooling tube volume",                 "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""}, //middle cooling tube
      {"ICTE", "middle aluminum cooling tube east cone volume",       "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICTW", "middle aluminum cooling tube west cone volume",       "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IBCW", "middle Novec 7200 cooling liquid volume",             "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""}, //liquid coolant
      {"IBRB", "G10 T-board box volume",                              "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""}, //T-Board
      {"IRSA", "Delrin T-board slot A box volume",                    "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""}, //connectors
      {"IRSB", "Delrin T-board slot B box volume",                    "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IRSC", "Delrin T-board slot C box volume",                    "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IBTS", "Slicon thermal sensor box volume",                    "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""}, //thermal sensor
      {"IBAP", "inactive silicon volume APV chips",                   "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""}, //APV chips
      {"ISCA", "east short cable A volume",                           "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""}, //east short cables
      {"ISCB", "east short cable B volume",                           "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISCC", "east short cable C volume",                           "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICLE", "east short tube liquid volume",                       "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""}, //east liquid coolant
      {"ICTJ", "east short cooling tube joint volume",                "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""}, //east cooling tube joint volume
      {"ICTA", "east short cooling tube connector volume",            "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""}, //east cooling tube connector volume
      {"ICTB", "east short cooling tube right volume",                "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""}, //east cooling tube right volume
      {"ICJS", "west cooling loop left tube volume",                  "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICJA", "west cooling loop left cone volume",                  "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICJU", "west cooling loop left connector volume",             "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICLN", "west cooling loop right liquid volume",               "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISRR", "PEEK CF 30 support block south side sub-volume",      "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""}, //support block
      {"ISRB", "PEEK CF 30 support block north side sub-volume",      "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISRL", "PEEK CF 30 support block trapezoid sub-volume volume","HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISRO", "PEEK CF 30 support block top-right volume",           "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISRC", "PEEK CF 30 support block trapezoid volume",           "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISRS", "PEEK CF 30 support block small box volume",           "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICCT", "Cooling line and cablings in transition area",        "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1"       , "", ""},
      {"ICCA", "Copper cablings on PIT",                              "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1"       , "", ""},
      {"ICCL", "Al cooling lines on PIT",                             "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1"       , "", ""},
      {"ICLP", "Liquid coolant on PIT",                               "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1"       , "", ""}
   };

   // Build the volume map and loop over all found volumes
   Int_t nVolumes = sizeof(volumes) / sizeof(VolumeMap_t);
   gGeoManager->RestoreMasterVolume();
   gGeoManager->CdTop();

   // material for support block inner thin tube volume (ISRA)
   StiMaterial *matISRA = NULL;

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
      TGeoVolume *gvolu = gGeoManager->FindVolumeFast(volumes[i].name);
      assert(gvolu);
      // Access last added volume
      int row = getNRows() - 1;
      int sector = 0;

      // Make Sti detector active, i.e. use it in tracking
      StiDetector *stiDetector = getDetector(row, sector);
      
      // Retrieve material, placement, energy loss information of carbon foam stave for re-definitions.
      // The loaded carbon foam volumes (ICFC and ICFD) are disabled in tracking and removed by zero volume.
      // The new carbon foam stave will consist of three simple box volumes to avoid overlap when place
      // cooling tube and coolant. This method have been applied to IST east and west end-caps.
      // Detailed information can be found at:
      // https://drupal.star.bnl.gov/STAR/blog/ypwang/ist-sti-geometry
      TString ts,dir;int idx;
      if (std::string(volumes[i].name) == std::string("ICFC")) {
         int startRow = getNRows() - kIstNumLadders;

         for (int iICFC = 0; iICFC < kIstNumLadders; iICFC++) {
            row = startRow + iICFC;
            stiDetector = getDetector(row, sector);
            assert(strstr(stiDetector->getName().c_str(),"/ICFC_"));
            //	prepare "directory"
            dir = stiDetector->getName().c_str();idx = dir.Last('/'); assert(idx>=0); dir.Remove(idx+1,999);

            StiMaterial *matICFC                    = stiDetector->getMaterial();
            StiPlacement *stiPlacementICFC          = stiDetector->getPlacement();

            // Construct carbon foam stave north side volume
            StiDetector *stiDetectorN = getDetectorFactory()->getInstance();
            ts = dir; ts +="ICFCn";
            buildPlanerVolume(*stiDetectorN, ts.Data(), 8.825 * 0.5, 0.5663, 1.25 * 0.5, 0.625, 0., 0., stiPlacementICFC, matICFC);
            del(row, sector);
            add(row, sector, stiDetectorN);
            LOG_DEBUG << "StiIstDetectorBuilder1::build planar volume for ICFC north side " << stiDetectorN->getName() << " at layer " << row << endm;

            // Construct carbon foam stave bottom side volume
            StiDetector *stiDetectorB = getDetectorFactory()->getInstance();
            ts = dir; ts +="ICFCb.";
            buildPlanerVolume(*stiDetectorB, ts.Data(), 8.825 * 0.5, 0.042775, 0.47625 * 0.5, -0.238125, -0.2617625, 0., stiPlacementICFC, matICFC);
            int layer = getNRows();
            add(layer, sector, stiDetectorB);
            LOG_DEBUG << "StiIstDetectorBuilder1::build planar volume for ICFC bottom side " << stiDetectorB->getName() << " at layer " << layer << endm;

            //construct carbon foam stave south side volume
            StiDetector *stiDetectorS = getDetectorFactory()->getInstance();
            ts = dir;; ts += "ICFCs";
            buildPlanerVolume(*stiDetectorS, ts.Data(), 8.825 * 0.5, 0.5663, 0.77375 * 0.5, -0.863125, 0., 0., stiPlacementICFC, matICFC);
            layer = getNRows();
            add(layer, sector, stiDetectorS);
            LOG_DEBUG << "StiIstDetectorBuilder1::build planar volume for ICFC south side " << stiDetectorS->getName() << " at layer " << layer << endm;
         }

      }

      if (std::string(volumes[i].name) == std::string("ICFD")) {
         int startRow = getNRows() - kIstNumLadders;

         for (int iICFD = 0; iICFD < kIstNumLadders; iICFD++) {
            row = startRow + iICFD;
            stiDetector = getDetector(row, sector);
            assert(strstr(stiDetector->getName().c_str(),"/ICFD_"));
//		prepare "directory"
            dir = stiDetector->getName().c_str();idx = dir.Last('/'); assert(idx>=0); dir.Remove(idx+1,999);

            StiMaterial *matICFD                    = stiDetector->getMaterial();
            StiPlacement *stiPlacementICFD          = stiDetector->getPlacement();

            //construct carbon foam north side volume
            StiDetector *stiDetectorN = getDetectorFactory()->getInstance();
            ts = dir; ts+="ICFDn"; 
            buildPlanerVolume(*stiDetectorN, ts.Data(), 47.055 * 0.5, 0.58, 1.25 * 0.5, 0.625, 0., 0., stiPlacementICFD, matICFD);
            del(row, sector);
            add(row, sector, stiDetectorN);
            LOG_DEBUG << "StiIstDetectorBuilder1::build planar volume for ICFD north side " << stiDetectorN->getName() << " at layer " << row << endm;

            //construct carbon foam bottom side volume
            StiDetector *stiDetectorB = getDetectorFactory()->getInstance();
            ts = dir; ts+="ICFDb";
            buildPlanerVolume(*stiDetectorB, ts.Data(), 47.055 * 0.5, 0.049675, 0.47625 * 0.5, -0.238125, -0.2651625, 0., stiPlacementICFD, matICFD);
            int layer = getNRows();
            add(layer, sector, stiDetectorB);
            LOG_DEBUG << "StiIstDetectorBuilder1::build planar volume for ICFD bottom side " << stiDetectorB->getName() << " at layer " << layer << endm;

            //construct carbon foam south side volume
            StiDetector *stiDetectorS = getDetectorFactory()->getInstance();
            ts=dir;ts+="ICFDs"; 
            buildPlanerVolume(*stiDetectorS, ts.Data(), 47.055 * 0.5, 0.58, 0.77375 * 0.5, -0.863125, 0., 0., stiPlacementICFD, matICFD);
            layer = getNRows();
            add(layer, sector, stiDetectorS);
            LOG_DEBUG << "StiIstDetectorBuilder1::build planar volume for ICFD south side " << stiDetectorS->getName() << " at layer " << layer << endm;
         }
      }

      // Retrieve info. of east aluminum end-cap volume
      if (std::string(volumes[i].name) == std::string("IECE")) {
         int startRow = getNRows() - kIstNumLadders;

         for (int iIECE = 0; iIECE < kIstNumLadders; iIECE++) {
            row = startRow + iIECE;
            stiDetector = getDetector(row, sector);
            assert(strstr(stiDetector->getName().c_str(),"/IECE_"));
//		prepare "directory"
            dir = stiDetector->getName().c_str();idx = dir.Last('/'); assert(idx>=0); dir.Remove(idx+1,999);


            StiMaterial *matIECE                    = stiDetector->getMaterial();
            StiPlacement *stiPlacementIECE          = stiDetector->getPlacement();

            //construct east end-cap north side volume
            StiDetector *stiDetectorN = getDetectorFactory()->getInstance();
            ts = dir; ts +="IECEn";
            buildPlanerVolume(*stiDetectorN, ts.Data(), 2.25 * 0.5, 0.5413, 1.23485 * 0.5, 2.4326, 0., 0., stiPlacementIECE, matIECE);
            del(row, sector);
            add(row, sector, stiDetectorN);
            LOG_DEBUG << "StiIstDetectorBuilder1::build planar volume for IECE north side " << stiDetectorN->getName() << " at layer " << row << endm;

            //construct east end-cap bottom side volume
            StiDetector *stiDetectorB = getDetectorFactory()->getInstance();
            ts = dir; ts +="IECEb";
            buildPlanerVolume(*stiDetectorB, ts.Data(), 2.25 * 0.5, 0.0193, 0.5065 * 0.5, 1.5619, -0.261, 0., stiPlacementIECE, matIECE);
            int layer = getNRows();
            add(layer, sector, stiDetectorB);
            LOG_DEBUG << "StiIstDetectorBuilder1::build planar volume for IECE bottom side " << stiDetectorB->getName() << " at layer " << layer << endm;

            //construct east end-cap south side volume
            StiDetector *stiDetectorS = getDetectorFactory()->getInstance();
            ts = dir; ts +="IECEs"; //ts+=iIECE;
            buildPlanerVolume(*stiDetectorS, ts.Data(), 2.25 * 0.5, 0.5413, 4.35865 * 0.5, -0.870675, 0., 0., stiPlacementIECE, matIECE);
            layer = getNRows();
            add(layer, sector, stiDetectorS);
            LOG_DEBUG << "StiIstDetectorBuilder1::build planar volume for IECE south side " << stiDetectorS->getName() << " at layer " << layer << endm;
         }
      }

      // Modify dimensions of west carbon end-cap volume
      if (std::string(volumes[i].name) == std::string("IECW")) {
         int startRow = getNRows() - kIstNumLadders;

         for (int iIECW = 0; iIECW < kIstNumLadders; iIECW++) {
            row = startRow + iIECW;
            stiDetector = getDetector(row, sector);
            assert(strstr(stiDetector->getName().c_str(),"/IECW_"));
//		prepare "directory"
            dir = stiDetector->getName().c_str();idx = dir.Last('/'); assert(idx>=0); dir.Remove(idx+1,999);


            StiMaterial *matIECW                    = stiDetector->getMaterial();
            StiPlacement *stiPlacementIECW          = stiDetector->getPlacement();

            //construct west end-cap north side volume
            StiDetector *stiDetectorN = getDetectorFactory()->getInstance();
            ts = dir; ts +="IECWn"; //ts+=iIECW;
            buildPlanerVolume(*stiDetectorN, ts.Data(), 2.25 * 0.5, 0.555, 1.23485 * 0.5, 2.4326, 0., 0., stiPlacementIECW, matIECW);
            del(row, sector);
            add(row, sector, stiDetectorN);
            LOG_DEBUG << "StiIstDetectorBuilder1::build planar volume for IECW north side " << stiDetectorN->getName() << " at layer " << row << endm;

            //construct west end-cap bottom side volume
            StiDetector *stiDetectorB = getDetectorFactory()->getInstance();

            ts = dir; ts +="IECWb"; //ts+=iIECW;
            buildPlanerVolume(*stiDetectorB, ts.Data(), 2.25 * 0.5, 0.01925, 0.5065 * 0.5, 1.5619, -0.267875, 0., stiPlacementIECW, matIECW);
            int layer = getNRows();
            add(layer, sector, stiDetectorB);
            LOG_DEBUG << "StiIstDetectorBuilder1::build planar volume for IECW bottom side " << stiDetectorB->getName() << " at layer " << layer << endm;

            //construct west end-cap south side volume
            StiDetector *stiDetectorS = getDetectorFactory()->getInstance();
            ts = dir; ts +="IECWs"; //ts+=iIECW;
            buildPlanerVolume(*stiDetectorS, ts.Data(), 2.25 * 0.5, 0.555, 4.35865 * 0.5, -0.870675, 0., 0., stiPlacementIECW, matIECW);
            layer = getNRows();
            add(layer, sector, stiDetectorS);
            LOG_DEBUG << "StiIstDetectorBuilder1::build planar volume for IECW south side " << stiDetectorS->getName() << " at layer " << layer << endm;
         }
      }

      // Modify/Simplify west cooling loop (ICJR) to a box shape with same volume, and retrieve placement/material information from ICJS volume
      if (std::string(volumes[i].name) == std::string("ICJS")) {
         int startRow = getNRows() - kIstNumLadders;

         for (int iICJS = 0; iICJS < kIstNumLadders;iICJS += 2) {
            row = startRow + iICJS;
            stiDetector = getDetector(row, sector);
            assert(strstr(stiDetector->getName().c_str(),"/ICJS_"));
//		prepare "directory"
            dir = stiDetector->getName().c_str();idx = dir.Last('/'); assert(idx>=0); dir.Remove(idx+1,999);

            StiMaterial *matICJS                    = stiDetector->getMaterial();
            StiPlacement *stiPlacementICJS1         = stiDetector->getPlacement();

            stiDetector = getDetector(row + 1, sector);
//            dir = stiDetector->getName().c_str();idx = dir.Last('/'); assert(idx>=0); dir.Remove(idx+1,999);
            StiPlacement *stiPlacementICJS2         = stiDetector->getPlacement();

            StiDetector *stiDetectorICJR = getDetectorFactory()->getInstance();
            ts = dir; ts +="ICJRn."; ts+=iICJS ;
            buildPlanerVolume(*stiDetectorICJR, ts.Data(), 0.524188 * 0.5, 0.47625, 4.41625 * 0.5, stiPlacementICJS2->getNormalYoffset(), 0., 0.524188 * 0.5 + 6.35 * 0.5, stiPlacementICJS1, matICJS);
            int layer = getNRows();
            add(layer, sector, stiDetectorICJR);
            LOG_DEBUG << "StiIstDetectorBuilder1::build west cooling loop volume " << stiDetectorICJR->getName() << " at layer " << layer << endm;

         }
      }

      //resize the dimenssion of cooling/cabling in transition area
      if (std::string(volumes[i].name) == std::string("ICCT")) {
         assert(strstr(stiDetector->getName().c_str(),"/ICCT_"));
         StiCylindricalShape *stiShape = (StiCylindricalShape *) stiDetector->getShape();
         stiShape->setOuterRadius(20.28725);
      }

      // retrieve material, energy loss information for support block tube ring re-definition
      if (std::string(volumes[i].name) == std::string("ISRR")) {
         assert(strstr(stiDetector->getName().c_str(),"/ISRR_"));
         if (!matISRA)
            matISRA = stiDetector->getMaterial();
      }
   }

   if (matISRA) {

      //StiShape for the support block inner thin tube volume (as a whole for all 24 support blocks)
      float halfDepth = 0.5 * 1.27;
      float thickness = 0.15;
      float outerRadius = 12.0753;
      float openingAngle = 2 * M_PI;

      //east support block inner thin tube volume
      StiDetector *stiDetectorISRAeast = getDetectorFactory()->getInstance();
      buildTubeVolume(*stiDetectorISRAeast, "ISRAeast", halfDepth, thickness, outerRadius, openingAngle, -34.19005 + 0.15875, matISRA);
      int layer = getNRows();
      add(layer, 0, stiDetectorISRAeast);
      LOG_DEBUG << "StiIstDetectorBuilder1::build east support block thin tube volume " << stiDetectorISRAeast->getName() << " at layer " << layer << endm;

      //west support block inner thin tube volume
      StiDetector *stiDetectorISRAwest = getDetectorFactory()->getInstance();
      buildTubeVolume(*stiDetectorISRAwest, "ISRAwest", halfDepth, thickness, outerRadius, openingAngle, 24.68995 + 0.15875, matISRA);
      layer = getNRows();
      add(layer, 0, stiDetectorISRAwest);
      LOG_DEBUG << "StiIstDetectorBuilder1::build west support block thin tube volume " << stiDetectorISRAwest->getName() << " at layer " << layer << endm;
   }



}


void StiIstDetectorBuilder1::buildPlanerVolume(StiDetector &detector, std::string detName, float halfDepth, float thickness, float halfWidth, float yShift, float rShift, float zShift, StiPlacement *placement, StiMaterial *mat)
{
   //planar shape definition
   std::string shapeName = detName + "_planar";
   StiPlanarShape *stiShapeN = new StiPlanarShape(shapeName.data(), halfDepth, thickness, halfWidth);
   add(stiShapeN);

   //plannar placement definition
   float yOffset = placement->getNormalYoffset() + yShift;
   float normalRadius = placement->getNormalRadius() + rShift;
   StiPlacement *stiPlacementN = new StiPlacement();
   stiPlacementN->setZcenter(placement->getZcenter() + zShift);
   stiPlacementN->setLayerRadius(sqrt(yOffset * yOffset + normalRadius * normalRadius));
   stiPlacementN->setLayerAngle(placement->getNormalRefAngle() - atan2(yOffset, normalRadius));
   stiPlacementN->setRegion(StiPlacement::kMidRapidity);
   stiPlacementN->setNormalRep(placement->getNormalRefAngle(), normalRadius, yOffset);

   //detector definition
   detector.setName(detName.data());
   detector.setIsActive(new StiNeverActiveFunctor());
   detector.setShape(stiShapeN);
   detector.setPlacement(stiPlacementN);
   detector.setGas(getGasMat());
   detector.setMaterial(mat);
}


void StiIstDetectorBuilder1::buildTubeVolume(StiDetector &detector, std::string detName, float halfDepth, float thickness, float outerRadius, float openingAngle, float zCenter, StiMaterial *mat)
{
   //tube shape definition
   std::string shapeName = detName + "_tube";
   StiShape *stiShapeN = new StiCylindricalShape(shapeName.data(), halfDepth, thickness, outerRadius, openingAngle);
   add(stiShapeN);

   //tube placement definition
   StiPlacement *stiPlacementN = new StiPlacement();
   stiPlacementN->setZcenter(zCenter);
   stiPlacementN->setLayerRadius(outerRadius - thickness / 2);
   stiPlacementN->setLayerAngle(0);
   stiPlacementN->setRegion(StiPlacement::kMidRapidity);
   stiPlacementN->setNormalRep(0, outerRadius - thickness / 2, 0);

   //detector definition
   detector.setName(detName.data());
   detector.setIsActive(new StiNeverActiveFunctor());
   detector.setShape(stiShapeN);
   detector.setPlacement(stiPlacementN);
   detector.setGas(getGasMat());
   detector.setMaterial(mat);
}
