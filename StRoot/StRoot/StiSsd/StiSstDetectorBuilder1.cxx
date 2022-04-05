#include "StMessMgr.h"

#include "Sti/StiCylindricalShape.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetector.h"
#include "StiSsd/StiSstDetectorBuilder1.h"


StiSstDetectorBuilder1::StiSstDetectorBuilder1(bool active, bool buildIdealGeom)
   : StiSstDetectorBuilder(active, buildIdealGeom)
{
}


StiSstDetectorBuilder1::~StiSstDetectorBuilder1()
{}


/**
 * Creates a crude approximation of the SST detector. The geometry is modeled with three tubes with
 * modified dimensions and other physical properties of the SST mother volume defined in the ROOT
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
