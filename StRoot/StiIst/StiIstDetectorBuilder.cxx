#include <stdio.h>
#include <map>
#include <exception>
#include <stdexcept>

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
#include "Sti/StiElossCalculator.h"
#include "StiIst/StiIstIsActiveFunctor.h"
#include "StiIst/StiIstDetectorBuilder.h"
#include "TDataSetIter.h"
#include "THashList.h"
#include "tables/St_HitError_Table.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEnumerations.h"
#include "StEvent/StEventTypes.h"
#include "StDetectorDbMaker/StiIst1HitErrorCalculator.h"
#include "StIstDbMaker/StIstDbMaker.h"
#include "StIstUtil/StIstConsts.h"
#include "StBFChain/StBFChain.h"

using namespace std;
using namespace StIstConsts;


StiIstDetectorBuilder::StiIstDetectorBuilder(bool active, const string &inputFile, bool buildIdealGeom) :
   StiDetectorBuilder("Ist", active, inputFile), mSiMaterial(0), mHybridMaterial(0), mIstDb(0),
   mBuildIdealGeom(buildIdealGeom)
{ }


void StiIstDetectorBuilder::buildDetectors(StMaker &source)
{
   LOG_INFO << "StiIstDetectorBuilder::buildDetectors() -I- Started " << endm;

   setNRows(1);

   SetCurrentDetectorBuilder(this);

   // XXX:ds: Cannot rely on external maker! Must access DbMaker through
   // source.GetDataSet("istDb")
   if(!mBuildIdealGeom)
      mIstDb = (StIstDbMaker*) source.GetMaker("istDb");

   // Gas material must be defined. Here we use air properties
   _gasMat = add(new StiMaterial("PixelAir", 7.3, 14.61, 0.001205, 30420.*0.001205, 7.3 * 12.e-9));

   if (StiVMCToolKit::GetVMC()) {
      useVMCGeometry();
      buildInactiveVolumes();
   }
}


void StiIstDetectorBuilder::useVMCGeometry()
{
   cout << "StiIstDetectorBuilder::buildDetectors() -I- Use VMC geometry" << endl;

   unsigned int ROW = 1;

   THashList *istRot = 0;
   if(!mBuildIdealGeom)
      istRot = mIstDb->GetRotations();

   // Build the material map
   struct Material_t {
      const Char_t *name;
      StiMaterial    **p;
   };

   Material_t map[] = {
      {"AIR",     &_gasMat},
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


   for (int ladderIdx = 0; ladderIdx < kIstNumLadders; ++ladderIdx)
   {
      for (int sensorIdx = 0; sensorIdx < kIstNumSensorsPerLadder; sensorIdx++)
      {
         unsigned int matIst = 1000 + (ladderIdx) * kIstNumSensorsPerLadder + (sensorIdx + 1);
         LOG_DEBUG << "ladderIdx/sensorIdx/matIst : " << ladderIdx << " " << sensorIdx << " " << matIst << endm;

         char name[50];
         sprintf(name, "Ist/Ladder_%d/Sensor_%d", ladderIdx + 1, sensorIdx + 1);

         TString Path("HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1");
         Path += Form("/IBAM_%d/IBLM_%d/IBSS_1", ladderIdx + 1, sensorIdx + 1);
         gGeoManager->cd(Path); // retrieve info of IBSS volume

         TGeoHMatrix *combI = 0;
         if(!mBuildIdealGeom)
            combI = (TGeoHMatrix*) istRot->FindObject(Form("R%04i", matIst));
         else
            combI = gGeoManager->GetCurrentMatrix();

         if (combI) {
            combI->Print();
         } else {
            Error("useVMCGeometry()", "Could not find TGeoHMatrix for sensor %d in database", matIst);
            continue;
         }

         if (sensorIdx != 0) continue;

         TGeoNode *nodeT = gGeoManager->GetCurrentNode();
         // Extract volume geometry for this node
         TGeoBBox *box = (TGeoBBox *) nodeT->GetVolume()->GetShape();
         LOG_DEBUG << " DZ/DY/DX : " << box->GetDZ()
                   << " " << box->GetDY()
                   << " " << box->GetDX()
                   << " " << endm;

         //IBSS shape : DX =1.9008cm ; DY = .015cm ; DZ = 3.765 cm
         StiShape *sh  = new StiPlanarShape(name,
                                            kIstNumSensorsPerLadder * (box->GetDZ() + 0.10), // halfDepth + deadedge 0.16/2 + sensor gap 0.04/2
                                            2 * box->GetDY(),              // thickness
                                            box->GetDX());                 // halfWidth
         add(sh);

         Double_t     *xyz    = combI->GetTranslation();
         Double_t     *rot    = combI->GetRotationMatrix();
         StThreeVectorD centerVector(xyz[0], xyz[1], xyz[2]);
         StThreeVectorD normalVector(rot[1], rot[4], rot[7]);

         Double_t prod = centerVector * normalVector;

         if (prod < 0) normalVector *= -1;

         // Normalize normal vector, just in case....
         normalVector /= normalVector.magnitude();

         // Volume positioning
         StiPlacement *pPlacement = new StiPlacement;
         Double_t phi  = centerVector.phi();
         Double_t phiD = normalVector.phi();
         Double_t r    = centerVector.perp();
         pPlacement->setZcenter(0);
         pPlacement->setLayerRadius(r);

         pPlacement->setLayerAngle(phi);
         pPlacement->setRegion(StiPlacement::kMidRapidity);
         pPlacement->setNormalRep(phiD, r * TMath::Cos(phi - phiD), r * TMath::Sin(phi - phiD));
         assert(pPlacement);

         //Build final detector object
         StiDetector *p = getDetectorFactory()->getInstance();

         if ( !p ) {
            LOG_INFO << "StiIstDetectorBuilder::AverageVolume() -E- StiDetector pointer invalid." << endm;
            return;
         }

         p->setName(name);
         p->setIsOn(kTRUE);
         p->setIsActive(new StiIstIsActiveFunctor);

         p->setIsContinuousMedium(false);
         p->setIsDiscreteScatterer(true);
         p->setShape(sh);
         p->setPlacement(pPlacement);
         p->setGas(GetCurrentDetectorBuilder()->getGasMat());

         if (!p->getGas()) LOG_INFO << "gas not there!" << endm;

         p->setMaterial(mSiMaterial);
         p->setElossCalculator(ElossCalculator);
         p->setHitErrorCalculator(StiIst1HitErrorCalculator::instance());

         // Adding detector, note that no keys are set in IST!
         add(ROW, ladderIdx + 1, p);

         // Whole bunch of debugging information
         Float_t rad2deg = 180.0 / 3.1415927;
         LOG_DEBUG << "===>NEW:IST:pDetector:Name               = " << p->getName()                               << endm
                   << "===>NEW:IST:pPlacement:NormalRefAngle    = " << pPlacement->getNormalRefAngle()*rad2deg    << endm
                   << "===>NEW:IST:pPlacement:NormalRadius      = " << pPlacement->getNormalRadius()              << endm
                   << "===>NEW:IST:pPlacement:NormalYoffset     = " << pPlacement->getNormalYoffset()             << endm
                   << "===>NEW:IST:pPlacement:CenterRefAngle    = " << pPlacement->getCenterRefAngle()*rad2deg    << endm
                   << "===>NEW:IST:pPlacement:CenterRadius      = " << pPlacement->getCenterRadius()              << endm
                   << "===>NEW:IST:pPlacement:CenterOrientation = " << pPlacement->getCenterOrientation()*rad2deg << endm
                   << "===>NEW:IST:pPlacement:LayerRadius       = " << pPlacement->getLayerRadius()               << endm
                   << "===>NEW:IST:pPlacement:LayerAngle        = " << pPlacement->getLayerAngle()*rad2deg        << endm
                   << "===>NEW:IST:pPlacement:Zcenter           = " << pPlacement->getZcenter()                   << endm
                   << "===>NEW:IST:pDetector:Ladder             = " << ladderIdx + 1                              << endm
                   << "===>NEW:IST:pDetector:Sensor             = " << sensorIdx + 1                              << endm
                   << "===>NEW:IST:pDetector:row/ladder (ITTF)  = " << ROW << " / " << ladderIdx + 1              << endm
                   << "===>NEW:IST:pDetector:Active?            = " << p->isActive()                              << endm;
      }
   }
}


/**
 * Creates a crude approximation of the IST detector. The geometry is modeled
 * with a single tube using the dimensions and other physical properties of the
 * IST mother volume defined in the ROOT TGeo geometry.
 */
void StiIstDetectorBuilder::buildInactiveVolumes()
{
   // Build average inactive volumes
   const VolumeMap_t volumes[] = {
        {"IHTC", "Top Kapton hybrid east box volume",                   "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},//kapton hybrid
        {"IHTH", "Top Kapton hybrid west box volume",                   "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        {"IHBC", "Bottom Kapton hybrid east volume",                    "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        {"IHBH", "Bottom Kapton hybrid west volume",                    "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        //{"IBHC", "North Kapton hybrid edge tub volume",                       "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        {"ICFC", "carbon foam east box volume",                         "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},//carbon foam
        {"ICFD", "carbon foam west box volume",                         "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        {"ICCU", "CFRPMix honeycomb east box volume",                   "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},//carbon honeycomb
        {"ICCD", "CFRPMix honeycomb west box volume",                   "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        {"ISTC", "top carbon fiber skin east box volume",               "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},//top carbon skin
        {"ISTH", "top carbon fiber skin west box volume",               "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        {"ISBC", "bottom carbon fiber skin east box volume",            "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},//bottom carbon skin
        {"ISBH", "bottom carbon fiber skin west box volume",            "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        {"IECE", "east aluminum end-cap box volume",                    "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},//east AL end-cap
        {"IECW", "west PEEK CF 30 end-cap box volume",                  "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},//west carbon end-cap
        {"ICTC", "middle aluminum cooling tube volume",                 "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},//middle cooling tube
        {"ICTE", "middle aluminum cooling tube east cone volume",       "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        {"ICTW", "middle aluminum cooling tube west cone volume",       "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        {"IBCW", "middle Novec 7200 cooling liquid volume",             "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},//liquid coolant
        {"IBRB", "G10 T-board box volume",                              "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},//T-Board
        {"IRSA", "Delrin T-board slot A box volume",                    "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},//connectors
        {"IRSB", "Delrin T-board slot B box volume",                    "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        {"IRSC", "Delrin T-board slot C box volume",                    "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        {"IBTS", "Slicon thermal sensor box volume",                    "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},//thermal sensor
        {"IBAP", "inactive silicon volume APV chips",                   "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},//APV chips
        {"ISCA", "east short cable A volume",                           "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},//east short cables
        {"ISCB", "east short cable B volume",                           "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        {"ISCC", "east short cable C volume",                           "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        {"ICLE", "east short tube liquid volume",                       "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},//east liquid coolant
        {"ICTJ", "east short cooling tube joint volume",                "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},//east cooling tube joint volume
        {"ICTA", "east short cooling tube connector volume",            "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},//east cooling tube connector volume
        {"ICTB", "east short cooling tube right volume",                "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},//east cooling tube right volume
        {"ICJS", "west cooling loop left tube volume",                  "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        {"ICJA", "west cooling loop left cone volume",                  "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        {"ICJU", "west cooling loop left connector volume",             "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        {"ICLN", "west cooling loop right liquid volume",               "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        {"ISRR", "PEEK CF 30 support block south side sub-volume",      "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},//support block
        {"ISRB", "PEEK CF 30 support block north side sub-volume",      "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        {"ISRL", "PEEK CF 30 support block trapezoid sub-volume volume","HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        {"ISRO", "PEEK CF 30 support block top-right volume",           "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        {"ISRC", "PEEK CF 30 support block trapezoid volume",           "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        {"ISRS", "PEEK CF 30 support block small box volume",           "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1","",""},
        {"ICCT", "Cooling line and cablings in transition area",        "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1","",""},
        {"ICCA", "Copper cablings on PIT",                              "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1","",""},
        {"ICCL", "Al cooling lines on PIT",                             "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1","",""},
        {"ICLP", "Liquid coolant on PIT",                               "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1","",""}
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

        // Access last added volume
        int row = getNRows() - 1;
        int sector = 0;

        // Make Sti detector active, i.e. use it in tracking
        StiDetector *stiDetector = getDetector(row, sector);
        stiDetector->setIsOn(true);

        // Retrieve material, placement, energy loss information of carbon foam stave for re-definitions.
        // The loaded carbon foam volumes (ICFC and ICFD) are disabled in tracking and removed by zero volume.
        // The new carbon foam stave will consist of three simple box volumes to avoid overlap when place
        // cooling tube and coolant. This method have been applied to IST east and west end-caps.
        // Detailed information can be found at:
        // https://drupal.star.bnl.gov/STAR/blog/ypwang/ist-sti-geometry
        if (string(volumes[i].name) == string("ICFC"))
        {
            int startRow = getNRows() - kIstNumLadders;
            for(int iICFC=0; iICFC<kIstNumLadders; iICFC++) {
                row = startRow + iICFC;
                stiDetector = getDetector(row, sector);
                stiDetector->setIsOn(false);

                StiMaterial *matICFC                    = stiDetector->getMaterial();
                StiElossCalculator *elossCalculatorICFC = stiDetector->getElossCalculator();
                StiPlacement *stiPlacementICFC          = stiDetector->getPlacement();
                StiPlanarShape *stiShapeICFC            = (StiPlanarShape*) stiDetector->getShape();
                stiShapeICFC->setThickness(0);
                stiShapeICFC->setHalfWidth(0);
                stiShapeICFC->setHalfDepth(0);

                //construct carbon foam stave north side volume
                StiDetector *stiDetectorN = getDetectorFactory()->getInstance();
                buildPlanerVolume(*stiDetectorN, "ICFCn", 8.825*0.5, 0.5663, 1.25*0.5, 0.625, 0., 0., stiPlacementICFC, matICFC, elossCalculatorICFC);
                add(row, sector, stiDetectorN);
                LOG_DEBUG << "StiIstDetectorBuilder::build planar volume for ICFC north side " << stiDetectorN->getName() << " at layer " << row << endm;

                //construct carbon foam stave bottom side volume
                StiDetector *stiDetectorB = getDetectorFactory()->getInstance();
                buildPlanerVolume(*stiDetectorB, "ICFCb", 8.825*0.5, 0.042775, 0.47625*0.5, -0.238125, -0.2617625, 0., stiPlacementICFC, matICFC, elossCalculatorICFC);
                int layer = getNRows();
                add(layer, sector, stiDetectorB);
                LOG_DEBUG << "StiIstDetectorBuilder::build planar volume for ICFC bottom side " << stiDetectorB->getName() << " at layer " << layer << endm;

                //construct carbon foam stave south side volume
                StiDetector *stiDetectorS = getDetectorFactory()->getInstance();
                buildPlanerVolume(*stiDetectorS, "ICFCs", 8.825*0.5, 0.5663, 0.77375*0.5, -0.863125, 0., 0., stiPlacementICFC, matICFC, elossCalculatorICFC);
                layer = getNRows();
                add(layer, sector, stiDetectorS);
                LOG_DEBUG << "StiIstDetectorBuilder::build planar volume for ICFC south side " << stiDetectorS->getName() << " at layer " << layer << endm;

                matICFC                 = NULL;
                stiPlacementICFC        = NULL;
                elossCalculatorICFC     = NULL;
                stiShapeICFC            = NULL;
                stiDetector             = NULL;
           }
        }

        if (string(volumes[i].name) == string("ICFD"))
        {
            int startRow = getNRows() - kIstNumLadders;
            for(int iICFD=0; iICFD<kIstNumLadders; iICFD++) {
                row = startRow + iICFD;
                stiDetector = getDetector(row, sector);
                stiDetector->setIsOn(false);

                StiMaterial *matICFD                    = stiDetector->getMaterial();
                StiElossCalculator *elossCalculatorICFD = stiDetector->getElossCalculator();
                StiPlacement *stiPlacementICFD          = stiDetector->getPlacement();
                StiPlanarShape *stiShapeICFD            = (StiPlanarShape*) stiDetector->getShape();
                stiShapeICFD->setThickness(0);
                stiShapeICFD->setHalfWidth(0);
                stiShapeICFD->setHalfDepth(0);

                //construct carbon foam north side volume
                StiDetector *stiDetectorN = getDetectorFactory()->getInstance();
                buildPlanerVolume(*stiDetectorN, "ICFDn", 47.055*0.5, 0.58, 1.25*0.5, 0.625, 0., 0., stiPlacementICFD, matICFD, elossCalculatorICFD);
                add(row, sector, stiDetectorN);
                LOG_DEBUG << "StiIstDetectorBuilder::build planar volume for ICFD north side " << stiDetectorN->getName() << " at layer " << row << endm;

                //construct carbon foam bottom side volume
                StiDetector *stiDetectorB = getDetectorFactory()->getInstance();
                buildPlanerVolume(*stiDetectorB, "ICFDb", 47.055*0.5, 0.049675, 0.47625*0.5, -0.238125, -0.2651625, 0., stiPlacementICFD, matICFD, elossCalculatorICFD);
                int layer = getNRows();
                add(layer, sector, stiDetectorB);
                LOG_DEBUG << "StiIstDetectorBuilder::build planar volume for ICFD bottom side " << stiDetectorB->getName() << " at layer " << layer << endm;

                //construct carbon foam south side volume
                StiDetector *stiDetectorS = getDetectorFactory()->getInstance();
                buildPlanerVolume(*stiDetectorS, "ICFDs", 47.055*0.5, 0.58, 0.77375*0.5, -0.863125, 0., 0., stiPlacementICFD, matICFD, elossCalculatorICFD);
                layer = getNRows();
                add(layer, sector, stiDetectorS);
                LOG_DEBUG << "StiIstDetectorBuilder::build planar volume for ICFD south side " << stiDetectorS->getName() << " at layer " << layer << endm;

                matICFD                 = NULL;
                stiPlacementICFD        = NULL;
                elossCalculatorICFD     = NULL;
                stiShapeICFD            = NULL;
                stiDetector             = NULL;
           }
        }

        // Retrieve info. of east aluminum end-cap volume
        if (string(volumes[i].name) == string("IECE"))
        {
            int startRow = getNRows() - kIstNumLadders;
            for(int iIECE=0; iIECE<kIstNumLadders; iIECE++) {
                row = startRow + iIECE;
                stiDetector = getDetector(row, sector);
                stiDetector->setIsOn(false);

                StiMaterial *matIECE                    = stiDetector->getMaterial();
                StiElossCalculator *elossCalculatorIECE = stiDetector->getElossCalculator();
                StiPlacement *stiPlacementIECE          = stiDetector->getPlacement();
                StiPlanarShape *stiShapeIECE            = (StiPlanarShape*) stiDetector->getShape();
                stiShapeIECE->setThickness(0);
                stiShapeIECE->setHalfWidth(0);
                stiShapeIECE->setHalfDepth(0);

                //construct east end-cap north side volume
                StiDetector *stiDetectorN = getDetectorFactory()->getInstance();
                buildPlanerVolume(*stiDetectorN, "IECEn", 2.25*0.5, 0.5413, 1.23485*0.5, 2.4326, 0., 0., stiPlacementIECE, matIECE, elossCalculatorIECE);
                add(row, sector, stiDetectorN);
                LOG_DEBUG << "StiIstDetectorBuilder::build planar volume for IECE north side " << stiDetectorN->getName() << " at layer " << row << endm;

                //construct east end-cap bottom side volume
                StiDetector *stiDetectorB = getDetectorFactory()->getInstance();
                buildPlanerVolume(*stiDetectorB, "IECEb", 2.25*0.5, 0.0193, 0.5065*0.5, 1.5619, -0.261, 0., stiPlacementIECE, matIECE, elossCalculatorIECE);
                int layer = getNRows();
                add(layer, sector, stiDetectorB);
                LOG_DEBUG << "StiIstDetectorBuilder::build planar volume for IECE bottom side " << stiDetectorB->getName() << " at layer " << layer << endm;

                //construct east end-cap south side volume
                StiDetector *stiDetectorS = getDetectorFactory()->getInstance();
                buildPlanerVolume(*stiDetectorS, "IECEs", 2.25*0.5, 0.5413, 4.35865*0.5, -0.870675, 0., 0., stiPlacementIECE, matIECE, elossCalculatorIECE);
                layer = getNRows();
                add(layer, sector, stiDetectorS);
                LOG_DEBUG << "StiIstDetectorBuilder::build planar volume for IECE south side " << stiDetectorS->getName() << " at layer " << layer << endm;

                matIECE                 = NULL;
                stiPlacementIECE        = NULL;
                elossCalculatorIECE     = NULL;
                stiShapeIECE            = NULL;
                stiDetector             = NULL;
            }
      }

      // Modify dimensions of west carbon end-cap volume
      if (string(volumes[i].name) == string("IECW"))
      {
            int startRow = getNRows() - kIstNumLadders;
            for(int iIECW=0; iIECW<kIstNumLadders; iIECW++) {
                row = startRow + iIECW;
                stiDetector = getDetector(row, sector);
                stiDetector->setIsOn(false);

                StiMaterial *matIECW                    = stiDetector->getMaterial();
                StiElossCalculator *elossCalculatorIECW = stiDetector->getElossCalculator();
                StiPlacement *stiPlacementIECW          = stiDetector->getPlacement();
                StiPlanarShape *stiShapeIECW            = (StiPlanarShape*) stiDetector->getShape();
                stiShapeIECW->setThickness(0);
                stiShapeIECW->setHalfWidth(0);
                stiShapeIECW->setHalfDepth(0);

                //construct west end-cap north side volume
                StiDetector *stiDetectorN = getDetectorFactory()->getInstance();
                buildPlanerVolume(*stiDetectorN, "IECWn", 2.25*0.5, 0.555, 1.23485*0.5, 2.4326, 0., 0., stiPlacementIECW, matIECW, elossCalculatorIECW);
                add(row, sector, stiDetectorN);
                LOG_DEBUG << "StiIstDetectorBuilder::build planar volume for IECW north side " << stiDetectorN->getName() << " at layer " << row << endm;

                //construct west end-cap bottom side volume
                StiDetector *stiDetectorB = getDetectorFactory()->getInstance();
                buildPlanerVolume(*stiDetectorB, "IECWb", 2.25*0.5, 0.01925, 0.5065*0.5, 1.5619, -0.267875, 0., stiPlacementIECW, matIECW, elossCalculatorIECW);
                int layer = getNRows();
                add(layer, sector, stiDetectorB);
                LOG_DEBUG << "StiIstDetectorBuilder::build planar volume for IECW bottom side " << stiDetectorB->getName() << " at layer " << layer << endm;

                //construct west end-cap south side volume
                StiDetector *stiDetectorS = getDetectorFactory()->getInstance();
                buildPlanerVolume(*stiDetectorS, "IECWs", 2.25*0.5, 0.555, 4.35865*0.5, -0.870675, 0., 0., stiPlacementIECW, matIECW, elossCalculatorIECW);
                layer = getNRows();
                add(layer, sector, stiDetectorS);
                LOG_DEBUG << "StiIstDetectorBuilder::build planar volume for IECW south side " << stiDetectorS->getName() << " at layer " << layer << endm;

                matIECW                 = NULL;
                stiPlacementIECW        = NULL;
                elossCalculatorIECW     = NULL;
                stiShapeIECW            = NULL;
                stiDetector             = NULL;
            }
      }

      // Modify/Simplify west cooling loop (ICJR) to a box shape with same volume, and retrieve placement/material information from ICJS volume
      if (string(volumes[i].name) == string("ICJS"))
      {
            int startRow = getNRows() - kIstNumLadders;
            for(int iICJS=0; iICJS<kIstNumLadders;) {
                row = startRow + iICJS;
                stiDetector = getDetector(row, sector);
                StiMaterial *matICJS                    = stiDetector->getMaterial();
                StiElossCalculator *elossCalculatorICJS = stiDetector->getElossCalculator();
                StiPlacement *stiPlacementICJS1         = stiDetector->getPlacement();

                stiDetector = getDetector(row+1, sector);
                StiPlacement *stiPlacementICJS2         = stiDetector->getPlacement();

                StiDetector *stiDetectorICJR = getDetectorFactory()->getInstance();
                buildPlanerVolume(*stiDetectorICJR, "ICJRn", 0.524188*0.5, 0.47625, 4.41625*0.5, stiPlacementICJS2->getNormalYoffset(), 0., 0.524188*0.5+6.35*0.5, stiPlacementICJS1, matICJS, elossCalculatorICJS);
                int layer = getNRows();
                add(layer, sector, stiDetectorICJR);
                LOG_DEBUG << "StiIstDetectorBuilder::build west cooling loop volume " << stiDetectorICJR->getName() << " at layer " << layer << endm;

                iICJS += 2;
            }
      }

      //resize the dimenssion of cooling/cabling in transition area
      if (string(volumes[i].name) == string("ICCT"))
      {
          StiCylindricalShape *stiShape = (StiCylindricalShape*) stiDetector->getShape();
          stiShape->setOuterRadius(20.28725);
      }

      // retrieve material, energy loss information for support block tube ring re-definition
      if (string(volumes[i].name) == string("ISRR"))
      {
         if(!matISRA)
                matISRA = stiDetector->getMaterial();
      }
   }

   //StiElossCalculator for the support block inner thin tube volume
   StiElossCalculator *elossCalculatorISRA = new StiElossCalculator(matISRA->getZ()/matISRA->getA(), matISRA->getIonization()*matISRA->getIonization(), matISRA->getA(), matISRA->getZ(), matISRA->getDensity());

   //StiShape for the support block inner thin tube volume (as a whole for all 24 support blocks)
   float halfDepth = 0.5*1.27;
   float thickness = 0.15;
   float outerRadius = 12.0753;
   float openingAngle = 2*M_PI;

   //east support block inner thin tube volume
   StiDetector *stiDetectorISRAeast = getDetectorFactory()->getInstance();
   buildTubeVolume(*stiDetectorISRAeast, "ISRAeast", halfDepth, thickness, outerRadius, openingAngle, -34.19005+0.15875, matISRA, elossCalculatorISRA);
   int layer = getNRows();
   add(layer, 0, stiDetectorISRAeast);
   LOG_DEBUG << "StiIstDetectorBuilder::build east support block thin tube volume " << stiDetectorISRAeast->getName() << " at layer " << layer << endm;

   //west support block inner thin tube volume
   StiDetector *stiDetectorISRAwest = getDetectorFactory()->getInstance();
   buildTubeVolume(*stiDetectorISRAwest, "ISRAwest", halfDepth, thickness, outerRadius, openingAngle, 24.68995+0.15875, matISRA, elossCalculatorISRA);
   layer = getNRows();
   add(layer, 0, stiDetectorISRAwest);
   LOG_DEBUG << "StiIstDetectorBuilder::build west support block thin tube volume " << stiDetectorISRAwest->getName() << " at layer " << layer << endm;
}

void StiIstDetectorBuilder::buildPlanerVolume(StiDetector& detector, string detName, float halfDepth, float thickness, float halfWidth, float yShift, float rShift, float zShift, StiPlacement *placement, StiMaterial *mat, StiElossCalculator *elossCalculator)
{
   //planar shape definition
   string shapeName = detName + "_planar";
   StiPlanarShape *stiShapeN = new StiPlanarShape(shapeName.data(), halfDepth, thickness, halfWidth);
   add(stiShapeN);

   //plannar placement definition
   float yOffset = placement->getNormalYoffset() + yShift;
   float normalRadius = placement->getNormalRadius() + rShift;
   StiPlacement *stiPlacementN = new StiPlacement();
   stiPlacementN->setZcenter(placement->getZcenter() + zShift);
   stiPlacementN->setLayerRadius(sqrt(yOffset*yOffset + normalRadius*normalRadius));
   stiPlacementN->setLayerAngle(placement->getNormalRefAngle() - atan2(yOffset, normalRadius));
   stiPlacementN->setRegion(StiPlacement::kMidRapidity);
   stiPlacementN->setNormalRep(placement->getNormalRefAngle(), normalRadius, yOffset);

   //detector definition
   detector.setName(detName.data());
   detector.setIsOn(true);
   detector.setIsActive(new StiNeverActiveFunctor());
   detector.setIsContinuousMedium(false); // true for gases
   detector.setIsDiscreteScatterer(true); // true for anything other than gas
   detector.setShape(stiShapeN);
   detector.setPlacement(stiPlacementN);
   detector.setGas(getGasMat()); // XXX:ds: Not sure what this does
   detector.setMaterial(mat);
   detector.setElossCalculator(elossCalculator);
}

void StiIstDetectorBuilder::buildTubeVolume(StiDetector& detector, string detName, float halfDepth, float thickness, float outerRadius, float openingAngle, float zCenter, StiMaterial *mat, StiElossCalculator *elossCalculator)
{
   //tube shape definition
   string shapeName = detName + "_tube";
   StiShape *stiShapeN = new StiCylindricalShape(shapeName.data(), halfDepth, thickness, outerRadius, openingAngle);
   add(stiShapeN);

   //tube placement definition
   StiPlacement *stiPlacementN = new StiPlacement();
   stiPlacementN->setZcenter(zCenter);
   stiPlacementN->setLayerRadius(outerRadius-thickness/2);
   stiPlacementN->setLayerAngle(0);
   stiPlacementN->setRegion(StiPlacement::kMidRapidity);
   stiPlacementN->setNormalRep(0, outerRadius-thickness/2, 0);

   //detector definition
   detector.setName(detName.data());
   detector.setIsOn(true);
   detector.setIsActive(new StiNeverActiveFunctor());
   detector.setIsContinuousMedium(false); // true for gases
   detector.setIsDiscreteScatterer(true); // true for anything other than gas
   detector.setShape(stiShapeN);
   detector.setPlacement(stiPlacementN);
   detector.setGas(getGasMat()); // XXX:ds: Not sure what this does
   detector.setMaterial(mat);
   detector.setElossCalculator(elossCalculator);
}
