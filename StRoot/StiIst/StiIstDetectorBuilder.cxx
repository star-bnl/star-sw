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
#include "StiIstIsActiveFunctor.h"
#include "StiIstDetectorBuilder.h"
#include "Sti/StiElossCalculator.h"
#include "TDataSetIter.h"
#include "THashList.h"
#include "tables/St_HitError_Table.h"
#include "StEvent.h"
#include "StEventTypes.h"
#include "StDetectorDbMaker/StiIst1HitErrorCalculator.h"
#include "StIstDbMaker/StIstDbMaker.h"
#include "StIstUtil/StIstConsts.h"

using namespace std;


StiIstDetectorBuilder::StiIstDetectorBuilder(bool active, const string &inputFile)
   : StiDetectorBuilder("Ist", active, inputFile), mSiMaterial(0), mHybridMaterial(0), mIstDb(0)
{}


StiIstDetectorBuilder::~StiIstDetectorBuilder()
{
   mIstDb = 0;
}


void StiIstDetectorBuilder::buildDetectors(StMaker &source)
{
   LOG_INFO << "StiIstDetectorBuilder::buildDetectors() - I - Started " << endm;

   setNRows(1);
   mIstDb = (StIstDbMaker*) source.GetMaker("istDb");

   if (StiVMCToolKit::GetVMC()) { useVMCGeometry(); }
}


void StiIstDetectorBuilder::useVMCGeometry()
{
   cout << "StiIstDetectorBuilder::buildDetectors() -I- Use VMC geometry" << endl;

   unsigned int ROW      = 1;

   THashList *istRot = new THashList(144, 0);
   istRot = mIstDb->GetRotations();

   SetCurrentDetectorBuilder(this);

   // Build the volume map and loop over all found volumes
   const VolumeMap_t IstVolumes[] = {
      {"IBAT", "aluminum cooling tube volume",              "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICTC", "aluminum cooling tube center volume",       "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICTE", "aluminum cooling tube east volume",         "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICTW", "aluminum cooling tube west volume",         "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IBCW", "Novec 7200 cooling liquid volume",          "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IBCF", "carbon foam mother volume",                 "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICFC", "carbon foam volume east side",              "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICFD", "carbon foam volume west side",              "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICFB", "air groove box volume east side",           "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICFA", "air groove arc volume east side",           "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ILCC", "CFRPMix honeycomb mother volume",           "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICCU", "CFRPMix honeycomb volume east side",        "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICCD", "CFRPMix honeycomb volume west side",        "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ILST", "top carbon fiber skin mother volume",       "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISTC", "top carbon fiber skin volume east side",    "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISTH", "top carbon fiber skin volume west side",    "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ILSB", "bottom carbon fiber skin mother volume",    "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISBC", "bottom carbon fiber skin volume east side", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISBH", "bottom carbon fiber skin volume west side", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IBHT", "Kapton hybrid top mother volume",           "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IHTC", "Kapton hybrid east volume",                 "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IHTH", "Kapton hybrid west volume",                 "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IBHB", "Kapton hybrid bottom mother volume",        "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IHBC", "Kapton hybrid east volume",                 "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IHBH", "Kapton hybrid west volume",                 "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IBHC", "Kapton hybrid edge tub volume",             "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IHCA", "Kapton hybrid edge tub volume",             "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IBRB", "G10 T-board volume",                        "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IRSA", "Delrin T-board slot A volume",              "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IRSB", "Delrin T-board slot B volume",              "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IRSC", "Delrin T-board slot C volume",              "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IBTS", "slicon thermal sensor volume",              "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IECW", "PEEK CF 30 west end-cap mother volume",     "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICWT", "PEEK CF 30 west end-cap volume",            "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICWB", "PEEK CF 30 west end-cap volume",            "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IABW", "air groove west end-cap volume",            "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IAAW", "air groove west end-cap volume",            "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ITNW", "thread air hole northwest volume",          "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ITSW", "thread air hole southwest volume",          "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IESB", "thread air hole volume middle",             "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IESA", "thread air hole volume middle",             "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IESC", "thread air hole volume middle",             "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IECE", "Aluminum east end-cap volume",              "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICET", "aluminum east end-cap volume",              "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICEB", "aluminum east end-cap volume",              "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IABE", "air groove east end-cap volume",            "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IAAE", "air groove east end-cap volume",            "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ITNE", "thread air hole northeast volume",          "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ITSE", "thread air hole southeast volume",          "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IESE", "middle thread air hole volume",             "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IESS", "ground fixing thread air hole volume",      "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IBSR", "PEEK CF 30 support block mother volume",    "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISRH", "thread air hole volume",                    "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISRR", "PEEK CF 30 south side sub-volume",          "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISRB", "PEEK CF 30 north side sub-volume",          "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISRL", "PEEK CF 30 trapezoid sub-volume volume",    "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISRA", "PEEK CF 30 support block arc volume",       "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISRO", "PEEK CF 30 support block top-right volume", "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISRC", "PEEK CF 30 trapezoid volume",               "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISRS", "PEEK CF 30 small box volume",               "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISCM", "Aluminum screw mother volume",              "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISWS", "Aluminum screw washer volume",              "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISCH", "Aluminum screw hat volume",                 "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISCS", "Aluminum screw cylinder stud volume",       "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISCR", "Aluminum screw stud volume",                "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IBCJ", "west cooling loop mother volume",           "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICJR", "west cooling loop arc part volume",         "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICJS", "west cooling loop left tube volume",        "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICJN", "west cooling loop right tube volume",       "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICJA", "west cooling loop left cone volume",        "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICJB", "west cooling loop right cone volume",       "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICJU", "west cooling loop left connector volume",   "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICJD", "west cooling loop right connector volume",  "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICLS", "west cooling loop left liquid volume",      "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICLN", "west cooling loop right liquid volume",     "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISCA", "east short cable A volume",                 "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISCB", "east short cable B volume",                 "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISCC", "east short cable C volume",                 "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICLE", "east short tube liquid volume",             "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ISCT", "east short cooling tube mother volume",     "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICTJ", "east short cooling tube joint volume",      "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICTA", "east short cooling tube connector volume",  "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"ICTB", "east short cooling tube right volume",      "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IBLM", "mother silicon volume",                     "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IBTE", "inactive silicon volume top edge",          "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IBBE", "inactive silicon volume bottom edge",       "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IBLE", "inactive silicon volume left edge",         "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IBRE", "inactive silicon volume right edge",        "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IBAP", "inactive silicon volume APV chips",         "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""},
      {"IBSS", "active silicon volume",                     "HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1", "", ""}
   };

   // Build the material map
   struct Material_t {
      const Char_t *name;
      StiMaterial    **p;
   };
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
         mSiMaterial->getA(),
         mSiMaterial->getZ(),
         mSiMaterial->getDensity());


   for (unsigned int ladderIdx = 0; ladderIdx < kIstNumLadders; ++ladderIdx)
   {
      for (unsigned int sensorIdx = 0; sensorIdx < kIstNumSensorsPerLadder; sensorIdx++)
      {
         unsigned int matIst = 1000 + (ladderIdx) * 6 + (sensorIdx + 1);
         LOG_DEBUG << " ladderIdx/sensorIdx/matIst : " << ladderIdx << " " << sensorIdx << " " << matIst << endm;
         TGeoHMatrix *combI = (TGeoHMatrix *)istRot->FindObject(Form("R%04i", matIst));

         if (combI) {
            combI->Print();
         }

         //jb added
         if (sensorIdx != 0)continue;

         //we place the ladder as a whole
         char name[50];
         sprintf(name, "Ist/Ladder_%d/Sensor_%d", ladderIdx + 1, sensorIdx + 1);

         TString Path(IstVolumes[71].path);
         Path += Form("/IBAM_%d/IBLM_%d/IBSS_1", ladderIdx + 1, sensorIdx + 1);

         gGeoManager->cd(Path); // retrieve info of IBSS volume
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
         LOG_DEBUG << "===>NEW:IST:pDetector:Name               = " << p->getName()                               << endm;
         LOG_DEBUG << "===>NEW:IST:pPlacement:NormalRefAngle    = " << pPlacement->getNormalRefAngle()*rad2deg    << endm;
         LOG_DEBUG << "===>NEW:IST:pPlacement:NormalRadius      = " << pPlacement->getNormalRadius()              << endm;
         LOG_DEBUG << "===>NEW:IST:pPlacement:NormalYoffset     = " << pPlacement->getNormalYoffset()             << endm;
         LOG_DEBUG << "===>NEW:IST:pPlacement:CenterRefAngle    = " << pPlacement->getCenterRefAngle()*rad2deg    << endm;
         LOG_DEBUG << "===>NEW:IST:pPlacement:CenterRadius      = " << pPlacement->getCenterRadius()              << endm;
         LOG_DEBUG << "===>NEW:IST:pPlacement:CenterOrientation = " << pPlacement->getCenterOrientation()*rad2deg << endm;
         LOG_DEBUG << "===>NEW:IST:pPlacement:LayerRadius       = " << pPlacement->getLayerRadius()               << endm;
         LOG_DEBUG << "===>NEW:IST:pPlacement:LayerAngle        = " << pPlacement->getLayerAngle()*rad2deg        << endm;
         LOG_DEBUG << "===>NEW:IST:pPlacement:Zcenter           = " << pPlacement->getZcenter()                   << endm;
         LOG_DEBUG << "===>NEW:IST:pDetector:Ladder             = " << ladderIdx + 1                                << endm;
         LOG_DEBUG << "===>NEW:IST:pDetector:Sensor             = " << sensorIdx + 1                                << endm;
         LOG_DEBUG << "===>NEW:IST:pDetector:row/ladder (ITTF)  = " << ROW << " / " << ladderIdx + 1                 << endm;
         LOG_DEBUG << "===>NEW:IST:pDetector:Active?            = " << p->isActive()                              << endm;
      }
   }

   // Build the volume map and loop over all found volumes
   Int_t NoIstVols = sizeof(IstVolumes) / sizeof(VolumeMap_t);
   gGeoManager->RestoreMasterVolume();
   gGeoManager->CdTop();

   for (Int_t i = 0; i < NoIstVols; i++) {
      gGeoManager->cd(IstVolumes[i].path);
      TGeoNode *nodeT = gGeoManager->GetCurrentNode();

      if (! nodeT) continue;;

      StiVMCToolKit::LoopOverNodes(nodeT, IstVolumes[i].path, IstVolumes[i].name, MakeAverageVolume);
   }
}
