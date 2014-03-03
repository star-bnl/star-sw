/* $Id: StiPxlDetectorBuilder.cxx,v 1.25 2014/03/03 23:54:22 smirnovd Exp $ */

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
   mUseDbGeom(false)
{}


/** Build the pixel detector components. */
void StiPxlDetectorBuilder::buildDetectors(StMaker &source)
{
   LOG_INFO << "StiPxlDetectorBuilder::buildDetectors() -I- Started" << endm;

   // 2 real rows, but we have detector elements and support elements.
   setNRows(2);

   TObjectSet *pxlDbDataSet = (TObjectSet*) source.GetDataSet("pxlDb");
   if (pxlDbDataSet) {
      mPxlDb = (StPxlDb*) pxlDbDataSet->GetObject();
      LOG_INFO << "buildDetectors : initialize geometry with DB tables" << endm;
   }

   if(mUseDbGeom && !mPxlDb) {
      LOG_ERROR << "buildDetectors : no pxlDb for Geometry" << endm;
      return;
   }

   if (StiVMCToolKit::GetVMC()) { useVMCGeometry(); }
}


void StiPxlDetectorBuilder::useVMCGeometry()
{
   LOG_INFO << "StiPxlDetectorBuilder::useVMCGeometry() -I- Use VMC geometry" << endm;

   //THashList *PxlRot = new THashList(400, 0);
   // XXX:ds At the moment gStPxlDbMaker is not defined in offline/hft/StRoot/StPxlDbMaker or
   // StRoot/
   //PxlRot = gStPxlDbMaker->GetRotations();

   SetCurrentDetectorBuilder(this);

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
   Bool_t  _TpcRefSys_1 = kFALSE;

   if (gGeoManager->GetVolume("TpcRefSys_1")) _TpcRefSys_1 = kTRUE;

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

   for (UInt_t iSector = 1; iSector <= kNumberOfPxlSectors; ++iSector)
   {
      for (UInt_t iLadder = 1; iLadder <= kNumberOfPxlLaddersPerSector; ++iLadder)
      {
         for (UInt_t iSensor = 1; iSensor <= kNumberOfPxlSensorsPerLadder; iSensor++)
         {
            ostringstream geoPath;
            geoPath << "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1/PXLA_" << iSector << "/LADR_" << iLadder << "/PXSI_" << iSensor << "/PLAC_1";

            gGeoManager->cd(geoPath.str().c_str());
            TGeoVolume* sensorVol = gGeoManager->GetCurrentNode()->GetVolume();

            if (!sensorVol) {
               Error("useVMCGeometry()", "sensorVol PLAC not found");
               continue;
            }

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
            //if (ActiveVolume) {
            //LOG_DEBUG << " current node : " << name << " is set active" <<endm;
            stiDetector->setIsActive(new StiPxlIsActiveFunctor);
            //}
            //else {
            //LOG_DEBUG << " current node : " << name << " is set inactive" <<endm;
            //stiDetector->setIsActive(new StiNeverActiveFunctor);
            //}
            //if(nameP.Contains("PXSI")) {layer=layer+10;}

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
            int stiSector = 0;

            /* numbering is :
               ladder = 0-1- ...9 for inner layer --> stiRow =0
               ladder = 0-1-2 for sector 0 of outer layer, then 3-4-5 for the second sector until 29 for the last sectro
               ladder=1 is the inner ladder
            */
            // update 05-15 : inner ladder is ladder 1
            if (iLadder == 1) {
               stiRow = 0 ;
               stiSector = iSector-1;
            } else {
               stiRow = 1;
               stiSector = (iSector-1) * 3 + (iLadder-1);
            }

            stiDetector->setKey(1, stiRow);
            stiDetector->setKey(2, stiSector);
            add(stiRow, stiSector, stiDetector);

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
            LOG_DEBUG << "===>NEW:PIXEL:stiDetector:stiRow/stiSector (ITTF)  = " << stiRow << " / " << stiSector                     << endm;
            LOG_DEBUG << "===>NEW:PIXEL:stiDetector:Active?            = " << stiDetector->isActive()                              << endm;
         }
      }
   }

   return;

   const VolumeMap_t PxlVolumes[] = {
      /*
      {"GLUA","Glu volume",                 "HALL_1/CAVE_1/TpcRefSys_1","",""},
      {"GLUB","Glu volume",                 "HALL_1/CAVE_1/TpcRefSys_1","",""},
      {"GLUC","Glu volume",                 "HALL_1/CAVE_1/TpcRefSys_1","",""},
      {"ALCA","Aluminum Cable volume",      "HALL_1/CAVE_1/TpcRefSys_1","",""},
      {"CFBK","Carbon Fiber BacKing volume","HALL_1/CAVE_1/TpcRefSys_1","",""},
      {"DRIV","Driver Board",               "HALL_1/CAVE_1/TpcRefSys_1","",""},
      {"PXSI","mother silicon volume",      "HALL_1/CAVE_1/TpcRefSys_1","",""},
      {"PLAC","Active silicon volume",      "HALL_1/CAVE_1/TpcRefSys_1","",""}
      */
      //average weight of a ladder
      {"PXLA", "PXL LADDER",                 "HALL_1/CAVE_1/TpcRefSys_1", "", ""}
   };

   Int_t NoPxlVols = sizeof(PxlVolumes) / sizeof(VolumeMap_t);
   LOG_DEBUG << " # of volume(s) : " << NoPxlVols << endm;
   gGeoManager->RestoreMasterVolume();
   gGeoManager->CdTop();

   for (Int_t i = 0; i < NoPxlVols; i++) {
      TString path(PxlVolumes[i].path);

      if (! _TpcRefSys_1) path.ReplaceAll("/TpcRefSys_1", "");

      gGeoManager->cd(path);
      TGeoNode *nodeT = gGeoManager->GetCurrentNode();

      if (! nodeT) continue;;

      LOG_DEBUG << " current node : " << i << "/" << NoPxlVols << " path is : " << PxlVolumes[i].name << endm;
      LOG_DEBUG << " # of daughters : " << nodeT->GetNdaughters() << " weight : " << nodeT->GetVolume()->Weight() << endm;
      StiVMCToolKit::LoopOverNodes(nodeT, path, PxlVolumes[i].name, MakeAverageVolume);
   }
}


/*
 * $Log: StiPxlDetectorBuilder.cxx,v $
 * Revision 1.25  2014/03/03 23:54:22  smirnovd
 * Renamed local variables according to STAR style guidelines
 *
 * Revision 1.24  2014/03/03 20:56:44  smirnovd
 * Updated error messages + minor clean up changes
 *
 * Revision 1.23  2014/03/03 20:56:39  smirnovd
 * Made conversion from sensor local to global in one step. Previously was sensor->ladder->sector->global
 *
 * Revision 1.22  2014/03/03 20:56:34  smirnovd
 * Simplified retrieval of sensor volumes from the geometry
 *
 * Revision 1.21  2014/03/03 20:56:30  smirnovd
 * Allow user to switch between the survey and ideal pixel geometries
 *
 * Revision 1.20  2014/03/03 20:56:24  smirnovd
 * Switch to detector volumes in TpcRefSys
 *
 * Revision 1.19  2014/03/03 20:56:17  smirnovd
 * Corrected Sti volume position
 *
 * Revision 1.18  2014/02/28 01:42:07  smirnovd
 * Specify detector group id
 *
 * Revision 1.17  2014/02/28 01:42:02  smirnovd
 * Update comments
 *
 * Revision 1.16  2014/02/28 01:41:57  smirnovd
 * Remove excessive code and give consistent style
 *
 * Revision 1.15  2014/02/28 01:41:52  smirnovd
 * Remove old fake geometry
 *
 * Revision 1.14  2014/02/28 01:41:46  smirnovd
 * Class definition clean-up. Remove unused members, get rid of virtuality becasue there is no need for it. Give consisten
 * names
 *
 * Revision 1.13  2014/02/13 02:36:53  smirnovd
 * Minor style issue + exit function before considering inactive material
 *
 * Revision 1.12  2014/02/13 02:36:46  smirnovd
 * Major change in sti planes creation logic + bunch of variables renamed to improve readability
 *
 * Revision 1.11  2014/02/13 02:36:39  smirnovd
 * Revised sensor index
 *
 * Revision 1.10  2014/02/13 02:36:33  smirnovd
 * Remove outdated code
 *
 * Revision 1.9  2014/02/13 02:36:26  smirnovd
 * Move struct closer to where it is used
 *
 * Revision 1.8  2014/02/13 02:36:19  smirnovd
 * Includes and file header corrections
 *
 * Revision 1.7  2014/02/13 02:36:12  smirnovd
 * Minor corrections
 *
 * Revision 1.6  2014/02/13 02:36:03  smirnovd
 * Moved CVS log to the bottom
 *
 * Revision 1.19  2014/02/01 02:49:12  smirnovd
 * Renamed class member variables to be consistent with STAR styles
 *
 * Signed-off-by: Dmitri Smirnov <d.s@plexoos.com>
 *
 * Revision 1.18  2014/02/01 02:49:03  smirnovd
 * Switched to already defined constants
 *
 * Signed-off-by: Dmitri Smirnov <d.s@plexoos.com>
 *
 * Revision 1.17  2014/02/01 02:48:56  smirnovd
 * Minor stylistic clean-up
 *
 * Signed-off-by: Dmitri Smirnov <d.s@plexoos.com>
 *
 * Revision 1.16  2014/02/01 02:48:47  smirnovd
 * Remove pointless empty destructor
 *
 * Signed-off-by: Dmitri Smirnov <d.s@plexoos.com>
 *
 * Revision 1.15  2014/02/01 02:48:39  smirnovd
 * For the time being ignore undefined variable gStPxlDbMaker
 *
 * Signed-off-by: Dmitri Smirnov <d.s@plexoos.com>
 *
 * Revision 1.14  2014/02/01 02:48:30  smirnovd
 * Added ROOT header to get rid of compiler error
 *
 * Signed-off-by: Dmitri Smirnov <d.s@plexoos.com>
 *
 * Revision 1.13  2014/02/01 02:48:07  smirnovd
 * Improved style format by running astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
 *
 * Revision 1.12  2014/02/01 02:37:17  smirnovd
 * This commit is intended to sync with what we had in StRoot/StiRnD
 *
 * Revision 1.30  2014/01/23 17:38:18  bouchet
 * *** empty log message ***
 *
 * Revision 1.29  2013/03/11 17:24:08  bouchet
 * StiRnD for Y2013
 *
 * Revision 1.28  2012/12/18 20:52:32  bouchet
 * update for DEV13 geometry
 *
 * Revision 1.27  2011/04/22 22:00:18  fisyak
 * warn off
 *
 * Revision 1.26  2010/08/25 21:57:41  fisyak
 * Get rid off access to specfic detector tracking parameters which usage has been  disable since 2008/06/11
 *
 * Revision 1.25  2009/03/16 13:51:00  fisyak
 * Move out all Sti Chairs into StDetectorDb
 *
 * Revision 1.24  2009/02/09 02:47:19  andrewar
 * UPGR15 update. Will break backward compatibility with older geometries.
 *
 * Revision 1.23  2008/04/03 20:04:20  fisyak
 * Straighten out DB access via chairs
 *
 * Revision 1.22  2007/10/20 00:16:27  fisyak
 * Active hit errors from DB
 *
 * Revision 1.21  2007/10/16 19:50:24  fisyak
 * rename Hft => Pxl, remove Hpd, Igt and Fst
 *
 * Revision 1.20  2007/05/16 15:02:57  andrewar
 * Removed couts in favor of LOG_INFO.
 *
 * Revision 1.19  2007/05/03 06:14:56  andrewar
 * Geometry fix to conform to StiHit:setGlobal() test.
 *
 * Revision 1.18  2007/03/30 02:14:19  andrewar
 * Removed some debug output.
 *
 * Revision 1.17  2006/11/30 16:37:19  andrewar
 * Removed call to dbase for tracking parameter loading for the review. Dynamic
 * access will be debugged and restored after the STAR review. Hit errors are
 * forced to 60um.
 *
 * Revision 1.16  2006/11/29 04:02:01  andrewar
 * Make use of pre-existing STAR DB inteface.
 *
 * Revision 1.15  2006/11/29 00:44:04  andrewar
 * Added call to get tracking parameters from DBase.
 *
 * Revision 1.14  2006/11/17 15:39:03  wleight
 * Changes to make PXL hits work with UPGR05 geometry
 *
 * Revision 1.13  2006/04/19 19:49:47  andrewar
 * Added call to setLayerAngle, needed for detector container sort.
 *
 * Revision 1.12  2006/02/23 00:22:54  andrewar
 * Set Detector Id to kPxlId, corrected Ist*pars -> Pixel*pars
 *
 * Revision 1.11  2006/02/17 21:39:32  andrewar
 * Added calls to StiDetector::setKey(key,val)
 *
 */
