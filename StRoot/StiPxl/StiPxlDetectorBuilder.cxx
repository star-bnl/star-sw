/* $Id: StiPxlDetectorBuilder.cxx,v 1.9 2014/02/13 02:36:26 smirnovd Exp $ */

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
#include "StPxlDbMaker/StPxlDbMaker.h"
#include "StPxlUtil/StPxlConstants.h"


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
StiPxlDetectorBuilder::StiPxlDetectorBuilder(bool active, const string &inputFile)
   : StiDetectorBuilder("Pixel", active, inputFile)
{
}


/** Build the pixel detector components. */
void StiPxlDetectorBuilder::buildDetectors(StMaker &source)
{
   char name[50];
   LOG_INFO << "StiPxlDetectorBuilder::buildDetectors() -I- Started" << endm;

   unsigned int nRows = 2;

   // 2 real rows, but we have detector elements and support elements.
   setNRows(nRows);

   if (StiVMCToolKit::GetVMC()) { useVMCGeometry(); return; }

   _gasMat    = add(new StiMaterial("PixelAir", 7.3, 14.61, 0.001205, 30420.*0.001205, 7.3 * 12.e-9));
   mSiMaterial = add(new StiMaterial("PixelSi",  14.,  28.0855,   2.33,     21.82,   14.*12.*1e-9) );
   mHybridMaterial = add(new StiMaterial("PixelHyb", 14.,  28.0855,   2.33,     21.82,   14.*12.*1e-9) );

   //Instantiate energy loss detector for si material
   //const static double I2Ar = (15.8*18) * (15.8*18) * 1e-18; // GeV**2
   //double ionization = material->getIonization();
   double ionization = mSiMaterial->getIonization();

   StiElossCalculator *siElossCalculator = new StiElossCalculator(mSiMaterial->getZOverA(),
         ionization * ionization, mSiMaterial->getA(), mSiMaterial->getZ(), mSiMaterial->getDensity());
   StiPlanarShape *pShape;

   for (unsigned int row = 0; row < nRows; row++) {
      pShape = new StiPlanarShape;

      if (!pShape) throw runtime_error("StiPxlDetectorBuilder::buildDetectors() - FATAL - pShape==0||ifcShape==0");

      sprintf(name, "Pixel/Layer_%d", row);
      pShape->setName(name);
      pShape->setThickness(0.0280); //cm
      pShape->setHalfDepth( 20. / 2. );
      pShape->setHalfWidth(1.0);

      for (unsigned int sector = 0; sector < 24; sector++) {
         StiPlacement *pPlacement = new StiPlacement;
         pPlacement->setZcenter(0.);
         double phi = phiForPixelSector(sector) + psiForPixelSector(sector);
         double r = radiusForPixelSector(sector) * cos(psiForPixelSector(sector)) - 0.0040; // note 40 microns offset
         double dY = radiusForPixelSector(sector) * sin(psiForPixelSector(sector));
         /*printf(" sector: %g phi: %g radius: %g normal: %g dY: %g\n",sector
              ,phi*180/3.1415
              << " radius:"<<radiusForPixelSector(sector)
              << " normal r:"<<r
              << "     dY:"<<dY<<endl;*/
         pPlacement->setNormalRep(phi, r, dY);
         pPlacement->setLayerRadius(r);
         pPlacement->setLayerAngle(phi);
         pPlacement->setRegion(StiPlacement::kMidRapidity);
         sprintf(name, "Pixel/Layer_%d/Ladder_%d", row, sector);

         StiDetector *pDetector = _detectorFactory->getInstance();
         pDetector->setName(name);
         pDetector->setIsOn(true);
         pDetector->setIsActive(new StiPxlIsActiveFunctor);
         pDetector->setIsContinuousMedium(true);
         pDetector->setIsDiscreteScatterer(false);
         pDetector->setMaterial(mSiMaterial);
         pDetector->setGas(_gasMat);
         pDetector->setGroupId(kPxlId);
         pDetector->setShape(pShape);
         pDetector->setPlacement(pPlacement);
         pDetector->setHitErrorCalculator(StiPxlHitErrorCalculator::instance());
         pDetector->setElossCalculator(siElossCalculator);

         if (sector < 18) {
            pDetector->setKey(1, 1);
            pDetector->setKey(2, sector);
            add(1, sector, pDetector);
         }
         else {
            pDetector->setKey(1, 0);
            pDetector->setKey(2, sector - 18);
            add(0, (sector - 18), pDetector);
         }

         //cout << "Setting detector: " << name << " with key values: "
         //     << pDetector->getKey(1) << " "  << pDetector->getKey(2) << endl;
      }
   }

   LOG_INFO << " -I- Done" << endl;
}


void StiPxlDetectorBuilder::useVMCGeometry()
{
   LOG_INFO << "StiPxlDetectorBuilder::useVMCGeometry() -I- Use VMC geometry" << endm;

   //THashList *PxlRot = new THashList(400, 0);
   // XXX:ds At the moment gStPxlDbMaker is not defined in offline/hft/StRoot/StPxlDbMaker or
   // StRoot/
   //PxlRot = gStPxlDbMaker->GetRotations();

   //check geometry tables
   /*
     for(int ii=0;ii<400;++ii){
     TGeoHMatrix *combP=(TGeoHMatrix *) PxlRot->FindObject(Form("R%03i",ii+1));
     if(combP){
     combP->Prnt();
     }
     }
   */
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
         mSiMaterial->getA(),
         mSiMaterial->getZ(),
         mSiMaterial->getDensity());

   for (UInt_t ii = 0; ii < kNumberOfPxlSectors; ++ii) {
      for (UInt_t jj = 0; jj < kNumberOfPxlLaddersPerSector; ++jj) {
         for (UInt_t kk = 0; kk < kNumberOfPxlSensorsPerLadder; kk++)
         {
            if (ii != 1 && ii != 3 && ii != 6) continue;

            //for run 13, only sector 2,4,7
            int matPix = 0;
            matPix = (ii) * 40 + (jj) * 10 + (kk + 1);
            LOG_DEBUG << " ii/jj/kk/matPix : " << ii << " " << " " << jj << " " << kk << " " << matPix << endm;

            if (kk != 0) continue;

            //we place the ladder as a whole
#if 0
            TString path(PxlVolumes[7].path);

            //LOG_DEBUG << " path : " << path << endm;
            if (! _TpcRefSys_1) path.ReplaceAll("/TpcRefSys_1", "");

            gGeoManager->cd(path); // retrieve info of PLAC volume
            TGeoNode *nodeT = gGeoManager->GetCurrentNode();
            // Extract volume geometry for this node
            TGeoBBox *box = (TGeoBBox *) nodeT->GetVolume()->GetShape();
#else
            TGeoVolume *vol = gGeoManager->GetVolume("PLAC");

            if (! vol) continue;

            TGeoBBox *box = (TGeoBBox *) vol->GetShape();
#endif
            char name[50];
            sprintf(name, "Pixel/Sector_%d/Ladder_%d/Sensor_%d", ii + 1, jj + 1, kk + 1);
            LOG_DEBUG << " weigh/daughters/Material/A/Z : " << vol->Weight() << " "
                      << vol->GetNdaughters() << " " << vol->GetMaterial()->GetName() << " "
                      << vol->GetMaterial()->GetA() << " " << vol->GetMaterial()->GetZ() << endm;
            LOG_DEBUG << " DZ/DY/DX : " << box->GetDZ()
                      << " " << box->GetDY()
                      << " " << box->GetDX()
                      << " " << endm;

            //PLAC shape : DX =.961cm ; DY = .002cm ; DZ = .94 cm

            StiShape *sh  = new StiPlanarShape(name, 10*box->GetDZ(), box->GetDY(), box->GetDX());

            add(sh);
            TGeoHMatrix *combP = (TGeoHMatrix *)PxlRot->FindObject(Form("R%03i", matPix));
            assert(combP);
            combP->Print();

            Double_t *xyz = combP->GetTranslation();
            Double_t *rot = combP->GetRotationMatrix();

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

            //Build final detector object

            StiDetector *p = getDetectorFactory()->getInstance();

            if ( !p ) {
               LOG_INFO << "StiPxlDetectorBuilder::AverageVolume() -E- StiDetector pointer invalid." << endm;
               return;
            }

            //char name[50];
            //sprintf(name, "Pixel/Sector_%d/Ladder_%d/Sensor_%d", ii,jj,kk);
            p->setName(name);
            p->setIsOn(kTRUE);
            //if (ActiveVolume) {
            //LOG_DEBUG << " current node : " << name << " is set active" <<endm;
            p->setIsActive(new StiPxlIsActiveFunctor);
            //}
            //else {
            //LOG_DEBUG << " current node : " << name << " is set inactive" <<endm;
            //p->setIsActive(new StiNeverActiveFunctor);
            //}
            //if(nameP.Contains("PXSI")) {layer=layer+10;}

            p->setIsContinuousMedium(false);
            p->setIsDiscreteScatterer(true);
            p->setShape(sh);
            p->setPlacement(pPlacement);
            p->setGas(GetCurrentDetectorBuilder()->getGasMat());

            if (!p->getGas()) LOG_INFO << "gas not there!" << endm;

            p->setMaterial(mSiMaterial);
            p->setElossCalculator(ElossCalculator);
            p->setHitErrorCalculator(StiPxlHitErrorCalculator::instance());

            Int_t ROW    = 0;
            Int_t SECTOR = 0;

            /* numbering is :
               ladder = 0-1- ...9 for inner layer --> ROW =0
               ladder = 0-1-2 for sector 0 of outer layer, then 3-4-5 for the second sector until 29 for the last sectro
               ladder=4 is the inner ladder
            */
            // update 05-15 : inner ladder is ladder 1
            if (jj == 0) {
               ROW = 0 ;
               SECTOR = ii;
            }
            else {
               ROW = 1;
               SECTOR = (ii) * 3 + (jj - 1);
            }

            p->setKey(1, ROW);
            p->setKey(2, SECTOR);
            add(ROW, SECTOR, p);

            // Whole bunch of debugging information

            Float_t rad2deg = 180.0 / 3.1415927;
            LOG_DEBUG << "===>NEW:PIXEL:pDetector:Name               = " << p->getName()                               << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pPlacement:NormalRefAngle    = " << pPlacement->getNormalRefAngle()*rad2deg    << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pPlacement:NormalRadius      = " << pPlacement->getNormalRadius()              << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pPlacement:NormalYoffset     = " << pPlacement->getNormalYoffset()             << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pPlacement:CenterRefAngle    = " << pPlacement->getCenterRefAngle()*rad2deg    << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pPlacement:CenterRadius      = " << pPlacement->getCenterRadius()              << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pPlacement:CenterOrientation = " << pPlacement->getCenterOrientation()*rad2deg << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pPlacement:LayerRadius       = " << pPlacement->getLayerRadius()               << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pPlacement:LayerAngle        = " << pPlacement->getLayerAngle()*rad2deg        << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pPlacement:Zcenter           = " << pPlacement->getZcenter()                   << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pDetector:sector             = " << ii + 1                                       << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pDetector:Ladder             = " << jj + 1                                       << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pDetector:sensor             = " << kk + 1                                       << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pDetector:row/sector (ITTF)  = " << ROW << " / " << SECTOR                      << endm;
            LOG_DEBUG << "===>NEW:PIXEL:pDetector:Active?            = " << p->isActive()                              << endm;
         }
      }
   }

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
