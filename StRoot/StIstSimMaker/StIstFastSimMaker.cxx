/* $Id: StIstFastSimMaker.cxx,v 1.5 2015/02/25 20:36:26 smirnovd Exp $ */

#include "Stiostream.h"
#include "StIstFastSimMaker.h"
#include "StHit.h"
#include "StEventTypes.h"
#include "StEvent.h"
#include "StMcEvent.hh"
#include "StMcEvent/StMcHit.hh"
#include "StMcIstHit.hh"
#include "StRoot/StIstUtil/StIstConsts.h"
#include "StIstHit.h"
#include "StIstHitCollection.h"
#include "StIstDbMaker/StIstDb.h"
#include "StMcEvent/StMcIstHit.hh"
#include "StMcEvent/StMcIstHitCollection.hh"
#include "StMcEventTypes.hh"

#include <stdio.h>
#include <map>
#include <exception>
#include <stdexcept>
#include "tables/St_g2t_ist_hit_Table.h"
#include "tables/St_HitError_Table.h"
#include "TGeoManager.h"
#include "TGeoMatrix.h"
#include "TDataSet.h"
#include "StarClassLibrary/StRandom.hh"

ClassImp(StIstFastSimMaker)

StIstFastSimMaker::StIstFastSimMaker( const char *name ) : StMaker(name), istRot(NULL), mIstDb(NULL), mBuildIdealGeom(kFALSE),
   myRandom(new StRandom()), mSmear(true)
{
   int seed = time(NULL);
   myRandom->setSeed(seed);
}


//____________________________________________________________
Int_t StIstFastSimMaker::InitRun(int RunNo)
{
   LOG_INFO << "StIstFastSimMaker::InitRun" << endm;

   TDataSet *set = GetDataBase("Calibrations/tracker");
   St_HitError *istTableSet = (St_HitError *)set->Find("ist1HitError");
   HitError_st *istHitError = istTableSet->GetTable();
   resXIst1 = sqrt(istHitError->coeff[0]);
   resZIst1 = sqrt(istHitError->coeff[3]);

   TObjectSet *istDbDataSet = (TObjectSet *)GetDataSet("ist_db");

   if (istDbDataSet) {
      mIstDb = (StIstDb *)istDbDataSet->GetObject();
      assert(mIstDb);
   }
   else {
      LOG_ERROR << "InitRun : no istDb" << endm;
      return kStErr;
   }

   // geometry Db tables
   istRot = mIstDb->getRotations();

   return kStOk;
}

//______________________________________________________________________________
Int_t StIstFastSimMaker::Make()
{
   LOG_INFO << "StIstFastSimMaker::Make()" << endm;

   // Get the input data structures from StEvent and StMcEvent
   StEvent *rcEvent =  (StEvent *) GetInputDS("StEvent");

   if (! rcEvent) {LOG_INFO << "No StEvent on input" << endl; return kStWarn;}

   StMcEvent *mcEvent = (StMcEvent *) GetInputDS("StMcEvent");

   if (! mcEvent) {LOG_INFO << "No StMcEvent on input" << endl; return kStWarn;}

   TDataSetIter geant(GetInputDS("geant"));

   if ( mBuildIdealGeom && !gGeoManager ) {
      GetDataBase("VmcGeometry");
   }

   g2t_ist_hit_st *g2tIst = 0;
   St_g2t_ist_hit *g2t_ist_hit = (St_g2t_ist_hit *)geant("g2t_ist_hit");

   if (g2t_ist_hit) g2tIst = g2t_ist_hit->GetTable();

   // Store hits into Ist Hit Collection
   StIstHitCollection *istHitCollection = 0;

   if (rcEvent) {
      istHitCollection = rcEvent->istHitCollection();

      if (!istHitCollection) {
         gMessMgr->Info() << "StIstFastSimMaker -E- no istHitCollection!\n";
         istHitCollection = new StIstHitCollection;
         LOG_WARN << "Make() has added a non existing StIstHitCollection" << endm;
         rcEvent->setIstHitCollection(istHitCollection);
      }
   }

   StThreeVectorF mHitError(0., 0., 0.);

   //Get MC Ist hit collection. This contains all ist hits.
   const StMcIstHitCollection *istMcHitCol = mcEvent->istHitCollection();

   //new simulator for new 1-layer design
   float smearedX = 0., smearedZ = 0.;

   if (istMcHitCol) {
      LOG_INFO << "ist MC hit collection found" << endm;
      int nIsthits = istMcHitCol->numberOfHits();
      LOG_DEBUG << "there are " << nIsthits << " ist hits" << endm;

      if (nIsthits) {
         if (istMcHitCol->layer(0)) {
            for (unsigned int kk = 0; kk < istMcHitCol->layer(0)->hits().size(); kk++) {
               StMcHit *mcH = istMcHitCol->layer(0)->hits()[kk];
               StMcIstHit *mcI = dynamic_cast<StMcIstHit *>(mcH);

               int matIst = 1000 + (mcI->ladder() - 1) * 6 + mcI->wafer();
               cout << " matIst : " << matIst << endl;
               TGeoHMatrix *combI = (TGeoHMatrix *)istRot->FindObject(Form("R%04i", matIst));

               if (combI) {
                  cout << " from Tables :" << endl;
                  combI->Print();
               }

               //YPWANG: McIstHit stored local position
               double globalIstHitPos[3] = {mcI->position().x(), mcI->position().y(), mcI->position().z()};
               double localIstHitPos[3] = {mcI->position().x(), mcI->position().y(), mcI->position().z()};
               LOG_DEBUG << "Before Smearing" << endm;
               LOG_DEBUG << "localIstHitPos = " << localIstHitPos[0] << " " << localIstHitPos[1] << " " << localIstHitPos[2] << endm;

               if (mSmear) { // smearing on
                  LOG_DEBUG << "Smearing start... " << endm;
                  smearedX = distortHit(localIstHitPos[0], resXIst1, kIstSensorActiveSizeRPhi / 2.0);
                  smearedZ = distortHit(localIstHitPos[2], resZIst1, kIstSensorActiveSizeZ / 2.0);

                  localIstHitPos[0] = smearedX;
                  localIstHitPos[2] = smearedZ;
                  LOG_DEBUG << Form("Smearing done...") << endm;
               }
               else { //smearing off
                  LOG_DEBUG << "No smearing, but discreting ... " << endm;
                  //discrete hit local position (2D structure of IST sensor pads)
                  float rPhiPos   = kIstSensorActiveSizeRPhi / 2.0 - localIstHitPos[0];
                  float zPos      = localIstHitPos[2] + kIstSensorActiveSizeZ / 2.0;
                  short meanColumn  = (short)floor( zPos / kIstPadPitchColumn ) + 1;
                  short meanRow     = (short)floor( rPhiPos / kIstPadPitchRow ) + 1;
                  rPhiPos = (meanRow - 1) * kIstPadPitchRow + 0.5 * kIstPadPitchRow; //unit: cm
                  zPos    = (meanColumn - 1) * kIstPadPitchColumn + 0.5 * kIstPadPitchColumn; //unit: cm
                  localIstHitPos[0] = kIstSensorActiveSizeRPhi / 2.0 - rPhiPos;
                  localIstHitPos[2] = zPos - kIstSensorActiveSizeZ / 2.0;
               }

               //YPWANG: do local-->global transform with geometry table
               combI->LocalToMaster(localIstHitPos, globalIstHitPos);
               StThreeVectorF gistpos(globalIstHitPos);
               LOG_DEBUG << "smeared globalIstHitPos = " << globalIstHitPos[0] << " " << globalIstHitPos[1] << " " << globalIstHitPos[2] << endm;
               LOG_DEBUG << "smeared localIstHitPos = " << localIstHitPos[0] << " " << localIstHitPos[1] << " " << localIstHitPos[2] << endm;
               LOG_DEBUG << "hit position(ladder/sensor): " << mcI->ladder() << " " << mcI->wafer() << endm;

               unsigned int hw =  ( mcI->ladder() - 1 ) * 6 + mcI->wafer();
               StIstHit *tempHit = new StIstHit(gistpos, mHitError, hw, mcI->dE(), 0);
               tempHit->setDetectorId(kIstId);
               tempHit->setId(mcI->key());
               tempHit->setIdTruth(g2tIst[kk].track_p, 100);
               tempHit->setLocalPosition(localIstHitPos[0], localIstHitPos[1], localIstHitPos[2]);
               istHitCollection->addHit(tempHit);

               LOG_DEBUG << "id() : " << tempHit->id()  << " idTruth: " << tempHit->idTruth() << endm;
               LOG_DEBUG << "from StMcIstHit x= " << mcI->position().x()   << "; y= " << mcI->position().y()   << "; z= " << mcI->position().z() << endm;
               LOG_DEBUG << "istHit location x= " << tempHit->position().x() << "; y= " << tempHit->position().y() << "; z= " << tempHit->position().z() << "; energy/charge = " << tempHit->charge() << endm;
            }//MC hits loop over
         }//end layer=0 cut
      }//end MC hits number cut

      gMessMgr->Info() << "StIstFastSimMaker::Make() -I- Loaded " << nIsthits << " ist hits. \n";
   }
   else {
      LOG_INFO << "No Ist MC hits found." << endm;
   }

   return kStOK;
}

Double_t StIstFastSimMaker::distortHit(double x, double res, double detLength)
{
   double test;
   test = x + myRandom->gauss(0, res);

   while ( fabs(test) > detLength) {
      test = x + myRandom->gauss(0, res);
   }

   return test;
}


/***************************************************************************
*
* $Log: StIstFastSimMaker.cxx,v $
* Revision 1.5  2015/02/25 20:36:26  smirnovd
* StIstFastSimMaker: Corrected style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.4  2015/02/25 20:32:14  smirnovd
* Minor adjustments to the code and comments
*
* Revision 1.3  2015/02/25 20:31:58  smirnovd
* Removed pointless methods. ::Init() and ::Finish() do not do much. Data members initialized in constructor
*
* Revision 1.2  2015/02/25 20:20:00  smirnovd
* Moved CVS log to the end of file and updated doxygen-style comments
*
* Revision 1.7  2014/10/13 22:21:56  smirnovd
* Moved CVS log to the end of file and updated doxygen-style comments
*
* Revision 1.6  2014/08/06 18:56:52  ypwang
* minor update due to coding style update of the StIstDb method
*
* Revision 1.5  2014/08/05 03:28:42  ypwang
* buildIdealGeom() added to switch between ideal VMC geometry or DB geometry, Db geometry was built by default
*
* Revision 1.4  2014/07/29 20:13:31  ypwang
* update the IST DB obtain method
*
* Revision 1.3  2014/02/08 03:34:16  ypwang
* updating scripts
*
*
****************************************************************************
* StIstFastSimMaker.cxx,v 1.0
* Revision 1.0 2013/11/04 16:25:30 Yaping
* Initial version
****************************************************************************/
