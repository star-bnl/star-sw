/* $Id: StIstFastSimMaker.cxx,v 1.11 2015/02/25 20:41:33 smirnovd Exp $ */

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
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include <vector>
#include <exception>
#include <stdexcept>
#include "tables/St_g2t_ist_hit_Table.h"
#include "tables/St_HitError_Table.h"
#include "TGeoManager.h"
#include "TGeoMatrix.h"
#include "TDataSet.h"
#include "StarClassLibrary/StRandom.hh"

ClassImp(StIstFastSimMaker)

StIstFastSimMaker::StIstFastSimMaker( const Char_t *name ) : StMaker(name), mIstRot(NULL), mIstDb(NULL), mBuildIdealGeom(kFALSE),
   mRandom(new StRandom()), mSmear(kTRUE)
{
   Int_t seed = time(NULL);
   mRandom->setSeed(seed);
}

//____________________________________________________________
StIstFastSimMaker::~StIstFastSimMaker(){ 
   if (mIstDb) delete mIstDb;
   if (mRandom) delete mRandom; 
}

//____________________________________________________________
void StIstFastSimMaker::Clear(Option_t *) {
   StMaker::Clear();
}

//____________________________________________________________
Int_t StIstFastSimMaker::Init() {
   LOG_INFO << "StIstFastSimMaker::Init()" << endm;

   mBuildIdealGeom = kTRUE; //setup an ideal simulation of the IST
   mSmear = kTRUE; //do smearing for IST hit by default

   return kStOk;
}

//____________________________________________________________
Int_t StIstFastSimMaker::InitRun(int runNo)
{
   LOG_INFO << "StIstFastSimMaker::InitRun" << endm;

   TDataSet *set = GetDataBase("Calibrations/tracker");
   St_HitError *istTableSet = (St_HitError *)set->Find("ist1HitError");
   HitError_st *istHitError = istTableSet->GetTable();
   mResXIst1 = sqrt(istHitError->coeff[0]);
   mResZIst1 = sqrt(istHitError->coeff[3]);

   TObjectSet *istDbDataSet = (TObjectSet *)GetDataSet("ist_db");

   if (istDbDataSet) {
      mIstDb = (StIstDb *)istDbDataSet->GetObject();
   }
   else {
      LOG_ERROR << "InitRun : no mIstDb" << endm;
      return kStErr;
   }

   // geometry Db tables
   mIstRot = mIstDb->getRotations();

   return kStOk;
}

//______________________________________________________________________________
Int_t StIstFastSimMaker::Make()
{
   LOG_INFO << "StIstFastSimMaker::Make()" << endm;
   if (!mIstRot) {
      LOG_FATAL << "Make(): mIstRot is not initialized" << endm;
      return kStFatal;
   }

   // Get the input data structures from StEvent and StMcEvent
   StEvent *rcEvent =  (StEvent *) GetInputDS("StEvent");

   if (! rcEvent) {LOG_INFO << "No StEvent on input" << endl; return kStWarn;}

   StMcEvent *mcEvent = (StMcEvent *) GetInputDS("StMcEvent");

   if (! mcEvent) {LOG_INFO << "No StMcEvent on input" << endl; return kStWarn;}

   if ( mBuildIdealGeom && !gGeoManager ) {
      GetDataBase("VmcGeometry");
   }

   // Store hits into Ist Hit Collection
   StIstHitCollection *istHitCollection = 0;

   istHitCollection = rcEvent->istHitCollection();

   if (!istHitCollection) {
      gMessMgr->Info() << "StIstFastSimMaker -E- no istHitCollection!\n";
      istHitCollection = new StIstHitCollection;
      LOG_WARN << "Make() has added a non existing StIstHitCollection" << endm;
      rcEvent->setIstHitCollection(istHitCollection);
   }

   StThreeVectorF mHitError(0., 0., 0.);

   //Get MC Ist hit collection. This contains all ist hits.
   const StMcIstHitCollection *istMcHitCol = mcEvent->istHitCollection();

   //new simulator for new 1-layer design
   Float_t smearedX = 0., smearedZ = 0.;

   if (istMcHitCol) {
      LOG_INFO << "ist MC hit collection found" << endm;
      Int_t nIsthits = istMcHitCol->numberOfHits();
      LOG_DEBUG << "there are " << nIsthits << " ist hits" << endm;

      if (nIsthits) {
         if (istMcHitCol->layer(0)) {
            for (UInt_t kk = 0; kk < istMcHitCol->layer(0)->hits().size(); kk++) {
               StMcHit *mcH = istMcHitCol->layer(0)->hits()[kk];
               StMcIstHit *mcI = dynamic_cast<StMcIstHit *>(mcH);

               Int_t matIst = 1000 + (mcI->ladder() - 1) * 6 + mcI->wafer();
               cout << " matIst : " << matIst << endl;

               TGeoHMatrix *combI = NULL;
	       //Access VMC geometry once no IST geometry Db tables available or using ideal geoemtry is set
	       if( (!mIstRot || mBuildIdealGeom) && gGeoManager) {
		  TString Path("HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1");
		  Path += Form("/IBAM_%d/IBLM_%d/IBSS_1", mcI->ladder(), mcI->wafer());
		  gGeoManager->RestoreMasterVolume();
		  gGeoManager->CdTop();
		  gGeoManager->cd(Path);
		  combI = (TGeoHMatrix *)gGeoManager->GetCurrentMatrix();
	       }
	       else { //using mis-aligned gemetry from IST geometry DB tables
		  combI = (TGeoHMatrix *)mIstRot->FindObject(Form("R%04i", matIst));  
	       }

               if (combI) {
                  cout << " geometry matrix :" << endl;
                  combI->Print();
               }

               //YPWANG: McIstHit stored local position
               Double_t globalIstHitPos[3] = {mcI->position().x(), mcI->position().y(), mcI->position().z()};
               Double_t localIstHitPos[3] = {mcI->position().x(), mcI->position().y(), mcI->position().z()};
               LOG_DEBUG << "Before Smearing" << endm;
               LOG_DEBUG << "localIstHitPos = " << localIstHitPos[0] << " " << localIstHitPos[1] << " " << localIstHitPos[2] << endm;

               if (mSmear) { // smearing on
                  LOG_DEBUG << "Smearing start... " << endm;
                  smearedX = distortHit(localIstHitPos[0], mResXIst1, kIstSensorActiveSizeRPhi / 2.0);
                  smearedZ = distortHit(localIstHitPos[2], mResZIst1, kIstSensorActiveSizeZ / 2.0);

                  localIstHitPos[0] = smearedX;
                  localIstHitPos[2] = smearedZ;
                  LOG_DEBUG << Form("Smearing done...") << endm;
               }
               else { //smearing off
                  LOG_DEBUG << "No smearing, but discreting ... " << endm;
                  //discrete hit local position (2D structure of IST sensor pads)
                  Float_t rPhiPos   = kIstSensorActiveSizeRPhi / 2.0 - localIstHitPos[0];
                  Float_t zPos      = localIstHitPos[2] + kIstSensorActiveSizeZ / 2.0;
                  Short_t meanColumn  = (Short_t)floor( zPos / kIstPadPitchColumn ) + 1;
                  Short_t meanRow     = (Short_t)floor( rPhiPos / kIstPadPitchRow ) + 1;
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

               UInt_t hw =  ( mcI->ladder() - 1 ) * 6 + mcI->wafer();
               StIstHit *tempHit = new StIstHit(gistpos, mHitError, hw, mcI->dE(), 0);
               tempHit->setDetectorId(kIstId);
               tempHit->setId(mcI->key());
	       mcI->parentTrack()? tempHit->setIdTruth(mcI->parentTrack()->key(), 100): tempHit->setIdTruth(-999);
               tempHit->setLocalPosition(localIstHitPos[0], localIstHitPos[1], localIstHitPos[2]);
               istHitCollection->addHit(tempHit);

               LOG_DEBUG << "id() : " << tempHit->id()  << " idTruth: " << tempHit->idTruth() << endm;
               LOG_DEBUG << "from StMcIstHit x= " << mcI->position().x()   << "; y= " << mcI->position().y()   << "; z= " << mcI->position().z() << endm;
               LOG_DEBUG << "istHit location x= " << tempHit->position().x() << "; y= " << tempHit->position().y() << "; z= " << tempHit->position().z() << "; energy/charge = " << tempHit->charge() << endm;
            }//MC hits loop over
         }//end layer=0 cut
      }//end MC hits number cut

      LOG_DEBUG << "StIstFastSimMaker::Make() -I- Loaded " << nIsthits << " ist hits. \n";
   }
   else {
      LOG_FATAL << "No Ist MC hits found." << endm;
      return kStFatal;
   }

   return kStOK;
}


/**
 * Calculates and returns new value for the local coordinate x by smearing it
 * acccording to a normal distribution N(mean, sigma) = N(x, res). The returned
 * value is constrained to be within the characteristic dimension detLength
 * provided by the user.
 */
Double_t StIstFastSimMaker::distortHit(const Double_t x, const Double_t res, const Double_t detLength) const
{
   // Do not smear x when it is outside the physical limits. Issue a warning instead
   if (fabs(x) > detLength) {
      LOG_WARN << "distortHit() - Generated hit is outside detector sensor plane" << endm;
      return x;
   }

   Double_t smeared_x;

   do {
      smeared_x = mRandom->gauss(x, res);
   } while ( fabs(smeared_x) > detLength);

   return smeared_x;
}


/***************************************************************************
*
* $Log: StIstFastSimMaker.cxx,v $
* Revision 1.11  2015/02/25 20:41:33  smirnovd
* adding method to access VMC geometry once no avaible geometry DB tables or set to use ideal geoemtry
*
* Revision 1.10  2015/02/25 20:41:27  smirnovd
* Further general codeing style updates according to Jason W. reviews
*
* Revision 1.9  2015/02/25 20:39:57  smirnovd
* minor update for mIstRot initialization check
*
* Revision 1.8  2015/02/25 20:39:51  smirnovd
* STAR Coding Standards style upates according to Jason W. comments
*
* Revision 1.7  2015/02/25 20:39:43  smirnovd
* Minor refactoring of StPxlFastSim::distortHit() to include a new warning for unphysical hit position
*
* Revision 1.6  2015/02/25 20:36:36  smirnovd
* No need to check for valid pointer to StEvent object as it is already done at the begining of Make() routine
*
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
* IST GEANT hit is transformed to either ideal or misaligned geometry of 
* realistic detector, with smearing or pixelization. The GEANT hit dE is 
* directly propagated to IST hit in GeV.
****************************************************************************/
