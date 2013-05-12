/*
 *
 * Author: A. Rose, LBL, Y. Fisyak, BNL, M. Miller, MIT, M. Mustafa
 *
 *
 **********************************************************
 * $Log: StPxlFastSim.cxx,v $
 * Revision 1.1  2013/05/12 21:43:32  jeromel
 * Initial revision, code peer review closed 2013/05/06
 *
 * Revision 1.5  2013/05/09 02:58:36  mstftsm
 * Fixed a bug which called for sensor local Z value.
 *
 *
 */

#include <stdio.h>

#include "StMessMgr.h"
#include "Stypes.h"
#include "Stiostream.h"
#include "StPxlFastSim.h"
#include "StEvent/StPxlHit.h"
#include "StEvent/StPxlHitCollection.h"
#include "StMcEvent/StMcPxlHitCollection.hh"
#include "StMcEvent/StMcPxlHit.hh"
#include "tables/St_HitError_Table.h"
#include "StarClassLibrary/StRandom.hh"
#include "StThreeVectorF.hh"

#include "TGeoManager.h"
#include "TGeoMatrix.h"
#include "TDataSet.h"

StPxlFastSim::~StPxlFastSim()
{
   if (mRandom) delete mRandom;
}
//____________________________________________________________
Int_t StPxlFastSim::initRun(const TDataSet& calib_db, const Int_t run)
{
   // run is not used in the current implementation, but might be necessary in the future.

   LOG_INFO << "StPxlFastSim::init()" << endm;

   if (!mRandom) mRandom = new StRandom();
   Int_t seed = time(NULL);
   mRandom->setSeed(seed);

   St_HitError *pxlTableSet = (St_HitError*)calib_db.Find("PixelHitError");

   if (!pxlTableSet)
   {
      LOG_ERROR << "StPxlFastSim - E - PixelHitError is not available" << endm;
      return kStErr;
   }

   HitError_st* pxlHitError = pxlTableSet->GetTable();

   if (!pxlHitError)
   {
      LOG_ERROR << "StPxlFastSim - E - pxl hit table is not available in PixelHitError" << endm;
      return kStErr;
   }

   // please note that what is called local Y in the PXL sensor design
   // is actually called Z in STAR coordinates convention
   if(pxlHitError->coeff[0]<=0 || pxlHitError->coeff[3]<=0)
   {
	   LOG_ERROR << "StPxlFastSim - E - negative or corrupted PXL hits errors in DB" <<endm;
	   return kStErr;
   }

   mResXPix = sqrt(pxlHitError->coeff[0]); // local x
   mResZPix = sqrt(pxlHitError->coeff[3]); // local Y
   mResYPix = 0;//sqrt(pxlHitError->coeff[2]); // needs to be updated in the DB later

   return kStOk;
}
//____________________________________________________________
Int_t StPxlFastSim::addPxlHits(const StMcPxlHitCollection& mcPxlHitCol,
                               StPxlHitCollection& pxlHitCol)
{
   Float_t smearedX = 0, smearedY = 0, smearedZ = 0;


   // Loop over sectors
   for (UInt_t iSec = 0; iSec < mcPxlHitCol.numberOfSectors(); iSec++)
   {
      const StMcPxlSectorHitCollection* mcPxlSectorHitCol = mcPxlHitCol.sector(iSec);
      if (!mcPxlSectorHitCol) continue;

      for (UInt_t iLad = 0; iLad < mcPxlSectorHitCol->numberOfLadders(); iLad++)
      {
         const StMcPxlLadderHitCollection* mcPxlLadderHitCol = mcPxlSectorHitCol->ladder(iLad);
         if (!mcPxlLadderHitCol) continue;

         for (UInt_t iSen = 0; iSen < mcPxlLadderHitCol->numberOfSensors(); iSen++)
         { 
            const StMcPxlSensorHitCollection* mcPxlSensorHitCol = mcPxlLadderHitCol->sensor(iSen);
            if (!mcPxlSensorHitCol) continue;

            UInt_t nSenHits = mcPxlSensorHitCol->hits().size();
	    LOG_DEBUG << "Sector/Ladder/Sensor = " << iSec+1 <<"/"<<iLad+1<<"/"<<iSen+1 << ". Number of sensor hits = "<< nSenHits <<endm;

            // Loop over hits in the sensor
            for (UInt_t iHit = 0; iHit < nSenHits; iHit++)
            {
               StMcPxlHit* mcPix = mcPxlSensorHitCol->hits()[iHit];

               Long_t volId = mcPix->volumeId();
               Int_t sector = mcPix->sector();
               Int_t ladder = mcPix->ladder();
               Int_t sensor = mcPix->sensor();

               TString Path("");
               LOG_DEBUG << endm;
               Path = Form("/HALL_1/CAVE_1/IDSM_1/PXMO_1/PXLA_%i/LADR_%i/PXSI_%i/PLAC_1", sector, ladder, sensor);
               LOG_DEBUG << "PATH: " << Path << endm;
               LOG_DEBUG << "pxl hit volId/sector/ladder/sensor is " << volId << "/" << sector << "/" << ladder << "/" << sensor << endm;

               gGeoManager->RestoreMasterVolume();
               gGeoManager->CdTop();
               gGeoManager->cd(Path);

               Double_t globalPixHitPos[3] = {mcPix->position().x(), mcPix->position().y(), mcPix->position().z()};
               Double_t localPixHitPos[3]  = {0, 0, 0};
               gGeoManager->GetCurrentMatrix()->MasterToLocal(globalPixHitPos, localPixHitPos);

               LOG_DEBUG << "globalPixHitPos = " << globalPixHitPos[0] << " " << globalPixHitPos[1] << " " << globalPixHitPos[2] << endm;
               LOG_DEBUG << "localPixHitPos = " << localPixHitPos[0] << " " << localPixHitPos[1] << " " << localPixHitPos[2] << endm;
               // please note that what is called local Y in the PXL sensor design
               // is actually called Z in STAR coordinates convention and vice-versa
               smearedX = distortHit(localPixHitPos[0], mResXPix, PXL_ACTIVE_X_LENGTH / 2.0);
               smearedZ = distortHit(localPixHitPos[2], mResZPix, PXL_ACTIVE_Y_LENGTH / 2.0);
               if(mResYPix) smearedY = distortHit(localPixHitPos[1], mResYPix, 0.0020); // Not properly constrained yet
	       else smearedY = localPixHitPos[1];
               localPixHitPos[0] = smearedX;
               localPixHitPos[2] = smearedZ;
               localPixHitPos[1] = smearedY;
               LOG_DEBUG << "smearedlocal = " << localPixHitPos[0] << " " << localPixHitPos[1] << " " << localPixHitPos[2] << endm;
               Double_t smearedGlobalPixHitPos[3] = {0, 0, 0};
               gGeoManager->GetCurrentMatrix()->LocalToMaster(localPixHitPos, smearedGlobalPixHitPos);

               StThreeVectorF gpixpos(smearedGlobalPixHitPos);
               StThreeVectorF mRndHitError(0., 0., 0.);

               UInt_t hw = sector * 10 + ladder; // needs to be updated later after clustering alogrithms are finalized
               StPxlHit* tempHit = new StPxlHit(gpixpos, mRndHitError, hw, mcPix->dE() , 0);
               tempHit->setSector(iSec + 1);
               tempHit->setLadder(mcPix->ladder());
               tempHit->setSensor(mcPix->sensor());
               tempHit->setIdTruth(mcPix->parentTrack()->key(), 100);
	       tempHit->setDetectorId(kPxlId);
	       tempHit->setId(mcPix->key());
               tempHit->setLocalPosition(localPixHitPos[0], localPixHitPos[1], localPixHitPos[2]);

               LOG_DEBUG << "key() : " << mcPix->key() - 1 << " idTruth: " << mcPix->parentTrack()->key() << endm;
               LOG_DEBUG << "from StMcPxlHit : x= " << mcPix->position().x() << ";  y= " << mcPix->position().y() << ";  z= " << mcPix->position().z() << endm;
               LOG_DEBUG << "pxlHit location x= " << tempHit->position().x() << "; y= " << tempHit->position().y() << "; z= " << tempHit->position().z() << endm;

               pxlHitCol.addHit(tempHit);
            }
         }
      }
   }

   return kStOK;
}

//____________________________________________________________
Double_t StPxlFastSim::distortHit(Double_t x, Double_t res, Double_t constraint)
{
   Double_t test;

   test = x + mRandom->gauss(0, res);

   while (fabs(test) > constraint)
   {
      test = x + mRandom->gauss(0, res);
   }

   return test;
}
/*
 *
 * Author: M. Mustafa
 *
 *
 **********************************************************
 * $Log: StPxlFastSim.cxx,v $
 * Revision 1.1  2013/05/12 21:43:32  jeromel
 * Initial revision, code peer review closed 2013/05/06
 *
 * Revision 1.5  2013/05/09 02:58:36  mstftsm
 * Fixed a bug which called for sensor local Z value.
 *
 */

