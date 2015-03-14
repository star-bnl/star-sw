/*
 *
 * Author: A. Rose, LBL, Y. Fisyak, BNL, M. Miller, MIT, M. Mustafa
 *
 *
 **********************************************************
 * $Log: StPxlFastSim.cxx,v $
 * Revision 1.9  2015/03/13 00:21:53  perev
 * Upload StMcIst Amilkar
 *
 * Revision 1.8  2015/01/27 19:11:49  mstftsm
 * Set idTruth of StPxlHit to -999 if parentTrack of mcHit does not exist (for protection).
 *
 * Revision 1.7  2015/01/27 01:31:09  smirnovd
 * Minor refactoring of StPxlFastSim::distortHit() to include a new warning for unphysical hit position
 *
 * Revision 1.6  2014/07/03 19:46:37  mstftsm
 * Revereted the changes made for the pileup adder. That does not belong to the master branch.
 *
 * Revision 1.4  2014/03/13 17:00:19  mstftsm
 * StPxlSimMaker has a method to switch on random seed for StRandom generatos in simulators. Default is not a random seed.
 *
 * Revision 1.2  2013/11/14 19:10:27  mstftsm
 * StMcPxlHit has been changed to be on local coordinates. We no longer transfor from global to local before smearing
 *
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
#include "StPxlDbMaker/StPxlDb.h"

#include "StMcEvent/StMcVertex.hh"
#include "StPhysicalHelixD.hh"
#include "StParticleDefinition.hh"
#include "StarMagField.h"
#include "SystemOfUnits.h"
#include "TMath.h"

#include "TGeoManager.h"
#include "TGeoMatrix.h"

#include "TDataSet.h"
#include "TObjectSet.h"

StPxlFastSim::~StPxlFastSim()
{
   if (mRandom) delete mRandom;
   if (mPxlDb) delete mPxlDb;
}
//____________________________________________________________
Int_t StPxlFastSim::initRun(const TDataSet& calib_db, const TObjectSet* pxlDbDataSet, const Int_t run)
{
   // run is not used in the current implementation, but might be necessary in the future.
   LOG_INFO << "StPxlFastSim::init()" << endm;

   if(pxlDbDataSet != 0)
   {
	   mPxlDb = (StPxlDb *)pxlDbDataSet->GetObject();
	   if (!mPxlDb)
	   {
		   LOG_ERROR << "StPxlFastSim - E - mPxlDb is not available" << endm;
		   return kStErr;
	   }
	   else
	   {
		   LOG_INFO << "StPxlFastSim - Using geometry from pxlDB" <<endm;
	   }
   }
   else
   {
	   LOG_INFO << "StPxlFastSim - Using ideal geometry" <<endm;
   }

   if (!mRandom) mRandom = new StRandom();
   if(mUseRandomSeed)
   {
	   Int_t seed = time(NULL);
	   mRandom->setSeed(seed);
	   LOG_INFO << "StPxlFastSim - smearing random generator is using random seed = " << seed <<endm;
   }
   else
   {
	   LOG_INFO << "StPxlFastSim - smearing random generator is using default seed" <<endm;
   }

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
   if (pxlHitError->coeff[0] <= 0 || pxlHitError->coeff[3] <= 0)
   {
      LOG_ERROR << "StPxlFastSim - E - negative or corrupted PXL hits errors in DB" << endm;
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
            LOG_DEBUG << "Sector/Ladder/Sensor = " << iSec + 1 << "/" << iLad + 1 << "/" << iSen + 1 << ". Number of sensor hits = " << nSenHits << endm;

            // Loop over hits in the sensor
            for (UInt_t iHit = 0; iHit < nSenHits; iHit++)
            {
               StMcPxlHit* mcPix = mcPxlSensorHitCol->hits()[iHit];

               //Long_t volId = mcPix->volumeId();
               Int_t sector = mcPix->sector();
               Int_t ladder = mcPix->ladder();
               //Int_t sensor = mcPix->sensor();

               Double_t localPixHitPos[3] = {mcPix->position().x(), mcPix->position().y(), mcPix->position().z()};

               LOG_DEBUG << "localPixHitPos = " << localPixHitPos[0] << " " << localPixHitPos[1] << " " << localPixHitPos[2] << endm;
               // please note that what is called local Y in the PXL sensor design
               // is actually called Z in STAR coordinates convention and vice-versa
               smearedX = distortHit(localPixHitPos[0], mResXPix, PXL_ACTIVE_X_LENGTH / 2.0);
               smearedZ = distortHit(localPixHitPos[2], mResZPix, PXL_ACTIVE_Y_LENGTH / 2.0);
               if (mResYPix) smearedY = distortHit(localPixHitPos[1], mResYPix, 0.0020); // Not properly constrained yet
               else smearedY = localPixHitPos[1];
               localPixHitPos[0] = smearedX;
               localPixHitPos[2] = smearedZ;
               localPixHitPos[1] = smearedY;
               LOG_DEBUG << "smearedlocal = " << localPixHitPos[0] << " " << localPixHitPos[1] << " " << localPixHitPos[2] << endm;
               Double_t smearedGlobalPixHitPos[3] = {0, 0, 0};
	       localToMatser(localPixHitPos,smearedGlobalPixHitPos,iSec+1,iLad+1,iSen+1);

               StThreeVectorF gpixpos(smearedGlobalPixHitPos);
               StThreeVectorF mRndHitError(0., 0., 0.);

               UInt_t hw = sector * 10 + ladder; // needs to be updated later after clustering alogrithms are finalized
               StPxlHit* tempHit = new StPxlHit(gpixpos, mRndHitError, hw, mcPix->dE() , 0);
               tempHit->setSector(iSec + 1);
               tempHit->setLadder(mcPix->ladder());
               tempHit->setSensor(mcPix->sensor());
               mcPix->parentTrack()? tempHit->setIdTruth(mcPix->parentTrack()->key(), 100): tempHit->setIdTruth(-999);
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

Int_t StPxlFastSim::addPxlHitsEmb(const StSPtrVecMcTrack& McTracks, 
				  StMcPxlHitCollection& mcPxlHitCol, 
				  StPxlHitCollection& pxlHitCol){
   
  Long_t NumOfMcTracks = McTracks.size();
  
  Float_t smearedX = 0, smearedY = 0, smearedZ = 0;
  
  //-->Set Magnetic field   (MUST BE A BETTER WAY TO DO THIS)
  Float_t center[3]={0,0,0}; 
  Float_t B[3]={0,0,0};  
  StarMagField::Instance()->BField(center,B);
  Float_t BField   = B[2]*kilogauss;
  //<--End set Magnetic field
  
  for (Int_t i=0;i<NumOfMcTracks;i++){   //-1->Start looping over the Mc Tracks
    
    StMcTrack *Trk = McTracks[i];
    if(!Trk) continue;
       
    StPhysicalHelixD tHelix( Trk->momentum(),
			     Trk->startVertex()->position(),
			     BField,
			     Trk->particleDefinition()->charge()); //Set the helix of the McTrack

    for(Int_t sector=1;sector<=10;sector++){//-2->Start looping over the Sectors
      for(Int_t ladder=1;ladder<=4 ;ladder++){//-3->Start looping over the Ladders
	for(Int_t sensor=1;sensor<=10;sensor++){//-4->Start looping over the Sensors
	  
	  TGeoHMatrix *interceptionM = (TGeoHMatrix *)mPxlDb->geoHMatrixSensorOnGlobal(sector,ladder,sensor);
	  if(!interceptionM) continue;
	  Double_t *Rotat = interceptionM->GetRotationMatrix();
	  Double_t *Trans = interceptionM->GetTranslation();
	  const StThreeVectorF senNorm(Rotat[1],Rotat[4],Rotat[7]);
	  const StThreeVectorF senCent(Trans);
	  
	  Double_t s = tHelix.pathLength(senCent,senNorm);//Set the path of the Mc track helix through the real geometry
	  if (s<0) continue;                              //Take only tracks in same direction of its momentum
	  
	  StThreeVectorF XXX = tHelix.at(s);                   //Get the interception coordinates
	  
	  Double_t xg[3] = {XXX.x(),XXX.y(),XXX.z()};  //Need to be in Double_t for the MasterToLocal
	  Double_t localPixHitPos[3] = {0,0,0};                    //
	  interceptionM->MasterToLocal(xg,localPixHitPos);
	  
	  if(!IsOnSensor(localPixHitPos)) continue;   // Local x and y coordinates must be within the wafer

	  //-->Pt correction
	  //Parameters are different for each layer also sign are oposite due sensor orientation
	  //These parameter are for pions
	  Int_t q = Trk->particleDefinition()->charge()!=0 ? 
	    Trk->particleDefinition()->charge()/abs(Trk->particleDefinition()->charge()): 0;
	  if (ladder==1) localPixHitPos[0]= localPixHitPos[0] - q*(43.87 *TMath::Landau(Trk->pt(),-0.433446,0.0161694,0)  + 0.001);
	  if (ladder!=1) localPixHitPos[0]= localPixHitPos[0] + q*(104931*TMath::Landau(Trk->pt(),-0.322185,0.000803993,0) + 0.01);
	  //<--Pt correction
	  
	  //-6->Look if the Track has a Mc hit to calculate the residual
	  Double_t McHit[3]={999,999,999};
	  MatchHit(mcPxlHitCol,Trk,localPixHitPos,McHit,sector,ladder,sensor,kTRUE);
	  if (McHit[0]==999) //Projection is not in the same sensor
	    MatchHit(mcPxlHitCol,Trk,localPixHitPos,McHit,sector,ladder,sensor,kFALSE);  
	  //-7-> Need to add the projected McTrack without partner McHit into StMcPxlHitCollection
	  if (McHit[0]==999) {       
	    StMcPxlHit* newHit = new StMcPxlHit(localPixHitPos,Trk->momentum(),Trk->energy(),
						0.002,                                  //step size in volume  NEEDTOFIX ~0.002 
						0,                                      //Time of flight NEEDTOFIX but is ~0
						Trk->key(),            
						sensor*100+ladder*10000+sector*1000000, //STAR volume id from g2t
						Trk);
	    mcPxlHitCol.addHit(newHit); 
	  } //<-7- End add the projected McTrack without partner McHit into StMcPxlHitCollection
	  //<-6- End look if the Track has a Mc hit
	  
	  //-5->Start inserting to pxlHitColl (same as Mustafa)
	  LOG_DEBUG << "localPixHitPos = " << localPixHitPos[0] << " " << localPixHitPos[1] << " " << localPixHitPos[2] << endm;
	  // please note that what is called local Y in the PXL sensor design
	  // is actually called Z in STAR coordinates convention and vice-versa
	  smearedX = distortHit(localPixHitPos[0], mResXPix, PXL_ACTIVE_X_LENGTH / 2.0);
	  smearedZ = distortHit(localPixHitPos[2], mResZPix, PXL_ACTIVE_Y_LENGTH / 2.0);
	  if (mResYPix) smearedY = distortHit(localPixHitPos[1], mResYPix, 0.0020); // Not properly constrained yet
	  else smearedY = localPixHitPos[1];
	  //localPixHitPos[0] = smearedX;
	  //localPixHitPos[2] = smearedZ;
	  //localPixHitPos[1] = smearedY;
	  LOG_DEBUG << "smearedlocal = " << localPixHitPos[0] << " " << localPixHitPos[1] << " " << localPixHitPos[2] << endm;
	  Double_t smearedGlobalPixHitPos[3] = {0, 0, 0};
	  localToMatser(localPixHitPos,smearedGlobalPixHitPos,sector,ladder,sensor);
	  
	  StThreeVectorF gpixpos(smearedGlobalPixHitPos);
	  StThreeVectorF mRndHitError(0., 0., 0.);
	  
	  UInt_t hw = sector * 10 + ladder; // needs to be updated later after clustering alogrithms are finalized
	  StPxlHit* tempHit = new StPxlHit(gpixpos, mRndHitError, hw, Trk->energy() , 0);//In StPxlHit.h is the charge not energy 
	  tempHit->setSector(sector);
	  tempHit->setLadder(ladder);
	  tempHit->setSensor(sensor);
	  //mcPix->parentTrack()? tempHit->setIdTruth(mcPix->parentTrack()->key(), 100): tempHit->setIdTruth(-999);
	  tempHit->setIdTruth(Trk->key(), 100);
	  tempHit->setDetectorId(kPxlId);
	  tempHit->setId(Trk->key());     // Switch from: mcPix->key());
	  tempHit->setLocalPosition(localPixHitPos[0], localPixHitPos[1], localPixHitPos[2]);
	  
	  LOG_DEBUG << "pxlHit location x= " << tempHit->position().x() 
		    << "; y= " << tempHit->position().y() 
		    << "; z= " << tempHit->position().z() << endm;
	  
	  pxlHitCol.addHit(tempHit);
	  //<-5-End inserting to pxlHitColl (same as Mustafa)

	}//<-4-End looping over the Sensor
      }//<-3-End looping over the Ladder
    }//<-2-End looping over the Sector
  
  }//<-1-End looping over the Mc Tracks
  
  return kStOK;
}
//__________________________________________________________________________
Bool_t StPxlFastSim::IsOnSensor(Double_t LocalPosition[3]){
  if((LocalPosition[0] >(PXL_ACTIVE_X_LENGTH/2.)) || (LocalPosition[0] < (-PXL_ACTIVE_X_LENGTH/2.)) || 
     (LocalPosition[2]>(PXL_ACTIVE_Y_LENGTH/2.)) || (LocalPosition[2] < (-PXL_ACTIVE_Y_LENGTH/2.)))
    return kFALSE;
    else return kTRUE;
}
//________________________________________________________________________________
void StPxlFastSim::MatchHit(StMcPxlHitCollection& mcPxlHitCol, StMcTrack *Trk, Double_t local[3], Double_t Mchiit[3], Int_t sec, Int_t lad, Int_t sen, Bool_t flag){
  if (!flag) {sec=1; lad=1; sen=1;} 
  for(Int_t sector=sec;sector<=10;sector++){//-->Start looping over the Sectors
    for(Int_t ladder=lad;ladder<=4;ladder++){//-->Start looping over the Ladders
      for(Int_t sensor=sen;sensor<=10;sensor++){
	UInt_t nSenHits = mcPxlHitCol.sector(sector-1)->ladder(ladder-1)->sensor(sensor-1)->hits().size();
	for(UInt_t hiit=0;hiit<nSenHits;hiit++){
	  StMcPxlHit* mcPix = mcPxlHitCol.sector(sector-1)->ladder(ladder-1)->sensor(sensor-1)->hits()[hiit];
	  if (!mcPix) continue;
	  if (Trk->key() == mcPix->parentTrack()->key()) {
	    Mchiit[0] = mcPix->position().x();
	    Mchiit[1] = mcPix->position().y();
	    Mchiit[2] = mcPix->position().z();
	    //-->Change the mcHit to the new McTrack projection 
	    mcPix->setPosition(local);
	    mcPix->setLocalMomentum(Trk->momentum());
	    mcPix->setdE(Trk->energy());
	    //mcPix->setdS();
	    //mcPix->setTof();
	    mcPix->setKey(Trk->key());
	    if (!flag) mcPix->setVolumeId(sensor*100 + ladder*10000 + sector*1000000);
	    mcPix->setParentTrack(Trk);
	    //<-- End Change the mcHit to the new McTrack projection 
	    break;
	  }
	} 
      } 
    } 
  }
}
//____________________________________________________________
/**
 * Calculates and returns new value for the local coordinate x by smearing it
 * acccording to a normal distribution N(mean, sigma) = N(x, res). The returned
 * value is constrained to be within the characteristic dimension detLength
 * provided by the user.
 */
double StPxlFastSim::distortHit(const double x, const double res, const double detLength) const
{
  // Do not smear x when it is outside the physical limits. Issue a warning instead
  if (fabs(x) > detLength) {
    LOG_WARN << "distortHit() - Generated hit is outside detector sensor plane" << endm;
    return x;
  }
  
  double smeared_x;
  
  do {
    smeared_x = mRandom->gauss(x, res);
  } while ( fabs(smeared_x) > detLength);
  
  return smeared_x;
}


//____________________________________________________________
void StPxlFastSim::localToMatser(Double_t* local,Double_t* master,Int_t sector,Int_t ladder,Int_t sensor)
{
  if(mPxlDb)
    {
      TGeoHMatrix *combP = (TGeoHMatrix *)mPxlDb->geoHMatrixSensorOnGlobal(sector, ladder,sensor);
      combP->LocalToMaster(local, master);
    }
  else
    {
      TString Path("");
      LOG_DEBUG << endm;
      Path = Form("/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1/PXLA_%i/LADR_%i/PXSI_%i/PLAC_1", sector, ladder, sensor);
      
      gGeoManager->RestoreMasterVolume();
      gGeoManager->CdTop();
      gGeoManager->cd(Path);
      
      gGeoManager->GetCurrentMatrix()->LocalToMaster(local, master);
    }
}
/*
 *
 * Author: M. Mustafa
 *
 *
 **********************************************************
 * $Log: StPxlFastSim.cxx,v $
 * Revision 1.9  2015/03/13 00:21:53  perev
 * Upload StMcIst Amilkar
 *
 * Revision 1.8  2015/01/27 19:11:49  mstftsm
 * Set idTruth of StPxlHit to -999 if parentTrack of mcHit does not exist (for protection).
 *
 * Revision 1.7  2015/01/27 01:31:09  smirnovd
 * Minor refactoring of StPxlFastSim::distortHit() to include a new warning for unphysical hit position
 *
 * Revision 1.6  2014/07/03 19:46:37  mstftsm
 * Revereted the changes made for the pileup adder. That does not belong to the master branch.
 *
 * Revision 1.4  2014/03/13 17:00:19  mstftsm
 * StPxlSimMaker has a method to switch on random seed for StRandom generatos in simulators. Default is not a random seed.
 *
 * Revision 1.2  2013/11/14 19:10:27  mstftsm
 * StMcPxlHit has been changed to be on local coordinates. We no longer transfor from global to local before smearing
 *
 * Revision 1.1  2013/05/12 21:43:32  jeromel
 * Initial revision, code peer review closed 2013/05/06
 *
 * Revision 1.5  2013/05/09 02:58:36  mstftsm
 * Fixed a bug which called for sensor local Z value.
 *
 */

