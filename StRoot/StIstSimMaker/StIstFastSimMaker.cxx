/* Add embedding functions and set to ladders format (Amilkar) 
$Id: StIstFastSimMaker.cxx,v 1.32 2015/03/13 00:21:06 perev Exp $ */

#include "TGeoManager.h"
#include "TDataSet.h"

#include "StIstSimMaker/StIstFastSimMaker.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEnumerations.h"
#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcHit.hh"
#include "StMcEvent/StMcIstHit.hh"
#include "StIstUtil/StIstConsts.h"
#include "StEvent/StIstHit.h"
#include "StEvent/StIstHitCollection.h"
#include "StIstDbMaker/StIstDb.h"
#include "StMcEvent/StMcIstHit.hh"
#include "StMcEvent/StMcIstHitCollection.hh"
#include "StThreeVectorF.hh"
#include "tables/St_HitError_Table.h"

#include "StMcEvent/StMcVertex.hh"
#include "StPhysicalHelixD.hh"
#include "StParticleDefinition.hh"
#include "StarMagField.h"
#include "SystemOfUnits.h"
#include "TMath.h"

ClassImp(StIstFastSimMaker)

StIstFastSimMaker::StIstFastSimMaker( const Char_t *name, bool useRandomSeed) : StMaker(name), mIstRot(NULL), mIstDb(NULL), mBuildIdealGeom(kTRUE),
  mRandom(useRandomSeed ? time(0) : 65539), mSmear(kTRUE)
{
}

//____________________________________________________________
Int_t StIstFastSimMaker::Init() {
  //   LOG_INFO << "StIstFastSimMaker::Init()" << endm;
   return kStOk;
}

//____________________________________________________________
Int_t StIstFastSimMaker::InitRun(int runNo)
{
   LOG_INFO << "StIstFastSimMaker::InitRun" << endm;
   if (mBuildIdealGeom && !gGeoManager) {

      GetDataBase("VmcGeometry");

      if (!gGeoManager) {
         LOG_ERROR << "Init() - "
            "Cannot initialize StIstFastSimMaker due to missing global object of TGeoManager class. "
            "Make sure STAR geometry is properly loaded with BFC AgML option" << endm;
         return kFatal;
      }
   }

   TDataSet *calibDataSet = GetDataBase("Calibrations/tracker");
   St_HitError *istTableSet = (St_HitError *) calibDataSet->Find("ist1HitError");
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

   if (!mIstRot) {
      LOG_FATAL << "InitRun(): mIstRot is not initialized" << endm;
      return kStFatal;
   }

   return kStOk;
}


/**
 * Retrieves GEANT hit information from StMcEvent then fills the StEvent's
 * StIstHitCollection with (possibly smeared) hit positions in either ideal or
 * misaligned geometry. Thus created StIstHitCollection is used in tracking.
 */
Int_t StIstFastSimMaker::Make()
{
   using namespace StIstConsts;

   // Get the input data structures from StEvent and StMcEvent
   StEvent *rcEvent =  (StEvent *) GetInputDS("StEvent");

   if (! rcEvent) {LOG_WARN << "Make() - StEvent not found" << endl; return kStWarn;}

   StMcEvent *mcEvent = (StMcEvent *) GetInputDS("StMcEvent");

   if (! mcEvent) {LOG_WARN << "Make() - StMcEvent not found" << endl; return kStWarn;}

   // Store hits into Ist Hit Collection
   StIstHitCollection *istHitCollection = rcEvent->istHitCollection();

   if (!istHitCollection) {
      istHitCollection = new StIstHitCollection;
      rcEvent->setIstHitCollection(istHitCollection);
      LOG_WARN << "Make() - Added new StIstHitCollection to StEvent" << endm;
   }

   //Get MC Ist hit collection. This contains all ist hits.
   //const StMcIstHitCollection *istMcHitCol = mcEvent->istHitCollection();
   StMcIstHitCollection *istMcHitCol = mcEvent->istHitCollection();     //Amilkar: not constant any more

   if (!istMcHitCol) {
      LOG_FATAL << "No Ist MC hits found." << endm;
      return kStFatal;
   }

   Int_t nIsthits = istMcHitCol->numberOfHits();
   LOG_DEBUG << "StIstFastSimMaker::Make() -I- Loaded " << nIsthits << " ist hits. \n";
   
   addIstHits(istMcHitCol,istHitCollection);                             //NEED to put something to select
   //addIstHitsEmb(mcEvent->tracks(),istMcHitCol,istHitCollection);      //

   //return kStOK;
}

//_____________________________________________________________
Int_t StIstFastSimMaker::addIstHits(StMcIstHitCollection *McIstHitCol, StIstHitCollection *IstHitCol){
  for (Int_t ladder = 1; ladder <=  McIstHitCol->numberOfLadders(); ladder++){//-1->Start ladders
    for (Int_t sensor = 1; sensor <= kIstNumSensorsPerLadder; sensor++){//-2->Start sensors
      Int_t nSenHits = McIstHitCol->ladder(ladder-1)->sensor(sensor-1)->hits().size();
      LOG_DEBUG << "Ladder/Sensor = " << ladder << "/" << sensor << ". Number of sensor hits = " << nSenHits << endm;
      
      for (Int_t iHit = 0; iHit < nSenHits; iHit++){//-3->Start hits
	StMcIstHit* McHit = McIstHitCol->ladder(ladder-1)->sensor(sensor-1)->hits()[iHit];
	if (!McHit) continue;
	
	//-4-> Set Db tables
	Int_t matIst = 1000 + (ladder - 1) * kIstNumSensorsPerLadder + sensor;
	LOG_DEBUG << " matIst : " << matIst << endm;
	TGeoHMatrix *combI = NULL;
	//Access VMC geometry once no IST geometry Db tables available or using ideal geoemtry is set
	if (mBuildIdealGeom) {
	  TString path("HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1");
	  path += Form("/IBAM_%ld/IBLM_%ld/IBSS_1", ladder, sensor);
	  gGeoManager->RestoreMasterVolume();
	  gGeoManager->CdTop();
	  gGeoManager->cd(path);
	  combI = (TGeoHMatrix *)gGeoManager->GetCurrentMatrix();
	}
	else { //using mis-aligned gemetry from IST geometry DB tables
	  combI = (TGeoHMatrix *)mIstRot->FindObject(Form("R%04i", matIst));  
	}
	//<-4- End set Db tables
	
	//-5->Set smearing
	Double_t globalIstHitPos[3] = {McHit->position().x(), McHit->position().y(), McHit->position().z()};
	Double_t localIstHitPos[3] = {McHit->position().x(), McHit->position().y(), McHit->position().z()};
	
	if (mSmear) { // smearing on
	  localIstHitPos[0] = distortHit(localIstHitPos[0], mResXIst1, kIstSensorActiveSizeRPhi / 2.0);
	  localIstHitPos[2] = distortHit(localIstHitPos[2], mResZIst1, kIstSensorActiveSizeZ / 2.0);
	}
	else { //smearing off
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
	//<-5-End set smearing

	//-6->Do local toglobal transform with geometry table and fill StIstHitCollection
	combI->LocalToMaster(localIstHitPos, globalIstHitPos);
	StThreeVectorF gistpos(globalIstHitPos);
	
	UInt_t hw =  (ladder-1) * kIstNumSensorsPerLadder + sensor;
	StThreeVectorF mHitError(0., 0., 0.);
	StIstHit *tempHit = new StIstHit(gistpos, mHitError, hw, McHit->dE(), 0);
	tempHit->setDetectorId(kIstId);
	tempHit->setId(McHit->key());
	McHit->parentTrack()? tempHit->setIdTruth(McHit->parentTrack()->key(), 100): tempHit->setIdTruth(-999);
	tempHit->setLocalPosition(localIstHitPos[0], localIstHitPos[1], localIstHitPos[2]);
	IstHitCol->addHit(tempHit);
	//<-6-End local toglobal transform with geometry table and fill StIstHitCollection
	
      }//<-3-End hits
      
    }//<-2-End sensors
  }//<-1-End ladders
  return kStOK;
}
//_____________________________________________________________
Int_t StIstFastSimMaker::addIstHitsEmb(const StSPtrVecMcTrack &McTracks, 
				 StMcIstHitCollection *McIstHitCol, 
				 StIstHitCollection *IstHitCol){
  
  Long_t NumOfMcTracks = McTracks.size();
  
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

    for (Int_t ladder = 1; ladder <=  McIstHitCol->numberOfLadders(); ladder++){//-1->Start ladders
      for (Int_t sensor = 1; sensor <= kIstNumSensorsPerLadder; sensor++){//-2>Start looping over the Sensors
    
      
	TGeoHMatrix *interceptionM = (TGeoHMatrix *)mIstDb->getHMatrixSensorOnGlobal(ladder,sensor);
	if(!interceptionM) continue;
	Double_t *Rotat = interceptionM->GetRotationMatrix();
	Double_t *Trans = interceptionM->GetTranslation();
	const StThreeVectorF senNorm(Rotat[1],Rotat[4],Rotat[7]);
	const StThreeVectorF senCent(Trans);
	
	Double_t s = tHelix.pathLength(senCent,senNorm);//Set the path of the Mc track helix through the real geometry
	if (s<0) continue;                              //Take only tracks in same direction of its momentum
	
	StThreeVectorF XXX = tHelix.at(s);                   //Get the interception coordinates
	
	Double_t xg[3] = {XXX.x(),XXX.y(),XXX.z()};  //Need to be in Double_t for the MasterToLocal
	Double_t localIstHitPos[3] = {0,0,0};                    //
	interceptionM->MasterToLocal(xg,localIstHitPos);
	
	//-->Pt correction
	//Fit Landau funtion from (0.1 , 5)
	Int_t q = Trk->particleDefinition()->charge()!=0 ? 
	  Trk->particleDefinition()->charge()/abs(Trk->particleDefinition()->charge()): 0;
	localIstHitPos[0]= localIstHitPos[0] - q*1568.62*TMath::Landau(Trk->pt(),-0.463681,0.0128018 ,0);
	//<--Pt correction

	if(!IsOnSensor(localIstHitPos)) continue;   // Local x and y coordinates must be within the wafer
	
	//-5->Look if the Track has a Mc hit to calculate the residual
	Double_t McHit[3]={999,999,999};
	MatchHit(McIstHitCol,Trk,localIstHitPos,McHit,ladder,sensor,kTRUE);
	if (McHit[0]==999) //Projection is not in the same sensor
	  MatchHit(McIstHitCol,Trk,localIstHitPos,McHit,ladder,sensor,kFALSE);  
	//-6-> Need to add the projected McTrack without partner McHit into StMcIstHitCollection
	if (McHit[0]==999) {       
	  StMcIstHit* newHit = new StMcIstHit(localIstHitPos,Trk->momentum(),Trk->energy(),
					      0.002,                                  //step size in volume  NEEDTOFIX ~0.002 
					      0,                                      //Time of flight NEEDTOFIX but is ~0
					      Trk->key(),            
					      sensor*10000 + (1+ladder)*1000000, //STAR volume id from g2t
					      Trk);
	  McIstHitCol->addHit(newHit); 
	} //<-6- End add the projected McTrack without partner McHit into StMcIstHitCollection
	  //<-7- End look if the Track has a Mc hit
	
	//-4->Start inserting to istHitColl (same as Yaping)
	Int_t matIst = 1000 + (ladder - 1 ) * kIstNumSensorsPerLadder + sensor;
	LOG_DEBUG << " matIst : " << matIst << endm;
	TGeoHMatrix *combI = NULL;
	//Access VMC geometry once no IST geometry Db tables available or using ideal geoemtry is set
	if (mBuildIdealGeom) {
	  TString path("HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1");
	  path += Form("/IBAM_%ld/IBLM_%ld/IBSS_1", ladder, sensor);
	  gGeoManager->RestoreMasterVolume();
	  gGeoManager->CdTop();
	  gGeoManager->cd(path);
	  combI = (TGeoHMatrix *)gGeoManager->GetCurrentMatrix();
	}
	else { //using mis-aligned gemetry from IST geometry DB tables
	  combI = (TGeoHMatrix *)mIstRot->FindObject(Form("R%04i", matIst));  
	}
	//NO SMEARING
	/*if (mSmear) { // smearing on
	  localIstHitPos[0] = distortHit(localIstHitPos[0], mResXIst1, kIstSensorActiveSizeRPhi / 2.0);
	  localIstHitPos[2] = distortHit(localIstHitPos[2], mResZIst1, kIstSensorActiveSizeZ / 2.0);
	}
	else { //smearing off
	  //discrete hit local position (2D structure of IST sensor pads)
	  Float_t rPhiPos   = kIstSensorActiveSizeRPhi / 2.0 - localIstHitPos[0];
	  Float_t zPos      = localIstHitPos[2] + kIstSensorActiveSizeZ / 2.0;
	  Short_t meanColumn  = (Short_t)floor( zPos / kIstPadPitchColumn ) + 1;
	  Short_t meanRow     = (Short_t)floor( rPhiPos / kIstPadPitchRow ) + 1;
	  rPhiPos = (meanRow - 1) * kIstPadPitchRow + 0.5 * kIstPadPitchRow; //unit: cm
	  zPos    = (meanColumn - 1) * kIstPadPitchColumn + 0.5 * kIstPadPitchColumn; //unit: cm
	  localIstHitPos[0] = kIstSensorActiveSizeRPhi / 2.0 - rPhiPos;
	  localIstHitPos[2] = zPos - kIstSensorActiveSizeZ / 2.0;
	}*/
	
	combI->LocalToMaster(localIstHitPos, xg);//globalIstHitPos);
	StThreeVectorF gistpos(xg);//globalIstHitPos);
        UInt_t hw =  (ladder-1) * kIstNumSensorsPerLadder + sensor;
	StThreeVectorF mHitError(0., 0., 0.);
	StIstHit *tempHit = new StIstHit(gistpos, mHitError, hw, Trk->energy(), 0);
	//tempHit->setLadder(ladder);            //CANNOT SET THE LADDER AND SENSOR
	//tempHit->setSensor(sensor);
	tempHit->setDetectorId(kIstId);
	tempHit->setId(Trk->key());
	//McHit->parentTrack()? tempHit->setIdTruth(McHit->parentTrack()->key(), 100): tempHit->setIdTruth(-999);
	tempHit->setLocalPosition(localIstHitPos[0], localIstHitPos[1], localIstHitPos[2]);
	IstHitCol->addHit(tempHit);
	//<-4-End inserting to istHitColl (same as Yaping)
	
      }//<-3-End looping over the Sensor
    }//<-2-End looping over the Ladder
    
  }//<-1-End looping over the Mc Tracks
  
  return kStOK;
}
//__________________________________________________________________________
Bool_t StIstFastSimMaker::IsOnSensor(Double_t LocalPosition[3]){
  if((LocalPosition[0] >(kIstSensorActiveSizeRPhi/2.)) || (LocalPosition[0] < (-kIstSensorActiveSizeRPhi/2.)) || 
     (LocalPosition[2]>(kIstSensorActiveSizeZ/2.)) || (LocalPosition[2] < (-kIstSensorActiveSizeZ/2.)))
    return kFALSE;
    else return kTRUE;
}
//________________________________________________________________________________
void StIstFastSimMaker::MatchHit(StMcIstHitCollection *McIstHitCol, StMcTrack *Trk, 
			    Double_t local[3], Double_t Mchiit[3], 
			    Int_t lad, Int_t sen, Bool_t flag){
  if (!flag) {lad=1; sen=1;} 
  for(Int_t ladder=lad;ladder<=24;ladder++){//-->Start looping over the Ladders
    for(Int_t sensor=sen;sensor<=6;sensor++){
      UInt_t nSenHits = McIstHitCol->ladder(ladder-1)->sensor(sensor-1)->hits().size();
      for(UInt_t hiit=0;hiit<nSenHits;hiit++){
	StMcIstHit* mcIst = McIstHitCol->ladder(ladder-1)->sensor(sensor-1)->hits()[hiit];
	if (!mcIst) continue;
	if (Trk->key() == mcIst->parentTrack()->key()) {
	  Mchiit[0] = mcIst->position().x();
	  Mchiit[1] = mcIst->position().y();
	  Mchiit[2] = mcIst->position().z();
	  //-->Change the mcHit to the new McTrack projection 
	  mcIst->setPosition(local);
	  mcIst->setLocalMomentum(Trk->momentum());
	  mcIst->setdE(Trk->energy());
	  //mcPix->setdS();
	  //mcPix->setTof();
	  mcIst->setKey(Trk->key());
	  if (!flag) mcIst->setVolumeId(sensor*10000 + (1+ladder)*1000000);
	  mcIst->setParentTrack(Trk);
	  //<-- End Change the mcHit to the new McTrack projection 
	  break;
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
Double_t StIstFastSimMaker::distortHit(const Double_t x, const Double_t res, const Double_t detLength)
{
   // Do not smear x when it is outside the physical limits. Issue a warning instead
   if (fabs(x) > detLength) {
      LOG_WARN << "distortHit() - Generated hit is outside detector sensor plane" << endm;
      return x;
   }

   Double_t smeared_x;

   do {
      smeared_x = mRandom.Gaus(x, res);
   } while ( fabs(smeared_x) > detLength);

   return smeared_x;
}
