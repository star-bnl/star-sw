/////////////////////////////////////////////////////////////////////
//
// $Id: StPeCTrigger.cxx,v 1.13 2013/10/28 14:18:16 ramdebbe Exp $
// $Log: StPeCTrigger.cxx,v $
// Revision 1.13  2013/10/28 14:18:16  ramdebbe
// added arrays to handle bbc and zdc information
//
// Revision 1.12  2013/01/24 15:44:59  ramdebbe
// added ZDC shower max information to output tree and bbc small tubes individual ADC. Returns UPC_Main trigger
//
// Revision 1.11  2012/06/26 18:29:29  ramdebbe
// previous entry did not include actual changes
//
// Revision 1.10  2012/06/13 15:43:15  ramdebbe
// topo and main triggers for different run periods
//
// Revision 1.9  2007/04/28 17:56:35  perev
// Redundant StChain.h removed
//
// Revision 1.8  2003/11/25 01:54:37  meissner
// correct several bugs: eta cut for tracks, charge sorting, add counting of FTPC and TPC primary tracks, Add bbc information
//
// Revision 1.7  2003/09/02 17:58:46  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.6  2002/12/16 23:04:02  yepes
// Field comes in KGauss and should be passed to routines in Teslas
// problem pointed out by Vladimir
//
// Revision 1.5  2002/03/19 22:23:52  meissner
// New variables: zdc unatt., Trigger word, MC tree if Geant Branch, DCA  for primary pairs, all tracks for secondary pairs (Test)
//
// add un attenuated zdc;s
// Revision 1.4  2001/04/25 18:12:05  perev
// HPcorrs
//
// Revision 1.3  2001/02/21 20:54:25  yepes
// *** empty log message ***
//
// Revision 1.0  2000/12/11 
//
//////////////////////////////////////////////////////////////////////
#include <Stiostream.h>
#include "StPeCTrigger.h"
#include "StPeCMaker.h"
#include "StEventTypes.h"
#include "Stypes.h"
#include "StMessMgr.h"
#include "StPeCCtbSlat.h"



ClassImp(StPeCTrigger)

StPeCTrigger::StPeCTrigger() {
//
//  Set y2k L0
//
   l0_2000 = new StPeCL0 ();
   l0_2000->setYear1Input ();
   l0_2000->setP4SLuts ();
//
//  Set y2k corrected L0
//
   l0_2000Corrected = new StPeCL0 ();
   l0_2000Corrected->setYear1Input ();
   l0_2000Corrected->setP4PLuts ();
//
//  Set y2k+1 L0
//
   l0Offline2001 = new StPeCL0 ();
   l0Offline2001->setYear2Input ();
   l0Offline2001->setCountingLuts ();
//
   ctbSlats  = new TClonesArray ("StPeCCtbSlat", 10);

}
StPeCTrigger::~StPeCTrigger() {
   ctbSlats->Clear();
   delete ctbSlats ;
}

void StPeCTrigger::clear() {
   ctbSlats->Clear();
}

Int_t StPeCTrigger::process(StEvent *event)
{
  unsigned int i,j;
  const StTriggerId * ttid;
  // get trigger word 
  tw = event->l0Trigger()->triggerWord();
  runN   = event->runId(); 
  LOG_INFO << "StPeCTrigger::reading StEvent ---------- " << endm;
  l0_2000->setInfoLevel ( infoLevel );
//  l0_2000Corrected->setInfoLevel ( infoLevel );

  StL0Trigger* l0Data = event->l0Trigger();


  // read trigger information with up to date methods
  //
 const  StTriggerData * trigData = event->triggerData();
  if(!trigData) {
    LOG_ERROR << "StTriggerData not available "<< endm;
  }
  nBtofTriggerHits = trigData->tofMultiplicity();
  nBTOFhits =  event->btofCollection()->tofHits().size();
  bunchId = trigData->bunchId7Bit();
  lastDSM0 = trigData->lastDSM(0);
  lastDSM1 = trigData->lastDSM(1);
  StTriggerIdCollection* triggerIdColl = event->triggerIdCollection();
  if(triggerIdColl) {
    ttid= triggerIdColl->nominal();

//     LOG_INFO << "StPeCTrigger::trigger ID ----max------ " <<ttid->maxTriggerIds()  << endm;

//     for(int it=0;it<ttid->maxTriggerIds();it++){
//      LOG_INFO << "StPeCTrigger::trigger ID ---------- " <<ttid->triggerId(it)  << endm;
//     }
//     if(ttid->isTrigger(260002)) {
//       cout << "Got IDS ------ ZDC_Monitor  ------------------------------------------------------ " <<endl; 
//     }
//     LOG_INFO << "StPeCTrigger::process run number ---------- " <<runN << endm;
    if(runN>11011035 && runN<= 11039028) trg_3000     =ttid->isTrigger(1);       //run10 UPC Main unofficial
    if(runN>11011035 && runN<= 11039028) trg_3001     =ttid->isTrigger(11);      //run10 UPC Topo not present
    if(runN>11039028 && runN<= 11077018) trg_3000     =ttid->isTrigger(260023);  //run10 UPC Main official 260750
    if(runN>11050046 && runN<= 11077018) trg_3001     =ttid->isTrigger(1);      //run10 UPC Topo was never official

    if(runN>12130030 && runN<= 12146002) trg_3000     =ttid->isTrigger(4);       //run11 UPC Main unofficial
    if(runN>12130030 && runN<= 12179050) trg_3001     =ttid->isTrigger(11);      //run11 UPC Topo was never official
    if(runN>12146002 && runN<= 12152016) trg_3000     =ttid->isTrigger(350007);  //run11 UPC Main official
    if(runN>12152016 && runN<= 12179050) trg_3000     =ttid->isTrigger(350017);  //run11 UPC Main official

    if(runN>13116029 && runN<= 13116046) trg_3000     =ttid->isTrigger(1);       //run12 UU UPC Main unofficial
    if(runN>13110010 && runN<= 13116042) trg_3001     =ttid->isTrigger(2);       //run12 UU UPC Topo
    if(runN>13121046 && runN<= 13125024) trg_3001     =ttid->isTrigger(1);       //run12 UU UPC Topo
    if(runN>13125024 && runN<= 13130033) trg_3001     =ttid->isTrigger(400604);  //run12 UU UPC Topo
    if(runN>13131006 && runN<= 13136015) trg_3001     =ttid->isTrigger(400614);      //run12 UU UPC Topo
    if(runN>13116047 && runN<= 13117027) trg_3000     =ttid->isTrigger(400601);  //run12 UU UPC Main official
    if(runN>13117027 && runN<= 13117036) trg_3000     =ttid->isTrigger(400611);  //run12 UU UPC Main official
    if(runN>13117036 && runN<= 13118017) trg_3000     =ttid->isTrigger(400621);  //run12 UU UPC Main official
    if(runN>13118017 && runN<= 13130033) trg_3000     =ttid->isTrigger(400631);  //run12 UU UPC Main official
    if(runN>13131006 && runN<= 13136015) trg_3000     =ttid->isTrigger(400641);  //run12 UU UPC Main official

//     trg_3000     =ttid->isTrigger(1);  
//     trg_3001     =0;   //ttid->isTrigger(11); //run11 from 011035 to 039028  no topo trigger  //use in firstList.list
//     trg_3000     =ttid->isTrigger(260750);  
//     trg_3001     =0;   //ttid->isTrigger(11); //run11 from 011035 to 039028  no topo trigger used in secondList.list 
//     trg_3000     =ttid->isTrigger(400631); //run12 UU 
//     trg_3001     =ttid->isTrigger(400604); //run12 UU starting  from 13125025 to end. Used in thirdList.list and fourth list (out of 4 lists) 
//     trg_3000     =ttid->isTrigger(410601); //run12 cuAu 
//     trg_3001     =ttid->isTrigger(410604); //run12 cuAu starting  from 13139056 to end. 
  }
  StTriggerDetectorCollection* trg = event->triggerDetectorCollection();
  if ( !trg ) {
     printf ( "StPeCTrigger::StPeCTrigger: No trigger information \n" ) ;
     return 1 ;
  }
  StCtbTriggerDetector& ctb = trg->ctb();
  StMwcTriggerDetector& mwc = trg->mwc();
  StZdcTriggerDetector& zdc = trg->zdc();
  StBbcTriggerDetector& bbc = trg->bbc();
  StEmcTriggerDetector& emc = trg->emc();


//   if ( l0Data && infoLevel > 0 ){
//     cout << "L0 mwcCtbMultiplicity = " << l0Data->mwcCtbMultiplicity() << endl;
//   }
  if(&zdc){
    if ( infoLevel > 10 ) {
       cout << "ZDC sum " << zdc.adcSum() << endl;
       cout << "ZDC sum west " << zdc.adcSum(west) << endl;
       cout << "ZDC sum east " << zdc.adcSum(east) << endl;
    }
    // attenuated signals 
    zdcWest = zdc.adcSum(west) ;
    zdcEast = zdc.adcSum(east) ;
    zdcSum  = zdc.adcSum() ;
    // unattenuated  // see StZdcTriggerDetector documentation
    zdcWestUA = zdc.adc(0) ;
    zdcEastUA = zdc.adc(4) ;
    zdcSumUA  = zdcEastUA+zdcWestUA ;
    int hori = 1;
    int vert = 0;
    //SMD information
    for(int i=0;i<8;i++){
      zdcSMDEastH[i] = trigData->zdcSMD(east, hori, i+1);
      zdcSMDEastV[i] = trigData->zdcSMD(east, vert, i+1);
      zdcSMDWestH[i] = trigData->zdcSMD(west, hori, i+1);
      zdcSMDWestV[i] = trigData->zdcSMD(west, vert, i+1);
    }

  }
  else {
    zdcWestUA = 0 ;
    zdcEastUA = 0 ;
    zdcSumUA  = 0 ;

    zdcWest = 0 ;
    zdcEast = 0 ;
    zdcSum  = 0 ;

    for(int i=0;i<8;i++){
      zdcSMDEastH[i] = 0;
      zdcSMDEastV[i] = 0;
      zdcSMDWestH[i] = 0;
      zdcSMDWestV[i] = 0;
    }
  }

//
//   MWC
//
  float mwcsum = 0.0;

  float mwcThres = 2. ;
  nMwcHits = 0 ;
  mwcSW = 0 ;
  mwcNW = 0 ;
  mwcTW = 0 ;
  mwcBW = 0 ;
  mwcSE = 0 ;
  mwcNE = 0 ;
  mwcTE = 0 ;
  mwcBE = 0 ;
  if(&mwc){
    for(i=0; i<mwc.numberOfSectors(); i++){
      for(j=0; j<mwc.numberOfSubSectors(); j++){
	mwcsum += mwc.mips(i,j,0);
	if ( mwc.mips(i,j,0) > mwcThres ) { 
	   nMwcHits++ ;
           if      ( i ==  1 || i ==  2 || i ==  3 ) mwcSW++ ;
	   else if ( i ==  4 || i ==  5 || i ==  6 ) mwcBW++ ;
	   else if ( i ==  7 || i ==  8 || i ==  9 ) mwcNW++ ;
	   else if ( i == 10 || i == 11 || i ==  0 ) mwcTW++ ;
           else if ( i == 13 || i == 14 || i == 15 ) mwcNE++ ;
	   else if ( i == 16 || i == 17 || i == 18 ) mwcBE++ ;
	   else if ( i == 19 || i == 20 || i == 21 ) mwcSE++ ;
	   else if ( i == 22 || i == 23 || i == 12 ) mwcTE++ ;
	}
      }
    }
    if ( infoLevel > 1 ) 
       cout << "mwc mips " << mwcsum << endl;
  }
//
//   CTB
//
  float ctbsum=0.0;
  ctbSW = 0 ;
  ctbNW = 0 ;
  ctbTW = 0 ;
  ctbBW = 0 ;
  ctbSE = 0 ;
  ctbNE = 0 ;
  ctbTE = 0 ;
  ctbBE = 0 ;
  nCtbHits = 0 ;
  float ctbThres = 2. ;
  if(&ctb){
    TClonesArray &pCtbSlats = *ctbSlats;
    for(i=0; i<120; i++){
      for(j=0; j<2; j++){
	ctbsum += ctb.mips(i,j,0);
	if ( ctb.mips(i,j,0) > 0 ) {
	   Byte_t i_eta = (Byte_t)(j+1);
	   Byte_t i_phi = (Byte_t)(i+1);
	   Int_t  adc   = (Int_t)ctb.mips(i,j,0) ;
           new(pCtbSlats[nCtbHits++]) StPeCCtbSlat(i_phi,i_eta,adc) ;
	}

	if ( ctb.mips(i,j,0) > ctbThres ) {
           if      ( i >=   5 && i <  20 ) ctbSW++ ;
	   else if ( i >=  20 && i <  35 ) ctbBW++ ;
	   else if ( i >=  35 && i <  50 ) ctbNW++ ;
	   else if ( i >=  50 && i <  60 ) ctbTW++ ;
	   else if (             i <   5 ) ctbTW++ ;
           else if ( i >=  65 && i <  80 ) ctbNE++ ;
	   else if ( i >=  80 && i <  95 ) ctbBE++ ;
	   else if ( i >=  95 && i < 110 ) ctbSE++ ;
	   else if ( i >= 110            ) ctbTE++ ;
	   else if ( i >=  60 && i <  65 ) ctbTE++ ;
         }
      }
    }
    if ( infoLevel > 10 ) 
       cout << "CTB MIP total sum = " << ctbsum << endl;
  }
  ctbSum = ctbsum ;
  mwcSum = mwcsum ;



  // BBC 
  if(&bbc){

    if ( infoLevel > 10 ) {
      cout << "BBC East adc[0] = " << bbc.adc(8) << endl;
      bbc.dump();
    }

    //    bbcAdcSumEastSm=bbc.adcSumEast();
    bbcAdcSumEastSm=bbc.adc(8);
    bbcAdcSumWestSm=bbc.adcSumWest();
    bbcAdcSumEastLg=bbc.adcSumEastLarge();;
    bbcAdcSumWestLg=bbc.adcSumWestLarge();;

    bbcNHitEastSm=bbc.nHitEast();
    bbcNHitWestSm=bbc.nHitWest();

    bbcNHitEastLg=bbc.nHitEastLarge();;
    bbcNHitWestLg=bbc.nHitWestLarge();;
 
//     bbcTacEast=bbc.tdcEarliestEast();
//     bbcTacWest=bbc.tdcEarliestWest();
//     cout<<"just before using triData "<<endl;
    if(trigData){
      bbcTacEast = trigData->bbcEarliestTDC(east);
      bbcTacWest = trigData->bbcEarliestTDC(west); 
      bbcTimeDiff = trigData->bbcTimeDifference();
      for(int i=0;i<36;i++){
	bbcTDCEast[i] = trigData->bbcTDC(east, i+1);
	bbcTDCWest[i] = trigData->bbcTDC(west, i+1);
	bbcADCEast[i] = trigData->bbcADC(east, i+1);
	bbcADCWest[i] = trigData->bbcADC(west, i+1);
      } 
    }
//     cout<<"after use trigData"<<endl;

  } else {
    bbcAdcSumEastSm=0;
    bbcAdcSumWestSm=0;
    bbcAdcSumEastLg=0;
    bbcAdcSumWestLg=0;

    bbcNHitEastSm=0;
    bbcNHitWestSm=0;
    bbcNHitEastLg=0;
    bbcNHitWestLg=0;

    bbcTacEast=0;
    bbcTacWest=0;

    for(int i=0;i<36;i++){
      bbcTDCEast[i] = 0;
      bbcTDCWest[i] = 0;
    }  
  }
//   if(ttid->isTrigger(260002)){
//     LOG_INFO << "StPeCTrigger::BBC West East ---------- " <<bbcAdcSumWestSm<<" -- "<<bbcAdcSumWestLg<<" --- "<< bbcAdcSumEastSm<<" -- "<<bbcAdcSumEastLg<< endm; 
//   }   
//
//   Simulate l0
//
  p4  = l0_2000->process ( event ) ;
  p4c = l0_2000Corrected->process ( event ) ;
  p5  = l0Offline2001->process ( event ) ;
//
//   Take quadrants from 2001 wiring
//
  // L0 simulator
  Bool_t trgOut[40];
  {for ( int i = 0 ; i < 40 ; i++ ) trgOut[i] = 0 ;}

  if ( infoLevel > 20 ) {
     printf ( "trgBits : " ) ;
     for ( int i = 0 ; i < 24 ; i++ ) {
        printf ( "%d ", trgOut[i] ) ;
     }
     printf ( "\n" ) ;
  }
  //
  // EMC 
  //
  //cout<<" StPeCTrigger ++++++++++++ number of towers +++++++ "<<emc.numberOfTowers()<<endl;
//
//   Get Ftpc info
//
  StFtpcHitCollection*        ftpHitC = 0 ;
  StFtpcPlaneHitCollection*   plane;
  StFtpcSectorHitCollection*  sector;

  ftpW = 0 ;
  ftpE = 0 ;

  ftpHitC = event->ftpcHitCollection();
  int nFtpcPlanes = 20 ;
  if ( ftpHitC ) {
     printf ( "there is a hit collection \n" ) ;
     for ( int iPlane = 0 ; iPlane < nFtpcPlanes ; iPlane++ ) {
        plane = 0 ;
	plane = ftpHitC->plane(iPlane);
	if ( !plane ) continue ;
	for ( int iSector = 0 ; iSector < 6 ; iSector++ ) {
	   sector = 0 ;
	   sector = plane->sector(iSector);
	   if ( !sector ) continue ;
           StSPtrVecFtpcHit& hits = sector->hits();
	   for ( unsigned int iHit = 0 ; iHit < hits.size() ; iHit++ ) {
//     printf ( "plane %d sector %d x y z %f %f %f \n", iPlane, iSector,
//               hits[iHit]->position().x(), hits[iHit]->position().y(), hits[iHit]->position().z()) ;
	      if ( iPlane < 10 ) ftpW++ ;		
	      else              ftpE++ ;

	   }
	}
     }

  }

  return trg_3000 ;
}


Int_t StPeCTrigger::process(StMuDst* mudst)
{
  unsigned int i,j;
  //make sure trg_2001 is reset before possible use
  trg_2001 = 0;
  trg_3001 = 0;

  // get trigger word 
  tw = mudst->event()->l0Trigger().triggerWord();
  LOG_INFO << "StPeCTrigger::reading StMuDst ---------- " << endm;
  // read trigger information with up to date methods
  //

  const StTriggerData * trigData;
  trigData =  const_cast< StTriggerData *> (mudst->event()->triggerData());

  if(!trigData) {
    LOG_ERROR << "StTriggerData not available in StMuDst "<< endm;
  }
    lastDSM0 = trigData->lastDSM(0);
    lastDSM1 = trigData->lastDSM(1);
  // get run number
  //

   runN = mudst->event()->eventInfo().runId(); 
   LOG_INFO << "StPeCTrigger Run ID: " << runN << endm;

  // get triggger ids
  vector<unsigned int> triggerIds;
  StMuTriggerIdCollection  tt=mudst->event()->triggerIdCollection();

  StTriggerId ttid;
  if(&tt) {
    ttid= tt.nominal();
    if(&ttid) {

    triggerIds=  ttid.triggerIds();
    std::vector<unsigned int>::iterator iiter;
//     for(iiter=triggerIds.begin(); iiter!=triggerIds.end(); iiter++) {
//     }
  
//     trg_3000     =ttid.isTrigger(260022);  //ZDC_monitor  run10  this two lines are used for trigger efficiency study Comment the 8 lines that follow
//     trg_3001     =ttid.isTrigger(260750);  //UPC_Main     run10

    if(runN>11011035 && runN<= 11039028) trg_3000     =ttid.isTrigger(1);       //run10 UPC Main unofficial
    if(runN>11011035 && runN<= 11039028) trg_3001     =ttid.isTrigger(11);      //run10 UPC Topo not present
    if(runN>11039028 && runN<= 11077018) trg_3000     =ttid.isTrigger(260750);  //run10 UPC Main official 260750
    if(runN>11050046 && runN<= 11077018) trg_3001     =ttid.isTrigger(1);      //run10 UPC Topo was never official

    if(runN>12130030 && runN<= 12146002) trg_3000     =ttid.isTrigger(4);       //run11 UPC Main unofficial
    if(runN>12130030 && runN<= 12179050) trg_3001     =ttid.isTrigger(11);      //run11 UPC Topo was never official
    if(runN>12146002 && runN<= 12152016) trg_3000     =ttid.isTrigger(350007);  //run11 UPC Main official
    if(runN>12152016 && runN<= 12179050) trg_3000     =ttid.isTrigger(350017);  //run11 UPC Main official

    if(runN>13116029 && runN<= 13116046) trg_3000     =ttid.isTrigger(1);       //run12 UU UPC Main unofficial
    if(runN>13110010 && runN<= 13116042) trg_3001     =ttid.isTrigger(2);       //run12 UU UPC Topo
    if(runN>13121046 && runN<= 13125024) trg_3001     =ttid.isTrigger(1);       //run12 UU UPC Topo
    if(runN>13125024 && runN<= 13130033) trg_3001     =ttid.isTrigger(400604);  //run12 UU UPC Topo
    if(runN>13131006 && runN<= 13136015) trg_3001     =ttid.isTrigger(400614);      //run12 UU UPC Topo
    if(runN>13116047 && runN<= 13117027) trg_3000     =ttid.isTrigger(400601);  //run12 UU UPC Main official
    if(runN>13117027 && runN<= 13117036) trg_3000     =ttid.isTrigger(400611);  //run12 UU UPC Main official
    if(runN>13117036 && runN<= 13118017) trg_3000     =ttid.isTrigger(400621);  //run12 UU UPC Main official
    if(runN>13118017 && runN<= 13130033) trg_3000     =ttid.isTrigger(400631);  //run12 UU UPC Main official
    if(runN>13131006 && runN<= 13136015) trg_3000     =ttid.isTrigger(400641);  //run12 UU UPC Main official



//     LOG_INFO << "StPeCTrigger::value of trg_3000 ZDC_monitor ---------- " <<trg_3000<< endm;
//     LOG_INFO << "StPeCTrigger::value of trg_3001 UPC_Main ---------- " <<trg_3001<< endm;
    LOG_INFO << "StPeCTrigger::value of trg_3000 UPC_Main ---------- " <<trg_3000<< endm;
    LOG_INFO << "StPeCTrigger::value of trg_3001 UPC_Topo ---------- " <<trg_3001<< endm;
    

    lastDSM0 = trigData->lastDSM(0);
    lastDSM1 = trigData->lastDSM(1);

//     cout<<" lastDSM 0 "<<lastDSM0<<" lastDSM1 "<<lastDSM1<<endl;

    if(runN==14116064){
      trg_3000 =  ttid.isTrigger(1);
      trg_3001 =  ttid.isTrigger(2);
      trg_3001 =  ttid.isTrigger(4);
    }
    if(runN>14137093){
      trg_3000 =  ttid.isTrigger(3);
      trg_3001 =  ttid.isTrigger(430104);
      trg_3001 =  ttid.isTrigger(430605);
    }

    }
  }

    int bit5  = trigData->lastDSM(5/16)  & (1<<(5  %16)) ? 1 : 0;
    int bit6  = trigData->lastDSM(6/16)  & (1<<(6  %16)) ? 1 : 0;
    int bit17 = trigData->lastDSM(17/16) & (1<<(17 %16)) ? 1 : 0;
    int bit18 = trigData->lastDSM(18/16) & (1<<(18 %16)) ? 1 : 0;
    int bit22 = trigData->lastDSM(22/16) & (1<<(22 %16)) ? 1 : 0;
    int bit23 = trigData->lastDSM(23/16) & (1<<(23 %16)) ? 1 : 0;
    int bit24 = trigData->lastDSM(24/16) & (1<<(24 %16)) ? 1 : 0;
    int bit25 = trigData->lastDSM(25/16) & (1<<(25 %16)) ? 1 : 0;
    int bit96 = trigData->lastDSM(96/16) & (1<<(96 %16)) ? 1 : 0;

  if(!bit5 && bit6 && !bit17 && !bit18 && bit22 && bit23 && bit24 && bit25 && !bit96) {
    trg_2001 = 1;
//     cout<<"UPC_Main set "<<endl;
//     cout<<"UPC_Main bits --- "<<bit5<<"        "<<bit6<<"       "<<bit17<<"       "<<bit18<<"   "<<bit22<<"        "<<bit23<<"       "<<bit24<<"       "<<bit25<<"    "<<bit96<<endl;
  }
  l0_2000->setInfoLevel ( infoLevel );
//  l0_2000Corrected->setInfoLevel ( infoLevel );

  StL0Trigger* l0Data = &mudst->event()->l0Trigger();

  StCtbTriggerDetector& ctb = mudst->event()->ctbTriggerDetector();
//  StMwcTriggerDetector& mwc = &mudst->event()->ctbTriggerDetector();
  StZdcTriggerDetector& zdc = mudst->event()->zdcTriggerDetector();
  StBbcTriggerDetector& bbc = mudst->event()->bbcTriggerDetector();

  if(&zdc){
    if ( infoLevel > 10 ) {
       cout << "ZDC sum " << zdc.adcSum() << endl;
       cout << "ZDC sum west " << zdc.adcSum(west) << endl;
       cout << "ZDC sum east " << zdc.adcSum(east) << endl;
    }
    // attenuated signals 
    zdcWest = trigData->zdcAttenuated(west);
    zdcEast = trigData->zdcAttenuated(east);
    zdcSum  = zdcEast+zdcWest ;

    zdcWestTDC = trigData->zdcTDC(west);
    zdcEastTDC = trigData->zdcTDC(east);
    zdcTimeDifference  = trigData->zdcTimeDifference();

    // unattenuated  // 
    zdcWestUA = trigData->zdcUnAttenuated(west);
    zdcEastUA = trigData->zdcUnAttenuated(east);
    zdcSumUA  = zdcEastUA+zdcWestUA ;
    int hori = 1;
    int vert = 0;
    //SMD information

    for(int i=0;i<8;i++){
      zdcSMDEastH[i] = trigData->zdcSMD(east, hori, i+1);
      zdcSMDEastV[i] = trigData->zdcSMD(east, vert, i+1);
      zdcSMDWestH[i] = trigData->zdcSMD(west, hori, i+1);
      zdcSMDWestV[i] = trigData->zdcSMD(west, vert, i+1);
    }

    zdcSMDHighestStripEastH = trigData->zdcSMDHighestStrip(east, hori);
    zdcSMDHighestStripEastV = trigData->zdcSMDHighestStrip(east, vert);
    zdcSMDHighestStripWestH = trigData->zdcSMDHighestStrip(west, hori);
    zdcSMDHighestStripWestV = trigData->zdcSMDHighestStrip(west, vert);

  }
  else {
    zdcWestUA = 0 ;
    zdcEastUA = 0 ;
    zdcSumUA  = 0 ;

    zdcWest = 0 ;
    zdcEast = 0 ;
    zdcSum  = 0 ;
    zdcWestTDC = 0;
    zdcEastTDC = 0;
    zdcTimeDifference  = 0;

    for(int i=0;i<8;i++){
      zdcSMDEastH[i] = 0;
      zdcSMDEastV[i] = 0;
      zdcSMDWestH[i] = 0;
      zdcSMDWestV[i] = 0;
    }


    zdcSMDHighestStripEastH = 0;
    zdcSMDHighestStripEastV = 0;
    zdcSMDHighestStripWestH = 0;
    zdcSMDHighestStripWestV = 0;

  }
//
//   CTB
//
  float ctbsum=0.0;
  ctbSW = 0 ;
  ctbNW = 0 ;
  ctbTW = 0 ;
  ctbBW = 0 ;
  ctbSE = 0 ;
  ctbNE = 0 ;
  ctbTE = 0 ;
  ctbBE = 0 ;
  nCtbHits = 0 ;
  float ctbThres = 2. ;
  if(&ctb){
    TClonesArray &pCtbSlats = *ctbSlats;
    for(i=0; i<120; i++){
      for(j=0; j<2; j++){
	ctbsum += ctb.mips(i,j,0);
	if ( ctb.mips(i,j,0) > 0 ) {
	   Byte_t i_eta = (Byte_t)(j+1);
	   Byte_t i_phi = (Byte_t)(i+1);
	   Int_t  adc   = (Int_t)ctb.mips(i,j,0) ;
           new(pCtbSlats[nCtbHits++]) StPeCCtbSlat(i_phi,i_eta,adc) ;
	}

	if ( ctb.mips(i,j,0) > ctbThres ) {
           if      ( i >=   5 && i <  20 ) ctbSW++ ;
	   else if ( i >=  20 && i <  35 ) ctbBW++ ;
	   else if ( i >=  35 && i <  50 ) ctbNW++ ;
	   else if ( i >=  50 && i <  60 ) ctbTW++ ;
	   else if (             i <   5 ) ctbTW++ ;
           else if ( i >=  65 && i <  80 ) ctbNE++ ;
	   else if ( i >=  80 && i <  95 ) ctbBE++ ;
	   else if ( i >=  95 && i < 110 ) ctbSE++ ;
	   else if ( i >= 110            ) ctbTE++ ;
	   else if ( i >=  60 && i <  65 ) ctbTE++ ;
         }
      }
    }
    if ( infoLevel > 10 ) 
       cout << "CTB MIP total sum = " << ctbsum << endl;
  }
  ctbSum = ctbsum ;
  mwcSum = 0 ;
  //
  //RD 24JULY temporary fix to see vertex for minbias_monitor events
   StThreeVectorF vtx = mudst->event()->primaryVertexPosition();

   size_t Nvert = mudst->numberOfPrimaryVertices();
//    LOG_INFO << "StPeCTrigger::fill(event mudst) #vertices: "  <<Nvert<< endm; 
   for (size_t verti = 0;verti<Nvert;++verti){
//      LOG_INFO << "StPeCTrigger::  vertex Index: "<<verti<<endm;
     StMuPrimaryVertex* V= mudst->primaryVertex(verti);
     assert(V);
     mudst->setVertexIndex(verti);
     const StThreeVectorF &r=V->position();
//      cout<<" z vertex "<<r.z()<<endl;
   }
//   StThreeVectorF  vtx = vertex->position(); 
   ctbSum = vtx.z();
//    cout<<" single vertex z "<<ctbSum<<endl;
  //
  //
  // BBC 
  if(&bbc){

    bbcAdcSumEastSm=bbc.adcSumEast();
    bbcAdcSumWestSm=bbc.adcSumWest();
    bbcAdcSumEastLg=bbc.adcSumEastLarge();;
    bbcAdcSumWestLg=bbc.adcSumWestLarge();;

//     bbcNHitEastSm=bbc.nHitEast();
//     bbcNHitWestSm=bbc.nHitWest();
//     bbcNHitEastLg=bbc.nHitEastLarge();;
//     bbcNHitWestLg=bbc.nHitWestLarge();;
 
    bbcTacEast = trigData->bbcEarliestTDC(east);
    bbcTacWest = trigData->bbcEarliestTDC(west); 
    bbcTimeDiff = trigData->bbcTimeDifference();
    for(int i=0;i<36;i++){
      bbcTDCEast[i] = trigData->bbcTDC(east, i+1);
      bbcTDCWest[i] = trigData->bbcTDC(west, i+1);
      bbcADCEast[i] = trigData->bbcADC(east, i+1);
      bbcADCWest[i] = trigData->bbcADC(west, i+1);
    }



  } else {
    bbcAdcSumEastSm=0;
    bbcAdcSumWestSm=0;
    bbcAdcSumEastLg=0;
    bbcAdcSumWestLg=0;

    bbcNHitEastSm=0;
    bbcNHitWestSm=0;
    bbcNHitEastLg=0;
    bbcNHitWestLg=0;
    
    bbcTacEast=0;
    bbcTacWest=0;
    bbcTimeDiff = 0;

    for(int i=0;i<36;i++){
      bbcTDCEast[i] = 0;
      bbcTDCWest[i] = 0;
    }


  
  }
  //
  //12-JULY-2012 RD get TOF information in trigger
  //
  unsigned short tofMult;
  tofMult = trigData->tofMultiplicity();
  nBtofTriggerHits = tofMult;
  nBTOFhits =  mudst->numberOfBTofHit();;
  
  double leadingT ;
  for(int itof=0;itof<tofMult;itof++) {
   StMuBTofHit * leading = mudst->btofHit(itof);    //->leadingEdgeTime(); 
   //leadingT = leading->leadingEdgeTime();
  }
  //
  // 1-MAY-2013  pass bunchId 
  //
  bunchId = trigData->bunchId7Bit();
//
//   Simulate l0
//
  p4  = l0_2000->process(mudst);
  p4c = l0_2000Corrected->process(mudst);
  p5  = l0Offline2001->process(mudst);
//
//   Take quadrants from 2001 wiring
//
  // L0 simulator
  Bool_t trgOut[40];
  {for ( int i = 0 ; i < 40 ; i++ ) trgOut[i] = 0 ;}

  if ( infoLevel > 20 ) {
     printf ( "trgBits : " ) ;
     for ( int i = 0 ; i < 24 ; i++ ) {
        printf ( "%d ", trgOut[i] ) ;
     }
     printf ( "\n" ) ;
  }
//   return 0 ; //standard return (unused)
  return trg_3000 ; //to pass UPC_Main trigger selection 
}
