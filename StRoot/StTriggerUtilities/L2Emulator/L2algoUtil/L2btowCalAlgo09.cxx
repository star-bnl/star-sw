#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

/*********************************************************
  $Id: L2btowCalAlgo09.cxx,v 1.3 2010/04/18 02:53:35 pibero Exp $
  \author Jan Balewski, MIT, 2009 
 *****************************************************
  Descripion: 
  calibrates Barrel towers, result is used by other L2-algo
 *****************************************************/


#ifdef  IS_REAL_L2  //in l2-ana  environment
  #include "../L2algoUtil/L2EmcDb.h"
  #include "../L2algoUtil/L2Histo.h"
#else
  #include "L2EmcDb.h"
  #include "L2Histo.h"
  #include "L2EmcGeom.h"
#endif

#include "L2btowCalAlgo09.h"

/************************
the _only_ copy of L2event for all L2-algos
**************************/
L2eventStream2009  globL2eventStream2009;
/*************************/

//=================================================
//=================================================
L2btowCalAlgo09::L2btowCalAlgo09(const char* name, L2EmcDb* db, L2EmcGeom *geoX, char* outDir, int resOff)  :  L2VirtualAlgo2009( name,  db,  outDir, true, false, resOff) { 
  /* called one per days
     all memory allocation must be done here
  */

  geom=geoX; 
  if (!geom)
    criticalError("L2btowCalAlgo is broken -- can't find geom.");

  setMaxHist(32);
  createHisto();

  // initialize BTOW-Calibrated-data
  int k;
  for(k=0;k<L2eventStream2009::mxToken;k++){
    L2BtowCalibData09 & btowCalibData=globL2eventStream2009.btow[k];  
    btowCalibData.nInputBlock=0;
    btowCalibData.hitSize=0;
  }
 }

/* ========================================
  ======================================== */
int 
L2btowCalAlgo09::initRunUser( int runNo, int *rc_ints, float *rc_floats) {

  par_adcMask=(unsigned short)(-0x10); 
  par_pedOff=0x10/2; //hex must match with above 
 
  // unpack params from run control GUI
  par_dbg       =  rc_ints[0];
  par_gainType  =  rc_ints[1];
  par_nSigPed   =  rc_ints[2];

  par_twEneThres = rc_floats[0];
  par_hotEtThres = rc_floats[1];;

  // verify consistency of input params
  int kBad=0;
  kBad+=0x00001 * (par_gainType<kGainZero || par_gainType>kGainOffline);
  kBad+=0x00002 * (par_nSigPed<2 || par_nSigPed>5);
  kBad+=0x00004 * (par_twEneThres<0.1 ||  par_twEneThres>1.5);

  if (mLogFile) { 
    fprintf(mLogFile,"L2%s algorithm initRun(R=%d), compiled: %s , %s\n params:\n",getName(),mRunNumber,__DATE__,__TIME__);
    fprintf(mLogFile," - use BTOW=%d,  gain Ideal=%d or Offline=%d, debug=%d\n",
	    par_gainType>=kGainIdeal, par_gainType==kGainIdeal, par_gainType==kGainOffline, par_dbg);
    fprintf(mLogFile," - thresholds: ADC-ped> %d*sigPed .AND. energy>%.2f GeV \n", par_nSigPed, par_twEneThres);

    fprintf(mLogFile," - hot tower thresholds:  ET/GeV=%.2f\n",par_hotEtThres);
    fprintf(mLogFile,"initRun() params checked for consistency, Error flag=0x%04x\n",kBad);
  }

  // initialize BTOW-Calibrated-data                                                                                                         
  int k;
  for(k=0;k<L2eventStream2009::mxToken;k++){
    L2BtowCalibData09 & btowCalibData=globL2eventStream2009.btow[k];
    btowCalibData.nInputBlock=0;
    btowCalibData.hitSize=0;
  }

  
  if(kBad) return -kBad;

  // clear content of all histograms
  int i;
  for (i=0; i<mxHA;i++) if(hA[i])hA[i]->reset();

  // update title of histos
  char txt[1000];
  sprintf(txt,"BTOW tower, E_T>%.2f GeV (input); x: BTOW RDO index=chan*30+fiber; y: counts",par_hotEtThres);
  hA[10]->setTitle(txt);
  
  sprintf(txt,"BTOW tower, Et>%.2f GeV (input); x: BTOW softID",par_hotEtThres);
  hA[11]->setTitle(txt);
  sprintf(txt,"BTOW tower, Et>%.2f GeV (input); x: eta bin, [-1,+1];  y: phi bin ~ TPC sector",par_hotEtThres);
  hA[12] ->setTitle(txt);
  
  sprintf(txt,"#BTOW towers / event , Et>%.2f GeV; x: # BTOW towers; y: counts",par_hotEtThres);
  hA[14] ->setTitle(txt);
  
  // re-caluclate geometry properties
  geom->btow.clear(); 
  int nB=0; /* counts # of unmasekd towers */ 
  int nBg=0; /* counts # of reasonable calibrated towers */ 
  int nEneThr=0, nPedThr=0; //BTOW count # of towers above & below threshold
  if(par_gainType>=kGainIdeal)  // this disables the whole loop below
  for(i=0; i<EmcDbIndexMax; i++) {
    const L2EmcDb::EmcCDbItem *x=mDb->getByIndex(i);
    if(mDb->isEmpty(x)) continue;  /* dropped not mapped  channels */
    /*....... B A R R E L  .................*/
    if (!mDb->isBTOW(x) ) continue; /* drop if not BTOW */
    if(x->fail) continue;          /* dropped masked channels */
    if(x->gain<=0) continue;       /* dropped uncalibrated towers , tmp */
    nB++;   

    float adcThres=x->ped+par_nSigPed* fabs(x->sigPed);
    float otherThr=x->ped+par_twEneThres*x->gain;
    //    if(strstr("01tg34",x->name))  printf("%s g=%f  adcThrEne=%.1f  adcThrAbovePed=%.1f  rdo=%d\n",x->name,x->gain,adcThres,otherThr,x->rdo);

    if(adcThres<otherThr) { //use energy threshold if higher
      adcThres=otherThr;
      nEneThr++;
    } else {
      nPedThr++;
    }
    
    /* use rdo index to match RDO order in the ADC data banks */    
    if(x->eta<=0 || x->eta>BtowGeom::mxEtaBin) return -90;
    int ietaTw= (x->eta-1); /* correct */

    // use ideal gains for now, hardcoded

    if (par_gainType!=kGainIdeal) return -102;    
    geom->btow.gain2Ene_rdo[x->rdo]=geom->btow.idealGain2Ene[ietaTw];
    geom->btow.gain2ET_rdo[x->rdo]=geom->getIdealAdc2ET();
    
    geom->btow.thr_rdo[x->rdo]=(int) (adcThres);
    geom->btow.ped_rdo[x->rdo]=(int) (x->ped);
    geom->btow.ped_shifted_rdo[x->rdo]=(unsigned short)(par_pedOff - x->ped);
    nBg++;      
  }
  
  if (mLogFile) {
    fprintf(mLogFile,"  found  towers working=%d calibrated=%d, based on ASCII DB\n",nB,nBg);
    fprintf(mLogFile,"  thresh defined by energy=%d  or NsigPed=%d \n",nEneThr, nPedThr);
  }

  return 0; //OK


}               

/* ========================================
  ======================================== */
void 
L2btowCalAlgo09::calibrateBtow(int token, int bemcIn, ushort *rawAdc){
  // Btow calibration is a special case,  must have one exit  at the end
  computeStart();
  token&=L2eventStream2009::tokenMask; // only protect against bad token, Gerard's trick
 
  //...... token is valid no w ........
  L2BtowCalibData09 & btowCalibData=globL2eventStream2009.btow[token];  
  btowCalibData.nInputBlock++;
  
  // clear data for this token from previous event
  btowCalibData.hitSize=0;

  int nTower=0; /* counts mapped & used ADC channels */
  int nHotTower=0;
  if(bemcIn && par_gainType>kGainZero) { // EVEVEVEVEVE
    // ............process this event ...............
    short rdo;
    int low_noise_adc;
    int adc; // pedestal subtracted
    float et;
    float low_noise_et; // bit-chopped
    ushort *thr=geom->btow.thr_rdo;
    ushort *ped=geom->btow.ped_rdo;
    ushort *ped_shifted=geom->btow.ped_shifted_rdo;
    float *gain2ET=geom->btow.gain2ET_rdo;
    float *gain2Ene=geom->btow.gain2Ene_rdo;
    HitTower1 *hit=btowCalibData.hit;
    for(rdo=0; rdo<BtowGeom::mxRdo; rdo++){
      if(rawAdc[rdo]<thr[rdo])continue;
      if(nTower>=L2BtowCalibData09::mxListSize) break; // overflow protection
      adc=rawAdc[rdo]-ped[rdo];  //do NOT correct for common pedestal noise
      et=adc/gain2ET[rdo]; 
      low_noise_adc=(rawAdc[rdo]+ped_shifted[rdo]) & par_adcMask;//note that ped_shifted is defined as pedOffset-ped.
      low_noise_et=low_noise_adc/gain2ET[rdo];
      hit->rdo=rdo;
      hit->adc=adc;
      hit->et=et;
      hit->low_noise_et=low_noise_et;
      hit->ene=adc/gain2Ene[rdo]; 
      hit++;
      nTower++; //printf("nBt=%d\n",nTower);
      // only monitoring
      if(et >par_hotEtThres) {
	hA[10]->fill(rdo);
	nHotTower++;
      }
    }
    btowCalibData.hitSize=nTower;
    
    // QA histos
    hA[13]->fill(nTower);
    hA[14]->fill(nHotTower);
    if(nTower>=L2BtowCalibData09::mxListSize) mhN->fill(5); // was overflow
  
  } // EVEVEVEVEVE

  // debugging should be off for any time critical computation
  if(par_dbg>0){
    printf("L2-%s-compute: set adcL size=%d, get=%d\n",getName(),nTower,999); //tmp
   printf("dbg=%s: found  nTw=%d\n",getName(),nTower);
    if(par_dbg>0)   print0();
    printCalibratedData(token);
  } 
  
  computeStop(token);

} 

/* ======================================== 
   ======================================== */

void
L2btowCalAlgo09::clear(int token){
  token&=L2eventStream2009::tokenMask; // only protect against bad token, Gerard's trick
  //...... token is valid no w ........
  L2BtowCalibData09 & btowCalibData=globL2eventStream2009.btow[token];
  btowCalibData.hitSize=0;
  return;
}

/* ========================================
  ======================================== */
void 
L2btowCalAlgo09::computeUser(int token ){

  printf("computeUser-%s FATAL CRASH\n If you see this message it means l2new is very badly misconfigured \n and L2-btow-calib algo was not executed properly\n before calling other individual L2-algos. \n\n l2new will aborted now - fix the code, Jan B.\n",getName());
  criticalError("L2btowCalAlgo09::computeUser has been called and should not have been.  Serious problem in L2");
}


/* ========================================
  ======================================== */
void
L2btowCalAlgo09::finishRunUser() {  /* called once at the end of the run */
  // do whatever you want, log-file & histo-file are still open

  //report mean and RMS of #towers/event:
  int mean=0, RMS=0;
  hA[13]->findMean(&mean,&RMS);
  if (mLogFile){
    fprintf(mLogFile,"#BTOW_nonzero_towers_per_event: mean=%d, rms=%d\n",mean,RMS);
  }
  // search for hot tower, re-project histos vs. other representations
 int bHotSum=1,bHotId=-1;
 const int *data20=hA[10]->getData();
 const L2EmcDb::EmcCDbItem *xB=mDb->getByIndex(402); // some wired default?
  
  int i;
  for(i=0; i<EmcDbIndexMax; i++) {
    const L2EmcDb::EmcCDbItem *x=mDb->getByIndex(i);
    if(mDb->isEmpty(x)) continue;
    if (!mDb->isBTOW(x) ) continue;
    int softId=atoi(x->tube+2);
    int ieta= (x->eta-1);
    int iphi= (x->sec-1)*BtowGeom::mxSubs + x->sub-'a' ;
    hA[11]->fillW(softId,data20[x->rdo]);
    hA[12]->fillW(ieta, iphi,data20[x->rdo]);
    if(bHotSum<data20[x->rdo]) {
      bHotSum=data20[x->rdo];
      bHotId=softId;
      xB=x;
    }
  }
  
  if (mLogFile){
    fprintf(mLogFile,"#BTOW_hot tower _candidate_ (bHotSum=%d of %d eve) :, softID %d , crate %d , chan %d , name %s\n",bHotSum,mEventsInRun,bHotId,xB->crate,xB->chan,xB->name);
  }
  
  //...... QA tokens .....
  int tkn1=99999, tkn2=0; // min/max token
  int nTkn=0;
  int tkn3=-1, nTkn3=-1; // most often used token
  
  int k;
  for(k=0;k<L2eventStream2009::mxToken;k++){
    L2BtowCalibData09 & btowCalibData=globL2eventStream2009.btow[k];  
    if(btowCalibData.nInputBlock==0) continue;
    hA[1]->fillW(k,btowCalibData.nInputBlock);
    if(nTkn3<btowCalibData.nInputBlock){
      nTkn3=btowCalibData.nInputBlock;
      tkn3=k;
    }

    nTkn++;
    if(tkn1>k) tkn1=k;
    if(tkn2<k) tkn2=k;
  }
  if (mLogFile){
    fprintf(mLogFile,"#BTOW_token_QA:  _candidate_ hot token=%d used %d for %d events, token range [%d, %d], used %d tokens\n",tkn3,nTkn3,mEventsInRun,tkn1,tkn2,nTkn);
  }

}


//=======================================
//=======================================
void 
L2btowCalAlgo09::createHisto() {
  memset(hA,0,mxHA*sizeof(L2Histo*));
  //token related spectra
  hA[1]=new  L2Histo(1,"L2-btow-calib: seen tokens;  x:  token value; y: events ",L2eventStream2009::mxToken);
  
  // BTOW  raw spectra
  hA[10]=new L2Histo(10,"btow hot tower 1", BtowGeom::mxRdo); // title upadted in initRun
  hA[11]=new L2Histo(11,"btow hot tower 2", BtowGeom::mxRdo); // title upadted in initRun
  hA[12]=new L2Histo(12,"btow hot tower 3", BtowGeom::mxEtaBin,BtowGeom::mxPhiBin); // title upadted in initRun  
  hA[13]=new L2Histo(13,"BTOW #tower w/ energy /event; x: # BTOW towers; y: counts", 200); 
  hA[14]=new L2Histo(14,"# hot towers/event", 100); 

}



/* ========================================
  ======================================== */
void 
L2btowCalAlgo09::print0(){ // full raw input  ADC array
  // empty
 }


/****************************************************
  $Log: L2btowCalAlgo09.cxx,v $
  Revision 1.3  2010/04/18 02:53:35  pibero
  Fixed memset bug.

  Revision 1.2  2010/04/17 16:42:09  pibero
  *** empty log message ***

  Revision 1.5  2008/02/01 00:16:40  balewski
  add mxListSize to BTOW/ETOW calibration

  Revision 1.4  2008/01/30 00:47:15  balewski
  Added L2-Etow-calib

  Revision 1.3  2008/01/18 23:29:13  balewski
  now L2result is exported

  Revision 1.2  2008/01/16 23:32:34  balewski
  toward token dependent compute()

  Revision 1.1  2007/12/19 02:30:18  balewski
  new L2-btow-calib-2008

  Revision 1.1  2007/11/19 22:18:25  balewski
  most L2algos provide triggerID's

 
*/


