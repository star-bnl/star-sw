#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>

/*********************************************************************
 * $Id: L2pedAlgo.cxx,v 1.11 2009/11/19 15:48:46 balewski Exp $
 * \author Jan Balewski, IUCF, 2006 
 *********************************************************************
 * Descripion:
 * pedestal algo in L2 , for BTOW & ETOW
 *********************************************************************
 */  


#ifdef  IS_REAL_L2  //in l2-ana  environment
  #include "../L2algoUtil/L2EmcDb.h"
  #include "../L2algoUtil/L2Histo.h"
#else
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2EmcDb.h"
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2Histo.h"
#endif

#include "L2pedAlgo.h"
#include "L2pedResults2006.h"

//=================================================
//=================================================
L2pedAlgo::L2pedAlgo(const char* name, L2EmcDb* db, char* outDir, int resOff) 
  :  L2VirtualAlgo( name,  db,  outDir, resOff) { 
  /* called one per days
     all memory allocation must be done here
  */

  par_pedSubtr=false;
  par_saveBinary=false;
  par_dbg=0;
  par_prescAccept=0;

  int i;
  for(i=0;i<MaxBtowRdo;i++) {
    char tit[100];
    sprintf(tit,"BTOW ADC for rdo=%d; ADC+%d",i,-minAdc);
    btowAdc[i]=new L2Histo(20000+i,tit,maxAdc-minAdc+1); 
  }
  
  for(i=0;i<MaxEtowRdo;i++) {
    char tit[100];
    sprintf(tit,"ETOW ADC for rdo=%d; ADC ",i);
    etowAdc[i]=new L2Histo(10000+i,tit,maxAdc-minAdc+1); 
 }

  // aux histos
  memset(hA,0,sizeof(hA));
  hA[10]=new   L2Histo(10, (char*)"total event counter; x=cases",6);
  hA[11]=new   L2Histo(11, (char*)"L2 time used per input event;  x: time (CPU 20*kTics); y: events ",500);

  // BTOW  raw spectra
  hA[20]=new   L2Histo(20, (char*)"BTOW pedRes  Y=ADC-DBped ; x: chan + 160*crate", 4800);
  hA[21]=new   L2Histo(21, (char*)"BTOW pedRes  Z=ADC-DBped, saturated @ |3|; x:  etaBin ,[-1,+1];  y: phi bin ~sector",40,120);

  // ETOW  raw spectra
  hA[30]=new   L2Histo(30, (char*)"ETOW pedRes Y=ADC-DBped ; x: chan + 128*crate", 768);
  hA[31]=new   L2Histo(31, (char*)"ETOW pedRes Z=ADC-DBped, saturated @ |3|; x: 12 - Endcap etaBin ,[+1,+2];  y: phi bin ~sector",12,60);
 

  printf("L2pedAlgo instantiated, logPath='%s'\n",mOutDir);

}

/*========================================
  ======================================== */
int
L2pedAlgo::initRun(int runNo, int *rc_ints, float *rc_floats) {
  //myName is not used.
  // update DB if run # has changed
  //printf("aaa L2pedAlgo::initRun runNo=%d\n",runNo);
  if(mDb->initRun(runNo)) return -27; 
  // DB must be initialized prior to lookup tables

  // unpack input params


  par_pedSubtr  =rc_ints[0]!=0;
  par_speedFact =rc_ints[1];
  par_saveBinary=rc_ints[2]!=0;
  par_dbg       =rc_ints[3];
  par_prescAccept=rc_ints[4];

  if(par_prescAccept<0) par_prescAccept=0; // prescale can't be negative
  //note speedFactor can be only powers of 2, range [1-256]
  if(par_speedFact<1) par_speedFact=1;
  if(par_speedFact>2) {// ASSURE ONLY POWERS OF 2, round down
    if(par_speedFact<4) par_speedFact=2;
    else if(par_speedFact<8) par_speedFact=4;
    else if(par_speedFact<16) par_speedFact=8;
    else if(par_speedFact<32) par_speedFact=16;
    else if(par_speedFact<64) par_speedFact=32;
    else if(par_speedFact<192) par_speedFact=64;
    else par_speedFact=192;
  }
  // can't be bigger, since there is only 6 etow crates

  s_lastB=s_lastE=0;
  s_stepB=MaxBtowRdo/ par_speedFact ;
  s_stepE=MaxEtowRdo/ par_speedFact ;


  /* .... clear content */
  memset(db_btowPed,       0,sizeof(db_btowPed));
  memset(db_etowPed,       0,sizeof(db_etowPed));
  

  int i;  
  int nBtowOk=0, nEtowOk=0;
  for(i=0; i<EmcDbIndexMax; i++) {
    const L2EmcDb::EmcCDbItem *x=mDb->getByIndex(i);
    if(mDb->isEmpty(x)) continue; // dropped not mapped channels
    if (mDb->isBTOW(x) ) {
	  db_btowPed[x->rdo]=(int) (x->ped);
	  nBtowOk++;
    } else if (mDb->isETOW(x) ) {
      db_etowPed[x->rdo]=(int) (x->ped);
      nEtowOk++;
    }
  }
  
  printf("L2ped algorithm init()... params:\n  dbg=%d, pedSubtr=%d saveBinHist=%d  speedFact=%d prescAccept=%d\n  mapped channels: nBtow=%d nEtow=%d\n",par_dbg,par_pedSubtr,par_saveBinary,par_speedFact,par_prescAccept,nBtowOk,nEtowOk);

  for(i=0;i<MaxBtowRdo;i++)  btowAdc[i]->reset();
  for(i=0;i<MaxEtowRdo;i++)  etowAdc[i]->reset();
  for(i=0;i<mxHA;i++) if(hA[i]) hA[i]->reset();

  nInp=0;
  run_number=runNo;
  return 0; // OK
}               


/*========================================
  ======================================== */
bool
L2pedAlgo::doEvent(int L0trg, int inpEveId, TrgDataType* trgData, 
		   int bemcIn, ushort *bemcData,
		   int eemcIn, ushort *eemcData){
  // not used: L0trg, inpEveId
  mAccept=true; // it never aborts
  /* STRICT TIME BUDGET  START ...., well a bit relaxed for this algo*/
  rdtscl_macro(mEveTimeStart);
  nInp++;
  hA[10]->fill(0);
  if(par_prescAccept>0) { // value=1 accepts 100% at maximal speed
    if((rand()>>4) % par_prescAccept ) return false;
    hA[10]->fill(5);
    return true;
  }

  myTrigData=trgData;
  
  /* *****************************************
     the code below is NOT optimized for speed
     one would need to make loop indexed by RDO to speed it up
     not worth for pedestal calculation
     This version takes 2,500 kTicks/eve --> ~1.5msec
     ******************************************** */

  short rdo;  
  if( bemcIn ) {
    short first=s_lastB%MaxBtowRdo;
    s_lastB=first+s_stepB;
    if(first==0) hA[10]->fill(1);
    // printf("B: f=%d l=%d\n",first,s_lastB);
    
    for(rdo=first; rdo<s_lastB; rdo++){
      int adc=bemcData[rdo];
      if(par_pedSubtr) adc-=db_btowPed[rdo];
      btowAdc[rdo]->fill(adc-minAdc);
      // printf("B: rdo=%d raw=%d ped=%d val=%d\n",rdo,bemcData[rdo],db_etowPed[rdo],adc-minAdc);
    }
  }
  
  
  if( eemcIn ) {
    short first=s_lastE%MaxEtowRdo;
    s_lastE=first+s_stepE;
    if(first==0) hA[10]->fill(2);
    //printf("E: f=%d l=%d\n",first,s_lastE);
    
    for(rdo=first; rdo<s_lastE; rdo++){
      int adc=eemcData[rdo];
      if(par_pedSubtr) adc-=db_etowPed[rdo];
      etowAdc[rdo]->fill(adc-minAdc);
      //    printf("E: rdo=%d raw=%d ped=%d val=%d\n",rdo,eemcData[rdo],db_etowPed[rdo],adc-minAdc);
    }
  }
  
  //...this  event will be accepted
  L2pedResults2006 out; // all output bits lump together
  memset(&out,0,sizeof(out));
  
  out.int0.decision=
    ( (bemcIn>0)    <<3 ) +
    ( (eemcIn>0)    <<4 ) +
  
    (  par_pedSubtr <<6 ) ;
 
  rdtscl_macro(mEveTimeStop);
  mEveTimeDiff=mEveTimeStop-mEveTimeStart;
  int  kTick=mEveTimeDiff/1000;
  hA[11]->fill(kTick/20);
  
  //  printf("jkTick=%d\n",kTick);
  const ushort maxKT=30000;
  out.int0.kTick=  kTick>maxKT ? maxKT : (int)kTick;
  
  unsigned int *outPlace=myTrigData->TrgSum.L2Result+ mResultOffset;
  memcpy(outPlace,&out,sizeof( L2pedResults2006));
  
  if(par_dbg) L2pedResults2006_print(&out);
  
  return  mAccept;
}

/*========================================
  ======================================== */
void 
L2pedAlgo::finishRun() {/* called once at the end of the run */
  if(run_number<=0) return; // algo not used for current run
  int nBtowLow=0, nBtowHigh=0 ,nEtowLow=0, nEtowHigh=0 ;

  char fname[1000];
  sprintf(fname,"%s/run%d.l2ped.out",mOutDir,run_number); // actual L2 output destination 
  printf("L2ped_finish('%s') , finding pedestals...\n",fname);
  
  FILE *fd=fopen(fname,"w");
  if(fd==0) {printf("failed to open output %s file,skip ped_finish()\n",fname); return;}  
  fprintf(fd,"#L2-ped algorithm finishRun(%d), compiled: %s , %s\n",run_number,__DATE__,__TIME__);
  fprintf(fd,"#params: pedSubtr=%d speedFact=%d saveBin=%d debug=%d prescAccept=%d\n",par_pedSubtr,par_speedFact,par_saveBinary,par_dbg,par_prescAccept);
  hA[10]->printCSV(fd); // event accumulated
  int iMax=-3, iFWHM=-4;
  hA[11]->findMax( &iMax, &iFWHM);
  fprintf(fd,"#L2ped  CPU/eve MPV %d kTicks,  FWHM=%d, seen eve=%d\n",iMax, iFWHM,nInp);
  printf("L2ped  CPU/eve MPV %d kTicks,  FWHM=%d, seen eve=%d\n",iMax, iFWHM,nInp);
  if(par_saveBinary)  fprintf(fd,"#L2ped  will save full spectra for all towers\n");

  int par_topAdc=100;
  int maxPedDeviation=5;
//int iadcHigh=maxPedDeviation-minAdc;
// int iadcLow =maxPedDeviation+minAdc;

// fixed sign on July 23, 2007, JanB
  int iadcHigh=maxAdc - maxPedDeviation;
  int iadcLow =minAdc + maxPedDeviation;

  char xAxis[100];
  sprintf(xAxis,"raw ADC + %d",-minAdc);
  if(par_pedSubtr) sprintf(xAxis,"ADC - ped + %d",-minAdc);
  
  
  fprintf(fd,"# L2ped-Adc spectra, run=%d, Z-scale is ln(yield), only first digit shown;  maxPedDev=%d  table format:\n# name, ped, sigPed, crate, chan, softID-m-s-e, RDO_ID;\n#                                   ADC spectrum: [%d ...  <=-10 ... *=0  ...  >=+10 ... :=+20 ... %d],  Xaxis=%s\n",run_number,maxPedDeviation,minAdc,par_topAdc,xAxis);
  
  int i;
  int nB=0;
  for(i=0; i<EmcDbIndexMax; i++) {
    const L2EmcDb::EmcCDbItem *x=mDb->getByIndex(i);
    if(mDb->isEmpty(x)) continue;  /* dropped not mapped  channels */
    if (mDb->isBTOW(x) ||mDb->isETOW(x) ) { //
      // mDb->printItem(x);
      nB++;
      /* if(nB>30) return; */
      int iMax=-3, iFWHM=-4; 
      char pedQA='?';
      L2Histo *h=0;
      if(mDb->isBTOW(x)) h= btowAdc[x->rdo];
      else if(mDb->isETOW(x)) h= etowAdc[x->rdo];
      else continue;


      int pedRes=999;
      int maxRes=3;
      if(h->findMax( &iMax, &iFWHM)) {
	pedQA='0';
	if(iMax<iadcLow) pedQA='-';
	else if(iMax>iadcHigh) pedQA='+';
 	pedRes=iMax+minAdc;
	if(!par_pedSubtr) pedRes=int(pedRes - x->ped); // this looks funny but is right
      }

      if(mDb->isBTOW(x)) { //BTOW ...............
	if(pedQA=='-' )  nBtowLow++;
	else if(pedQA=='+')  nBtowHigh++;
 	//........ residual monito histos ....
	int ieta= (x->eta-1);
	int iphi= (x->sec-1)*10 + x->sub-'a' ;
	int ihard=x->chan+(x->crate-1)*160;
	if(x->fail) pedRes =0;
	hA[20]->fillW(ihard,pedRes);
	if(x->fail) {
	  pedRes =-100;
	} else {
	  if(pedRes<-maxRes) pedRes=-maxRes; 
	  if(pedRes>maxRes) pedRes=maxRes;
	} 	
	hA[21]->fillW(ieta, iphi,pedRes);
     } else {             // ETOW  ...........
	if(pedQA=='-' )  nEtowLow++;
	else if(pedQA=='+')  nEtowHigh++;  
	//........ residual monito histos ....
	int ieta= 12-x->eta;
	int iphi= (x->sec-1)*5 + x->sub-'A' ;
	int ihard=x->chan+(x->crate-1)*128;
	if(x->fail) pedRes =0;
	hA[30]->fillW(ihard,pedRes);
	if(x->fail) {
	  pedRes =-100;
	} else {
	  if(pedRes<-maxRes) pedRes=-maxRes; 
	  if(pedRes>maxRes) pedRes=maxRes;
	} 	
	hA[31]->fillW(ieta, iphi,pedRes);
      }
   

      char okC=' ';
      if(x->fail) okC='#';
      fprintf(fd,"%c%s %3d %4.1f 0x%02x 0x%02x %15s %4d ",okC,x->name,iMax+minAdc,iFWHM/2.3,x->crate, x->chan,x->tube, x->rdo);
      h->printPed(fd,minAdc,par_topAdc,' ');
      fprintf(fd,"qa=%c\n",pedQA);
      // exit(1); 
    } /* end of BTOW & ETOW */
  } /* end of pixels */

   
  fprintf(fd,"#L2ped_finishRun() # of towers with |ped-pedDB| >10 chan\n#    BTOW: nLow=%d nHigh=%d ;   ETOW  nLow=%d, nHigh=%d\n",nBtowLow, nBtowHigh,nEtowLow, nEtowHigh);
  fprintf(fd,"#    found peds for nB+E=%d , seen events=%d\n",nB,nInp);
  printf("l2ped_finish() found peds for nB+E=%d , seen events=%d\n",nB,nInp);

  fclose(fd);


  sprintf(fname,"%s/run%d.l2ped.hist.bin",mOutDir,run_number); // full spectra in binary form
  
  fd=fopen(fname,"w");
  if(fd==0) {
    printf("failed to open output %s file,skip ped_finish()\n",fname); 
    goto end;
  }  
  
  for(int j=0;j<mxHA;j++) { // save auxil histos
    if(hA[j]==0) continue;
    hA[j]->write(fd);
  }
  
  if(par_saveBinary) {
    printf("l2ped_finish('%s') , save FULL spectra binary...\n",fname);

    // .................. SAVE FULL SPECTRA BINARY ...........
    for(i=0;i<mxHA;i++) if(hA[i]) hA[i]->write(fd);
    for(i=0; i<EmcDbIndexMax; i++) {
      const L2EmcDb::EmcCDbItem *x=mDb->getByIndex(i);
      if(mDb->isEmpty(x)) continue;  /* dropped not mapped  channels */
      L2Histo *h=0;
      char tit[200];
      if(mDb->isBTOW(x) ){
	h= btowAdc[x->rdo];
	//L2EmcDb::printItem(x);
	sprintf(tit,"BTOW=%s cr/ch=%03d/%03d stat/0x=%04x+%04x soft=%s; %s",x->name, x->crate, x->chan, x->stat, x->fail,x->tube,xAxis);
	//printf("tit=%s=\n",tit);	
      } else if(mDb->isETOW(x) ) {
	h= etowAdc[x->rdo];
	//L2EmcDb::printItem(x);
	sprintf(tit,"ETOW=%s cr/ch=%03d/%03d stat/0x=%04x+%04x pname=%s; %s",x->name, x->crate, x->chan, x->stat, x->fail,x->tube,xAxis);
      }
      if(h==0) continue; //just in case
      h->setTitle(tit);
      h->write(fd); // change title, add cr/chan/name, pedSubtrFlag
      // break;
    }
    printf("l2ped_finish() binary full spectra saved\n");
  }
  fclose(fd);
 end:
  run_number=-2;
}

         

/**********************************************************************
  $Log: L2pedAlgo.cxx,v $
  Revision 1.11  2009/11/19 15:48:46  balewski
  add (char*) to many strings to make SL5 happ, few other adjustments

  Revision 1.10  2007/11/19 22:18:31  balewski
  most L2algos provide triggerID's

  Revision 1.9  2007/11/18 21:58:58  balewski
  L2algos triggerId list fixed

  Revision 1.8  2007/11/13 23:06:09  balewski
  toward more unified L2-algos

  Revision 1.7  2007/11/13 00:12:38  balewski
  added offline triggerID, take1

  Revision 1.6  2007/11/08 04:02:33  balewski
  run on l2ana as well

  Revision 1.5  2007/11/02 20:44:54  balewski
  cleanup

  Revision 1.4  2007/11/02 17:43:11  balewski
  cleanup & it started to work w/ L2upsilon

  Revision 1.3  2007/11/02 03:03:50  balewski
  modified L2VirtualAlgo

  Revision 1.2  2007/10/25 02:07:06  balewski
  added L2upsilon & binary event dump

  Revision 1.1  2007/10/11 00:33:24  balewski
  L2algo added

  Revision 1.5  2006/03/28 19:46:51  balewski
  ver16b, in l2new

  Revision 1.4  2006/03/11 17:08:35  balewski
  now CVS comments should work

*/
 
