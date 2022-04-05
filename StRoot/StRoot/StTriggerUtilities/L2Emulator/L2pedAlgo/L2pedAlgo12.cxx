#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <fakeRtsLog.h>

/**********************************************************
 * $Id: L2pedAlgo12.cxx,v 1.4 2012/03/21 18:18:03 jml Exp $
 * \author Jan Balewski, IUCF, 2006 
 **********************************************************
 * Descripion:
 * pedestal algo in L2 , for BTOW & ETOW
 **********************************************************
 */  


#ifdef  IS_REAL_L2  //in l2-ana  environment
  #include "../L2algoUtil/L2EmcDb2012.h"
  #include "../L2algoUtil/L2EmcGeom2012.h"
  #include "../L2algoUtil/L2Histo.h"
#else
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2EmcDb2012.h"
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2Histo.h"
#endif

#include "L2pedAlgo12.h"
#include "L2pedResults2012.h"

//=================================================
//=================================================
L2pedAlgo12::L2pedAlgo12(const char* name, const char *uid, L2EmcDb2012* db, char* outDir, int resOff) 
  :  L2VirtualAlgo2012(name, uid,  db,  outDir, false, false, resOff) { 
  /* called one per days
     all memory allocation must be done here
  */

  par_pedSubtr=false;
  par_saveBinary=false;
  par_dbg=0;
  par_prescAccept=0;

  int i;
  char tit[1000];
  for(i=0;i<BtowGeom::mxRdo;i++) {
    sprintf(tit,"BTOW ADC for rdo=%d; ADC+%d",i,-minAdc);
    btowAdc[i]=new L2Histo(20000+i,tit,maxAdc-minAdc+1); 
  }
  
  for(i=0;i<EtowGeom::mxRdo;i++) {
    sprintf(tit,"ETOW ADC for rdo=%d; ADC ",i);
    etowAdc[i]=new L2Histo(10000+i,tit,maxAdc-minAdc+1); 
  }

  // aux histos
  par_maxMatt=160;
  setMaxHist(64); // set upper range, I uses only 2^N -it is easier to remember
 
  //j  memset(hA,0,sizeof(hA));
  hA[10]=new   L2Histo(10,"total event counter; x=cases",6);
  hA[11]=new   L2Histo(11,"L2 time used per input event;  x: time (CPU 20*kTics); y: events ",500);

  // BTOW  raw spectra
  hA[20]=new   L2Histo(20,"BTOW pedRes  Y=ADC-DBped ; x: chan + 160*crate", 4800);
  hA[21]=new   L2Histo(21,"BTOW pedRes  Z=ADC-DBped, saturated @ |3|; x:  etaBin ,[-1,+1];  y: phi bin ~sector",40,120);
  sprintf(tit,"BTOW pedRes ; x:  chan + 160*crate ;y: ADC+%d",-minAdc);

  hA[22]=new   L2Histo(22,tit,4800,par_maxMatt);

  // ETOW  raw spectra
  hA[30]=new   L2Histo(30,"ETOW pedRes Y=ADC-DBped ; x: chan + 128*crate", 768);
  hA[31]=new   L2Histo(31,"ETOW pedRes Z=ADC-DBped, saturated @ |3|; x: 12 - Endcap etaBin ,[+1,+2];  y: phi bin ~sector",12,60);
  sprintf(tit,"ETOW pedRes ; x:  chan + 128*crate ;y: ADC+%d",-minAdc);

  hA[33]=new   L2Histo(33,tit,768,par_maxMatt);
 

  //------- self-consistency checks, should never fail
  if (sizeof(L2pedResults2012)!= L2pedResults2012::mySizeChar) 
    criticalError("L2pedAlgo12 has failed consistency check. sizeof(L2pedAlgo12)!= L2pedResults2012::mySizeChar");
}

/*========================================
  ======================================== */
int
L2pedAlgo12::initRunUser(int runNo, int *rc_ints, float *rc_floats) {
  //myName is not used.
  // update DB if run # has changed
  //OLD APPROACH, abandoned on Feb 4, 2008, JanB if(mDb->initRun(runNo)) return -27; 
  // DB must be initialized prior to lookup tables

  if(mDb->getRun()!=runNo) return -700; // L2EmcDb not initialized properly
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
  s_stepB=BtowGeom::mxRdo/ par_speedFact ;
  s_stepE=EtowGeom::mxRdo/ par_speedFact ;


  /* .... clear content */
  memset(db_btowPed,       0,sizeof(db_btowPed));
  memset(db_etowPed,       0,sizeof(db_etowPed));
  

  int i;  
  int nBtowOk=0, nEtowOk=0;
  for(i=0; i<EmcDbIndexMax; i++) {
    const L2EmcDb2012::EmcCDbItem *x=mDb->getByIndex(i);
    if(mDb->isEmpty(x)) continue; // dropped not mapped channels
    if (mDb->isBTOW(x) ) {
	  db_btowPed[x->rdo]=(int) (x->ped);
	  nBtowOk++;
    } else if (mDb->isETOW(x) ) {
      db_etowPed[x->rdo]=(int) (x->ped);
      nEtowOk++;
    }
  }
  
  for(i=0;i<BtowGeom::mxRdo;i++)  btowAdc[i]->reset();
  for(i=0;i<EtowGeom::mxRdo;i++)  etowAdc[i]->reset();
  for(i=0;i<mxHA;i++) if(hA[i]) hA[i]->reset();

  nInp=0;
  run_number=runNo;
  return 0; // OK
}               


/*========================================
  ======================================== */
bool
L2pedAlgo12::doPedestals(int inpEveId, int* L2Result, 
		   int bemcIn, ushort *bemcData,
		   int eemcIn, ushort *eemcData){
  /* STRICT TIME BUDGET  START ...., well a bit relaxed for this algo */
  unsigned long mEveTimeStart;
  rdtscl_macro(mEveTimeStart);
  //computeStart();//swapped out previous line for this one.
  nInp++;
  hA[10]->fill(0);
  if(par_prescAccept>0) { // value=1 accepts 100% at maximal speed
    if((rand()>>4) % par_prescAccept ) return false;
    hA[10]->fill(5);
    return true;
  }

  
  /* *****************************************
     the code below is NOT optimized for speed
     one would need to make loop indexed by RDO to speed it up
     not worth for pedestal calculation
     This version takes 2,500 kTicks/eve --> ~1.5msec
     ******************************************** */

  short rdo;  
  if( bemcIn ) {
    short first=s_lastB%BtowGeom::mxRdo;
    s_lastB=first+s_stepB;
    if(first==0) hA[10]->fill(1);
    
    for(rdo=first; rdo<s_lastB; rdo++){
      int adc=bemcData[rdo];
      if(par_pedSubtr) adc-=db_btowPed[rdo];
      btowAdc[rdo]->fill(adc-minAdc);
    }
  }
  
  
  if( eemcIn ) {
    short first=s_lastE%EtowGeom::mxRdo;
    s_lastE=first+s_stepE;
    if(first==0) hA[10]->fill(2);
    
    for(rdo=first; rdo<s_lastE; rdo++){
      int adc=eemcData[rdo];
      if(par_pedSubtr) adc-=db_etowPed[rdo];
      etowAdc[rdo]->fill(adc-minAdc);
    }
  }
  
  //...this  event will be accepted
  L2pedResults2012 out; // all output bits lump together
  memset(&out,0,sizeof(out));
    out.int0.decision=
    ( (bemcIn>0)    <<3 ) +
    ( (eemcIn>0)    <<4 ) +
  
    (  par_pedSubtr <<6 ) ;
  
  unsigned long mEveTimeStop;
  rdtscl_macro(mEveTimeStop);
  unsigned long mEveTimeDiff=mEveTimeStop-mEveTimeStart;
  int  kTick=mEveTimeDiff/1000;
  hA[11]->fill(kTick/20);
  
  const ushort maxKT=30000;
  out.int0.kTick=  kTick>maxKT ? maxKT : (int)kTick;
  
  int *outPlace=L2Result+ mResultOffset;
  memcpy(outPlace,&out,sizeof( L2pedResults2012));

  if(par_dbg) L2pedResults2012_print(&out);

  //  computeStop(token);
  
  return true;
}


/* ========================================
  ======================================== */
void 
L2pedAlgo12::computeUser(int token ){

  criticalError("L2pedAlgo12::computeUser has been called and should not have been.  Serious problem in L2");

}

/*========================================
  ======================================== */
void 
L2pedAlgo12::finishRunUser() {/* called once at the end of the run */
  if(run_number<=0) return; // algo not used for current run

  int nBtowLow=0, nBtowHigh=0 ,nEtowLow=0, nEtowHigh=0 ;

  if(mLogFile==0) { LOG(ERR,"no open output log file,skip ped_finish()\n"); return;}  
  fprintf(mLogFile,"#L2-ped algorithm finishRun(%d), compiled: %s , %s\n",run_number,__DATE__,__TIME__);
  fprintf(mLogFile,"#params: pedSubtr=%d speedFact=%d saveBin=%d debug=%d prescAccept=%d\n",par_pedSubtr,par_speedFact,par_saveBinary,par_dbg,par_prescAccept);
  hA[10]->printCSV(mLogFile); // event accumulated
  int iMax=-3, iFWHM=-4;
  hA[11]->findMax( &iMax, &iFWHM);
  fprintf(mLogFile,"#L2ped  CPU/eve MPV %d kTicks,  FWHM=%d, seen eve=%d\n",iMax, iFWHM,nInp);
  if(par_saveBinary)  fprintf(mLogFile,"#L2ped  will save full spectra for all towers\n");

  int par_topAdc=85;
  int maxPedDeviation=5;
  int iadcHigh=maxAdc - maxPedDeviation;
  int iadcLow =minAdc + maxPedDeviation;

  char xAxis[100];
  sprintf(xAxis,"raw ADC + %d",-minAdc);
  if(par_pedSubtr) sprintf(xAxis,"ADC - ped + %d",-minAdc);
  
  
  fprintf(mLogFile,"# L2ped-Adc spectra, run=%d, Z-scale is ln(yield), only first digit shown;  maxPedDev=%d  table format:\n# name, ped, sigPed, crate, chan, softID-m-s-e, RDO_ID;\n#                                   ADC spectrum: [%d ...  <=-10 ... *=0  ...  >=+10 ... :=+20 ... %d],  Xaxis=%s\n",run_number,maxPedDeviation,minAdc,par_topAdc,xAxis);
  
  int i;
  int nB=0;
  for(i=0; i<EmcDbIndexMax; i++) {
    const L2EmcDb2012::EmcCDbItem *x=mDb->getByIndex(i);
    if(mDb->isEmpty(x)) continue;  /* dropped not mapped  channels */
    if (mDb->isBTOW(x) ||mDb->isETOW(x) ) { //
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
	
	// copy fraction of 1Dped histos to 2D histos
	const int *Data=h->getData();
	for(int k=0;k<par_maxMatt;k++) 
	  hA[22]->fillW(ihard,k,Data[k]);

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

	// copy fraction of 1Dped histos to 2D histos
	const int *Data=h->getData();
	for(int k=0;k<par_maxMatt;k++) 
	  hA[33]->fillW(ihard,k,Data[k]);

      }
   
      char okC=' ';
      if(x->fail) okC='#';
      fprintf(mLogFile,"%c%s %3d %4.1f 0x%02x 0x%02x %15s %4d ",okC,x->name,iMax+minAdc,iFWHM/2.3,x->crate, x->chan,x->tube, x->rdo);
      h->printPed(mLogFile,minAdc,par_topAdc,' ');
      fprintf(mLogFile,"qa=%c\n",pedQA);
      // exit(1); 
    } /* end of BTOW & ETOW */
  } /* end of pixels */

   
  fprintf(mLogFile,"#L2ped_finishRun() # of towers with |ped-pedDB| >10 chan\n#    BTOW: nLow=%d nHigh=%d ;   ETOW  nLow=%d, nHigh=%d\n",nBtowLow, nBtowHigh,nEtowLow, nEtowHigh);
  fprintf(mLogFile,"#    found peds for nB+E=%d , seen events=%d\n",nB,nInp);

  // sprintf(fname,"%s/run%d.l2ped.hist.bin",mOutDir1.c_str(),run_number); // full spectra in binary form
  
  if(mHistFile==0) {
    LOG(ERR,"Can't open .hist.bin file,skip ped_finish()\n"); 
    goto end;
  }  

  
  if(par_saveBinary) {
    LOG(DBG,"l2ped_finish() , save FULL spectra binary...\n");

    // .................. SAVE FULL SPECTRA BINARY ...........
    for(i=0; i<EmcDbIndexMax; i++) {
      const L2EmcDb2012::EmcCDbItem *x=mDb->getByIndex(i);
      if(mDb->isEmpty(x)) continue;  /* dropped not mapped  channels */
      L2Histo *h=0;
      char tit[400];
      if(mDb->isBTOW(x) ){
	h= btowAdc[x->rdo];
	//L2EmcDb2012::printItem(x);
	sprintf(tit,"BTOW=%s cr/ch=%03d/%03d stat/0x=%04x+%04x soft=%s; %s",x->name, x->crate, x->chan, x->stat, x->fail,x->tube,xAxis);
      } else if(mDb->isETOW(x) ) {
	h= etowAdc[x->rdo];
	//L2EmcDb2012::printItem(x);
	sprintf(tit,"ETOW=%s cr/ch=%03d/%03d stat/0x=%04x+%04x pname=%s; %s",x->name, x->crate, x->chan, x->stat, x->fail,x->tube,xAxis);
      }
      if(h==0) continue; //just in case
      h->setTitle(tit);
      h->write(mHistFile); // change title, add cr/chan/name, pedSubtrFlag
      // break;
    }
  }

 end:
  run_number=-2;
}

         

/**********************************************************************
  $Log: L2pedAlgo12.cxx,v $
  Revision 1.4  2012/03/21 18:18:03  jml
  got rid of printfs from 2012 files

  Revision 1.3  2011/10/19 16:12:12  jml
  more 2012 stuff

  Revision 1.2  2011/10/19 15:39:44  jml
  2012

  Revision 1.1  2011/10/18 15:11:43  jml
  adding 2012 algorithms

  Revision 1.1  2010/04/17 17:14:37  pibero
  *** empty log message ***

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
 
