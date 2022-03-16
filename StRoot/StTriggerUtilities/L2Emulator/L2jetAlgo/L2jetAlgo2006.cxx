#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

/*********************************************************************
 * $Id: L2jetAlgo2006.cxx,v 1.2 2009/11/19 15:48:45 balewski Exp $
 * \author Jan Balewski, IUCF, 2006 
 *********************************************************************
 * Descripion:
  Reco of mono- & di-jets in L2 using BTOW+ETOW
  depends on L2-DB class 
 *********************************************************************
 */



#ifdef  IS_REAL_L2  //in l2-ana  environment
  #include "../L2algoUtil/L2EmcDb.h"
  #include "../L2algoUtil/L2Histo.h"
#else
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2EmcDb.h"
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2Histo.h"
#endif

#include "L2jetAlgo2006.h"
#include "L2jetResults2006.h" // note, there may be another copy of it in ofl-CVS - do not mix them up, JB
#include "Map_DeltaPhiJets.h"

//=================================================
//=================================================
L2jetAlgo2006::L2jetAlgo2006(const char* name, L2EmcDb* db, char* outDir, int resOff) 
  :  L2VirtualAlgo( name,  db,  outDir, resOff) { 
  /* called one per days
     all memory allocation must be done here
  */
  par_maxADC=4095;
  par_maxEt=60;
  par_adcMask= (unsigned short) (-0x10); // to clear 4 LSF bits
  par_pedOff=0x10/2; //WARN, must match 'par_adcMask'
  createHisto();
  run_number=-1;
  printf("L2jetAlgo2006 instantiated, logPath='%s'\n",mOutDir);
  eve_Jet[0]=new L2Jet;
  eve_Jet[1]=new L2Jet;
}

/*========================================
  ======================================== */
bool
L2jetAlgo2006::paramsChanged( int *rc_ints, float *rc_floats) {
  int i;
  for(i=0;i<5;i++)
    if(rc_ints[i]!=raw_ints[i]) goto  foundProblem;

  for(i=0;i<5;i++)
    if(fabs(rc_floats[i]-raw_floats[i])>0.00001)  goto  foundProblem;

  return false;

 foundProblem:
      if (mLogFile) fprintf(mLogFile,"L2jet-ghost initRun - inconsistent params, ABORT initialization\n");
  return true;
}

/*========================================
  ======================================== */
int 
L2jetAlgo2006::initRun( int runNo, int *rc_ints, float *rc_floats) {

  // update DB if run # has changed
  if(mDb->initRun(runNo)) return -7; 
  // DB must be initialized prior to lookup tables

  if(run_number==runNo) {  
    if (mLogFile) fprintf(mLogFile,"L2jet::initRun-%s(%d)=ghost already initilized, only check params\n",mName, runNo);
    printf("L2jet::initRun-%s(%d)=ghost already initilized, only checking params\n",mName, runNo);
    int ret= paramsChanged(rc_ints, rc_floats);
    // 0=ok, 1=fatal problem
    if(ret){
      run_number=-77;
      if (mLogFile) { 
	fprintf(mLogFile,"L2jet algorithm init: crashA in internal logic\n");
	fclose(mLogFile);
      }
      return ret;     
    }
  }

  // clear content of all histograms
  int i;
  for (i=0; i<mxHA;i++) if(hA[i])hA[i]->reset();

  /* .... clear content, set threshold @ max as default */
  memset(db_btowThr,    0xFFFF,sizeof(db_btowThr));
  memset(db_btowPedS,       0,  sizeof(db_btowPedS));
  memset(db_btowGainCorr,  0,  sizeof(db_btowGainCorr));
  memset(db_btowL2PhiBin,  0,  sizeof(db_btowL2PhiBin));
  memset(db_btowL2PatchBin,0,  sizeof(db_btowL2PatchBin));

  memset(db_etowThr,    0xFFFF,sizeof(db_etowThr));
  memset(db_etowPedS,       0  ,sizeof(db_etowPedS));
  memset(db_etowGainCorr,  0  ,sizeof(db_etowGainCorr));
  memset(db_etowL2PhiBin,  0  ,sizeof(db_etowL2PhiBin));
  memset(db_etowL2PatchBin,0  ,sizeof(db_etowL2PatchBin));

  /* gain correction factor is mapped to 6 bits, 
     range: [5,60] 
     WARN: do NOT change 3 params below  w/o understaning of
     projection algo,JB
  */

  int par_IgainCorrOne=30;
  int par_IgainCorrMin=5;
  int par_IgainCorrMax=60;

  run_startUnix=time(0);
  run_number  =runNo;  // serves as a flag this run is initialized
  raw_ints    =rc_ints;
  raw_floats  =rc_floats;
  run_nEventOneJet=run_nEventDiJet= run_nEventRnd=0;

  char Fname[1000];
  sprintf(Fname,"%s/run%d.l2jet.out",mOutDir,run_number);
  printf("L2jet::initRun-%s('%s') ...\n",mName,Fname);

  mEventsInRun=0;
  mLogFile = fopen(Fname,"w");
  if( mLogFile==0) printf(" L2jetAlgo2006() UNABLE to open run summary log file, continue anyhow\n");
  //  mLogFile = stdout; //tmp

  // unpack params from run control GUI
  par_cutTag     =  rc_ints[0];
  par_useBtowEast= (rc_ints[1]&1 )>0;
  par_useBtowWest= (rc_ints[1]&2)>0;
  par_useEndcap  =  rc_ints[2]&1;
  int par_adcThr =  rc_ints[3]; // needed only in initRun()
  par_minPhiBinDiff=rc_ints[4];

  par_oneJetThr   =  rc_floats[0];
  par_diJetThrHigh=  rc_floats[1];
  par_diJetThrLow =  rc_floats[2];
  par_rndAccProb  =  rc_floats[3];
  par_dbg      =(int)rc_floats[4];

  // set other hardcoded or calculated  params
  par_energyScale=par_maxADC*par_IgainCorrOne/par_maxEt;
  // to monitor hot towers  in E+B Emc
  float monTwEtThr=2.0; // (GeV) Et threshold,WARN,  edit histo title by hand
  par_hotTwEtThr= (int)(monTwEtThr*par_energyScale); // now it is integer4 energy
  // thres for rnd accept 
  par_rndAccThr= int(par_rndAccProb* RAND_MAX);
  if(par_rndAccProb<0) {
    par_rndAccThr=0;
    par_rndAccProb=0.;
  } else if (par_rndAccProb>0.9999) {
    par_rndAccThr=RAND_MAX;
    par_rndAccProb=1.0;
  }
  if (mLogFile) { 
    fprintf(mLogFile,"L2jet algorithm initRun(%d), compiled: %s , %s\n params:\n",run_number,__DATE__,__TIME__);
    fprintf(mLogFile," - use BTOW: East=%d West=%d, Endcap=%d  L2ResOffset=%d\n", par_useBtowEast, par_useBtowWest,par_useEndcap ,mResultOffset);
    fprintf(mLogFile," - threshold: ADC-ped> %d \n", par_adcThr);
    fprintf(mLogFile," - min phi opening angle Jet1<->Jet2: %d in L2phiBins\n",par_minPhiBinDiff);   
    fprintf(mLogFile," - diJet  Et thrHigh= %.2f (GeV)   thrLow= %.2f  (GeV)\n", par_diJetThrHigh, par_diJetThrLow); 
    fprintf(mLogFile," - oneJet Et thr = %.2f (GeV) ; rndAccProb=%f;  cutTag=%d \n",par_oneJetThr,par_rndAccProb,par_cutTag);
    fprintf(mLogFile," - debug=%d, hot tower threshold: Et> %.1f GeV ( only monitoring)\n",par_dbg, monTwEtThr);
  }

  // verify consistency of input params
  int kBad=0;
  kBad+=0x0001 * ( !par_useBtowEast & !par_useBtowWest & !par_useEndcap);
  kBad+=0x0002 * (par_adcThr<par_pedOff); 
  kBad+=0x0004 * (par_adcThr>16); 
  kBad+=0x0008 * (par_minPhiBinDiff<5);
  kBad+=0x0010 * (par_minPhiBinDiff>=15);
  kBad+=0x0020 * (par_oneJetThr<3.);
  kBad+=0x0040 * (par_oneJetThr>12.);
  kBad+=0x0080 * (par_diJetThrLow<2.9); 
  kBad+=0x0100 * (par_diJetThrHigh<par_diJetThrLow); 
  kBad+=0x0200 * (par_diJetThrHigh>12.); 
  kBad+=0x0400 * (par_cutTag<=0 || par_cutTag>255);
  kBad+=0x0800 * (par_rndAccProb<0. || par_rndAccProb>1.);
  if (mLogFile) {
    fprintf(mLogFile,"L2jet initRun() params checked for consistency, Error flag=0x%04x\n",kBad);
    if(kBad)   fprintf(mLogFile,"L2jet initRun()  ABORT\n");
  }

  if(kBad) {
    run_number=-66;
    if (mLogFile) { 
      fprintf(mLogFile,"L2jet algorithm init: crashB in internal logic\n");
      fclose(mLogFile);
      return kBad;
    }
  }

  char tit[100];
  sprintf(tit,"# BTOW towers>ped+%d (input); x: # of towers/event",par_adcThr);
  hA[47]->setTitle(tit);
  
  sprintf(tit,"# ETOW towers>ped+%d (input); x: # of towers/event",par_adcThr);
  hA[48]->setTitle(tit);
  
  /*  const float eta[mxEta]={1.95,1.855,1.765,1.675,1.59,1.51,1.435,1.365,1.3,1.235,1.17,1.115}; */
  
  /* the first 13 entries mark the bounds of the 12 eta Bins.  14th value is not used */
  const float edgeEtaBinEtow[] = {
    2.0    ,
    1.9008 , 1.8065 , 1.7168 , 1.6317 , 1.5507 , 1.4738 ,
    1.4007 , 1.3312 , 1.2651 , 1.2023 , 1.1427 , 1.086  ,
    0.0
  };
 
  const int mxEtaBinsE=12,mxEtaBinsB=40;
  float idealGainEtow[mxEtaBinsE], idealGainBtow[mxEtaBinsB];
  float coshEtow[mxEtaBinsE],coshBtow[mxEtaBinsB]; 

  for(i=0;i<mxEtaBinsE;i++ ){
    float avrEta=(edgeEtaBinEtow[i]+edgeEtaBinEtow[i+1])/2.;
    coshEtow[i]=cosh(avrEta);
    idealGainEtow[i]=par_maxADC/par_maxEt/coshEtow[i];
    // if (mLogFile && i%4==0)  fprintf(mLogFile,"aim: ETow iEtaBin=%d eta=%.3f idealG=%.2f (GeV E_T), cosH=%.3f\n",i,avrEta, idealGainEtow[i], coshEtow[i]);
  }


  for(i=0;i<mxEtaBinsB;i++ ){
    float avrEta=-0.975 +i*0.05; /* assume BTOW has fixed eta bin size */
    coshBtow[i]=cosh(avrEta);
    idealGainBtow[i]=par_maxADC/par_maxEt/coshBtow[i];
    //  if (mLogFile && i%4==0)  fprintf(mLogFile,"aim: Btow iEtaBin=%2d eta=%.3f idealG=%.2f (GeV E_T), cosH=%.3f\n",i,avrEta, idealGainBtow[i], coshBtow[i]);
  }

  
  // rebuild local lookup tables
  
  int etowEtaBin2Patch[mxEtaBinsE]={14,14,13,13,12,12,11,11,11,10,10,10};

  int nB=0, nE=0; /* counts # of unmasekd towers */ 
  int nBg=0, nEg=0; /* counts # of reasonable calibrated towers */ 

  for(i=0; i<EmcDbIndexMax; i++) {
    const L2EmcDb::EmcCDbItem *x=mDb->getByIndex(i);
    if(mDb->isEmpty(x)) continue;  /* dropped not mapped  channels */
    if(x->fail) continue; /* dropped masked channels */
    if(x->gain<=0) continue; /* dropped uncalibrated towers , tmp */
    /* if(x->sec!=1) continue;   tmp, to test patch mapping */

    /* WARN, calculate index to match RDO order in the ADC data banks */
    int ietaP, iphiP;
    if (mDb->isBTOW(x) ) {
      /*....... B A R R E L  .................*/
      nB++;   
      if(x->eta<0 || x->eta>mxEtaBinsB) goto crashIt_1;
      if(!par_useBtowEast && x->eta<=20) continue;
      if(!par_useBtowWest && x->eta>=21) continue;
      ietaP= (x->eta-1)/4; /* correct */
      int iphiTw=(x->sec-1)*10 + x->sub-'a';
      // allign in phi  TP @ L2 w/ L0 
      iphiTw--;
      if(iphiTw<0) iphiTw=119;
      // now cr0x1e, mod1, subm2, is beginning of the first BTOW TP
      iphiP= iphiTw/4 ; /* correct */
      //      if(ietaP==0 && iphiP==5) 
      //if( (iphiTw==21 || iphiTw==22) && x->eta<=4) printf("%s  %s ietaP=%d iPhiP=%d  cr0x%02x ch=%03d\n",x->name, x->tube,ietaP, iphiP,x->crate,x->chan);
      int IgainCor=int(par_IgainCorrOne*idealGainBtow[x->eta-1]/x->gain);
      //printf("%s IgainCor=%d\n",x->name,IgainCor);
      /* gain outliers ignored   */
      if(IgainCor <par_IgainCorrMin) continue;
      if(IgainCor >par_IgainCorrMax) continue;  
      db_btowGainCorr[x->rdo]=IgainCor;
      
      db_btowL2PhiBin[x->rdo]=iphiP;
      db_btowL2PatchBin[x->rdo]=ietaP+ iphiP*cl2jetMaxEtaBins;
      db_btowThr[x->rdo]=(int) (x->ped+par_adcThr);
      db_btowPedS[x->rdo]=(unsigned short) (par_pedOff-x->ped);
      nBg++;
    } else if(mDb->isETOW(x) &&  par_useEndcap) {
      /*....... E N D C A P ........................*/
      nE++;   
      int iphiTw= (x->sec-1)*5 + x->sub-'A';
      // allign in phi  TP @ L2 w/ L0 
      iphiTw--;
      if(iphiTw<0) iphiTw=59;
      // now subsector 01TB is beginning of the first(==0) ETOW TP in phi
      iphiP= iphiTw/2 ; /* correct */
      if(x->eta<0 || x->eta>mxEtaBinsE) goto crashIt_1;
      ietaP=etowEtaBin2Patch[x->eta-1];
      //printf("%s  %s ietaP=%d iPhiP=%d\n",x->name, x->tube,ietaP, iphiP);
      int IgainCor=int(par_IgainCorrOne*idealGainEtow[x->eta-1]/x->gain);
      // printf("%s IgainCor=%d\n",x->name,IgainCor);
      /* gain outliers ignored   */
      if(IgainCor <par_IgainCorrMin) continue;
      if(IgainCor >par_IgainCorrMax) continue;  
      db_etowGainCorr[x->rdo]=IgainCor;
      
      db_etowL2PhiBin[x->rdo]=iphiP;
      db_etowL2PatchBin[x->rdo]=ietaP+ iphiP*cl2jetMaxEtaBins;
      db_etowThr[x->rdo]=(int) (x->ped+par_adcThr);
      db_etowPedS[x->rdo]=(unsigned short) (par_pedOff-x->ped);
      nEg++;
    }
    
  }

  if (mLogFile) {
    fprintf(mLogFile,"L2jet algorithm: found  working/calibrated: %d/%d=ETOW  & %d/%d=BTOW, based on ASCII DB\n",nE,nEg,nB,nBg);
  }

  return 0; //OK
  
 crashIt_1: /* fatal initialization error */
  run_number=-55;
  if (mLogFile) { 
    fprintf(mLogFile,"L2jet algorithm init: crashC in internal logic\n");
    fclose(mLogFile);
  }
 return -6;

}               


/*========================================
  ======================================== */
bool
L2jetAlgo2006::doEvent(int L0trg, int inpEveId, TrgDataType* trgData, 
		   int bemcIn, ushort *bemcData,
		   int eemcIn, ushort *eemcData){
  /* STRICT TIME BUDGET  START ....*/
  rdtscl_macro(mEveTimeStart);

  if(L0trg==1)   hA[10]->fill(1);
  else if(L0trg==2)   hA[10]->fill(2);
  
  if(eve_ID!=inpEveId) {//UUUU
    //.................... this event has NOT been processed

  /*
    Chris doesn't want us to write anything out 
    during event processing ...
  */
  
  eve_ID=inpEveId; // every events is processed only once
  mEventsInRun++;
  clearEvent(); /* price=13 kTicks */  
  int runTimeSec=time(0)- run_startUnix;
  hA[10]->fill(0);
  hA[12]->fill(runTimeSec);
  
  if(par_dbg>1) printf("\n......... in  L2Jet_doEvent(ID=%d)... bIn=%d eIn=%d\n",eve_ID,bemcIn,eemcIn);
  
  if (bemcIn || eemcIn){//VVVV this algo has nothing to do w/o any Ecal data 
  
  eve_TrigData=(TrgDataType* )trgData; /* internal copy of the pointer */


  //===== step 1: unpack raw data blocks ==============
   
  int nBtowTw=0, nEtowTw=0;
  /*......... BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB */
  if(bemcIn==1 && (par_useBtowEast||par_useBtowWest) ) {
    nBtowTw=projectAdc( bemcData, MaxBtowRdo,
			db_btowThr, db_btowPedS, db_btowGainCorr,
			db_btowL2PhiBin, db_btowL2PatchBin,
			hA[20] );
  }

  /*........... EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE */
  if(eemcIn==1 && par_useEndcap ) {
    nEtowTw=projectAdc( eemcData, 720,
			db_etowThr, db_etowPedS, db_etowGainCorr,
			db_etowL2PhiBin, db_etowL2PatchBin,
			hA[30] );
  }

  //===== step 2: scan phi projection of the whole ECAL for two peaks===
  // total transverse energy  in the whole calorimeter 
  int    itotEne=scanPhi();
  float  totEneGeV=itotEne/par_energyScale;
  int    itotEneGeV=(int)totEneGeV;

  //===== step 3 : scan two eta projections for peak in phi ===
  int iJ;
  for(iJ=0; iJ< mxJ; iJ++) {
    scanEta(iJ); // find eta location
    weightedPhi(iJ); // find weighted phi
    // auxiliary calculations
    L2Jet *J=eve_Jet[iJ];
    J->eneGeV=J->iene/par_energyScale;
    J->phiRad=0.21*(6.0-J->fphiBin); // by construction can't exceed 1.3
    // assert range
    // this should be done by the lookup table , ok for now
    while(J->phiRad<0) J->phiRad+=6.2832;
    while(J->phiRad>6.2832) J->phiRad-=6.2832;
  }

  if(eve_Jet[0]->eneGeV <eve_Jet[1]->eneGeV) {// swap jets fo E1>E2
    L2Jet *Jx=eve_Jet[0];
    eve_Jet[0]=eve_Jet[1];
    eve_Jet[1]=Jx;
  }
  
  if(par_dbg>2) printf("doEvent iphiBin1=%d iene1=%d , iphiBin2=%d iene2=%d\n",eve_Jet[0]->iphiBin,eve_Jet[0]->iene,eve_Jet[1]->iphiBin,eve_Jet[1]->iene);
  
  //====== step 4: make trigger decisions====
  bool acceptDiJet=( eve_Jet[0]->eneGeV > par_diJetThrHigh) && ( eve_Jet[1]->eneGeV > par_diJetThrLow);
  bool acceptOneJet=( eve_Jet[0]->eneGeV> par_oneJetThr) ;

  bool acceptRnd=rand()< par_rndAccThr;
  mAccept=acceptDiJet || acceptOneJet || acceptRnd;

  //====== step 5: update various monitorig histos

  // histogramming reco Et1-Et2, no cuts
  int iet1 =(int)eve_Jet[0]->eneGeV;
  int iet2 =(int)eve_Jet[1]->eneGeV;
  int ieta1=(int)eve_Jet[0]->fetaBin;
  int ieta2=(int)eve_Jet[1]->fetaBin;
  int iphi1=(int)eve_Jet[0]->fphiBin;
  int iphi2=(int)eve_Jet[1]->fphiBin;

  hA[40]->fill(iet1,iet2);

  hA[41]->fill(ieta1,iphi1);
  hA[42]->fill(ieta2,iphi2);
  hA[43]->fill(iphi1,iphi2);
  hA[44]->fill(iet1);
  hA[45]->fill(iet2);
  hA[46]->fill(itotEneGeV);
  hA[47]->fill(nBtowTw);
  hA[48]->fill(nEtowTw);

  // sivers delta zeta, the map is still worng
  int kphi1=int(eve_Jet[0]->phiRad*10.);
  int kphi2=int(eve_Jet[1]->phiRad*10.);
  int idelZeta=map_DelPhiJets[kphi1*MxPhiRad10 + kphi2];

  if( mAccept)  hA[10]->fill(8);

  if(acceptOneJet ){ 
    hA[10]->fill(4);
    run_nEventOneJet++;
    hA[13]->fill(runTimeSec);
    hA[50]->fill(iet1);
    hA[51]->fill(ieta1,iphi1);
    hA[52]->fill(ieta1);
    hA[53]->fill(iphi1);
  }
 
 if(acceptDiJet  ){
    hA[10]->fill(5);
    run_nEventDiJet++;
    hA[14]->fill(runTimeSec);
    hA[60]->fill(iet1,iet2);
    hA[61]->fill(ieta1,iphi1);
    hA[62]->fill(ieta2,iphi2);
    hA[63]->fill(iphi1,iphi2);
    hA[64]->fill(iet1);
    hA[65]->fill(iet2);
    hA[66]->fill(ieta1);
    hA[67]->fill(ieta2);
    hA[68]->fill(iphi1);
    hA[69]->fill(iphi2);
    hA[70]->fill(idelZeta);
    hA[71]->fill(ieta1,idelZeta);
    hA[72]->fill(ieta1,ieta2);
    hA[73]->fill((iphi1+iphi2)/2,idelZeta);
    hA[74]->fill(itotEneGeV);
   }
  if(acceptRnd ){ 
    hA[10]->fill(6);
    run_nEventRnd++;
    hA[15]->fill(runTimeSec);
  }


  //====== step 6:  fill L2Result  (except time)
  L2jetResults2006 out; // all output bits lump together
  memset(&out,0,sizeof(out)); // clear content
  
  out.int0.version=L2JET_RESULTS_VERSION;
  out.int0.decision=
    ( par_useBtowEast <<0 ) +
    ( par_useBtowWest <<1 ) +
    ( par_useEndcap   <<2 ) +
    ( (bemcIn>0)      <<3 ) + 
    ( (eemcIn>0)      <<4 ) + 
    (  acceptRnd      <<5 ) +
    (  acceptOneJet   <<6 ) +
    (  acceptDiJet    <<7 ) ;
  out.int0.cutTag=par_cutTag;

  out.int1.iTotEne=(unsigned short)(totEneGeV*100.); // now 1=10 MeV
  out.int2.nBtowTw=nBtowTw;
  out.int2.nEtowTw=nEtowTw;
  
  out.jet1.jPhi=(int)(eve_Jet[0]->phiRad*28.65);  //so phi/deg=2*jPhi
  out.jet1.jEta=(int)(eve_Jet[0]->fetaBin*10.);
  out.jet1.iEne=(unsigned short)(eve_Jet[0]->eneGeV*100.); // now 1=10 MeV
  
  out.jet2.jPhi=(int)(eve_Jet[1]->phiRad*28.65);
  out.jet2.jEta=(int)(eve_Jet[1]->fetaBin*10.);
  out.jet2.iEne=(unsigned short)(eve_Jet[1]->eneGeV*100.); // now 1=10 MeV

  rdtscl_macro(mEveTimeStop);
  mEveTimeDiff=mEveTimeStop-mEveTimeStart;
  int  kTick=mEveTimeDiff/1000;
  //   printf("jj=%f t1=%d t2=%d \n",mEveTimeDiff/1000.,mEveTimeStart,mEveTimeStop);
  mhT->fill(kTick);

  out.int0.kTick=  kTick>255 ? 255 : kTick;
 
  //calculate and match the check sum
  out.int1.checkSum=-L2jetResults2006_doCheckSum(&out);
  // unsigned char  cSum=L2jetResults2006_doCheckSum(&out); printf("cSum2=%d\n",cSum);

  //===== step 7: write L2Result
  uint *outPlace=eve_TrigData->TrgSum.L2Result+mResultOffset;
  memcpy(outPlace,&out,sizeof( L2jetResults2006));

  // dirty tests, clean it up before real use

  if(par_dbg){//WWWW
    L2jetResults2006_print(&out);
    printf(" phiRad1=%f  phiRad2=%f \n",eve_Jet[0]->phiRad,eve_Jet[1]->phiRad);
    printf("idelZeta=%d  delZeta/deg=%.1f \n\n",idelZeta,idelZeta/31.416*180);


    //tmp printouts of errors:
    if( out.jet1.iEne+out.jet2.iEne > out.int1.iTotEne) {
      printf("L2jet-fatal error, eve=%d, iEtot=%d < iEJ1=%d + iEJ2=%d, continue\n",inpEveId, out.int1.iTotEne,out.jet1.iEne,out.jet2.iEne);
    }
    if(iphi1==iphi2) {
      printf("L2jet-fatal error,neveId=%d, phi1,2=%d,%d\n",mEventsInRun,iphi1,iphi2);
      dumpPatchEneA();    
    }
    
    if( L2jetResults2006_doCheckSum(&out)) {
      printf("L2jet-fatal error, wrong cSum=%d\n", L2jetResults2006_doCheckSum(&out));
      L2jetResults2006_print(&out);
    }
  }  // end of WWWW
  }// end of VVVV (etow or btow has some data)
  }// end of UUUU event processing


  return   mAccept;
}


/*========================================
  ======================================== */
void 
L2jetAlgo2006::finishRun() {  /* called once at the end of the run */
  if(run_number<0) return; // already finished
  // save run summary histos
  char Fname[1000];
  sprintf(Fname,"%s/run%d.l2jet.hist.bin",mOutDir,run_number);
  printf("L2jet::finishRun('%s') , save histo ...\n",Fname);
  mHistFile = fopen(Fname,"w");

  if (mLogFile) {
    fprintf(mLogFile,"L2-jet algorithm finishRun(%d)\n",run_number);
    fprintf(mLogFile," - %d events seen by L2 di-jet\n",mEventsInRun);
    fprintf(mLogFile," - accepted: rnd=%d  oneJet=%d diJet=%d \n", run_nEventRnd,  run_nEventOneJet, run_nEventDiJet);

    // print few basic histos
    
    hA[10]->printCSV(mLogFile); // event accumulated

  }
  finishRunHisto(); // still needs current DB

  if( mHistFile==0) {
    printf(" L2jetAlgo2006: finishRun() UNABLE to open run summary log file, continue anyhow\n");
    if (mLogFile)
      fprintf(mLogFile,"L2 di-jet histos NOT saved, I/O error\n");
  } else { // save histos  
    int j;
    int nh=0;
    for(j=0;j<mxHA;j++) {
      if(hA[j]==0) continue;
      hA[j]->write(mHistFile);
      nh++;
    }
    finishCommonHistos();
    fclose(mHistFile);
    mHistFile=0;
    if (mLogFile)
      fprintf(mLogFile,"L2 di-jet: %d histos saved to '%s'\n",nh,Fname);
  }

  run_number=-2; // clear run #
 
  /* close the output file if it is open */
  if (mLogFile && mLogFile!=stdout) {
    fclose(mLogFile);
    mLogFile=0;
  }
  
}


//=======================================
//=======================================
void 
L2jetAlgo2006::createHisto() {
  memset(hA,0,sizeof(hA));

  hA[10]=new   L2Histo(10, (char*)"total event counter; x=cases",9);
  hA[11]=new   L2Histo(11, (char*)"L2 time used per input event;  x: time (CPU kTics), range=100muSec; y: events ",160);

  int mxRunDration=2500;
  hA[12]=new   L2Histo(12, (char*)"rate of input events; x: time in this run (seconds); y: rate (Hz)", mxRunDration);
  
  hA[13]=new   L2Histo(13, (char*)"rate of  accepted one-Jet; x: time in this run (seconds); y: rate (Hz)", mxRunDration);
  hA[14]=new   L2Histo(14, (char*)"rate of  accepted di-Jet ; x: time in this run (seconds); y: rate (Hz)", mxRunDration);
  hA[15]=new   L2Histo(15, (char*)"rate of  random accepted  ; x: time in this run (seconds); y: rate (Hz)", mxRunDration);

  // BTOW  raw spectra
  hA[20]=new   L2Histo(20, (char*)"BTOW tower, Et>2.0 GeV (input); x: BTOW RDO index=chan*30+fiber; y: counts", 4800);
  hA[21]=new   L2Histo(21, (char*)"BTOW tower, Et>2.0 GeV (input); x: BTOW softID", 4800);
  hA[22]=new   L2Histo(22, (char*)"BTOW tower, Et>2.0 GeV (input); x: eta bin, [-1,+1];  y: phi bin ~sector",40,120);
  
  // ETOW  raw spectra
  hA[30]=new   L2Histo(30, (char*)"ETOW tower, Et>2.0 GeV (input); x: ETOW RDO index=chan*6+fiber; y: counts", 720 );
  hA[31]=new   L2Histo(31, (char*)"ETOW tower, Et>2.0 GeV (input); x: i=chan+128*crate", 768);
  hA[32]=new   L2Histo(32, (char*)"ETOW tower, Et>2.0 GeV (input); x: 12 - Endcap etaBin ,[+1,+2];  y: phi bin ~sector",12,60);
  
 // Di-Jet raw yields
 hA[40]=new   L2Histo(40, (char*)"Et Jet1-Jet2 (input); x: Jet1 Et/GeV ; Jet2 Et/GeV",12,12);
 hA[41]=new   L2Histo(41, (char*)"diJet1 eta-phi (input); x: iEta [-1,+2] ; y: iPhi ~sector ",15,30);
 hA[42]=new   L2Histo(42, (char*)"diJet2 eta-phi (input); x: iEta [-1,+2]  ; y: iPhi ~sector",15,30);

 hA[43]=new  L2Histo(43, (char*)"diJet phi1-phi2 (input); x: iPhi1 ~sector ; y: iPhi2 ~sector ",30,30);

 hA[44]=new  L2Histo(44, (char*)"Jet1 Et (input); x: Et (GeV)", 60);
 hA[45]=new  L2Histo(45, (char*)"Jet2 Et (input); x: Et (GeV)", 60);
 hA[46]=new  L2Histo(46, (char*)"total Et (input); x: Et (GeV)", 60);
 hA[47]=new  L2Histo(47, (char*)"# BTOW towers>thrXX (input); x: # of towers/event", 200);
 hA[48]=new  L2Histo(48, (char*)"# ETOW towers>thrXX (input); x: # of towers/event", 100);

 // ........accepted one-jet events
 hA[50]=new  L2Histo(50, (char*)"one-Jet Et (accepted); x: jet Et (GeV)", 60);
 hA[51]=new  L2Histo(51, (char*)"one-Jet eta-phi (accepted); x: iEta [-1,+2] ; y: iPhi ~sector ",15,30);
 hA[52]=new  L2Histo(52, (char*)"one-Jet eta (accepted); x: iEta [-1,+2]", 15);
 hA[53]=new  L2Histo(53, (char*)"one-Jet phi (accepted); x: iPhi ~sector", 30);

 // Di-Jet accepted
 hA[60]=new   L2Histo(60, (char*)"Et of Jet1 vs. Jet2  (accepted); x: Jet1/GeV ; Jet2/GeV",12,12);
 hA[61]=new   L2Histo(61, (char*)"diJet1 eta-phi   (accepted); x: iEta [-1,+2] ; y: iPhi ~sector ",15,30);
 hA[62]=new   L2Histo(62, (char*)"diJet2 eta-phi   (accepted); x: iEta [-1,+2]  ; y: iPhi ~sector",15,30);

 hA[63]=new  L2Histo(63, (char*)"diJet phi1-phi2   (accepted); x: iPhi1 ~sector ; y: iPhi2 ~sector ",30,30);

 hA[64]=new  L2Histo(64, (char*)"diJet1 Et  (accepted); x: Et (GeV)", 60);
 hA[65]=new  L2Histo(65, (char*)"diJet2 Et   (accepted); x: Et (GeV)", 60);

 hA[66]=new  L2Histo(66, (char*)"diJet1  eta (accepted); x: i Eta [-1,+2]", 15);
 hA[67]=new  L2Histo(67, (char*)"diJet2  eta (accepted); x: i Eta [-1,+2]", 15);
 hA[68]=new  L2Histo(68, (char*)"diJet1 phi (accepted); x: iPhi ~sector", 30);
 hA[69]=new  L2Histo(69, (char*)"diJet2 phi (accepted); x: iPhi ~sector", 30);
 hA[70]=new  L2Histo(70, (char*)"diJet delZeta  (accepted); x: delta zeta  (rad*10)", MxPhiRad10);
 hA[71]=new  L2Histo(71, (char*)"diJet delZeta vs. eta1 (accepted); x: iEta1 [-1,+2] ; y: delta zeta  (rad*10)",15, MxPhiRad10);
 hA[72]=new  L2Histo(72, (char*)"diJet eta2 vs. eta1  (accepted); x: iEta1 [-1,+2] ;x: iEta2 [-1,+2] ",15,15);
 hA[73]=new  L2Histo(73, (char*)"diJet   delZeta vs. avrPhi (accepted); x: (iphi1+iphi2)/2  (12 deg/bin); y: delta zeta  (rad*10)",30, MxPhiRad10);
 hA[74]=new  L2Histo(74, (char*)"total Et diJet (accepted); x: Et (GeV)", 60);

}

//=======================================
//=======================================
void 
L2jetAlgo2006::clearEvent(){
  /*  printf("clearEvent_L2jet() executed\n"); */

  eve_TrigData=0;
  mAccept=false;
  mEveTimeDiff=0;
  memset(eve_patchEne,0,sizeof(eve_patchEne));
  memset(eve_phiEne,0,sizeof(eve_phiEne));
  eve_Jet[0]->clear();
  eve_Jet[1]->clear();
}



//=======================================
//=======================================
int 
L2jetAlgo2006::projectAdc( ushort *rawAdc, int nRdo,
	     ushort *thr, ushort *pedS, ushort *gainCorr,
	     ushort *phiBin, ushort *patchBin,
	     L2Histo *hHot	 ){
  
  int tmpNused=0; /* counts mapped & used ADC channels */

  short rdo;  
  int adc,adc4; 
  for(rdo=0; rdo<nRdo; rdo++){
    if(rawAdc[rdo]<thr[rdo])continue;
    // adc=rawAdc[rdo]-ped[rdo]; // old, before drop of 4 LSB was introduced.
    adc=(rawAdc[rdo]+pedS[rdo]) & par_adcMask ;
    adc4=adc*gainCorr[rdo];
    eve_patchEne[patchBin[rdo]]+=adc4;
    eve_phiEne[phiBin[rdo]]+=adc4;    
    tmpNused++; /* drop it later */
    // only monitoring
    //   if(par_dbg>3) printf("pro rdo=%d adc=%d  nTw=%d\n",rdo,adc,tmpNused);
    if(adc4 >par_hotTwEtThr) hHot->fill(rdo);
  }
 
  return tmpNused;
}

//=======================================
//=======================================
int 
L2jetAlgo2006::scanPhi(){
  /* build running sum for every 3 phi-bins:
     - clone data in extra 2 bins to avoid 'if' statement for circular phi
     - calculate running sum
     - find absolute maximum
     - find 2nd maximum seperated in phi
 */
 
  int  phiEneSum[cl2jetMaxPhiBins];
  memset(phiEneSum,0,sizeof(phiEneSum));
 
  /* WARN: must match l2jet_par_mxPhiBin, provides 2 extra bins */
  eve_phiEne[cl2jetMaxPhiBins+0]=eve_phiEne[0];
  eve_phiEne[cl2jetMaxPhiBins+1]=eve_phiEne[1];
  
  int i;
  int sum;
  int sumMax1=0, iMax1=-1;
  int sumTot=0;

  int  *phiEneA=eve_phiEne;
  for(i=0;i<cl2jetMaxPhiBins;i++,phiEneA++){
    sumTot+=phiEneA[0];
    sum=phiEneA[0]+phiEneA[1]+phiEneA[2]; 
    phiEneSum[i]=sum;
    if(sumMax1>sum) continue;
    sumMax1=sum;
    iMax1=i;
  }

  if(par_dbg>2) printf("phiScan: sum1=%d, iphi1=%d\n",sumMax1,iMax1);
  /* find second maximum in phi*/

  int sumMax2=0, iMax2=-1; 
  char doWrap=0;
  int k1=iMax1-par_minPhiBinDiff;
  int k2=iMax1+par_minPhiBinDiff;
  if (k1<0) { k1+=cl2jetMaxPhiBins; doWrap+=1; }
  if (k2>=cl2jetMaxPhiBins) { k2-=cl2jetMaxPhiBins; doWrap+=2; }
    
  if (!doWrap) { /* masking range does NOT wrap up */
    for(i=0;i<cl2jetMaxPhiBins;i++){
      /* printf("aa %d-%d =%d?%d -->%d\n",i,iMax1,abs(i-iMax1),l2jet_par_minPhiBdist,abs(i-iMax1)<l2jet_par_minPhiBdist);  */
      if(i>=k1 && i<=k2) continue;
      if(sumMax2>phiEneSum[i]) continue;
      sumMax2=phiEneSum[i];
      iMax2=i;
    }
  } else { /* masking range  WRAPS up */
    for(i=k2;i<k1;i++){
      /*  printf("bc %d-%d =%d?%d -->%d\n",i,iMax1,abs(i-iMax1),l2jet_par_minPhiBdist,abs(i-iMax1)<l2jet_par_minPhiBdist); */
      if(sumMax2>phiEneSum[i]) continue;
      sumMax2=phiEneSum[i];
      iMax2=i;
    }
  }
  if(par_dbg>2) printf("phiScan: sum2=%d, iphi2=%d\n",sumMax2,iMax2);


  eve_Jet[0]->iphiBin=iMax1;
  eve_Jet[1]->iphiBin=iMax2;
  
  return sumTot;

}



//========================================
//========================================
void 
L2jetAlgo2006::scanEta(int iJ){
  L2Jet *J=eve_Jet[iJ];
  
  int iphi0=J->iphiBin;

  /* - sum  energy vs. etaBin for selected 3 phi-bins:
     - calculate running sum for every 3 eta-bins 
     - find absolute maximum for 3bin sum
     - find energy weighted eta of the jet
  */

  int  eneA[cl2jetMaxEtaBins];
  memset(eneA,0,sizeof(eneA));

  int sum;
  int sumMax=1; // sth very small to avoid division by 0 if ECAL is empty
  int iMax=0; // default to most East eta bin

  /* collaps 3 phi bins on eta-axis */
  int ix,iy;
  for(iy=0;iy<cl2jet_par_mxPhiBin;iy++) {
    int jy=(iphi0+iy)%cl2jetMaxPhiBins; /* phi must wrap up */
    int  *patchEneA=eve_patchEne+(jy*cl2jetMaxEtaBins);
    for(ix=0;ix<cl2jetMaxEtaBins;ix++,patchEneA++){
      eneA[ix]+=*patchEneA;
    }
  }

  int  *eneAp=eneA;
  for(ix=0;ix<cl2jetMaxEtaBins-cl2jet_par_mxEtaBin+1;ix++,eneAp++) {
    /* printf(" J1 ieta=%d E=%.2f\n", ix,eneA[0]/l2jet_par_energyScale);
       printf("            E=%.2f\n",etaEneA[ix]/l2jet_par_energyScale); */
    sum=eneAp[0]+eneAp[1]+eneAp[2]; 
    //  printf("ix=%d sum=%d sumMax=%d\n",ix,sum,sumMax);
    if(sumMax>sum) continue;
    sumMax=sum;
    iMax=ix;
  }

  // calculate center of gravity
  int sumX=0;
  for(ix=iMax;ix<iMax+cl2jet_par_mxEtaBin;ix++){
    sumX+=ix*eneA[ix];
  }
  float fetaBin=0.5+1.*sumX/sumMax;


  if(par_dbg>2){
    if(par_dbg>3) {
      printf("scanEta iphi0=%d\n eta profile:\n L2eta-bin energy\n",iphi0);
      for(ix=0;ix<cl2jetMaxEtaBins;ix++){
	printf("%d %d\n",ix,eneA[ix]);
      }
    }
    printf("scanEta: sum=%d, ietaLeft=%d\n",sumMax,iMax);
    printf("sumX=%d fetaBinmax=%.1f  \n",sumX,fetaBin);
  }

  J->fetaBin=fetaBin;
  J->iene=sumMax;    
  J->ietaBin=iMax;
}

//========================================
//========================================
void 
L2jetAlgo2006:: dumpPatchEneA(){
  // dump L2 array with energy 
  int ix,iy;
  for(iy=0;iy<cl2jetMaxPhiBins;iy++) {
    int  *patchEneA=eve_patchEne+(iy*cl2jetMaxEtaBins);// phi bins are consecutive
    
    for(ix=0;ix<cl2jetMaxEtaBins;ix++,patchEneA++){
      printf(" %6d",*patchEneA);
    }
    printf(" iPhi=%d\n",iy);
  }
  
}

//========================================
//========================================
void 
L2jetAlgo2006::weightedPhi(int iJ){
  L2Jet *J=eve_Jet[iJ];

  
  // empty calo protection
  if(J->iene<=1) { J->fphiBin=J->iphiBin+.333; return;}

  int iphi0=J->iphiBin;
  int ieta0=J->ietaBin;

  /*
    - find energy weighted phi of the jet
    - if phi>360 it wraps phi  over 
  */
  // printf("weightedPhi() inp: iphi0=%d ieta0=%d\n",iphi0, ieta0);
  int sum=1, sumY=0; // make sum non zero so it never fails
  int sum1;//wrk variable

  int ix,iy;
  // pick 3x3 eta-phi patch and calculate weighted phi==Y
  for(iy=iphi0;iy<iphi0+cl2jet_par_mxPhiBin;iy++) {
    int jy=iy % cl2jetMaxPhiBins; /* iphi must wrap up */
    int  *patchEneA=eve_patchEne+(jy*cl2jetMaxEtaBins);// phi bins are consecutive
#if 0
    for(ix=0;ix<cl2jetMaxEtaBins;ix++){
      printf(" %6d",patchEneA[ix]);
    }
    printf(" iy=%d \n",iy);
#endif
    
    sum1=0;
    for(ix=ieta0;ix<ieta0+cl2jet_par_mxEtaBin;ix++){
      sum1+=patchEneA[ix];
    }
    sum+=sum1; // do it again, coul dbe passed from scanEta
    sumY+=sum1*iy;
    //printf("jy=%d  sym1=%d sum=%d sumY=%d\n",jy,sum1, sum,sumY);    
  }
  
  float fphiBinMax=0.5 + 1.*sumY/sum;
  if( fphiBinMax>cl2jetMaxPhiBins) fphiBinMax-=cl2jetMaxPhiBins;
  if(par_dbg>2)  printf("weightedPhi() sum=%d sumY=%d fphiBin=%.2f\n",sum,sumY, fphiBinMax);
  J->fphiBin=fphiBinMax;
}

//=======================================
//=======================================
void 
L2jetAlgo2006::finishRunHisto(){
  // auxialiary operations on histograms at the end of the run

  const int *data20=hA[20]->getData();
  const int *data30=hA[30]->getData();

  int bHotSum=1,bHotId=-1;
  int eHotSum=1;

  const L2EmcDb::EmcCDbItem *xE=mDb->getByIndex(402), *xB=mDb->getByIndex(402);
  int i;
  for(i=0; i<EmcDbIndexMax; i++) {
    const L2EmcDb::EmcCDbItem *x=mDb->getByIndex(i);
    if(mDb->isEmpty(x)) continue; 
    if (mDb->isBTOW(x) ) {
      int softId=atoi(x->tube+2);
      int ieta= (x->eta-1);
      int iphi= (x->sec-1)*10 + x->sub-'a' ;
      //mDb->printItem(x); printf("softID=%d\n",softId);
      hA[21]->fillW(softId,data20[x->rdo]);
      hA[22]->fillW(ieta, iphi,data20[x->rdo]);
      if(bHotSum<data20[x->rdo]) {
	bHotSum=data20[x->rdo];
	bHotId=softId;
	xB=x;
      }
   }// end of BTOW
    else  if (mDb->isETOW(x) ) {
      int ihard=x->chan+(x->crate-1)*128;
      int ieta= 12-x->eta;
      int iphi= (x->sec-1)*5 + x->sub-'A' ;
      hA[31]->fillW(ihard,data30[x->rdo]);
      hA[32]->fillW(ieta, iphi,data30[x->rdo]);
      if(eHotSum<data30[x->rdo]) {
	eHotSum=data30[x->rdo];
	xE=x;
      }

    }// end of BTOW
  }
  if (mLogFile){
    fprintf(mLogFile,"L2jet::finishRun()\n");
    fprintf(mLogFile,"#BTOW_hot tower _candidate_ (bHotSum=%d) :, softID %d , crate %d , chan %d , name %s\n",bHotSum,bHotId,xB->crate,xB->chan,xB->name); 
   fprintf(mLogFile,"#ETOW_hot tower _candidate_ (eHotSum=%d) :, name %s , crate %d , chan %d\n",eHotSum,xE->name,xE->crate,xE->chan); 
  }


}


/**********************************************************************
  $Log: L2jetAlgo2006.cxx,v $
  Revision 1.2  2009/11/19 15:48:45  balewski
  add (char*) to many strings to make SL5 happ, few other adjustments

  Revision 1.1  2007/11/19 22:18:27  balewski
  most L2algos provide triggerID's

  Revision 1.8  2007/11/14 03:58:14  balewski
  cleanup of common timing measurement

  Revision 1.7  2007/11/13 23:06:07  balewski
  toward more unified L2-algos

  Revision 1.6  2007/11/13 00:12:36  balewski
  added offline triggerID, take1

  Revision 1.5  2007/11/08 04:02:31  balewski
  run on l2ana as well

  Revision 1.4  2007/11/02 17:43:08  balewski
  cleanup & it started to work w/ L2upsilon

  Revision 1.3  2007/11/02 03:03:47  balewski
  modified L2VirtualAlgo

  Revision 1.2  2007/10/25 02:07:02  balewski
  added L2upsilon & binary event dump

  Revision 1.1  2007/10/11 00:33:19  balewski
  L2algo added

  Revision 1.5  2006/03/28 19:46:49  balewski
  ver16b, in l2new

  Revision 1.4  2006/03/11 17:08:33  balewski
  now CVS comments should work

*/


