#include <StEEmcUtil/database/EEmcDbItem.h>
class StChain;
class StMuEmcCollection;
class   EEmcDbItem;
class StEEmcDb;
StEEmcDb  *myDb;
StChain *chain=0;
TH1F * hx[8];
TH1F * hr[8];
TH1F * hd[8];
TH1F * hs[24];

int rdMuDst2spec(
	  char* file    = "R50530.MuDst.root",
	  Int_t nFiles  = 1, 
	  char* inDir   = "./",
	  int nEve=5 ){ 

  inDir   = "/star/data39/reco/production62GeV/ReversedFullField/P04id/2004/089//";
  file="st_physics_5089006_raw_4040002.MuDst.root";

  initHisto();

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcUtil");  
  gSystem->Load("StEEmcDbMaker");



  // create chain    
  chain = new StChain("StChain"); 
  
  // Now we add Makers to the chain...   
  muMk = new StMuDstMaker(0,0,inDir,file,"MuDst.root",nFiles);
  
  St_db_Maker *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb");
  new StEEmcDbMaker("eemcDb");

  // myMk1->setSectors(1,8);
  //  myDb->setTimeStampDay(20040320);  // format: yyyymmdd
  //myMk1->setPreferedFlavor("set-b","eemcPMTcal");


  chain->Init();
  chain->ls(3);
  myDb = (StEEmcDb*)chain->GetDataSet("StEEmcDb");

  exit;
  int eventCounter=0;
  int stat=0;
 
  //---------------------------------------------------
  while ( stat==0 ) {// loop over events
    if(eventCounter>=nEve) break;
    eventCounter++;
    chain->Clear();
    stat = chain->Make();

    // Access to muDst .......................
    StMuEvent* muEve = muMk->muDst()->event();
    int nPrim = muMk->muDst()->primaryTracks()->GetEntries();  // get number of primary tracks
    StEventInfo &info=muEve->eventInfo();
    // if(eventCounter%100!=0)continue;

    printf("\n\n ====================%d  processing eventID %d nPrim=%d ==============\n", eventCounter,info.id(),nPrim);

    hx[0]->Fill(nPrim);
    StMuEmcCollection* emc = muMk->muDst()->emcCollection();
    if (!emc) {
      printf(" No EMC data for this event\n");
      return kStOK;
    }
     printEEtower(emc);
     printEEpre(emc);
     printEEsmd(emc);
    
  }

}

//===========================================
//===========================================
printEEtower( StMuEmcCollection* emc ) {
  int sec,eta,sub,adc;
  // StMuEmcHit *hit;
  
  int i, nh;
  
  printf("\Total %d hits in Tower (only ADC>0)\n",emc->getNEndcapTowerADC());
  nh=0;
  for (i=0; i< emc->getNEndcapTowerADC(); i++) {
    emc->getEndcapTowerADC(i,adc,sec,sub,eta);
    //  if (adc<=0) continue; // print only non-zero values
    // printf("i=%d  Tower %2.2dT%c%2.2d   adc=%4d\n",i,sec,sub+'A'-1,eta,adc );
    //    printf("  Tower isec=%d ieta=%d isub=%d    adc=%4d\n",sec,eta, sub,adc );
    //int adcX=1000+ (eta-1) + (sub-1)*12 +(sec-1)*60;    assert(adc==adcX );


    // fill some histo
    int irad=sub -1 + 5*(sec-1) +60 *(eta-1);
    EEmcDbItem *x=myDb->getTile(sec,'A'+sub-1,eta,'T');
    if(x==0) continue;
    if(x->fail) continue;
    //printf("x=%p ped=%f gain=%f\n",x,x->ped, x->gain);
    if(adc<x->thr) continue;
    nh++;
    adc-=x->ped;
    if(adc>20) hr[0]->Fill(irad);
    ((TH2F*) hd[0])->Fill(irad,adc);
  }
  printf("  Total %d towers with ADC>Thr & !fail \n",nh);
}


//===========================================
//===========================================
printEEpre( StMuEmcCollection* emc ) {
  int sec,eta,sub,pre,adc;
  StMuEmcHit *hit;
  
  int i, nh;
  nh= emc->getNEndcapPrsHits();
  printf("\nTotal %d hits in pre1+2+post\n",nh);
  for (i=0; i<nh; i++) {
    hit=emc->getEndcapPrsHit(i,sec,sub,eta,pre);
    int ss=sub + 5*(pre-1);
    adc=hit->getAdc();
    //printf("i=%d  pre/post(%d) %2.2d%c%c%2.2d : energy=%f  adc=%d\n",i,pre,sec,pre+'P'-1,sub+'A'-1,eta,hit->getEnergy(),adc);
   
    // fill some histo
    EEmcDbItem *x=myDb->getTile(sec,'A'+sub-1,eta,'P'+pre-1);
    if(x==0) continue;
    if(x->fail) continue;
    if(adc<x->thr) continue;
    adc-=x->ped;
    int irad=sub -1 + 5*(sec-1) +60 *(eta-1);  
    if(adc>20)   hr[pre]->Fill(irad);
  }
}


//===========================================
//===========================================
printEEsmd( StMuEmcCollection* emc ) {
  int sec,strip,adc;
  char uv='U';
  
  for(uv='U'; uv<='V'; uv++) {
    int nh= emc->getNEndcapSmdHits(uv);
    printf("\nTotal %d hits in SMD-%c\n",nh,uv);
    for (int i=0; i<nh; i++) {
      hit=emc->getEndcapSmdHit(uv,i,sec,strip);
      adc=hit->getAdc();
      // printf("  SMD-%c  %2.2d%c%3.3d : energy=%f  adc=%d\n",uv,sec,uv,strip,hit->getEnergy(),adc);
   
      // fill some histo
      EEmcDbItem *x=myDb->getByStrip(sec,uv,strip);
      if(x==0) continue;
      if(x->fail) continue;
      if(adc<x->thr) continue;
      adc-=x->ped;
      
      int ip=2*(sec-1) +uv -'U';
      if(adc>20) hs[ip]->Fill(strip);

    }
  }
}

//===========================================
//===========================================
initHisto() {
  hx[0]=new TH1F("nPrim","no of prim tracks per event",200,0.,2000);

  hr[0]=new TH1F("adcT","ADC-ped>20  vs Tower ID, spiral mode; X=iphi+60*ieta",720,-0.5,719.5);
  hr[1]=new TH1F("adcP","ADC-ped>20  vs pres-1 ID, spiral mode; X=iphi+60*ieta",720,-0.5,719.5);
  hr[2]=new TH1F("adcQ","ADC-ped>20  vs pres-2 ID, spiral mode; X=iphi+60*ieta",720,-0.5,719.5);
  hr[3]=new TH1F("adcR","ADC-ped>20  vs post ID, spiral mode; X=iphi+60*ieta",720,-0.5,719.5);
 
  int ip;
  for(ip=0;ip<24;ip++) {
    char uv='U'+ip%2;
    int sec=1+ip/2;
    char tt1[100], tt2[100];
    sprintf(tt1,"adc%c%02d",uv,sec);
    sprintf(tt2,"ADC-ped>20   smd-%c%02d ; strip ID",uv,sec);
    hs[ip]=new TH1F(tt1,tt2,288,0.5,288.5);
  }
    

  TH2F *h2=new TH2F("adc2T","ADC-ped  vs Tower ID, spiral mode; X=iphi+60*ieta",720,-0.5,719.5,50,0,300);
  hd[0]=(TH1F*) h2; 

}

