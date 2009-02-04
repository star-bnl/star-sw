// $Id: EEsmdCal.cxx,v 1.20 2009/02/04 20:33:22 ogrebeny Exp $
 
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include <TClonesArray.h>
#include <TObjArray.h> 
#include <TH1.h> 
#include <TH2.h> 
#include <TFile.h> 

#include "EEsmdCal.h"
#include "EEsmdPlain.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"
#include "StEEmcUtil/EEmcSmdMap/EEmcSmdMap.h"
#include "StEEmcUtil/database/cstructs/eemcConstDB.hh"
#include "StEEmcUtil/database/EEmcDbItem.h"

#ifdef StRootFREE
  #include "EEmcDb/EEmcDb.h"
#else
  #include "StEEmcUtil/database/StEEmcDb.h"
#endif

 
ClassImp(EEsmdCal)
//--------------------------------------------------
//--------------------------------------------------
EEsmdCal::EEsmdCal(){
  nInpEve=0; 
  HList=0; 
  eeDb=0;

  // clear all histo pointers just in case
  memset(hT,0,sizeof(hT));
  memset(hSs,0,sizeof(hSs));
  // memset(hSp,0,sizeof(hSp));

  dbMapped=-1;
  memset(dbT,0,sizeof(dbT));
  memset(dbS,0,sizeof(dbS));

  // initialization
  smdHitPl=new EEsmdPlain [MaxSmdPlains];
  geoTw=new EEmcGeomSimple;
  geoSmd= EEmcSmdGeom::instance();
  mapSmd= EEmcSmdMap::instance();

  thrMipSmdE=-1; emptyStripCount=-2; 
  twMipRelEneLow=-3; twMipRelEneHigh=-4;
  offCenter=0.7; thrMipPresAdc=-5;
  
  maxStripAdc=120; // suppress large jump in ped or sticky bits

  // chose which stat bits are fatal
  killStat=EEMCSTAT_ONLPED  | EEMCSTAT_HOTSTR ;

  printf("EEsmdCal() constructed\n");
}

//--------------------------------------------------
//--------------------------------------------------
EEsmdCal::~EEsmdCal() {/* noop */}


//-------------------------------------------------
//-------------------------------------------------
void EEsmdCal::init( ){
  printf("EEsmdCal() init , calibrate sector=%d\n",sectID);

  initAuxHisto();

  initTileHistoAdc('a',"inclusive ADC ",kBlack);
  initTileHistoAdc('d',"ADC, tag: UxV 2*thr",kBlack);
  initTileHistoAdc('e',"ADC, tag: mipT UxV 2*thr",kBlue);

  initTileHistoEne('f',"energy, tag: UxV 2*thr",kBlack);
  initTileHistoEne('g',"energy, tag: mipT UxV 2*thr",kBlue);
 
  initSmdHist('a',"inclusive ADC");
  initSmdHist('b',"ADC, tag: best MIP",kBlue);
  initSmdEneHist('e',"Energy (K)+(K+1)*tgh(eta): best MIP",kBlack);

  //.................... initialize MIP finding algo for SMD
  int i;
  for(i=0;i<MaxSmdPlains;i++) {
    smdHitPl[i].set(thrMipSmdE,emptyStripCount,i+'U');
  }

  printf("use thrMipSmdE/MeV=%.2f emptyStripCount=%d  twMipRelEne/high=%.2f/%.2f offCenter=%.2f maxStripAdc=%.1f thrMipPresAdc=%d\n", thrMipSmdE*1000.,emptyStripCount,twMipRelEneLow, twMipRelEneHigh,offCenter,maxStripAdc,thrMipPresAdc);
  assert(sectID>0 && sectID<=MaxSectors);

  //....................... initilize MIP energy in towers
  assert(twMipRelEneLow< twMipRelEneHigh);

  for(int i=0;i<MaxEtaBins;i++){
    float etaValue=(geoTw->getEtaMean(i));
    float tghEta=TMath::TanH(etaValue);
    twTghEta[i]=tghEta;
    towerMipE[i]= 1./(2.89*tghEta); // GeV EM
    presMipE[i]=0.0009/tghEta; //GeV MIP, 5mm*1.8mm
  }
  
  smdAvrMipE=0.0013; //GeV MIP;  7mm*1.8 MeV/cm
  
#if 0  //smdMap - histos for finding mapping
  int ij;
  for( ij=0;ij<12;ij++) {
    char t1[100];
    sprintf(t1,"hM%c",'a'+ij);
    hM[ij]=new TH1F(t1,t1,600,0.5,600.5);
    HList->Add( hM[ij]);
 }
#endif

}


//-------------------------------------------------
//-------------------------------------------------
void EEsmdCal::initRun(int runID){
  printf(" EEsmdCal::initRun(%d)\n",runID);

#if 1   //smdMap verification
  if(dbMapped>0)  {
    printf(" EEsmdCal::initRun(%d) N-th time, Ignore\n",runID);
    return; 
  }
#endif

  assert(dbMapped<0); // at the moment DB reloading is not implemented/tested,JB
  mapTileDb();
  dbMapped=runID;

  // now init all what relays on DB inofrmation
  addTwMipEbarsToHisto(kGreen,'g');

  addPresMipEbarsToHisto(kGreen,'P');
  addPresMipEbarsToHisto(kGreen,'Q');
  addPresMipEbarsToHisto(kGreen,'R');

  addSmdMipEbarsToHisto(kGreen,'U');
  addSmdMipEbarsToHisto(kGreen,'V');
  histoGains();
}

//--------------------------------------------------
//--------------------------------------------------
void EEsmdCal:: mapTileDb(){
  printf("EEsmdCal:: mapTileDb()\n");

  //....................  Tower like ....................
  char cT[mxTile]={'T','P','Q','R'};
  int iT=0;
  for(iT=0;iT<mxTile;iT++) {
    for(char iSub=0; iSub<MaxSubSec; iSub++){
      for(int iEta=0; iEta<MaxEtaBins; iEta++){
	int iPhi=iSect*MaxSubSec+iSub;
	dbT[iT][iEta][iPhi]=eeDb->getTile(sectID,iSub+'A',iEta+1,cT[iT]);
	//printf("%d %d %d '%s' \n",iT,iEta,iPhi,dbT[iT][iEta][iPhi]->name);
      }
    }
  }

  //................ SMD strips
  int iu;
  for(iu=0;iu<MaxSmdPlains;iu++) {
    int istr;
    for(istr=0;istr<MaxSmdStrips;istr++) {
      if(istr>=288) break; // ugly, but I gave up, JB
      dbS[iu][istr]=eeDb->getByStrip(sectID,iu+'U',istr+1);
    }
  }

}


//-------------------------------------------------
//-------------------------------------------------
void EEsmdCal::clear(){ // called for every event
    memset(tileAdc,0,sizeof(tileAdc));
    memset(tileEne,0,sizeof(tileEne));
    memset(tileThr,0,sizeof(tileThr));
    memset(smdAdc,0,sizeof(smdAdc));
    memset(smdEne,0,sizeof(smdEne));

    memset(killT,true,sizeof(killT));// default is dead

    int i;
    for(i=0;i<MaxSmdPlains;i++) {
    smdHitPl[i].clear();
   }

}
//-------------------------------------------------
//-------------------------------------------------
void EEsmdCal::findSectorMip( ){ 
  /*...................................
    ....                           ....
    ....   main physics analysis   ....
    ................................... */

  hA[9]->Fill(1);
  fillSmdHisto_a();  // inclusive SMD histos 

  for(char iSub=0; iSub<MaxSubSec; iSub++){
    for(int iEta=0; iEta<MaxEtaBins; iEta++){      
      int iPhi=iSect*MaxSubSec+iSub;
      fillOneTailHisto('a', iEta,iPhi);   // inclusive histos 
    }
  }
  
  // searching for MIP in SMD
  int ret=getUxVmip();

  if(ret>1)  hA[9]->Fill(2);// counts multiple MIP's per both planes
  if(ret==1) hA[9]->Fill(3);// counts multiple MIP's per both planes
  
  int nU,nV;
  EEsmdPlain *plU=smdHitPl+0;
  EEsmdPlain *plV=smdHitPl+1;


#if 0  //smdMap verification
  // verify mapping for a subset of strips
  // U-plan must have MIP
  if(plU->nMatch>0) {
    int ist;
    for(ist=0;ist<12;ist++)
    scanSpike(smdAdc[1][30+ist-1],hM[ist]);
  }
#endif

  // calibrate P,Q,R with UxV
  for(nU=0;nU<plU->nMatch;nU++){
    for(nV=0;nV<plV->nMatch;nV++){
      hA[9]->Fill(4);// any UxV pair
      calibAllwithMip(plU->iStrip[nU],plV->iStrip[nV]);
    }
  }

  return;
}


//-------------------------------------------------
//-------------------------------------------------
void EEsmdCal::calibAllwithMip(int iStrU, int iStrV){

  // find MIP for a UxV pair of strips

  //printf("\n jj iSect=%d,iStrU=%d,iStrV=%d\n",iSect,iStrU,iStrV);
  TVector3 r=geoSmd->getIntersection (iSect,iStrU,iStrV);
 
  //printf(" UxV = %f %f %f\n", r.x(),r.y(),r.z());
 
  int     iSecX, iSubX, iEtaX;
  Float_t dphi, deta;
  int ret=geoTw->getTower(r, iSecX, iSubX, iEtaX,dphi, deta);
  //printf("ret=%d, isecX=%d isubX=%d, ietaX=%d dphi=%f, deta=%f\n",ret,iSecX, iSubX, iEtaX, dphi, deta);

  if(ret==0 || iSecX!=iSect) return;  
  //................ UxV is in a tower boundary within selected sector

  hA[9]->Fill(5);
  hA[10]->Fill(iStrU+1);
  hA[11]->Fill(iStrV+1);
  
  // select central MIP in tower
  int inCenter=(fabs(dphi)<offCenter && fabs(deta)<offCenter);
  ((TH2F*) hA[21])->Fill( r.x(),r.y());

  if(!inCenter) return; 
  //...................... assure central UxV hit in a tower

  hA[9]->Fill(6);
  int iPhiX=iSect*MaxSubSec+iSubX;
  hA[24]->Fill(iPhiX+iEtaX*MaxPhiBins);

  //................ auxuiliary variables 
  // ped corrected adc
  float adcT=tileAdc[kT][iEtaX][iPhiX];
  float adcP=tileAdc[kP][iEtaX][iPhiX];
  float adcQ=tileAdc[kQ][iEtaX][iPhiX];
  float adcR=tileAdc[kR][iEtaX][iPhiX];

  // calibrated energy
  float eneT=tileEne[kT][iEtaX][iPhiX];  // GeV
  float eneP=tileEne[kP][iEtaX][iPhiX]*1000; // MeV
  float eneQ=tileEne[kQ][iEtaX][iPhiX]*1000; // MeV
  float eneR=tileEne[kR][iEtaX][iPhiX]*1000; // MeV

  // logical conditions:
  // to recover few tiles mark all faild towers as with ADC>thres
  bool thrP=false,  thrQ=false,  thrR=false, thrDum=false;
  bool *thr_p[mxTile]={&thrDum,&thrP,&thrQ,&thrR};

  int iT;
  for (iT=kP; iT<=kR;iT++) {// loop over pre/post
    bool thr=tileThr[iT][iEtaX][iPhiX] ;   // .... ADC must be above ped
    thr= thr || killT[iT][iEtaX][iPhiX];   //.... account for masked pixels
#if 0 //....  after pre/post calibration is avaliable
    float preMipRelEneLow=0.6, preMipRelEneHigh=3.0; // tmp
    float ene2Mip=tileEne[iT][iEtaX][iPhiX]/presMipE[iEtaX];
    thr= thr &&   ene2Mip>preMipRelEneLow &&  ene2Mip<preMipRelEneHigh; 
#endif
    (*thr_p[iT])=thr;    // .... record final answer
  }

  //printf("bb1 (adc)  P=%.1f Q=%.1f R=%.1f iphiX=%d\n",tileAdc[1][iEtaX][iPhiX],tileAdc[2][iEtaX][iPhiX],tileAdc[3][iEtaX][iPhiX],iPhiX);
  //printf("bb2 (bool) thr P=%d Q=%d R=%d\n",thrP,thrQ,thrR);


  //printf("bb3 (bool) thr P=%d Q=%d R=%d\n",thrP,thrQ,thrR);


  // check MIP upper/lower limits
  float RelTwEne=tileEne[kT][iEtaX][iPhiX]/towerMipE[iEtaX];
  bool mipT=  RelTwEne>twMipRelEneLow &&  RelTwEne<twMipRelEneHigh;
  mipT=  mipT || killT[kT][iEtaX][iPhiX]; // recover dead tower
  // printf("iphi=%d ieta=%d Tene=%f mipEne=%f mipT=%d adcT=%.1f\n",iPhiX,iEtaX,tileEne[kT][iEtaX][iPhiX],towerMipE[iEtaX],mipT,tileAdc[kT][iEtaX][iPhiX]);


  if(thrR) hA[9]->Fill(7);

  if(mipT )hA[9]->Fill(8);

  //...........  MIP  ADC  spectra .........................
  int iCut='d'-'a';
  if(         thrP && thrQ && thrR ) hT[iCut][kT][iEtaX][iPhiX]->Fill(adcT);
  if( mipT &&         thrQ && thrR ) hT[iCut][kP][iEtaX][iPhiX]->Fill(adcP);
  if( mipT && thrP         && thrR ) hT[iCut][kQ][iEtaX][iPhiX]->Fill(adcQ);
  if( mipT && thrP && thrQ         ) hT[iCut][kR][iEtaX][iPhiX]->Fill(adcR);


  iCut='e'-'a';
  if( mipT && thrP && thrQ && thrR ){
    hT[iCut][kT][iEtaX][iPhiX]->Fill(adcT);
    hT[iCut][kP][iEtaX][iPhiX]->Fill(adcP);
    hT[iCut][kQ][iEtaX][iPhiX]->Fill(adcQ);
    hT[iCut][kR][iEtaX][iPhiX]->Fill(adcR);
  }
  
  // .................  calibrate (energy) MIP spectra ...............
  iCut='f'-'a';
  if(         thrP && thrQ && thrR ) hT[iCut][kT][iEtaX][iPhiX]->Fill(eneT);
  if( mipT &&         thrQ && thrR ) hT[iCut][kP][iEtaX][iPhiX]->Fill(eneP);
  if( mipT && thrP         && thrR ) hT[iCut][kQ][iEtaX][iPhiX]->Fill(eneQ);
  if( mipT && thrP && thrQ         ) hT[iCut][kR][iEtaX][iPhiX]->Fill(eneR);

  iCut='g'-'a';
  if( mipT && thrP && thrQ && thrR ){
    hT[iCut][kT][iEtaX][iPhiX]->Fill(eneT);
    hT[iCut][kP][iEtaX][iPhiX]->Fill(eneP);
    hT[iCut][kQ][iEtaX][iPhiX]->Fill(eneQ);
    hT[iCut][kR][iEtaX][iPhiX]->Fill(eneR);
    ((TH2F*) hA[22])->Fill( r.x(),r.y());
  }

  //..................... calibration of SMD strips
  if( mipT && thrP && thrQ && thrR ) {
 
    int iuv,i2;
    const int mx=2; // # of strips from given plain
    int iStr[MaxSmdPlains];
    iStr[0]=iStrU; // working pointers
    iStr[1]=iStrV;
    float eUV=0;// sum from both plains
    for(iuv=0;iuv<MaxSmdPlains;iuv++) {
      float e12=0;
      for(i2=0;i2<mx;i2++) {
	int istrip=iStr[iuv]+i2;
	float adc=smdAdc[iuv][istrip];
	float ene=smdEne[iuv][istrip]*twTghEta[iEtaX]*1000; // MeV, at normal angle 	
	if(adc<2) continue;// drop pedestal, tmp
	e12+=ene; // sum pairs
	// re-calibratiop  of strips
	hSs['b'-'a'][iuv][istrip]->Fill(adc);
      }// end of loop over one plain
      eUV+=e12;
      // SMD energy for pairs
      int istrip1=iStr[iuv];
      ((TH2F*) hA[20])->Fill(istrip1+1,e12);
      hSs['e'-'a'][iuv][istrip1]->Fill(e12);      
      hA[14+iuv]->Fill(istrip1+1); 

    }// end of loop over UV plains
    ((TH2F*) hA[23])->Fill(iEtaX+1,eUV); 
  }
  
}

#if 0	
	// SMD light attenuation for pair of strips
	StructEEmcStrip* bar=geoSmd->getStripPtr(istrip,iuv,iSect);
	TVector3 r1=bar->end1;
	TVector3 r2=bar->end2;
	float dx,dy;
	if(r1.Eta()<r2.Eta()) {
	  dx=r1.x()-r.x(); 
	  dy=r1.y()-r.y();
	} else {
	  dx=r2.x()-r.x();
	  dy=r2.y()-r.y();
	} 
	float dist=sqrt(dx*dx+dy*dy);
	int iG=istrip/stripGang;
	assert(iG>=0 && iG <MaxAt);
	hSc[iuv][iSubX][iG]->Fill(ene); 
	hSeta[iuv][iSubX][iG]->Fill(r.Eta());
	hSdist[iuv][iSubX][iG]->Fill(dist);
#endif

//-------------------------------------------------
//-------------------------------------------------
int EEsmdCal::getUxVmip(){
  //  printf("\n\n EEsmdCal::getUxVmip() eve=%d\n",nInpEve);

  int iuv;
  for(iuv=0;iuv<MaxSmdPlains;iuv++) {
    EEsmdPlain *pl=smdHitPl+iuv;
    pl->scanAdc(smdEne[iuv], thrMipSmdE);// use if SMD gains are  known
    //pl->scanAdc(smdAdc[iuv], 15); // use if SMD gains are NOT known
    pl->findMipPattern();
    hA[12+iuv]->Fill(pl->nMatch);
    // pl->print(1);
    // printf("iuv=%d  nM=%d\n",iuv, pl->nMatch);  
  }
  
  int ret=smdHitPl[0].nMatch*smdHitPl[1].nMatch;

  //  printf("UxV ret=%d\n",ret);
  return ret;
}


#include <TLine.h> // just for drawing
#include <TText.h> // just for drawing
#include <TCanvas.h> // just for drawing

//-------------------------------------------------
//-------------------------------------------------
void EEsmdCal::finish(int k){
  printf("\n  EEsmdCal::finish(sec=%d) nInpEve=%d \n",sectID,nInpEve);

  if(k<=0) return;
  // some test drawing:
  printf("drawing ...\n");
  //  TFile* f=new TFile("outSec5b/mip05b-8zAB.hist.root");

  int iuv=1;
  TString tt="sec"; tt+=sectID;
  TCanvas *c=new TCanvas(tt,tt,400,400);
  c->Divide(1,2);
  //  TH2F * h=(TH2F *)f->Get("xy05ct");
  TH2F * h=(TH2F *)hA[22];

  for(iuv=0;iuv<2;iuv++) {
    c->cd(iuv+1);
    h->Draw("colz");
    char uv='U'+iuv;
    int i;
    for(i=0;i<13;i++) {
      int strip=i*20+21;   
      StructEEmcStrip* bar=geoSmd->getStripPtr(strip-1,iuv,iSect);
      // printf("len=%f\n",bar->lenght);
      
      TVector3 r1=bar->end1;
      TVector3 r2=bar->end2;
      
      TLine *ln=new TLine(r1.x(),r1.y(),r2.x(),r2.y());
      ln->Draw();
      ln->SetLineColor(kRed);

      if(strip==161 ||strip==201 ||strip==241 ) continue;
      TString strT=uv; strT+=strip;
      TText *tx;
      if(uv=='U') 
	tx=new TText(r2.x()+1,r2.y()-5,strT);
      else {
	if(strip>140)
	  tx=new TText(r2.x()-10,r2.y()-8,strT);
	else
	  tx=new TText(r2.x()-20,r2.y(),strT);
      }
      tx->Draw();
    }
    char sub;
    for(sub='A'; sub<='E';sub++) {
      int i=sub-'A';
      float x=45 -i*8;
      float y=-55 -i*5;
      TString strT=sub;
      TText *tx=new TText(x,y,strT);
      tx->Draw();
    }

  }
  

}


#if 0 //smdMap verification
//-------------------------------------------------
//-------------------------------------------------
void EEsmdCal:: scanSpike(float adc1, TH1F *h){
  float adcTh=20;
  if(adc1<adcTh) return;
  int istrip;
  for(istrip=0+3;istrip<MaxSmdStrips-3;istrip++) {
    int iuv;
    for(iuv=0;iuv<2;iuv++) {
#if 0
      if(smdAdc[iuv][istrip-3]>=adcTh) continue;
      if(smdAdc[iuv][istrip-2]>=adcTh) continue;
      if(smdAdc[iuv][istrip+2]>=adcTh) continue;
      if(smdAdc[iuv][istrip+3]>=adcTh) continue;

      if(smdAdc[iuv][istrip-1]>=adcTh) continue;
      if(smdAdc[iuv][istrip+1]>=adcTh) continue;
#endif
      if(smdAdc[iuv][istrip  ]< adcTh) continue;
      h->Fill(1+300*iuv+istrip);
    }
  }
}
#endif

