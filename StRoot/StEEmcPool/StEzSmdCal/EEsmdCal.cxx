// $Id: EEsmdCal.cxx,v 1.7 2004/07/10 18:40:48 balewski Exp $
 
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
#include "StEEmcDbMaker/EEmcDbItem.h"

#ifdef StRootFREE
    #include "EEmcDb/EEmcDb.h"
#else
  #include "StEEmcDbMaker/StEEmcDbMaker.h"
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
  memset(hSp,0,sizeof(hSp));

  dbMapped=-1;
  memset(dbT,0,sizeof(dbT));

  // initialization
  smdHitPl=new EEsmdPlain [MaxSmdPlains];
  geoTw=new EEmcGeomSimple;
  geoSmd= EEmcSmdGeom::instance();
  mapSmd= EEmcSmdMap::instance();

  thrMipSmdE=-1; emptyStripCount=-2; 
  twMipRelEneLow=-3; twMipRelEneHigh=-4;

  offCenter=0.7;

  printf("EEsmdCal() constructed\n");
}

//--------------------------------------------------
//--------------------------------------------------
EEsmdCal::~EEsmdCal() {/* noop */}

//-------------------------------------------------
//-------------------------------------------------
void EEsmdCal::initRun(int runID){
  printf(" EEsmdCal::initRun(%d)\n",runID);

  assert(dbMapped<0); // at the moment DB reloading is not implemented/tested,JB
  mapTileDb();
  dbMapped=runID;

  // now init all what relays on DB inofrmation
  addTwMipEbarsToHisto(kGreen,'e');
  addPresMipEbarsToHisto(kGreen,'P');
  addPresMipEbarsToHisto(kGreen,'Q');
  addPresMipEbarsToHisto(kGreen,'R');

}

//-------------------------------------------------
//-------------------------------------------------
void EEsmdCal::init( ){
  printf("EEsmdCal() init , calibrate sector=%d\n",sectID);

  initAuxHisto();

  initTileHistoAdc('a',"inclusive ADC ",kBlack);
  initTileHistoAdc('b',"ADC, tag: thrR",kRed);
  initTileHistoAdc('c',"ADC, tag: thrR UxV",kBlue);
  initTileHistoAdc('d',"ADC, tag: UxV 2*thr",kBlack);
  initTileHistoAdc('e',"ADC, tag: mipT UxV 2*thr",kBlue);

  initTileHistoEne('f',"energy, tag: UxV 2*thr",kBlack);
  initTileHistoEne('g',"energy, tag: mipT UxV 2*thr",kBlue);

  initSmdHist('a',"inclusive ADC");
  initSmdHist('b',"ADC, tag: best MIP",kBlue);
  initSmdAttenHist(); 

  //.................... initialize MIP finding algo for SMD
  int i;
  for(i=0;i<MaxSmdPlains;i++) {
    smdHitPl[i].set(thrMipSmdE,emptyStripCount,i+'U');
  }

  printf("use thrMipSmdE=%.2f emptyStripCount=%d  twMipRelEne/high=%.2f/%.2f\npresMipElow/high=%.2f/%.2f\n", thrMipSmdE,emptyStripCount,twMipRelEneLow, twMipRelEneHigh,presMipElow,presMipEhigh);
  assert(sectID>0 && sectID<=MaxSectors);

  //....................... initilize energy cuts for MIPs in towers
  assert(twMipRelEneLow< twMipRelEneHigh);
  for(int i=0;i<MaxEtaBins;i++){
    float etaValue=(geoTw->getEtaMean(i));
    float mean=1./(2.89*TMath::TanH(etaValue));
    towerMipE[i]= mean;
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
      if(tileThr[kR][iEta][iPhi]<=0) continue;
      fillOneTailHisto('b', iEta,iPhi);   // tagged histos 
    }
  }
  
  // searching for MIP in SMD
  int ret=getUxVmip();

  if(ret>1)  hA[9]->Fill(2);// counts multiple MIP's per both planes
  if(ret==1) hA[9]->Fill(3);// counts multiple MIP's per both planes
  
  int nU,nV;
  EEsmdPlain *plU=smdHitPl+0;
  EEsmdPlain *plV=smdHitPl+1;

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

  TVector3 r=geoSmd->getIntersection (iSect,iStrU,iStrV);
  // printf(" UxV = %f %f %f\n", r.x(),r.y(),r.z());

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

  //................ auxuiliary variables 
  // logical conditions:
  // to recover few tiles mark all faild towers as with ADC>thres
  bool thrP= tileThr[kP][iEtaX][iPhiX] || dbT[kP][iEtaX][iPhiX]->fail;
  bool thrQ= tileThr[kQ][iEtaX][iPhiX] || dbT[kQ][iEtaX][iPhiX]->fail;
  bool thrR= tileThr[kR][iEtaX][iPhiX] || dbT[kR][iEtaX][iPhiX]->fail;

  // check MIP upper/lower limits
  float RelTwEne=tileEne[kT][iEtaX][iPhiX]/towerMipE[iEtaX];
  bool mipT=  RelTwEne>twMipRelEneLow &&  RelTwEne<twMipRelEneHigh;
  mipT=  mipT ||  dbT[kT][iEtaX][iPhiX]->fail; // recover dead tower
 
  // ped corrected adc
  float adcT=tileAdc[kT][iEtaX][iPhiX];
  float adcP=tileAdc[kP][iEtaX][iPhiX];
  float adcQ=tileAdc[kQ][iEtaX][iPhiX];
  float adcR=tileAdc[kR][iEtaX][iPhiX];

  // calibrated energy
  float eneT=tileEne[kT][iEtaX][iPhiX];
  float eneP=tileEne[kP][iEtaX][iPhiX];
  float eneQ=tileEne[kQ][iEtaX][iPhiX];
  float eneR=tileEne[kR][iEtaX][iPhiX];

  if(thrR){
    hA[9]->Fill(7);
    fillOneTailHisto('c', iEtaX,iPhiX);
  }

  if(mipT )hA[9]->Fill(8);

  int iCut='d'-'a';
  if( thrP && thrQ && thrR ) hT[iCut][kT][iEtaX][iPhiX]->Fill(adcT);
  if(         thrQ && thrR ) hT[iCut][kP][iEtaX][iPhiX]->Fill(adcP);
  if( thrP         && thrR ) hT[iCut][kQ][iEtaX][iPhiX]->Fill(adcQ);
  if( thrP && thrQ         ) hT[iCut][kR][iEtaX][iPhiX]->Fill(adcR);


  iCut='e'-'a';
  if( mipT && thrP && thrQ && thrR ) hT[iCut][kT][iEtaX][iPhiX]->Fill(adcT);
  if( mipT &&         thrQ && thrR ) hT[iCut][kP][iEtaX][iPhiX]->Fill(adcP);
  if( mipT && thrP         && thrR ) hT[iCut][kQ][iEtaX][iPhiX]->Fill(adcQ);
  if( mipT && thrP && thrQ         ) hT[iCut][kR][iEtaX][iPhiX]->Fill(adcR);
 
  // calibrated MIP spectra 
  iCut='f'-'a';
  if( thrP && thrQ && thrR ) hT[iCut][kT][iEtaX][iPhiX]->Fill(eneT);
  if(         thrQ && thrR ) hT[iCut][kP][iEtaX][iPhiX]->Fill(eneP);
  if( thrP         && thrR ) hT[iCut][kQ][iEtaX][iPhiX]->Fill(eneQ);
  if( thrP && thrQ         ) hT[iCut][kR][iEtaX][iPhiX]->Fill(eneR);

  iCut='g'-'a';
  if( mipT && thrP && thrQ && thrR ) hT[iCut][kT][iEtaX][iPhiX]->Fill(eneT);
  if( mipT &&         thrQ && thrR ) hT[iCut][kP][iEtaX][iPhiX]->Fill(eneP);
  if( mipT && thrP         && thrR ) hT[iCut][kQ][iEtaX][iPhiX]->Fill(eneQ);
  if( mipT && thrP && thrQ         ) hT[iCut][kR][iEtaX][iPhiX]->Fill(eneR);
  

  // SMD energy for pairs
  iCut='b'-'a';
  float eU=smdEne[kU][iStrU]+smdEne[kU][iStrU+1];
  float eV=smdEne[kV][iStrV]+smdEne[kV][iStrV+1];
  hSp[iCut][kU][iStrU]->Fill(eU);
  hSp[iCut][kV][iStrV]->Fill(eV);
  
  ((TH2F*) hA[22])->Fill( r.x(),r.y());
  ((TH2F*) hA[20])->Fill(iStrU+1,eU);
  ((TH2F*) hA[20])->Fill(iStrV+1,eV);
  ((TH2F*) hA[23])->Fill(iEtaX+1,eU+eV); 


  // calibration of SMD strips
  if( mipT && thrP && thrQ && thrR ) {
    int iuv,i2;
    const int mx=2; // # of strips from given plain
    int iStr[MaxSmdPlains];
    iStr[0]=iStrU;
    iStr[1]=iStrV;
    for(iuv=0;iuv<MaxSmdPlains;iuv++)
      for(i2=0;i2<mx;i2++) {
	int istrip=iStr[iuv]+i2;

	if(smdAdc[iuv][istrip]<3) continue;// drop pedestal, tmp
	// re-calibration of strips
	int iCut='b'-'a';
	hSs[iCut][iuv][istrip]->Fill(smdAdc[iuv][istrip]);
	if(smdAdc[iuv][istrip]>3) hA[14+iuv]->Fill(istrip+1); // only above ped

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
	hSc[iuv][iSubX][iG]->Fill(smdEne[iuv][istrip]); 
	hSeta[iuv][iSubX][iG]->Fill(r.Eta());
	hSdist[iuv][iSubX][iG]->Fill(dist);
      } 
  }
  
}


//-------------------------------------------------
//-------------------------------------------------
int EEsmdCal::getUxVmip(){
  //  printf("\n\n EEsmdCal::getUxVmip() eve=%d\n",nInpEve);

  int iuv;
  for(iuv=0;iuv<MaxSmdPlains;iuv++) {
    EEsmdPlain *pl=smdHitPl+iuv;
    pl->scanAdc(smdEne[iuv], thrMipSmdE);
    pl->findMipPattern();
    hA[12+iuv]->Fill(pl->nMatch);
    //    pl->print(1);
    // printf("iuv=%d  nM=%d\n",iuv, pl->nMatch);  
  }
  
  int ret=smdHitPl[0].nMatch*smdHitPl[1].nMatch;

  //printf("ret=%d\n",ret);
  return ret;
}


#include <TLine.h> // just for drawing
#include <TText.h> // just for drawing
#include <TCanvas.h> // just for drawing

//-------------------------------------------------
//-------------------------------------------------
void EEsmdCal::finish(int k){
  printf("\n  EEsmdCal::finish(sec=%d) nInpEve=%d \n",sectID,nInpEve);

  if(k<0) return;
  // some test drawing:
  printf("drawing ...\n");
  //  TFile* f=new TFile("outSec5b/mip05b-8zAB.hist.root");

  int iuv=1;
  TString tt="sec"; tt+=sectID;
  TCanvas *c=new TCanvas(tt,tt,600,600);
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


