// $Id: EEsmdCal.cxx,v 1.3 2004/06/22 23:31:10 balewski Exp $
 
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
  printf("EEsmdCal() constructed\n");
  thrMipSmdE=-1; emptyStripCount=-2; 

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
  addTwMipEbarsToHisto(kGreen);

  // Here you can overwrite DB informtaion by reading it from ASCII file
  //.........

}

//-------------------------------------------------
//-------------------------------------------------
void EEsmdCal::init( ){
  printf("EEsmdCal() init , calibrate sector=%d\n",sectID);

  initAuxHisto();

  initTileHist('a',"inclusive  ",kBlack);
  initTileHist('b',"tag: thrR",kRed);
  initTileHist('c',"tag: thrR UxV",kBlue);
  initTileHist('d',"tag: UxV 2xthr",kBlack);
  initTileHist('e',"tag: mipT UxV 2xthr",kBlue);

  initSmdHist('a',"inclusive");

  //.................... initialize MIP finding algo for SMD
  int i;
  for(i=0;i<MaxSmdPlains;i++) {
    smdHitPl[i].set(thrMipSmdE,emptyStripCount,i+'U');
  }
  printf("use thrMipSmdE=%f emptyStripCount=%d  twMipEdev=%f\n", thrMipSmdE,emptyStripCount, twMipEdev);
  assert(sectID>0 && sectID<=MaxSectors);

  //....................... initilize energy cuts for MIPs in towers
  assert(twMipEdev>0 && twMipEdev<1.);
  for(int i=0;i<MaxEtaBins;i++){
    float etaValue=(geoTw->getEtaMean(i));
    float mean=1./(2.89*TMath::TanH(etaValue));
    towerMipElow[i]= (1-twMipEdev)*mean;
    towerMipEhigh[i]=(1+twMipEdev)*mean;
    //printf("%f %f %f\n",etaValue,tileMin[i],tileMax[i]);
  }
}


//-------------------------------------------------
//-------------------------------------------------
void EEsmdCal::clear(){ // called for every event
    memset(tileAdc,0,sizeof(tileAdc));
    memset(tileEne,0,sizeof(tileEne));
    memset(tileThr,0,sizeof(tileThr));
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
  
  int kU,kV;
  EEsmdPlain *plU=smdHitPl+0;
  EEsmdPlain *plV=smdHitPl+1;
  for(kU=0;kU<plU->nMatch;kU++){
    for(kV=0;kV<plV->nMatch;kV++){
      hA[9]->Fill(4);// any UxV pair
      findOneMip(plU->iStrip[kU],plV->iStrip[kV]);
    }
  }
}


//-------------------------------------------------
//-------------------------------------------------
void EEsmdCal::findOneMip(int iStrU, int iStrV){
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
  int inCenter=(fabs(dphi)<0.7 && fabs(deta)<0.7);
  ((TH2F*) hA[21])->Fill( r.x(),r.y());

  if(!inCenter) return; 
  //...................... assure central UxV hit in a tower

  hA[9]->Fill(6);
  int iPhiX=iSect*MaxSubSec+iSubX;

  //................ auxuiliary variables 
  // logical
  bool thrP= tileThr[kP][iEtaX][iPhiX];
  bool thrQ= tileThr[kQ][iEtaX][iPhiX];
  bool thrR= tileThr[kR][iEtaX][iPhiX];
  float twEne=tileEne[kT][iEtaX][iPhiX];
  bool mipT= (twEne >towerMipElow[iEtaX] && twEne < towerMipEhigh[iEtaX]);
  // ped corrected adc
  float adcT=tileAdc[kT][iEtaX][iPhiX];
  float adcP=tileAdc[kP][iEtaX][iPhiX];
  float adcQ=tileAdc[kQ][iEtaX][iPhiX];
  float adcR=tileAdc[kR][iEtaX][iPhiX];

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
 
 
  float eU=smdEne[iSect][0][iStrU]+smdEne[iSect][0][iStrU+1];
  float eV=smdEne[iSect][1][iStrV]+smdEne[iSect][1][iStrV+1];
  
  ((TH2F*) hA[22])->Fill( r.x(),r.y());
  ((TH2F*) hA[20])->Fill(iStrU+1,eU);
  ((TH2F*) hA[20])->Fill(iStrV+1,eV);
  ((TH2F*) hA[23])->Fill(iEtaX+1,eU+eV); 
  
}
//-------------------------------------------------
//-------------------------------------------------
int EEsmdCal::getUxVmip(){
  //  printf("\n\n EEsmdCal::getUxVmip() eve=%d\n",nInpEve);

  int iuv;
  for(iuv=0;iuv<MaxSmdPlains;iuv++) {
    EEsmdPlain *pl=smdHitPl+iuv;
    pl->scanAdc(smdEne[iSect][iuv], thrMipSmdE);
    pl->findMipPattern();
    hA[12+iuv]->Fill(pl->nMatch);
    // pl->print(1);
    //printf("iuv=%d  nM=%d\n",iuv, pl->nMatch);  
  }
  
  int ret=smdHitPl[0].nMatch*smdHitPl[1].nMatch;

  //printf("ret=%d\n",ret);
  return ret;
}



//-------------------------------------------------
//-------------------------------------------------
void EEsmdCal::finish(){
  printf("\n  EEsmdCal::finish() nInpEve=%d\n",nInpEve);
}


