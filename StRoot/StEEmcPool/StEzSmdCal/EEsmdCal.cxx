// $Id: EEsmdCal.cxx,v 1.4 2004/06/29 16:37:41 balewski Exp $
 
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
  twMipEdev=-3; presMipElow=-4,  presMipElow=-5;

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

  // Here you can overwrite DB informtaion by reading it from ASCII file
#ifdef StRootFREE
  eeDb->changeGains("setG1.dat");
  eeDb->changeMask("setM1.dat");
#else
  assert(strstr("not implemented", "ix it"));
#endif

  // now init all what relays on DB inofrmation
  addTwMipEbarsToHisto(kGreen);
  addPresMipEbarsToHisto(kGreen,'P');
  addPresMipEbarsToHisto(kGreen,'Q');
  addPresMipEbarsToHisto(kGreen,'R');

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
  initTileHist('e',"tag: mipT UxV 2*thr",kBlue);

  initSmdHist('a',"inclusive");
  initSmdHist('b',"tag: best MIP",kBlue);

  //.................... initialize MIP finding algo for SMD
  int i;
  for(i=0;i<MaxSmdPlains;i++) {
    smdHitPl[i].set(thrMipSmdE,emptyStripCount,i+'U');
  }

  printf("use thrMipSmdE=%f emptyStripCount=%d  twMipEdev=%f presMipElow/high=%f/%f\n", thrMipSmdE,emptyStripCount, twMipEdev,presMipElow,presMipEhigh);
  assert(sectID>0 && sectID<=MaxSectors);

  //....................... initilize energy cuts for MIPs in towers
  assert(twMipEdev>0 && twMipEdev<1.);
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
      calibPQRwithMip(plU->iStrip[nU],plV->iStrip[nV]);
    }
  }

  // calibrate V with U
  for(nU=0;nU<plU->nMatch;nU++){
    calibSMDwithMip(kU,plU->iStrip[nU]);
  }



}


//-------------------------------------------------
//-------------------------------------------------
void EEsmdCal::calibSMDwithMip(int iU, int iStrU){
  int iCut='b'-'a';
  int stripStep=7;
  int iV=1-iU; // calibrated plane

  int nT=mapSmd-> getNTowers(iSect,iU,iStrU);
  //printf("nT=%d for iStrU=%d\n",nT, iStrU);
  int iSub, iEta;
  int i;
  for(i=0;i<nT;i++) {
    mapSmd-> getTower(iSect,iU,iStrU,i,iSub,iEta);
    // tmp use B & C only
    //if(iSub!=3 && iSub!=4) continue;
    int iPhi=iSect*MaxSubSec+iSub;

    int iMin,iMax;
    mapSmd-> getRangeTw2Smd(iSect,iSub,iEta,iU,iMin,iMax);
    if( iStrU< iMin+stripStep || iStrU>iMax-stripStep) continue;
    // printf(" found TW %c-plane isub/ieta=%d/%d istrip range %d - %d\n",'U'+iU,iEta,iSub,iMin,iMax);

   // note, current calibration is ch/MIP for pre/post
    int j;
    int nMip=0;
    for(j=kP; j<=kR ; j++) {
      nMip+=tileEne[j][iEta][iPhi]>presMipElow && tileEne[j][iEta][iPhi]<presMipEhigh;
      //isMip*= tileThr[j][iEta][iPhi];
    } 
    if(nMip<3) continue;

    float twEne=tileEne[kT][iEta][iPhi];
    //printf("TW ener=%f goal=%f diff%f\n", twEne,towerMipE[iEta], twEne-towerMipE[iEta]);

    bool mipT=fabs( twEne-towerMipE[iEta]) <twMipEdev;
    if(!mipT) continue;
    // printf("i=%d iSub=%d iEta%d\n",i,iSub,iEta);
    

    mapSmd-> getRangeTw2Smd(iSect,iSub,iEta,iV,iMin,iMax);
    // printf(" found %c-plane istrip range %d - %d\n",'U'+iV,iMin,iMax);
    
    for(j=iMin+stripStep; j<=iMax-stripStep; j++) {
      hSs[iCut][iV][j]->Fill(smdAdc[iV][j]);
      // QA of MIP's
      TVector3 r;
      if(iV==kV)
	r=geoSmd->getIntersection (iSect,iStrU,j);
      else // reverse
	r=geoSmd->getIntersection (iSect,j,iStrU);
      ((TH2F*) hA[24])->Fill( r.x(),r.y());
      hA[14+iV]->Fill(j+1);
    }

  }

}




//-------------------------------------------------
//-------------------------------------------------
void EEsmdCal::calibPQRwithMip(int iStrU, int iStrV){

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
  // logical conditions:
  // to recover few tiles mark all faild towers as with ADC>thres
  bool thrP= tileThr[kP][iEtaX][iPhiX] || dbT[kP][iEtaX][iPhiX]->fail;
  bool thrQ= tileThr[kQ][iEtaX][iPhiX] || dbT[kQ][iEtaX][iPhiX]->fail;
  bool thrR= tileThr[kR][iEtaX][iPhiX] || dbT[kR][iEtaX][iPhiX]->fail;

  // check MIP upper/lower limits
  float twEne=tileEne[kT][iEtaX][iPhiX];
  bool mipT=fabs( twEne-towerMipE[iEtaX]) <twMipEdev;
  mipT=  mipT ||  dbT[kT][iEtaX][iPhiX]->fail; // recover dead tower

 
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


