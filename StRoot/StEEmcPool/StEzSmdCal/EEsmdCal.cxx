// $Id: EEsmdCal.cxx,v 1.2 2004/06/15 20:03:26 balewski Exp $
 
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

  // initialization
  smdHitPl=new EEsmdPlain [MaxSmdPlains];
  geoTw=new EEmcGeomSimple;
  geoSmd= EEmcSmdGeom::instance();
  printf("EEsmdCal() constructed\n");
  thrMipSmdE=-1; emptyStripCount=-2; iTagLayer=-3;

}

//--------------------------------------------------
//--------------------------------------------------
EEsmdCal::~EEsmdCal() {/* noop */}


//-------------------------------------------------
//-------------------------------------------------
void EEsmdCal::init( ){
  printf("EEsmdCal() init , calibrate sector=%d\n",sectID);
  char cTag='T';
  if(iTagLayer>0) cTag='P'+iTagLayer-1;
  char tit[100];

  initAuxHisto();

  initTileHist('a',"inclusive  ");
  sprintf(tit,"%c-tagged",cTag); 
  initTileHist('b',tit);
  sprintf(tit,"%c-tagged +UxV",cTag); 
  initTileHist('c',tit);
  initSmdHist('a',"inclusive  ");
 
  printf("use thrMipSmdE=%f emptyStripCount=%d, %c-tagged\n", thrMipSmdE,emptyStripCount,cTag);
  assert(sectID>0 && sectID<=MaxSectors);
  assert(emptyStripCount>1);
  assert(iTagLayer>0 && iTagLayer<kTile);

  int i;
  for(i=0;i<MaxSmdPlains;i++) {
    smdHitPl[i].set(thrMipSmdE,emptyStripCount,i+'U');
  }

}


//-------------------------------------------------
//-------------------------------------------------
void EEsmdCal::clear(){ // called for every event
    memset(tileAdc,0,sizeof(tileAdc));
    memset(tileTag,0,sizeof(tileTag));
    memset(smdE,0,sizeof(smdE));
    int i;
    for(i=0;i<MaxSmdPlains;i++) {
    smdHitPl[i].clear();
   }

}

//-------------------------------------------------
//-------------------------------------------------
void EEsmdCal::findSectorMip( ){  // main physics analysis
  hA[9]->Fill(1);

  for(char iSub=0; iSub<MaxSubSec; iSub++){
    for(int iEta=0; iEta<MaxEtaBins; iEta++){      
      int iPhi=iSect*MaxSubSec+iSub;
      fillOneTailHisto('a', iEta,iPhi);   // inclusive histos 
      if(tileTag[iTagLayer ][iEta][iPhi]<=0) continue; 
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
  // find MIP for a pair of strips

  // only one UxV match is processed further

  TVector3 r=geoSmd->getIntersection (iSect,iStrU,iStrV);

  // printf(" UxV = %f %f %f\n", r.x(),r.y(),r.z());
  int     iSecX, iSubX, iEtaX;
  Float_t dphi, deta;
  int ret=geoTw->getTower(r, iSecX, iSubX, iEtaX,dphi, deta);
  // printf("ret=%d, isecX=%d isubX=%d, ietaX=%d dphi=%f, deta=%f\n",ret,isecX, isubX, ietaX, dphi, deta);

  if(ret==0 || iSecX!=iSect) return;  
  // UxV is in a tower boundary within selected sector

  hA[9]->Fill(5);
  hA[10]->Fill(iStrU+1);
  hA[11]->Fill(iStrV+1);
  
  // select central MIP in tower
  int inCenter=(fabs(dphi)<0.7 && fabs(deta)<0.7);
  ((TH2F*) hA[21])->Fill( r.x(),r.y());

  if(!inCenter) return; 
  // assure central UxV hit
  hA[9]->Fill(6);
  int iPhiX=iSect*MaxSubSec+iSubX;

  if(!tileTag[iTagLayer ][iEtaX][iPhiX]) return; 
  // assure tower is taged
  hA[9]->Fill(7);
  ((TH2F*) hA[22])->Fill( r.x(),r.y());
  
  fillOneTailHisto('c', iEtaX,iPhiX);
  
  float eU=smdE[iSect][0][iStrU]+smdE[iSect][0][iStrU+1];
  float eV=smdE[iSect][1][iStrV]+smdE[iSect][1][iStrV+1];

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
    pl->scanAdc(smdE[iSect][iuv], thrMipSmdE);
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


