// $Id: EEsmdCal.cxx,v 1.1 2004/06/12 04:09:20 balewski Exp $
 
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

  thrMipSmdE=0.2;

  smdHitPl=new EEsmdPlain [MaxSmdPlains];
  int i;
  for(i=0;i<MaxSmdPlains;i++) {
    smdHitPl[i].set(thrMipSmdE,10,i+'U');
  }

  geoTw=new EEmcGeomSimple;
  geoSmd= EEmcSmdGeom::instance();
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

  initTileHist('a',"inclusive  ");
  initTileHist('b',"gated UxV ");
  initTileHist('c',"veto UxV  ");

  initTileHist('d',"center UxV ");
  initTileHist('e',"veto center UxV  ");

  initSmdHist('a',"inclusive  ");
 
  printf("use thrMipSmdE=%f\n", thrMipSmdE);
  assert(sectID>0 && sectID<=MaxSectors);

}


//-------------------------------------------------
//-------------------------------------------------
void EEsmdCal::clear(){ // called for every event
    memset(tileAdc,0,sizeof(tileAdc));
    memset(tileThr,0,sizeof(tileThr));
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

  // FILL inclusive histos 
  fillTailHisto_a();
  fillSmdHisto_a();
  
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
      //      if(plU->type[kU]*plV->type[kV]<2) continue;// 1Ux1V cases are dropped
      hA[9]->Fill(5);// non1x1 cases
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
  //  UxV crossing is within selected sector
  // select central MIP in tower
  int inCenter=(fabs(dphi)<0.7 && fabs(deta)<0.7);
  int iPhiX=iSect*MaxSubSec+iSubX;

  hA[9]->Fill(6);
  hA[10]->Fill(iStrU+1);
  hA[11]->Fill(iStrV+1);

  ((TH2F*) hA[21])->Fill( r.x(),r.y());
  fillTailHisto1('b', iEtaX,iPhiX);

  if(inCenter) {
    hA[9]->Fill(7);
    fillTailHisto1('d', iEtaX,iPhiX);
    ((TH2F*) hA[22])->Fill( r.x(),r.y());
  }

  float eU=smdE[iSect][0][iStrU]+smdE[iSect][0][iStrU+1];
  float eV=smdE[iSect][1][iStrV]+smdE[iSect][1][iStrV+1];

  ((TH2F*) hA[20])->Fill(iStrU+1,eU);
  ((TH2F*) hA[20])->Fill(iStrV+1,eV);
  ((TH2F*) hA[23])->Fill(iEtaX+1,eU+eV); 

 
  // loop over 8 neighbours to fill background histos
  int jeta,jphi;
  for(jeta=iEtaX-1;jeta<=iEtaX+1;jeta++) {
    if(jeta<0) continue; // in the donnut
    if(jeta>=MaxEtaBins) continue; // out of the donnut
    for(jphi=iPhiX-1;jphi<=iPhiX+1; jphi++) {
      if(jeta==iEtaX && jphi==iPhiX) continue; // drop center of 3x3
      int kphi=jphi%MaxPhiBins;// join donnut in phi, important for sect 12 & 1

      // tmp
      if(kphi/MaxSubSec!=iSect) continue; // limit to histos in one sector
      fillTailHisto1('c', jeta,kphi);
      if(inCenter)fillTailHisto1('e', jeta,kphi);
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


