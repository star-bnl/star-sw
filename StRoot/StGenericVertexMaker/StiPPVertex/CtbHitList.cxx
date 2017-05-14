#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <cmath>

#include <St_base/StMessMgr.h>

#include <tables/St_g2t_ctf_hit_Table.h>
#include <StEvent/StTriggerData.h>


#include "StGenericVertexMaker/StiPPVertex/CtbHitList.h"

// This needs cleanup of the mapping code
extern void cts_get_ctb_indexes(long, long &, long &);

/*
  cts_get_ctb_indexes() is defined in $STAR/pams/ctf/cts/cts.cc
*/


//==========================================================
//==========================================================
CtbHitList::CtbHitList() :
  ScintHitList(-M_PI/60.,M_PI/30,60, -1.,0.5,4,(char *) "Ctb",2.,0.5){
  // CTB clibration: 2 MeV==5 ADC
  mCtbThres_mev=1; //to reject slats with low dE for M-C data
  mCtbThres_ch=2 ;  //to reject slats with low ADC for real data
  geantE=new float [nBin];
}


//==========================================================
//==========================================================
void
CtbHitList::initRun(float fac){
  const float  mCtbEtaSeg=0.5,  mCtbPhiSeg=M_PI/30; // NEVER chang this two , JB
  gMessMgr->Message("","I") <<"  CtbHitList::initRun(), gain change factor="<<fac<<endm;
  mCtbThres_mev*=fac; // tmp to test cuts
  mCtbThres_ch=(int) (fac*mCtbThres_ch);  // tmp to test cuts

  gMessMgr->Message("","I") 
    <<"  CtbHitList::initRun() CtbThres_Ch (real)="<<mCtbThres_ch
    <<"  or  CtbThres_MeV (M-C)="<<mCtbThres_mev
    <<endm;

  ScintHitList::initRun();
  int i;
  for(i=0;i<nBin;i++){
    // Assume 2004x geom, tmp, fix it by do masking based on geomtery type
    if(i==40 || i==50 ||i==100 || i==110) continue;
    setActive(i);
  }
  
  // M-C events
  long iPhi1,iEta1;
  // clear old lookup table
  for(iPhi1=1;iPhi1<mxPhi1;iPhi1++) 
    for(iEta1=1;iEta1<mxEta1;iEta1++)
      mcId2bin[iPhi1][iEta1]=-1;
  
  for(iPhi1=1;iPhi1<mxPhi1;iPhi1++) 
    for(iEta1=1;iEta1<mxEta1;iEta1++) {
      int iPhi0=iPhi1-1;
      int iEta0=iEta1-1; 
      int iBin0=iPhiEta2bin(iPhi0,iEta0); //tmp  take it out later
      float phi=iPhi0*mCtbPhiSeg;// phi is [0,2Pi] 
      float eta=iEta0*mCtbEtaSeg -0.75;
      int iEta=etaBin(eta); //my interal numbering scheme
      int iPhi=phiBin(phi);
      if(iEta<0) continue; // out of assumed sensitivity range       
      assert( iEta<nEta);
      assert( iPhi>=0);
      int iBin=iPhiEta2bin(iPhi,iEta);
      assert(iBin==iBin0); //tmp
  
      mcId2bin[iPhi1][iEta1]=iBin;     
    }
  
  // real data events
  int slat, tray;
  for (slat = 0; slat < mxSlat; slat++)
    for ( tray = 0; tray < mxTray; tray++) {
      float phi,eta;
      ctb_get_slat_from_data(slat,tray,phi,eta);
      int iEta=etaBin(eta); //my interal numbering scheme
      int iPhi=phiBin(phi);
      if(iEta<0) continue; // out of assumed sensitivity range       
      assert( iEta<nEta);
      assert( iPhi>=0);
      int iBin=iPhiEta2bin(iPhi,iEta);  
      realId2bin[slat][tray]=iBin;
    }
  
}

//==========================================================
//==========================================================
void
CtbHitList::clear(){
  ScintHitList::clear();
  memset(geantE,0,nBin*sizeof(float));
}

//==========================================================
//==========================================================
CtbHitList::~CtbHitList(){
  delete [] geantE ;
}


//==========================================================
//==========================================================
int
CtbHitList::etaBin(float eta){
  if(eta<eta0) return -1; // out of Eta range
  int iEta=(int)((eta-eta0)/dEta); 
  if( iEta>=nEta) return -1; // out of Eta range
  // printf("CtbHitList::etaBin eta=%f iEta=%d\n",eta,iEta);
  return iEta;
}

//==========================================================
//==========================================================
float
CtbHitList::bin2EtaLeft(int iEta){
  assert(iEta>=0);
  assert(iEta<nEta);
  float etaF= eta0+iEta*dEta ;
  return etaF;
}

//==========================================================
//==========================================================
void   
CtbHitList::buildFromMC(TDataSet *gds) {

  // CTB clibration: 2 MeV==5 ADC
  gMessMgr->Message("","I") <<" CtbHitList::buildFromMC thr/MeV="<<mCtbThres_mev<<endm;

  if(gds==0) return ;

  // -------------- E X T R A C T    C T B   H I T S   --------------------
  //access the CTB data  from GEANT
  St_g2t_ctf_hit *g2t_ctb_hit = (St_g2t_ctf_hit *) gds->Find("g2t_ctb_hit");
  if(g2t_ctb_hit == 0){
    LOG_DEBUG << "CtbHitList::buildMC() No CTB Hits in MC table for this event" << endm;
    LOG_DEBUG << "g2t_ctb_hit = " << g2t_ctb_hit << endm;
    return ;
  }

  g2t_ctf_hit_st *ctb_hit = NULL;

  //printf("All GEANT CTB hits=%d\n",(int)g2t_ctb_hit->GetNRows());

  if (g2t_ctb_hit->GetNRows() == 0)    gMessMgr->Message("","I") <<" CtbHitList::buildMC() Empty geant/ctb data set "<<endm;

  ctb_hit = g2t_ctb_hit->GetTable();

  //assert(ctb_hit);
  if (! ctb_hit){
    LOG_WARN << "CtbHitList::buildMC() no CTB hits" << endm;
    return ;
  }
  
 int i;
 for (i = 0; i < g2t_ctb_hit->GetNRows(); i++,ctb_hit++){
    float de_mev=ctb_hit->de*1000.;
    //    if(de_mev>0.5)  printf("CTB Hit i=%d  de/MeV=%f parent=%d\n",i,de_mev ,ctb_hit->track_p);
    long iPhi1,iEta1;
    cts_get_ctb_indexes(ctb_hit->volume_id,iPhi1,iEta1);

    // printf("iPhi=%d,iEta=%d de/MeV=%f \n",(int)iPhi,(int)iEta,de_mev);
    //printf("ctb_indexes , hit=%d, vol_id=%d, iPhi=%d, iEta=%d, de/MeV=%f\n",i,(int)ctb_hit->volume_id,(int)iPhi,(int)iEta );    

    int iBin=mcId2bin[iPhi1][iEta1];
    geantE[iBin]+=de_mev;
 }
 
 for(i=0;i<nBin;i++){
   if ( getActive(i)<0) continue;
   if( geantE[i]<mCtbThres_mev)  continue; // ignore low energy CTB slat
   setFired(i);
 }
 
}


//==========================================================
//==========================================================
void  
CtbHitList::buildFromData(StTriggerData *trgD){
  
  LOG_INFO << " CtbHitList::buildFromData CtbThres_Ch thres="<<mCtbThres_ch << endm;

  // access CTB from Akio's Maker
  
  if(!trgD){
    LOG_WARN << "CtbHitList::buildFromData: no trigData in real data" << endm;
    return ;
  }
  int slat, tray;
  for ( slat = 0; slat < mxSlat; slat++)
    for ( tray = 0; tray < mxTray; tray++) {
      float adc = trgD->ctbTraySlat(tray,slat,0);
      if(adc<mCtbThres_ch) continue;
      int iBin=realId2bin[slat][tray];
      // printf("CTB sl=%3d tr=%3d  %.1f  iBin=%d\n",slat,tray, adc,iBin );
      if ( getActive(iBin)<0) continue;
      setFired(iBin);
    }
#if 0
  // old method , run just for cross check, delete later, JB
  
  StTriggerDetectorCollection* trigCol = event->triggerDetectorCollection();
  if(!trigCol){
    LOG_WARN << "StCtbUtility scans: no trigCol in Data" << endm;
    return ;
  }

  StCtbTriggerDetector* ctbDet = &(trigCol->ctb());
  for (UInt_t slat = 0; slat < ctbDet->numberOfSlats(); slat++) 
    for (UInt_t tray = 0; tray < ctbDet->numberOfTrays(); tray++) {
      ctbHit curHit;
      curHit.adc = ctbDet->mips(tray,slat,0);
      if(curHit.adc<mCtbThres_ch) continue;
      // printf("A sl=%3d tr=%3d  %4f\n",slat,tray, curHit.adc );
    }
  
#endif
  return ;
}


//==========================================================
//==========================================================
void 
CtbHitList::ctb_get_slat_from_data(int slat, int tray, float & phiRad, float &eta) {
  float phiZero1 = 72 ; // magic lines from Pablo & Herb
  float phiZero2 = 108 ;
  float deltaPhi = 6 ;
  // tray=0,1
  // slat=0,...,119

  int iz ;
  float phi ;

  if ( tray < 60 )  {
    phi = phiZero1 - tray * deltaPhi ;
    iz = 0 ;
  }  else {
    phi = phiZero2 + (tray-60) * deltaPhi ;
    iz = 1 ;
  }
  if ( phi <   0. ) phi += 360 ;
  if ( phi > 360. ) phi -= 360 ;

  phiRad=phi/180*M_PI;
  eta=(1-2*iz)*(1+2*slat)*0.25;
  // printf("CTB hit: slat=%d, tray=%d,  phiDeg=%f/deg, eta=%f\n",slat,tray,phi,eta);

}


