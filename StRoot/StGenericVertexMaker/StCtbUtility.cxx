#include <stdio.h>
#include <cmath>
#include <sys/types.h>

#include "tables/St_g2t_ctf_hit_Table.h"
#include <StMessMgr.h>


#include <StTriggerData.h>

// This needs cleanup of the mapping code
extern void cts_get_ctb_indexes(long, long &, long &);

/*!
 * cts_get_ctb_indexes() is defined in $STAR/pams/ctf/cts/cts.cc
 */


#include "StCtbUtility.h"
//==========================================================
//==========================================================
StCtbUtility::StCtbUtility() {
  
  mCtbEtaSeg=0.5;   mCtbPhiSeg=M_PI/30; // NEVER chang this two , JB
  
  // parameters
  mCtbThres_mev=1; //to reject slats with low dE for M-C data, CTB clibration: 2 MeV==5 ADC
   mCtbThres_ch=2 ;  //to reject slats with low ADC for real data
   

}


//==========================================================
//==========================================================
void StCtbUtility::ctb_get_slat_from_data(int slat, int tray, float & phiRad, float &eta) {
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



//==========================================================
//==========================================================
void  StCtbUtility::collectCTBhitsMC(St_DataSet *gds){// M-C CTB
  // CTB clibration: 2 MeV==5 ADC
  // RETUN: true if GEANT table with hits exist (even if empty)
  gMessMgr->Message("","I") <<" use GEANT CTB hits,  ADC's  with 2 MeV=> 5 ADC, thr/MeV="<<mCtbThres_mev<<endm;
  
 
  if(gds==0) return ;
  
  // -------------- E X T R A C T    C T B   H I T S   --------------------
  //access the CTB data  from GEANT
  St_g2t_ctf_hit *g2t_ctb_hit = (St_g2t_ctf_hit *) gds->Find("g2t_ctb_hit");
  if(g2t_ctb_hit == 0){
    LOG_DEBUG << "No CTB Hits in MC table for this event" << endm;
    LOG_DEBUG << "g2t_ctb_hit = " << g2t_ctb_hit << endm;
    return ;
  }
  
  g2t_ctf_hit_st *ctb_hit = NULL;
  
  //printf("All GEANT CTB hits=%d\n",(int)g2t_ctb_hit->GetNRows());
  
  if (g2t_ctb_hit->GetNRows() == 0)    gMessMgr->Message("","I") <<" Empty geant/ctb data set "<<endm;

  ctb_hit = g2t_ctb_hit->GetTable();

  //assert(ctb_hit);
  if (! ctb_hit){
    LOG_WARN << "StCtbUtility::collectCTBhitsMC: no CTB hits" << endm;
    return ;
  }

  int i;
  for (i = 0; i < g2t_ctb_hit->GetNRows(); i++,ctb_hit++){
    float de_mev=ctb_hit->de*1000.;
    if(de_mev>0.01)  LOG_INFO<<Form("CTB Hit i=%d  de/MeV=%f parent=%d\n",i,de_mev ,ctb_hit->track_p)<<endm;
    if(de_mev <mCtbThres_mev)  continue; // ignore low energy CTB slat
    
    long iPhi,iEta;
    cts_get_ctb_indexes(ctb_hit->volume_id,iPhi,iEta);
    iPhi--; iEta--; // change range to [0,N-1]
    // printf("iPhi=%d,iEta=%d de/MeV=%f \n",(int)iPhi,(int)iEta,de_mev);
    assert(iPhi >= 0 && iPhi<60 && iEta>=0 && iEta<4);
    //printf("ctb_indexes , hit=%d, vol_id=%d, iPhi=%d, iEta=%d, de/MeV=%f\n",i,(int)ctb_hit->volume_id,(int)iPhi,(int)iEta );
    
    ctbHit curHit;
    curHit.adc=de_mev*2.5 ;
    curHit.phi=iPhi*mCtbPhiSeg;
    curHit.eta=iEta*mCtbEtaSeg -0.75;
    mCtbHits.push_back(curHit);
    
  }// end of loop over CTB hits
  
  
  gMessMgr->Message("","I") << mCtbHits.size() << " CTB slats accepted from M-C data"<<endm;
  
  return ;
}



//==========================================================
//==========================================================
void  StCtbUtility::collectCTBhitsData(StTriggerData *trgD){
  // returns true if one or more valid CTB hits are found.  
  LOG_INFO << "StCtbUtility scans real CTB hits" << endm;

  // access CTB from Akio's Maker
  
  if(!trgD){
    LOG_WARN << "StCtbUtility scans: no trigData in real data" << endm;
    return ;
  }

  //assert(trgD);
  for (UInt_t slat = 0; slat < 2; slat++) 
    for (UInt_t tray = 0; tray < 120; tray++) {
      ctbHit curHit;
      curHit.adc = trgD->ctbTraySlat(tray,slat,0);
      if(curHit.adc<mCtbThres_ch) continue;
      // printf("B sl=%3d tr=%3d  %4f\n",slat,tray, curHit.adc );
      ctb_get_slat_from_data(slat,tray,curHit.phi, curHit.eta);
      mCtbHits.push_back(curHit);
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
      LOG_INFO<<Form("A sl=%3d tr=%3d  %4f\n",slat,tray, curHit.adc )<<endm;
    }
  
#endif
  return ;
}


//==========================================================
//==========================================================
void  StCtbUtility::printCtb() {
  LOG_INFO<<Form("StCtbUtility::print(),nSlat=%d\n",mCtbHits.size())<<endm;

  unsigned int ih;
  for(ih=0;ih<mCtbHits.size();ih++) {// loop over CTB hits
    LOG_INFO<<Form("ih=%d eta=%.3f phi/deg=%.1f adc=%.1f\n",ih
           ,mCtbHits[ih].eta,mCtbHits[ih].phi/3.1416*180,mCtbHits[ih].adc)<<endm;
  }
}
     
