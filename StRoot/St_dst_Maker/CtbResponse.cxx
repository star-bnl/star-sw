#include "CtbResponse.h"

#include <TRandom.h>
#include <string.h>

#include "tables/St_g2t_ctf_hit_Table.h"
#include "tables/St_g2t_vertex_Table.h" 
#include "tables/St_g2t_track_Table.h"
#include "math_constants.h"

#include "St_trg_Maker/St_trg_Maker.h"
#include "StEventTypes.h"// for StEvent

#include "tables/St_event_header_Table.h"
#include "tables/St_dst_TrgDet_Table.h"

#define IFEMBED 1;

// This needs cleanup of the mapping code
extern void cts_get_ctb_indexes(long, long &, long &);


/*!
 * cts_get_ctb_indexes() is defined in $STAR/pams/ctf/cts/cts.cc
 *
 * INPUT flag = 0 - assume all 240 slats fired
 *              1 - use DAQ data
 *              2 - use M-C hits
 */

CtbResponse::CtbResponse(StVertexMaker *head, int *ipar, float *fpar,unsigned int mode) {
    
  LOG_INFO <<" THIS IS CtbResponse -START"<<endm;
  int i;

  this->SetCTBMode(mode);

  memset(GVER,0,sizeof(GVER));
  const float CtbEtaSeg=0.5, CtbPhiSeg=C_PI/30;
  const float timeGateWidth=70; // (ns)

  
  // params
  float CtbThres_ch=ipar[0]; // for Data
  float CtbThres_mev=fpar[0]; // for M-C
 
  St_trg_Maker *trg=(St_trg_Maker *)head->GetMaker("trg");
  //assert(trg);

  if(0){ // *************************************************************
    printf("assume 240 CTB slats fired for trigger the bXing\n");
    
    int bXing=trigBXing;
    long iPhi,iEta;
    for(iPhi=0;iPhi<60;iPhi++)
      for(iEta=0;iEta<4;iEta++) {
	struct Jcyl hit1;
	hit1.eta=iEta*CtbEtaSeg -0.75;
	hit1.phi=iPhi*CtbPhiSeg;
	hit1.gBXing=-8888; // ID of generated bXing by pileup mixing
	hits[bXing].push_back(hit1);
      }
    goto endCtb;
  }
  
  if( this->GetCTBMode() == 0){ //extract Ctb Hist from trigger Maker
    LOG_INFO <<"use DAQ  CTB slats fired for the trigger bXing"<<endm;
    
    int bXing=trigBXing; // ignore other bXings
    St_dst_TrgDet *trgDet=(St_dst_TrgDet*) trg->Find(".data/TrgDet");
    assert(trgDet);
    dst_TrgDet_st *tab= (dst_TrgDet_st *)trgDet->GetArray() ;

    for(int slat=0;slat<2;slat++) {
      for(int tray=0;tray<120;tray++) {
	//float adc=trg->jtd.ctb[tray][slat];
	float adc=tab->nCtb[tray][slat][0];
	head->hPiFi[0]->Fill(adc);
	//printf("slat=%d tray=%d  my=%f  herb=%f \n",slat,tray,adc,adc2);
	if(adc<CtbThres_ch) continue;
	int ID=tray+slat*120;
	head->hctb[5]->Fill(ID);
	float  phiDeg, eta;
	ctb_get_slat_from_data(slat, tray,  phiDeg, eta);
	//printf("ADC=%f slat=%d, tray=%d,  phiDeg=%f/deg, eta=%f\n",adc,slat,tray,phiDeg,eta);
	struct Jcyl hit1;
	hit1.eta=eta;
	hit1.phi=phiDeg/180.*C_PI;
	hit1.gBXing=-7777; // ID not used for DATA
	hit1.ID=ID;
	hits[bXing].push_back(hit1);
      }
    }
    
    goto endCtb;
  }

  if(this->GetCTBMode() == 2){
      cout << "Runing in Embedding Mode, Killing DAQ CTB's" << endl;
      St_dst_TrgDet *trgDet_temp=(St_dst_TrgDet*) trg->Find(".data/TrgDet");
      dst_TrgDet_st *tab_temp= (dst_TrgDet_st *)trgDet_temp->GetArray() ;
      
      for(int iTray = 0;iTray < 2;iTray++)
	  for(int iSlat = 0; iSlat < 120;iSlat++)
	      tab_temp->nCtb[iTray][iSlat][0] = 0; 
  }

  // *************  M-C events *****************************
  if( this->GetCTBMode() == 1 || this->GetCTBMode() == 2 ) {
    LOG_INFO <<"use M-C for CTB hits\nI will write CTB ADC's into DAQ Table with 2 MeV=> 5 ADC!!!"<<endm;
    St_DataSet *gds=head->GetDataSet("geant"); 
//VP    head->ls();
    assert(gds);
    
    // -------------- E X T R A C T    C T B   H I T S   --------------------
    //access the CTB data  from GEANT
    St_g2t_ctf_hit *g2t_ctb_hit = (St_g2t_ctf_hit *) gds->Find("g2t_ctb_hit");
    if(g2t_ctb_hit == 0){
      LOG_WARN << Form("No CTB Hits in MC File\ng2t_ctb_hit = %d",g2t_ctb_hit)<<endm;
      return;
    }
    //    assert(g2t_ctb_hit);
    g2t_ctf_hit_st *ctb_hit = NULL;
    
    St_g2t_track *g2t_track = (St_g2t_track *)gds->Find("g2t_track");
    g2t_track_st *tpc_track = g2t_track->GetTable();
    
    St_g2t_vertex  *g2t_ver=( St_g2t_vertex *)gds->Find("g2t_vertex");
    g2t_vertex_st *gver=g2t_ver->GetTable();
    
    LOG_INFO << Form("All GEANT CTB hits=%d\n",(int)g2t_ctb_hit->GetNRows())<<endm;
    
    if (g2t_ctb_hit->GetNRows() == 0)
      { LOG_INFO <<"Empty geant/ctb data set "<<endm;  return;}
    
    ctb_hit = g2t_ctb_hit->GetTable();  
    assert(ctb_hit);
    for (i = 0; i < g2t_ctb_hit->GetNRows(); i++,ctb_hit++){
      float de_mev=ctb_hit->de*1000.;
      //      cout << "CTB Hit " << i << " has Energy " << de_mev << " MeV" << endl;
      if(de_mev <CtbThres_mev){
	//cout << "Hit Ignored: below threshold" << endl;
	continue; // ignore double hits per CTB slat
      }

      float tof_ns=ctb_hit->tof*1.e9;
      // tmp for 1usec cutoff in digitalization
      if(tof_ns>999.) tof_ns+=gRandom->Rndm()*500;
      int parT=ctb_hit->track_p;
      int parV=-1;
      
      do {
	parV=tpc_track[parT-1].start_vertex_p; //  "-1" is (Fortran-->C++)
	parT=gver[parV-1].parent_p;
      } while(parT>0);
      assert(parV>=0);
      
      float timeOff=gver[parV-1].ge_tof*1.e9;
      float tofHit=tof_ns+timeOff; // add vertex offset to TOF
      
      //printf("hit=%2d, tof=%7.2f,de=%5.1f ",i,tof_ns, de_mev);
      //printf(" timeOff=%7.2f  TOF+timeOff=%7.2f egL=%2d parT=%3d partV=%d\n",timeOff, tofHit, gver[parV-1].eg_label,parT, parV);
      
      // associate hit with the bunch crossing ID
      float xx=tofHit/ bXingTimeSep -firstBXing;
      int bXing=(int)xx;
      // printf("  xx=%f, rr=%f gBXing=%.1f\n",xx,xx-bXing,gBXing);
      if(xx-bXing>timeGateWidth/bXingTimeSep){
	//cout << "Hit Ignored: out of time gate" << endl;
	continue; // out of time gate
      }
      //printf("  reco bXing=%d\n",bXing);
      
      if(bXing<0 || bXing>=MxTimeSlot){
	//cout << "Hit Ignored: bXing too far away" << endl;
	continue; // bXing too far away
      }
      // remember geant vertex for this bXing
      if(tof_ns<bXingTimeSep/2. && !GVER[bXing])GVER[bXing]=(void*)&gver[parV-1];
      
      long iPhi,iEta;
      cts_get_ctb_indexes(ctb_hit->volume_id,iPhi,iEta);
      iPhi--; iEta--; // change range to [0,N-1]
      assert(iPhi >= 0 && iPhi<60 && iEta>=0 && iEta<4);
      //cout << endl;
      //printf("ctb_indexes , hit=%d, vol_id=%d, iPhi=%d, iEta=%d, de/MeV=%f, TOF/ns=%.1f\n",i,(int)ctb_hit->volume_id,(int)iPhi,(int)iEta,ctb_hit->de*1000,tof_ns );
      
      struct Jcyl hit1;
      hit1.eta=iEta*CtbEtaSeg -0.75;
      hit1.phi=iPhi*CtbPhiSeg;
      hit1.gBXing=timeOff/bXingTimeSep; // ID of generated bXing by pileup mixing
      //  hit1.gVert=&gver[parV-1];
      //printf("x=%d, eta=%f, phi/deg=%f\n",nCtbH,CtbH[nCtbH].eta,CtbH[nCtbH].phi/3.1416*180);
      //cout << "eta: " << hit1.eta << " phi: " << hit1.phi * (180/ TMath::Pi()) << endl;
      hits[bXing].push_back(hit1);
      
      head->hctb[0]->Fill(tofHit);
      head->hctb[1]->Fill(tofHit);
      head->hctb[3]->Fill(tofHit);
      if(fabs(timeOff)<5)     head->hctb[2]->Fill(tofHit);
      head->hctb[4]->Fill(bXing);
      
      // Attempt to fill CTB's
      St_dst_TrgDet *trgDet=(St_dst_TrgDet*) trg->Find(".data/TrgDet");
      assert(trgDet);
      dst_TrgDet_st *tab= (dst_TrgDet_st *)trgDet->GetArray() ;
      
      int slat = 0;
      int tray = 0;

      if(iEta == 0){
	slat = 1;
	tray = iPhi + 102;
	if(tray > 119)
	  tray-= 60;
      }
      if(iEta == 1){
	slat = 0;
	tray = iPhi + 102;
	if(tray > 119)
	  tray-= 60;
      }
      if(iEta == 2){
	slat = 0;
	tray = 12 - iPhi;
	if(tray < 0)
	  tray += 60;
      }
      if(iEta == 3){
	slat = 1;
	tray = 12 - iPhi;
	if(tray < 0)
	  tray += 60;
      }
      // Don't know conversion to ADC. Just use 999 for now
      tab->nCtb[tray][slat][0] += floor(de_mev * (5.0 / 2.0)); // 2 MeV => 5 ADC counts (from H. Crawford)
      //cout << "GeantHit: slat = " << slat << " tray = " << tray << " adc: " << tab->nCtb[tray][slat][0] << endl;
            
    }// end of loop over CTB hits
  }// end of GEANT data
    //*************************************************************
 

#if 0 // STEvent not avaliable at this point of the chain
//{
    cout << "Filling StEvent" << endl;
    StEvent* event;
    event = (StEvent *) head->GetInputDS("StEvent");
    assert(event);
    
    StTriggerDetectorCollection *theTriggers = event->triggerDetectorCollection();
    if (!theTriggers) {
      printf("# Event: triggerDetectorCollection is missing\n");
      assert(0);
    }
    
    StL0Trigger *l0Trigger = event->l0Trigger();
    assert(l0Trigger );
    StCtbTriggerDetector &theCtb = theTriggers->ctb();
    
    for(unsigned int islat=0; islat<theCtb.numberOfSlats(); islat++)
      for(unsigned int itray=0; itray<theCtb.numberOfTrays(); itray++) {
	int adc=theCtb.mips(itray, islat, 0);
	if(adc) printf("a%d:%d %d\n",itray+1,islat+1,adc);
      }
    goto endCtb;
//}
#endif    
endCtb:
 
  head->hPiFi[1]->Fill(hits[trigBXing].size());
  printf("total CTB response\nbXing[-pre,0,post] \tnHits  GVER \n");
  for(i=0;i<MxTimeSlot;i++) {
    if(hits[i].size()==0) continue;
    printf("%d   \t \t%d",i+firstBXing,hits[i].size());
    g2t_vertex_st *v=(g2t_vertex_st *)GVER[i];
    if(v)
      printf("  x=%6.2f y=%6.2f z=%6.2f \n",v->ge_x[0],v->ge_x[1],v->ge_x[2]);
    else
      printf("    no vertex\n");
  }

  return;
}



  

  /*
  St_DataSet    *ds0=head->GetDataSet("dst/event_header"); assert(ds0);  
  St_event_header *evhd=(St_event_header*) ds0->Find("event_header");
  assert(evhd);
  evhd->Print(0,1);
  event_header_st *EVHD=evhd->GetTable(); assert(EVHD);  
  if(EVHD->exp_run_id!=2306006) goto use_MC;
  */



//======================================================================
//======================================================================
//======================================================================
void  CtbResponse::ctb_get_slat_from_data(int slat, int tray, float & phiDeg, float &eta) {
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
  
  phiDeg=phi;
  eta=(1-2*iz)*(1+2*slat)*0.25;
  printf("CTB hit: slat=%d, tray=%d,  phiDeg=%f/deg, eta=%f\n",slat,tray,phiDeg,eta);


} 


