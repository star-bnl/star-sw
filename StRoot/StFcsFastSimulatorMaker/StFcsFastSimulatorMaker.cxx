// $Id: StFcsFastSimulatorMaker.cxx,v 1.2 2021/03/30 13:40:09 akio Exp $                                            
//                                                                                                                     
// $Log: StFcsFastSimulatorMaker.cxx,v $
// Revision 1.2  2021/03/30 13:40:09  akio
// FCS code after peer review and moved from $CVSROOT/offline/upgrades/akio
//
// Revision 1.10  2021/02/25 21:54:41  akio
// Int_t -> int
//
// Revision 1.9  2021/02/25 19:25:48  akio
// Code modified for STAR code review
//
// Revision 1.8  2021/02/23 16:25:50  akio
// Modification to attend comments from STAR code review (Jason)
//
// Revision 1.7  2020/08/27 22:08:09  akio
// fix a continue bug found by Ting Lin
//
// Revision 1.6  2020/05/29 18:51:02  akio
// adding EPD g2t reading as PRES
//
// Revision 1.5  2019/10/23 17:15:42  akio
// *** empty log message ***
//
// Revision 1.4  2019/07/22 18:56:41  akio
// Added LeakyHcal option 2 and 3 for 2d light collection efficiency parametrization
//
// Revision 1.3  2019/06/26 18:02:28  akio
// MOdify StFcsHit creating to match StEvent update
//
// Revision 1.2  2019/05/16 16:11:55  akio
// Adding leaky hcal option
//
// Revision 1.1  2018/11/14 16:50:16  akio
// FCS codes in offline/upgrade/akio
//
//                                                                                                                     
//       Implementation of StFcsFastSimulatorMaker, the FCS fast simulator
//

#include "StFcsFastSimulatorMaker/StFcsFastSimulatorMaker.h"

#include "St_base/StMessMgr.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFcsCollection.h"
#include "StEvent/StFcsHit.h"
#include "tables/St_g2t_emc_hit_Table.h"
#include "tables/St_g2t_hca_hit_Table.h"
#include "tables/St_g2t_epd_hit_Table.h"
#include "StFcsDbMaker/StFcsDb.h"

namespace{
  static const char name[kFcsEHP][4]={"wca","hca","pre"};
}

StFcsFastSimulatorMaker::StFcsFastSimulatorMaker(const char* name) : StMaker(name) {
  setLeakyHcal(0);
  setHcalZDepEff(0);
}

int StFcsFastSimulatorMaker::Init() {
  mFcsDb = static_cast<StFcsDb*>(GetDataSet("fcsDb"));
  if (!mFcsDb) {
    LOG_ERROR << "StFcsFastSimulatorMaker initializing failed due to no StFcsDb" << endm;
    return kStErr;
  } 

  memset(mEcalMap,0,sizeof(mEcalMap));
  memset(mHcalMap,0,sizeof(mHcalMap));
  memset(mPresMap,0,sizeof(mPresMap));
  return kStOK;
}

void StFcsFastSimulatorMaker::Clear(Option_t *option){
  memset(mEcalMap,0,sizeof(mEcalMap));
  memset(mHcalMap,0,sizeof(mHcalMap));
  memset(mPresMap,0,sizeof(mPresMap));
  StMaker::Clear(option);
  return;
}

int StFcsFastSimulatorMaker::Make() {
  LOG_DEBUG << "StFcsFastSimulatorMaker::Make" << endm;
  
  // Get the existing StEvent, or add one if it doesn't exist.
  StEvent* event = static_cast<StEvent*>(GetDataSet("StEvent"));
  if (!event) {        
    event = new StEvent;
    AddData(event);
    LOG_DEBUG << "Creating StEvent" << endm;
  }  

  // Add an FCS collection to the event if one does not already exist.
  if (!event->fcsCollection()) {
    event->setFcsCollection(new StFcsCollection);
    LOG_DEBUG << "Creating StFcsCollection" << endm;
  }  
  
  fillStEvent(event);
  return kStOk;
}

/* Fill an event with StFcsHits. */
void StFcsFastSimulatorMaker::fillStEvent(StEvent* event) {  
  StFcsCollection * fcscollection = event->fcsCollection();
  int ehp;
  int ng2thit[kFcsEHP]={};
  StPtrVecFcsHit hits; //temp storage for hits
  int leakyHcal = IAttr("FcsLeakyHcal");
  int hcalZdepEff = IAttr("FcsHcalZdepEff");
  
  // Read the g2t table for FCS Wcal 
  ehp=0;
  St_g2t_emc_hit* hitTable = static_cast<St_g2t_emc_hit*>(GetDataSet(Form("g2t_%3s_hit",name[ehp])));
  if (!hitTable) {
    LOG_INFO << Form("g2t_%3s_hit table is empty",name[ehp]) << endm;
  }else{
    const int nHits = hitTable->GetNRows(); 
    ng2thit[ehp]=nHits;
    LOG_INFO << Form("g2t_%s_hit table has %d hit",name[ehp],nHits) << endm;
    if(nHits>0){
      const g2t_emc_hit_st* hit = hitTable->GetTable();
      if(!hit){
	LOG_INFO << Form("g2t_%3s_hit GetTable failed",name[ehp]) << endm;
      }else{
	for (int i=0; i < nHits; ++i) {
	  if (!hit) {hit++; continue;}
	  const int ns  = hit->volume_id / 1000 - 1;
	  const int id  = hit->volume_id % 1000 - 1;
	  const int det = mFcsDb->detectorId(ehp,ns);
	  if(det<0 || det>=kFcsNDet || id<0 || id>=kFcsEcalMaxId){
	    LOG_WARN << Form("ECAL det=%1d id=%3d volid=%5d e=%f out of range (%d)",
			     det,id,hit->volume_id,hit->de,kFcsMaxId) << endm;
	    hit++;
	    continue;
	  }else if(GetDebug()){
	    LOG_INFO << Form("ECAL det=%1d id=%3d volid=%4d e=%f",
			     det,id,hit->volume_id,hit->de) << endm;
	  }
	  float de = hit->de;
	  StFcsHit* fcshit=0;
	  if(mEcalMap[ns][id]==0){ // New hit
	    int ehp=0, rns=0, crt=0, sub=0, dep=0, ch=0;
	    mFcsDb->getDepfromId(det, id, ehp, rns, crt, sub, dep, ch);
	    fcshit = new StFcsHit(1, det, id, rns, ehp, dep, ch, de);
	    hits.push_back(fcshit);
	    mEcalMap[ns][id]=fcshit;
	  }else{ // Adding energy to old hit
	    fcshit = mEcalMap[ns][id];
	    fcshit->setEnergy(fcshit->energy() + de);
	  }
	  fcshit->addGeantTrack(hit->track_p,de);
	  hit++;
	}
      }
    }
  }
  
  // Read the g2t table for FCS Hcal 
  ehp=1;
  St_g2t_hca_hit* hitTable_h = static_cast<St_g2t_hca_hit*>(GetDataSet(Form("g2t_%3s_hit",name[ehp])));
  if (!hitTable_h) {
    LOG_INFO << Form("g2t_%3s_hit table is empty",name[ehp]) << endm;
  }else{
    const int nHits = hitTable_h->GetNRows();
    ng2thit[ehp]=nHits;
    LOG_INFO << Form("g2t_%s_hit table has %d hit",name[ehp],nHits) << endm;
    if(nHits>0){	    
      const g2t_hca_hit_st* hit = hitTable_h->GetTable();
      if(!hit){
	LOG_INFO << Form("g2t_%3s_hit GetTable failed",name[ehp]) << endm;
      }else{
	for (int i=0; i < nHits; i++) {
	  if (!hit) {hit++; continue;}
	  const int ns  = hit->volume_id / 1000 - 1;
	  const int id  = hit->volume_id % 1000 - 1;
	  const int det = mFcsDb->detectorId(ehp,ns);
	  if(det<0 || det>=kFcsNDet || id<0 || id>=kFcsHcalMaxId){
	    LOG_WARN << Form("HCAL det=%d id=%d volid=%5d e=%f out of range (%d)",
			     det,id,hit->volume_id,hit->de,kFcsMaxId) << endm;
	    hit++;
	    continue;
	  }else if(GetDebug()){
	    LOG_INFO << Form("HCAL det=%d id=%d volid=%5d e=%f",
			     det,id,hit->volume_id,hit->de) << endm;
	  }
	  StFcsHit* fcshit=0;
	  int ehp=0, rns=0, crt=0, sub=0, dep=0, ch=0;
	  mFcsDb->getDepfromId(det, id, ehp, rns, crt, sub, dep, ch);
	  if(leakyHcal==0  || leakyHcal==2){
	    float de;
	    de = hit->de;
	    //if(leakyHcal==2) de = hit->de2;
	    if(hcalZdepEff==1) de = hit->deA;
	    if(hcalZdepEff==2) de = hit->deB;
	    if(mHcalMap[ns][id]==0){ // New hit
	      fcshit = new StFcsHit(1, det, id, rns, ehp, dep, ch, de);
	      hits.push_back(fcshit);
	      mHcalMap[ns][id]=fcshit;
	    }else{ // Adding energy to old hit
	      fcshit = mHcalMap[ns][id];
	      fcshit->setEnergy(fcshit->energy() + de);
	    }
	    fcshit->addGeantTrack(hit->track_p,de);
	    hit++;
	  }else{ //leaky hcal with up to 4 WLSP getting lights from a tower
	    float de[4];
	    if(leakyHcal==1){
	      de[0] = hit->deA;
	      de[1] = hit->deB;
	      de[2] = hit->deC;
	      de[3] = hit->deD;
	    }else{ //tested and turned to be minor. Not implemented in official XML
	      //de[0] = hit->de2A;
	      //de[1] = hit->de2B;
	      //de[2] = hit->de2C;
	      //de[3] = hit->de2D;
	    }
	    int col = mFcsDb->getColumnNumber(det,id);	// col goes 1 ~ ncol
	    int ncol= mFcsDb->nColumn(det);
	    for(int j=0; j<4; j++){
	      int id2;
	      int jj = j-2;; //jj goes from -2 ~ +1
	      if(col==1 && jj<0)          {id2=id;}    // if col=1, add leaked light back to col=1
	      else if(col==2 && jj==-2)   {id2=id-1;}  // if col=2, add leaked light back to col=1
	      else if(col==ncol && jj==1) {id2=id;}    // if col=ncol, add leaked light back to col=ncol
	      else                        {id2=id+jj;} // add leaked lights to its neighbors
	      mFcsDb-> getDepfromId(det, id2, ehp, rns, crt, sub, dep, ch);
	      if(mHcalMap[ns][id2]==0){ // New hit
		int ehp=0, rns=0, crt=0, sub=0, dep=0, ch=0;
		mFcsDb-> getDepfromId(det, id2, ehp, rns, crt, sub, dep, ch);
		fcshit = new StFcsHit(1, det, id2, rns, ehp, dep, ch, de[j]);
		hits.push_back(fcshit);
		mHcalMap[ns][id2]=fcshit;
	      }else{ // Adding energy to old hit
		fcshit = mHcalMap[ns][id2];
		fcshit->setEnergy(fcshit->energy() + de[j]);
	      }
	      fcshit->addGeantTrack(hit->track_p,de[j]);
	    }
	    hit++;
	  }
	}
      }
    }
  }
  
  // Read the g2t table for FCS Pres == EPD now
  ehp=2;
  St_g2t_epd_hit* hitTable_p = static_cast<St_g2t_epd_hit*>(GetDataSet("g2t_epd_hit"));
  if (!hitTable_p) {
    LOG_INFO << Form("g2t_epd_hit table is empty") << endm;
  }else{
    const int nHits = hitTable_p->GetNRows(); 
    ng2thit[ehp]=nHits;
    LOG_INFO << Form("g2t_epd_hit table has %d hit",nHits) << endm;
    if(nHits>0){
      const g2t_epd_hit_st* hit = hitTable_p->GetTable();
      if(!hit){
	LOG_INFO << Form("g2t_epd_hit GetTable failed") << endm;
      }else{
	for (int i=0; i < nHits; ++i) {
	  if (!hit) {hit++; continue;}
	  const int volume_id = hit->volume_id;
	  const int ew        = volume_id/100000;  
	  const int pp        = (volume_id%100000)/1000;
	  int tt        = (volume_id%1000)/10;
	  const float de      = hit->de * 1000.0;
	  if(ew==1) {hit++; continue;} //west side only
	  //Hack! reverse TT Even/Odd. To be removed when EPD XML file fixed
	  if(tt>1){
	    if(tt%2==0) {tt+=1;}
	    else        {tt-=1;}
	  }
	  int det,id,ehp,ns,crt,slt,dep,ch;
	  mFcsDb->getIdfromEPD(pp,tt,det,id);
	  mFcsDb->getDepfromId(det,id,ehp,ns,crt,slt,dep,ch);
	  if(det<0 || det>=kFcsNDet || id<0 || id>=kFcsPresMaxId){
	    LOG_WARN << Form("Pres det=%1d id=%3d volid=%4d e=%f10.6  id out of range (%d)",
			     det,id,hit->volume_id,1000*hit->de,kFcsPresId) << endm;
	    hit++; 
	    continue;
	  }else if(GetDebug()){
	    LOG_INFO << Form("Pres det=%1d id=%3d volid=%4d e=%10.6f",
			     det,id,hit->volume_id,1000*hit->de) << endm;
	  }
	  StFcsHit* fcshit=0;
	  if(mPresMap[ns][id]==0){ // New hit
	    fcshit = new StFcsHit(1, det, id, ns, ehp, dep, ch, de);
	    hits.push_back(fcshit);
	    mPresMap[ns][id]=fcshit;
	  }else{ // Adding energy to old hit
	    fcshit = mPresMap[ns][id];
	    fcshit->setEnergy(fcshit->energy() + de);
	  }
	  hit++;
	  fcshit->addGeantTrack(hit->track_p,de);
	}
      }
    }
  }
  
  int nhittot=hits.size();
  int zs=0;
  int nhit[kFcsEHP]={};
  float etot[kFcsEHP]={};
  // Loop over hits and digitize & push to StEvent if it survives ZS
  for(int i=0; i<nhittot; i++){
    const int det = hits[i]->detectorId();
    const int id = hits[i]->id();
    float de  = hits[i]->energy();
    float sf  = mFcsDb->getSamplingFraction(det);
    float gain= mFcsDb->getGain(det, id);
    float corr= mFcsDb->getGainCorrection(det, id);
    int adc = static_cast<int>(de / (sf * gain * corr));
    adc = std::min(adc, 4095*8);  // Cap maximum ADC =12bit * 8 tim
    zs  = mFcsDb->getZeroSuppression(det);
    if(GetDebug()) LOG_INFO << Form("Det=%1d id=%3d dE=%8.3f SF=%6.3f gain=%6.3f corr=%6.3f ADC=%4d ZS=%2d digiE=%8.3f",
				    det,id,de,sf,gain,corr,adc,zs,adc*gain*corr) << endm;
    if(adc>zs){ // zero suppress low ADC
      float digi_energy = adc * gain * corr;
      int ehp = mFcsDb->ecalHcalPres(det);
      hits[i]->setAdc(0,adc);
      hits[i]->setEnergy(digi_energy);
      fcscollection->addHit(det,hits[i]); 
      etot[ehp] += digi_energy;
	     nhit[ehp]++;
    }else{ // just delete hits bellow ZS threshold      
      delete hits[i];
    }	    
  }
  
  if(GetDebug()>0){
    for(int ehp=0; ehp<kFcsEHP; ehp++){
      LOG_INFO << Form("%s Found %d g2t hits in %d cells, created %d hits with ADC>ZS(%d) and Etot=%8.3f",
		       name[ehp],ng2thit[ehp],nhit[ehp],
		       fcscollection->numberOfHits(ehp*2) + fcscollection->numberOfHits(ehp*2+1),
		       zs, etot[ehp]) 
	       << endm;
    }
    if(GetDebug()>1) fcscollection->print();
  }
}
