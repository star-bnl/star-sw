 // $Id: StFcsFastSimulatorMaker.cxx,v 1.6 2020/05/29 18:51:02 akio Exp $                                            
 //                                                                                                                     
 // $Log: StFcsFastSimulatorMaker.cxx,v $
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

 //#include <algorithm>  // For std::fill(), std::max(), std::min()

 #include "St_base/StMessMgr.h"
 #include "StEvent/StEvent.h"
 #include "StEvent/StFcsCollection.h"
 #include "StEvent/StFcsHit.h"
 #include "tables/St_g2t_emc_hit_Table.h"
 #include "tables/St_g2t_hca_hit_Table.h"
 #include "tables/St_g2t_ctf_hit_Table.h"
 #include "StFcsDbMaker/StFcsDbMaker.h"

 static const char name[kFcsEHP][4]={"wca","hca","pre"};

 StFcsFastSimulatorMaker::StFcsFastSimulatorMaker(const Char_t* name) : StMaker(name) {}

 Int_t StFcsFastSimulatorMaker::Make() {
     LOG_DEBUG << "StFcsFastSimulatorMaker::Make" << endm;

     if (!GetMaker("fcsDb")) {
	 LOG_ERROR << "No StFcsDbMaker. StFcsDbMaker library not loaded?" << endm;
	 return kStErr;
     } 

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
     StFcsDbMaker* dbMaker = static_cast<StFcsDbMaker*>(GetMaker("fcsDb"));
     StFcsCollection * fcscollection = event->fcsCollection();
     int ehp;
     int ng2thit[kFcsEHP]={};
     StPtrVecFcsHit hits; //temp storage for hits

     // Read the g2t table for FCS Wcal 
     ehp=0;
     St_g2t_emc_hit* hitTable = static_cast<St_g2t_emc_hit*>(GetDataSet(Form("g2t_%3s_hit",name[ehp])));
     if (!hitTable) {
	 LOG_INFO << Form("g2t_%3s_hit table is empty",name[ehp]) << endm;
     }else{
	 const Int_t nHits = hitTable->GetNRows(); 
	 ng2thit[ehp]=nHits;
	 LOG_INFO << Form("g2t_%s_hit table has %d hit",name[ehp],nHits) << endm;
	 if(nHits>0){
	     const g2t_emc_hit_st* hit = hitTable->GetTable();
	     if(!hit){
		 LOG_INFO << Form("g2t_%3s_hit GetTable failed",name[ehp]) << endm;
	     }else{
		 //table to keep pointer to hit for each det & channel
		 auto map = new StFcsHit*[kFcsNorthSouth][kFcsEcalMaxId](); //no need for memset with ()
		 for (Int_t i=0; i < nHits; ++i) {
		     if (!hit) continue;
		     const Int_t ns  = hit->volume_id / 1000 - 1;
		     const Int_t id  = hit->volume_id % 1000 - 1;
		     const Int_t det = dbMaker->detectorId(ehp,ns);
		     if(det<0 || det>=kFcsNDet || id<0 || id>=kFcsEcalMaxId){
			 LOG_WARN << Form("ECAL det=%1d id=%3d volid=%5d e=%f out of range (%d)",
					  det,id,hit->volume_id,hit->de,kFcsMaxId) << endm;
			 continue;
		     }else if(mDebug){
			 LOG_INFO << Form("ECAL det=%1d id=%3d volid=%4d e=%f",
					  det,id,hit->volume_id,hit->de) << endm;
		     }
		     Float_t de = hit->de;
		     StFcsHit* fcshit=0;
		     int timebin=0; //for now timebin=0 for sum is assumed
		     if(map[ns][id]==0){ // New hit
			 Int_t ehp=0, rns=0, crt=0, sub=0, dep=0, ch=0;
			 dbMaker->getDepfromId(det, id, ehp, rns, crt, sub, dep, ch);
			 fcshit = new StFcsHit(1, det, id, rns, ehp, dep, ch, de);
			 hits.push_back(fcshit);
			 map[ns][id]=fcshit;
		     }else{ // Adding energy to old hit
			 fcshit = map[ns][id];
			 fcshit->setEnergy(fcshit->energy() + de);
		     }
		     hit++;
		 }
		 delete [] map;
	     }
	 }
     }

     // Read the g2t table for FCS Hcal 
     ehp=1;
     St_g2t_hca_hit* hitTable_h = static_cast<St_g2t_hca_hit*>(GetDataSet(Form("g2t_%3s_hit",name[ehp])));
     if (!hitTable_h) {
	 LOG_INFO << Form("g2t_%3s_hit table is empty",name[ehp]) << endm;
     }else{
	 const Int_t nHits = hitTable_h->GetNRows();
	 ng2thit[ehp]=nHits;
	 LOG_INFO << Form("g2t_%s_hit table has %d hit",name[ehp],nHits) << endm;
	 if(nHits>0){	    
	     const g2t_hca_hit_st* hit = hitTable_h->GetTable();
	     if(!hit){
		 LOG_INFO << Form("g2t_%3s_hit GetTable failed",name[ehp]) << endm;
	     }else{
		 //table to keep pointer to hit for each det & channel
		 auto map = new StFcsHit*[kFcsNorthSouth][kFcsHcalMaxId](); //no need for memset with ()
		 for (Int_t i=0; i < nHits; i++) {
		     if (!hit) continue;
		     const Int_t ns  = hit->volume_id / 1000 - 1;
		     const Int_t id  = hit->volume_id % 1000 - 1;
		     const Int_t det = dbMaker->detectorId(ehp,ns);
		     if(det<0 || det>=kFcsNDet || id<0 || id>=kFcsHcalMaxId){
			 LOG_WARN << Form("HCAL det=%d id=%d volid=%5d e=%f out of range (%d)",
					  det,id,hit->volume_id,hit->de,kFcsMaxId) << endm;
			 continue;
		     }else if(mDebug){
			 LOG_INFO << Form("HCAL det=%d id=%d volid=%5d e=%f",
					  det,id,hit->volume_id,hit->de) << endm;
		     }
		     StFcsHit* fcshit=0;
		     int timebin=0; //for now timebin=0 for sum is assumed
		     Int_t ehp=0, rns=0, crt=0, sub=0, dep=0, ch=0;
		     dbMaker->getDepfromId(det, id, ehp, rns, crt, sub, dep, ch);
		     if(mLeakyHcal==0  || mLeakyHcal==2){
			 Float_t de;
			 de = hit->de;
			 //if(mLeakyHcal==2) de = hit->de2;
			 if(mHcalZdepEff==1) de = hit->deA;
			 if(mHcalZdepEff==2) de = hit->deB;
			 if(map[ns][id]==0){ // New hit
			     fcshit = new StFcsHit(1, det, id, rns, ehp, dep, ch, de);
			     hits.push_back(fcshit);
			     map[ns][id]=fcshit;
			 }else{ // Adding energy to old hit
			     fcshit = map[ns][id];
			     fcshit->setEnergy(fcshit->energy() + de);
			 }
			 hit++;
		     }else{ //leaky hcal with up to 4 WLSP getting lights from a tower
			 float de[4];
			 if(mLeakyHcal==1){
			     de[0] = hit->deA;
			     de[1] = hit->deB;
			     de[2] = hit->deC;
			     de[3] = hit->deD;
			 }else{
			     //de[0] = hit->de2A;
			     //de[1] = hit->de2B;
			     //de[2] = hit->de2C;
			     //de[3] = hit->de2D;
			 }
			 int col = dbMaker->getColumnNumber(det,id);	// col goes 1 ~ ncol
			 int ncol= dbMaker->nColumn(det);
			 for(int j=0; j<4; j++){
			     int id2;
			     int jj = j-2;; //jj goes from -2 ~ +1
			     if(col==1 && jj<0)          {id2=id;}    // if col=1, add leaked light back to col=1
			     else if(col==2 && jj==-2)   {id2=id-1;}  // if col=2, add leaked light back to col=1
			     else if(col==ncol && jj==1) {id2=id;}    // if col=ncol, add leaked light back to col=ncol
			     else                        {id2=id+jj;} // add leaked lights to its neighbors
			     dbMaker-> getDepfromId(det, id2, ehp, rns, crt, sub, dep, ch);
			     if(map[ns][id2]==0){ // New hit
				 Int_t adc=0, ehp=0, rns=0, crt=0, sub=0, dep=0, ch=0;
				 dbMaker-> getDepfromId(det, id2, ehp, rns, crt, sub, dep, ch);
				 fcshit = new StFcsHit(1, det, id2, rns, ehp, dep, ch, de[j]);
				 hits.push_back(fcshit);
				 map[ns][id2]=fcshit;
			     }else{ // Adding energy to old hit
				 fcshit = map[ns][id2];
				 fcshit->setEnergy(fcshit->energy() + de[j]);
			     }
			 }
			 hit++;
		     }
		 }
		 delete [] map;
	     }
	 }
     }

     // Read the g2t table for FCS Pres == EPD now
     ehp=2;
     //St_g2t_emc_hit* hitTable_p = static_cast<St_g2t_emc_hit*>(GetDataSet(Form("g2t_%3s_hit",name[ehp])));
     St_g2t_ctf_hit* hitTable_p = static_cast<St_g2t_ctf_hit*>(GetDataSet("g2t_epd_hit"));
     if (!hitTable_p) {
	 LOG_INFO << Form("g2t_epd_hit table is empty") << endm;
     }else{
	 const Int_t nHits = hitTable_p->GetNRows(); 
	 ng2thit[ehp]=nHits;
	 LOG_INFO << Form("g2t_epd_hit table has %d hit",nHits) << endm;
	 if(nHits>0){
	     const g2t_ctf_hit_st* hit = hitTable_p->GetTable();
	     if(!hit){
		 LOG_INFO << Form("g2t_epd_hit GetTable failed") << endm;
	     }else{
		 //table to keep pointer to hit for each det & channel
		 auto map = new StFcsHit*[kFcsNorthSouth][kFcsPresMaxId](); //no need for memset with ()
		 for (Int_t i=0; i < nHits; ++i) {
		     if (!hit) continue;		     
		     const int volume_id = hit->volume_id;
		     const int ew        = volume_id/100000;  
		     const int pp        = (volume_id%100000)/1000;
		           int tt        = (volume_id%1000)/10;
		     const float de      = hit->de * 1000.0;
		     if(ew==1) continue; //west side only
		     //hack! reverse TT Even/Odd
		     if(tt>1){
			 if(tt%2==0) {tt+=1;}
			 else        {tt-=1;}
		     }
		     int det,id,ehp,ns,crt,slt,dep,ch;
		     dbMaker->getIdfromEPD(pp,tt,det,id);
		     dbMaker->getDepfromId(det,id,ehp,ns,crt,slt,dep,ch);
		     if(det<0 || det>=kFcsNDet || id<0 || id>=kFcsPresMaxId){
			 LOG_WARN << Form("Pres det=%1d id=%3d volid=%4d e=%f10.6  id out of range (%d)",
					  det,id,hit->volume_id,1000*hit->de,kFcsPresId) << endm;
			 continue;
		     }else if(mDebug){
			 LOG_INFO << Form("Pres det=%1d id=%3d volid=%4d e=%10.6f",
					  det,id,hit->volume_id,1000*hit->de) << endm;
		     }
		     StFcsHit* fcshit=0;
		     int timebin=0; //for now timebin=0 for sum is assumed
		     if(map[ns][id]==0){ // New hit
			 fcshit = new StFcsHit(1, det, id, ns, ehp, dep, ch, de);
			 hits.push_back(fcshit);
			 map[ns][id]=fcshit;
		     }else{ // Adding energy to old hit
			 fcshit = map[ns][id];
			 fcshit->setEnergy(fcshit->energy() + de);
		     }
		     hit++;
		 }
		 delete [] map;
	     }
	 }
     }

     int nhittot=hits.size();
     int zs=0;
     int nhit[kFcsEHP]={};
     float etot[kFcsEHP]={};
     // Loop over hits and digitize
     for(int i=0; i<nhittot; i++){
	 const Int_t det = hits[i]->detectorId();
	 const Int_t id = hits[i]->id();
	 float de  = hits[i]->energy();
	 float sf  = dbMaker->getSamplingFraction(det);
	 float gain= dbMaker->getGain(det, id);
	 float corr= dbMaker->getGainCorrection(det, id);
	 int adc = static_cast<Int_t>(de / (sf * gain * corr));
	 adc = std::min(adc, 4095*8);     // Cap maximum ADC =12bit * 8 tim
	 zs  = dbMaker->getZeroSuppression(det);
	 if(mDebug) LOG_INFO << Form("Det=%1d id=%3d dE=%8.3f SF=%6.3f gain=%6.3f corr=%6.3f ADC=%4d ZS=%2d digiE=%8.3f",
				     det,id,de,sf,gain,corr,adc,zs,adc*gain*corr) << endm;
	 if(adc>zs){ // zero suppress low ADC
	     float digi_energy = adc * gain * corr;
	     int ehp = dbMaker->ecalHcalPres(det);
	     hits[i]->setAdc(0,adc);
	     hits[i]->setEnergy(digi_energy);
	     fcscollection->addHit(det,hits[i]); 
	     etot[ehp] += digi_energy;
	     nhit[ehp]++;
	 }else{
	     delete hits[i];
	}	    
    }

    for(int ehp=0; ehp<kFcsEHP; ehp++){
	LOG_INFO << Form("%s Found %d g2t hits in %d cells, created %d hits with ADC>ZS(%d) and Etot=%8.3f",
			 name[ehp],ng2thit[ehp],nhit[ehp],
			 fcscollection->numberOfHits(ehp*2) + fcscollection->numberOfHits(ehp*2+1),
			 zs, etot[ehp]) 
		 << endm;
    }
    //fcscollection->print();
}
