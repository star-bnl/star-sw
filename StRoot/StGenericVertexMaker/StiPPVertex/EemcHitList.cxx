#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <cmath>

#include <St_base/StMessMgr.h>
#include "St_db_Maker/St_db_Maker.h"

#include "StGenericVertexMaker/StiPPVertex/EemcHitList.h"

#include "StChain/StMaker.h"
#include "StEEmcUtil/database/StEEmcDb.h"
#include "StEEmcUtil/database/EEmcDbItem.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"


#include "StEvent/StEmcDetector.h"
#include "StEvent/StEmcModule.h"
#include "StEvent/StEmcRawHit.h"


//Z=270, 288,306 cm 


//==========================================================
//==========================================================
EemcHitList::EemcHitList(StEEmcDb* x, unsigned int y, EEmcGeomSimple *z) :
  ScintHitList(-M_PI/60.,M_PI/30,60, 999,999,8,(char *) "Eemc",4,0.75) {
  eeDb = x ? x : new StEEmcDb();
  killStatEEmc=y ;
  geomE = z ? z : new EEmcGeomSimple();
  assert(eeDb);
  assert(geomE);
  assert(nEta<=MaxEtaBins);
  float  kSigPed=5.0; 
  eeDb-> setThreshold( kSigPed);
  // the first 13 entries mark the bounds of the 12 eta Bins. 
  etaHL= geomE->getEtaBinRangeArray();
  gMessMgr->Message("","I") 
    <<"  EemcHitList::use kSigPed="<<kSigPed
    <<endm;
}


//==========================================================
//==========================================================
void
EemcHitList::initRun(St_db_Maker* db_maker){
  gMessMgr->Message("","D") <<" EemcHitList::initRun()"<<endm;
  ScintHitList::initRun();

  // clear old lookup table
  int sec,sub,etaB;
  for(sec=0;sec<MaxSectors;sec++)
    for(sub=0;sub<MaxSubSec;sub++)
      for(etaB=0;etaB<MaxEtaBins;etaB++) 
	name2bin[sec][sub][etaB]=-1;

  eeDb->loadTables(db_maker);

  // fill in new association
  int nB=0,nA=0;
  for(sec=1;sec<=MaxSectors;sec++)
    for(sub='A';sub<='E';sub++)
      for(etaB=1;etaB<=MaxEtaBins;etaB++) {
	//Db ranges: sec=1-12,sub=A-E,eta=1-12; slow method
	const EEmcDbItem *x=eeDb->getT(sec,sub,etaB);
	if(x==0) continue;
	nB++;
	if(x->fail ) continue;  // drop broken channels
	if(x->stat &  killStatEEmc) continue; // other problems
	
	int ieta=x->eta-1; // typical endcap bin numbering
	int isec=x->sec-1, isub=x->sub-'A';
	float eta=geomE->getEtaMean(ieta);  
	float phi=geomE->getPhiMean(isec,isub); //   // -pi <= phi < pi
	if( phi<0) phi+=2*M_PI; // I want phi in [0,2Pi]

	//........... convert it to private bins
	int iEta=etaBin(eta); // unique vertx finder bin numbering
	int iPhi=phiBin(phi);
	//printf("EE eta=%.3f iEta=%d , phi/deg=%.1f iPhi=%d name=%s\n",eta,iEta,phi/3.1416*180,iPhi,x->name);// x->print();
	if(iEta<0) continue; // out of assumed sensitivity range

	//printf("EE tower is good\n");
 
	assert( iEta<nEta);
	assert( iPhi>=0);

	int iBin=iPhiEta2bin(iPhi,iEta);
	assert(name2bin[isec][isub][ieta]==-1); //tmp, to check ETOW code
	name2bin[isec][isub][ieta]=iBin;

	//tmp
	assert(iBin>=0);
	assert(iBin<nBin);
	assert( active[iBin]==0 );
	// tmp end

	setActive(iBin);
	nA++;
      }
  
  LOG_INFO <<" EemcHitList::initRun() done,  active="<<nA<<" of "<<nB<<" ETOW  towers (only 8 upper eta rings are used) " <<endm; 
  
}

//==========================================================
//==========================================================
void
EemcHitList::clear(){
  ScintHitList::clear();
}

//==========================================================
//==========================================================
int
EemcHitList::etaBin(float eta){

  // in this aplication only 'nBin' ,counted from eta=1. --> eta=2., are condidered to be valid. nBin should be ~8, since no tracking is expected at eta >1.6
  
  if(eta <etaHL[12]) return -1; // missed EEMC
  int i;
  for(i=0;i<nEta;i++) {
    if(eta< etaHL[11-i]) return i;
  }
  return -1;
}
//==========================================================
//==========================================================
float
EemcHitList::bin2EtaLeft(int iEta){
  assert(iEta>=0);
  assert(iEta<nEta);
  float etaF=  etaHL[11-iEta] ;
  return etaF;
}


//==========================================================
//==========================================================
EemcHitList::~EemcHitList(){

}

//==========================================================
//==========================================================
void
EemcHitList::build ( StEmcDetector*det, float adcMin){
  for(unsigned int mod=1;mod<=det->numberOfModules();mod++) {
    StEmcModule*     module=det->module(mod);
    //printf("ETOW sector=%d nHit=%d\n",mod,module->numberOfHits());
    StSPtrVecEmcRawHit&     hit=  module->hits();
    int sec=mod; // range 1-12
    for(unsigned int ih=0;ih<hit.size();ih++){ // over cesctors
      StEmcRawHit *h=hit[ih];
      char sub='A'+h->sub()-1;// range A-E
      int eta=h->eta();// range 1-12
      int rawAdc=h->adc();
      //Db ranges: sec=1-12,sub=A-E,eta=1-12; slow method
      const EEmcDbItem *x=eeDb->getT(sec,sub,eta);
      if(x==0) continue;
      if(x->fail ) continue;  // drop broken channels
      if(x->stat &  killStatEEmc) continue; // other problems
      // functioning chan

      int iBin=name2bin[sec-1][sub-'A'][eta-1];
      if ( getActive(iBin)<0) continue;
      
      if(rawAdc< x->thr) continue; // still only ped
      float adc=rawAdc - x->ped ;
      if(adc < adcMin) continue; // too low response for MIP
      // printf("add Eemc hit %s adc=%.1f\n",x->name,adc);

      setFired(iBin);
    } // end of sector
    //  printf("sect=%d so far: nActive=%d nHit=%d\n",sec,nAct,nHit);
  }
};
 
