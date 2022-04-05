#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <cmath>

#include <StMessMgr.h>

#include "BemcHitList.h"

#include "StMaker.h"
#include "StEmcRawMaker/StBemcTables.h"
#include "StEmcRawMaker/defines.h" 
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcDetector.h"
#include "StEmcModule.h"
#include "StEmcRawHit.h"
//Rxy = 222, 242, 262 cm


namespace StEvPPV {
//==========================================================
//==========================================================
BemcHitList::BemcHitList() :
  ScintHitList(0.,M_PI/60,120, -1.,0.05,40,(char *) "Bemc",4,0.75) {
  myTable = new StBemcTables();
  geomB = StEmcGeom::instance("bemc");
  kSigPed=5.0; 

  gMessMgr->Message("","I") 
    <<"  BemcHitList::use kSigPed="<<kSigPed
    <<endm;
}


//==========================================================
//==========================================================
void
BemcHitList::initRun() {
  LOG_DEBUG <<Form("BemcHitList::initRun() start")<<endm;
  ScintHitList::initRun();
  // clear old lookup table
  int i,j,k;
  for(i=0;i<mxm;i++)
    for(j=0;j<mxe;j++)
      for(k=0;k<mxs;k++)
	mes2bin[i][j][k]=-1;

  // .. grab table
  StMaker*mk=(StMaker*)StMaker::GetChain()->GetMaker("GenericVertex");
  assert(mk);
  myTable->loadTables(mk  );

  int nB=0,nA=0;
  int id;
  for(id=1; id<=BTOWSIZE; id++) {

    //........... querry BTOW DB/geom
    int  status;
    myTable->getStatus(BTOW, id, status);
    int m,e,s;
    geomB->getBin(id,m,e,s);
    float eta,phi;
    geomB->getEta(m,e,eta);
    geomB->getPhi(m,s,phi);  // -pi <= phi < pi
    if( phi<0) phi+=2*M_PI; // I want phi in [0,2Pi]
    
    //........... convert it to private bins
    int iEta=etaBin(eta);
    int iPhi=phiBin(phi);
    assert( iEta>=0);
    assert( iPhi>=0);
    int iBin=iPhiEta2bin(iPhi,iEta);

    // if(id<41)
    //       printf("%d %d id=%d m,e,s=%d:%d:%d eta=%.4f phi/deg=%.2f bin %d:%d:%d\n",
    //		   eta<0 && phi>M_PI,  status, id,m,e,s,eta,phi/M_PI*180,iPhi,iEta,iBin);    
    assert(mes2bin[m-1][e-1][s-1]==-1); //tmp, to check BTOW code
    mes2bin[m-1][e-1][s-1]=iBin;

    nB++;
    if( status!= 1)  continue; // drop broken towers

    setActive(iBin);
    nA++;
  }

  LOG_INFO <<" BemcHitList::initRun() done,  active="<<nA<<" of "<<nB<<" BTOW towers" <<endm;

}

//==========================================================
//==========================================================
void
BemcHitList::clear(){
  ScintHitList::clear();

}

//==========================================================
//==========================================================
int
BemcHitList::etaBin(float eta){
  if(fabs(eta)>0.99)  return -1; // 20th eta bin is only 0.04 wide in eta
  int iEta=(int)((eta-eta0)/dEta); 
  if(iEta<0 || iEta>=nEta) return -1; // out of Eta range
  return iEta;
}

//==========================================================
//==========================================================
float
BemcHitList::bin2EtaLeft(int iEta){
  assert(iEta>=0);
  assert(iEta<nEta);
  float etaF= eta0+iEta*dEta ;
  if(etaF<-0.99) etaF=0.99;
  return etaF;
}


//==========================================================
//==========================================================
BemcHitList::~BemcHitList(){

}

//==========================================================
//==========================================================
void
BemcHitList::build ( StEmcDetector*det, float adcMin){
 for(int m = 1; m<=BEMCMODULES;m++) { //loop on modules...
    StEmcModule* module = det->module(m);
    assert(module);
    StSPtrVecEmcRawHit& rawHit=module->hits();
    for(UInt_t k=0;k<rawHit.size();k++) { //loop on hits in modules
      // check geometry
      int m=rawHit[k]->module();
      int e=rawHit[k]->eta();
      int s=abs(rawHit[k]->sub());
      int iBin=mes2bin[m-1][e-1][s-1];
      
      if ( getActive(iBin)<0) continue;

      int id;
      geomB->getId(m,e,s,id); // to get the software id    
      Float_t ped,sig;
      myTable->getPedestal(BTOW, id, 0, ped,sig); 
      float rawAdc = rawHit[k]->adc();
      if( rawAdc<ped+ kSigPed*sig ) continue;
      float adc= rawAdc -ped;
      if(adc< adcMin) continue;
      setFired(iBin);
    }
 }

};
}// end namespace StEvPPV

/*
  I think you want something like:
  myTable->getCalib(BTOW, id, 1, gain);  
  getCalib instead of getGain. It is a bit confiusing, but there are two 
  sets of calibration constants. The ones called 'calib' are the ones you 
  want. The 'gain tables' coudl in principle hold a second, relative gain 
  calibration (that might vary on a shorter timescale), but this option is 
  not used at the moment, so they are all one.
*/
