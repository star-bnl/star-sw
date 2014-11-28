//  $Id: StEmcAsciiDbMaker.cxx,v 1.6 2012/12/12 22:00:59 fisyak Exp $ 
// Emulates  L2 in offline for algorithm testing
// Interface to online/L2jetAlgo/
// Jan Balewski, Fall 2005

#ifdef __APPLE__
#include <sys/types.h>
#endif
#include <stdio.h>
#include <TH2.h>
#include <TFile.h>

#include <StMessMgr.h>

#include "StEmcAsciiDbMaker.h"
#include "StEmcRawMaker/defines.h"

#include <StEEmcUtil/database/StEEmcDb.h>
#include <StEEmcUtil/database/EEmcDbItem.h>
#include <StEEmcUtil/database/EEmcDbCrate.h>
//#include <St_db_Maker/St_db_Maker.h>

// BTOW stuff
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/database/StBemcTables.h"
#include "StEmcUtil/database/StEmcDecoder.h"


ClassImp(StEmcAsciiDbMaker)

//========================================
//========================================
  StEmcAsciiDbMaker::StEmcAsciiDbMaker() : StMaker("StEmcAsciiDb") {

  mGeomB =0;
  mEeDb=0;
  mMappB=0;
  mHList=0;
  mgain60Et=false;

  memset(hA,0,sizeof(hA));
  /* Alexandre A. P. Suaide wrote:
     The default if false for 2004/2005 runs and true
     for 2006 and future runs. You use true *only* if
     you would like to have the map corrected at decoder
     level for the 2005 data. */ 

}

//========================================
//========================================
StEmcAsciiDbMaker::~StEmcAsciiDbMaker() {
} 

//========================================
//========================================
Int_t StEmcAsciiDbMaker::Init() {

  //................EEMC stuff ..............
  mEeDb = (StEEmcDb*)this->GetDataSet("StEEmcDb");
  assert(mEeDb);
  mGeomB = StEmcGeom::instance("bemc");
  initAuxHisto();
  LOG_INFO << Form(" %s::Init()  setGain60Et=%d",GetName(),mgain60Et) <<endm;
  
  mMappB =0;
  return kStOk;
}

//========================================
//========================================
Int_t StEmcAsciiDbMaker::InitRun(int runNo){
  int yyyymmdd = this->GetDBTime().GetDate();
  int hhmmss = this->GetDBTime().GetTime();


  TString outPath="L2setup-"; outPath+=yyyymmdd; //<--- +yyyymmdd
  printf("outPath=%s=\n",outPath.Data());
  LOG_INFO << GetName()<<"::InitRun()  run=" <<runNo<<" outPath="<<outPath<<endm;

  // this is how BTOW mapping is accesible
  assert( mMappB==0) ; // do not know how to destroy previous instance,JB
  mMappB = new StEmcDecoder(yyyymmdd,hhmmss);
  //one can drop  mTowerMapBug and use default constructor- siad Pibero
  
  exportBtowDb(outPath+"/btowDb.current",runNo,yyyymmdd,hhmmss);
  exportEtowDb(outPath+"/etowDb.current",runNo,yyyymmdd,hhmmss);
  
  return kStOk;
} 



//========================================
//========================================
Int_t StEmcAsciiDbMaker::Make() {
  //empty
  return kStOk;

}

//========================================
//========================================
void StEmcAsciiDbMaker::Clear(const Option_t* ){
}

//========================================
//========================================
Int_t StEmcAsciiDbMaker::Finish() {
  return kStOk;
}


//========================================
//========================================
void StEmcAsciiDbMaker::exportBtowDb(TString fname, int runNo, int yyyymmdd,int hhmmss){
  LOG_INFO << GetName()<<"::exportBtowDb start -->"<<fname<<endm;
  
  // for calulation of  ideal gains @ maxEt=60 GeV - if needed
  const int par_maxADC=4096; // chan
  const float par_maxEt=60; // GeV Et

  StBemcTables *myTable=new StBemcTables;
  myTable->loadTables(this );

  FILE *fd=fopen(fname.Data(),"w");
  assert(fd);
  fprintf(fd,"# ::exportBtowDb(), time stamp:   day=%d  hhmmss=%06d run=R%d\n",yyyymmdd,hhmmss, runNo);
  if(mgain60Et)  fprintf(fd,"# ideal gains(ch/GeV) set at maxEt=60 GeV\n"); 
  else fprintf(fd,"# real gains(ch/GeV) from OFL DB\n"); 
 
  fprintf(fd,"# see online/EmcDbAnsiC/EmcCDbItem.h  for definitions\n");
  fprintf(fd,"#name,  crate,chan, sec,sub,eta, gain,    ped,  thr,   stat,fail, id-m-s-e,   RDO\n");

  int softID;
  int nB=0,nA=0,nC=0;
  for(softID=1; softID<=BTOWSIZE; softID++) {
    nA++;

    int RDO, CR,CHAN;
    assert(mMappB->GetDaqIdFromTowerId(softID,RDO)==1);// is good range
    assert(mMappB->GetTowerCrateFromDaqId(RDO,CR,CHAN)==1);
    //  printf("soft=%d DRO=%d CR=%d CHAN=%d\n",softID,RDO,CR,CHAN);

    //........... querry BTOW DB/geom
    int  status;
    myTable->getStatus(BTOW, softID, status);
    int m,e,s;
    mGeomB->getBin(softID,m,e,s);
    
    float etaF,phiF;
    mGeomB->getEta(m,e,etaF);
    mGeomB->getPhi(m,s,phiF);  // -pi <= phi < pi
    if( phiF<0) phiF+=2*C_PI; // I want phi in [0,2Pi]
    // printf("soft=%4d   DRO=%4d CR=0x%02x CHAN=%3d    eta=%.2f phi/deg=%.1f\n",softID,RDO,CR,CHAN,etaF,phiF/3.1416*180.);
    
    float ped,sig;
    myTable->getPedestal(BTOW, softID, 0, ped,sig);
    
    float gain;
    myTable->getCalib(BTOW, softID, 1, gain); 
        
    //........... convert it to private bins
 

    assert(fabs(etaF)<0.99);
    int kEta=1+(int)((etaF+1.)/0.05);
    
    int kPhi=24-(int)( phiF/C_PI*60.);
    if(kPhi<0) kPhi+=120;
    // convention:  kPhi=[0,119], kEta=[1,40]

    char name[10]="nnn", pname[100];
    int sec=1+kPhi/10;
    char sub='a'+kPhi%10;
    sprintf(name,"%02dt%c%02d",sec,sub,kEta);

    hA[2]->Fill(softID,1000*kPhi+(kEta-1));
    hA[3]->SetBinContent(kEta,kPhi+1,softID);

    //  printf("phiF/deg=%.1f  kPhi=%d %s\n",phiF/C_PI*180.,kPhi,name);    
     float myGain=-2;
     if(gain>0) {
       myGain=1/gain;
       nC++;
     }

     if(mgain60Et) myGain=par_maxADC/par_maxEt/cosh(etaF); // ideal gains?

     // printf("%s %f %d\n",name,myGain,nC);    assert(2==3);
     ushort stat=0;
     ushort fail=0;
     if(status==1) {
       nB++;
       float ph=phiF/C_PI*180.;
       while(ph<0) ph+=360.;
      ((TH2F*) hA[0])->Fill(etaF,ph,myGain);
     }

     float thr=ped+mEeDb->getKsigOverPed()*sig;
     sprintf(pname,"id%04d-%03d-%d-%02d",softID,m,s,e);
     fprintf(fd,"%6s   0x%02x 0x%02x   %2d %c %2d   %8.2f   %5.2f %5.2f  0x%02x 0x%02x  %8s %4d\n",
	    name,CR,CHAN,sec,sub,kEta,myGain,ped,thr,stat,fail,pname,RDO);
     //     if(softID>100) break;
  }
  fclose(fd);
 
  LOG_INFO << Form("exportBtowDb -->%s ,nTw=%d nNotFail=%d nGoodGain=%d",fname.Data(),nA,nB,nC) <<endm;

#if  0 // tmp, somehow HList does not write, fix it
  TString outName="btowMap.hist.root";
  TFile f( outName,"recreate");
  assert(f.IsOpen());
  printf("BTOW map histos are written  to '%s' ...\n",outName.Data());
  hA[2]->Write();
  hA[3]->Write();
  f.Close();
 
#endif
 

}

//========================================
//========================================
void StEmcAsciiDbMaker::exportEtowDb(TString fname, int runNo, int yyyymmdd,int hhmmss){
  LOG_INFO << GetName()<<"::exportEtowDb start -->"<<fname<<endm;
 
  // for calulation of  ideal gains @ maxEt=60 GeV - if needed
  const int par_maxADC=4096; // chan
  const float par_maxEt=60; // GeV Et
  const float etaF[12]={1.95,1.855,1.765,1.675,1.59,1.51,1.435,1.365,1.3,1.235,1.17,1.115};

  // not that nice redundance, JB
  const int cETOW_DATSIZE=160;
  const int cETOW_MAXFEE=6;

  FILE *fd=fopen(fname.Data(),"w");
  assert(fd);
  fprintf(fd,"# ::exportEtowDb(), time stamp:   day=%d  hhmmss=%06d run=R%d\n",yyyymmdd, runNo,hhmmss);

  if(mgain60Et)  fprintf(fd,"# ideal gains(ch/GeV) set at maxEt=60 GeV\n"); 
  else fprintf(fd,"# real gains(ch/GeV)  from OFL DB\n"); 

  fprintf(fd,"# see online/EmcDbAnsiC/EmcDbItemStruct.h  for definitions\n");
  fprintf(fd,"#name,  crate,chan, sec,sub,eta, gain,    ped,  thr,   stat,fail, PsoftId,   RDO\n");
  int nB=0,nA=0,nC=0;
  int icr;

  for(icr=0;icr<mEeDb->getNFiber();icr++) {
    const EEmcDbCrate *fiber=mEeDb->getFiber(icr);
    if(fiber->crID>6) continue; // drop non-tower crates
    assert(fiber->useIt); // code not ready to handle masked crates
    assert(fiber->crID==icr+1);
    int i;
    for(i=0;i<cETOW_DATSIZE;i++) {
      const EEmcDbItem *x=mEeDb->getByCrate(fiber->crID,i);
      if(x==0) continue;
      nA++;
      int rdo = icr + i*cETOW_MAXFEE;

      assert(x->eta>0 && x->eta<=12);

      float myGain=x->gain;
      if(mgain60Et) myGain=par_maxADC/par_maxEt/cosh(etaF[x->eta-1]); // ideal gains?


      fprintf(fd,"%6s   0x%02x 0x%02x   %2d %c %2d   %8.2f   %5.2f %5.2f  0x%02x 0x%02x  %8s %4d\n",
	      x->name,fiber->crID,x->chan,x->sec,x->sub,x->eta,myGain,x->ped,x->thr,0,0,x->tube,rdo);
      if( !x->fail) { 
	nB++;
	float ph=5*(x->sec-1)+x->sub-'A';
	((TH2F*) hA[1])->Fill(13-x->eta,ph,myGain);
      }

      //tmp  else printf("bad %s stat/fail:  0x%02x 0x%02x\n", x->name,x->stat,x->fail); //tmp
      if(x->gain>0)   nC++;
    }
  }
  fclose(fd);
  
  LOG_INFO << Form("exportEtowDb -->%s ,nTw=%d nNotFail=%d nGain=%d",fname.Data(),nA,nB,nC) <<endm;
}



//========================================
//========================================
void StEmcAsciiDbMaker::initAuxHisto() {
  assert(mHList);
  hA[0]=new TH2F("bGn","BTOW gains from DB in [ch/GeV] ; eta; STAR  phi (deg)",40,-1.,1.,120,0,360);

  hA[1]=new TH2F("eGn","ETOW gains from DB in [ch/GeV] ; eta bins reversed, (eta[+1,+2]); phi bin (5*sec-1 +sub-1)",12,0.5,12.5,60,-0.5,59.5);
 

  hA[2]=new TH1I("L2mapBTOWrev","map BTOW(softID) --> 1000*#phi_{bin} + #eta_{bin} , L2 di-jet convention; BTOW softID; composite {#eta,#phi} index ",4800,0.5,4800.5); 

  hA[3]=new TH2I("L2mapBTOW","map BTOW(#eta_{bin},#phi_{bin}) --> softID , L2 di-jet convention ;BTOW #eta_{bin} ;BTOW #phi_{bin} ",40,-0.5,39.5,120,-0.5,119.5); 


  for(int i=0;i<4;i++) mHList->Add( hA[i]);
  mHList->Print();

}

/* *******************************
 $Log: StEmcAsciiDbMaker.cxx,v $
 Revision 1.6  2012/12/12 22:00:59  fisyak
 add sys/types.h include for APPLE

 Revision 1.5  2009/02/26 20:30:54  balewski
 now mask from DB s NOT written to asscii files

 Revision 1.4  2009/02/04 20:33:06  ogrebeny
 Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388

 Revision 1.3  2008/12/06 13:20:42  balewski
 fix of histo code, was non-fatal

 Revision 1.2  2008/07/31 14:18:37  balewski
 saves BTOW reative location mapping, needs manual activatio

 Revision 1.1  2006/03/09 01:33:10  balewski
 start

*/

