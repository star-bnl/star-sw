//*-- Author : Victor Perevoztchikov
// 
// $Id: StJanBarrelDbMaker.cxx,v 1.5 2010/04/15 19:13:30 mattheww Exp $
#include <stdio.h>
#include <TFile.h>
#include <TH1.h>
#include <TH2.h>

#include "StJanBarrelDbMaker.h"

#include "StEmcRawMaker/defines.h"
#include "StEmcUtil/database/StBemcTables.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/database/StEmcDecoder.h"

ClassImp(StJanBarrelDbMaker)

//____________________________________________________________________
//____________________________________________________________________
StJanBarrelDbMaker::StJanBarrelDbMaker(const char *name):StMaker(name){
  mMappB=0;
  setDbType(kStarDb); setHList(0);
}


//____________________________________________________________________
//____________________________________________________________________

StJanBarrelDbMaker::~StJanBarrelDbMaker(){
  //
}


//____________________________________________________________________
//____________________________________________________________________

Int_t StJanBarrelDbMaker::Init(){
  LOG_INFO<<Form("::Init() ")<<endm;
  assert(HList);

  mBtowGeom = StEmcGeom::instance("bemc");
  mBprsGeom = StEmcGeom::instance("bprs");
  mSmdEGeom = StEmcGeom::instance("bsmde");
  mSmdPGeom = StEmcGeom::instance("bsmdp"); // needed only to map m-s-e --> softID
  mTables = new StBemcTables();

  initBprsGeometry();  

  //  janDb_bprsSoft2Hard=new TH1I("bprsMapS2H","BPRS software mapping Y=icr*5000 + position; BPRS softID",mxBtow,0.5, 0.5+mxBtow);
  //  HList->Add(janDb_bprsSoft2Hard);

  return StMaker::Init();
}

//____________________________________________________________________
//____________________________________________________________________

Int_t StJanBarrelDbMaker::InitRun  (int runNo){
  LOG_INFO<<Form("::InitRun(%d) algo params:dbType=%d",
		 runNo, par_dbType
		 )<<endm;
  
  // St_db_Maker* mydb =(St_db_Maker*) StMaker::GetChain()->GetMaker("StarDb");
  //assert(mydb);

  mMappB = new StEmcDecoder(GetDateTime().GetDate(),GetDateTime().GetTime());
  
  mTables->loadTables(this ); 
  janDb_bprsPed2D=0;
  janDb_bprsReMap=janDb_btowReMap=0; // optional mapping correction

  if(par_dbType==kJanDb ) { //  
    assert(par_dbType==kJanDb); // other options not implemnented,
    int pedRun=runNo;    
    

    LOG_INFO<<"upload local peds from  run"<<pedRun<<endm;
    char txt[100];

    const char *pedPath="/star/institutions/mit/balewski/2008-janDb-BprsPeds-v3.1";
    sprintf(txt,"%s/pedBprsR%d-allCap.hist.root",pedPath,pedRun);
    TFile* fd=new TFile(txt);  assert(fd->IsOpen());
    fd->ls();   
    janDb_bprsPed2D=(TH2S *)fd->Get("pedBPRScap"); assert(janDb_bprsPed2D);
    janDb_bprsSigPed2D=(TH2S *)fd->Get("rmsPedBPRScap"); assert(janDb_bprsSigPed2D);
    janDb_bprsStat=(TH1S *)fd->Get("statBPRSallCap"); assert( janDb_bprsStat); // use stats from every run

    const char *path= "calib-jan3.1/";

    sprintf(txt,"%s/mipGainBprs+Btow_v2.hist.root",path);
    TFile* fd5=new TFile(txt);  assert(fd5->IsOpen());
    fd5->ls();   
    const char *core[mxBTile]={"btow","bprs"};
    for(int ibp=0;ibp<mxBTile;ibp++) {
      TString tit=core[ibp]; tit+="MipGain";
      janDb_mipMean[ibp]=(TH1F *)fd5->Get(tit); assert(   janDb_mipMean[ibp]);
      tit=core[ibp]; tit+="MipSig";
      janDb_mipSig[ibp]=(TH1F *)fd5->Get(tit); assert(   janDb_mipSig[ibp]);
      tit=core[ibp]; tit+="MipStat";
      janDb_mipStat[ibp]=(TH1F *)fd5->Get(tit); assert(   janDb_mipStat[ibp]);
    }


    //Temp1 - use private mapping fix
    sprintf(txt,"%s/bprsReMap-ver3.hist.root",path);
    TFile* fd3=new TFile(txt);  assert(fd3->IsOpen());
    janDb_bprsReMap=(TH1I *)fd3->Get("bprsReMap");  assert(janDb_bprsReMap);
    printf("use BPRS softID corection: %s\n",txt);
  
    
    //Temp1b - use private mapping fix
    sprintf(txt,"%s/btowReMap-ver3.hist.root",path);
    TFile* fd4=new TFile(txt);  assert(fd4->IsOpen());
    janDb_btowReMap=(TH1I *)fd4->Get("btowReMap"); assert(janDb_btowReMap);
    printf("use BTOW softID corection: %s\n",txt);
  
    // compute MIP peak limits based on gains
    // save results to log file
    
    for(int ibp=0;ibp<mxBTile;ibp++) {
      for(int id=1;id<=mxBtow;id++) {
	float adcL=5., adcH=40.; // default for _BTOW_
	if(ibp==kBPrs) adcL=3.5; // BPRS
	int statGain=(int)mipStat(ibp)->GetBinContent(id);
	if(!statGain){ //valid gain
	  float mean=mipMean(ibp)->GetBinContent(id);
	  float sig=mipSig(ibp)->GetBinContent(id);
	  adcL=mean-sig;
	  adcH=mean+sig;
	  if(ibp==kBTow && adcL<5) adcL=5;
	  if(ibp==kBPrs && adcL<3.5) adcL=3.5;
	  if(adcH>2*mean) adcH=2*mean;
	  if(id%77==0) 
	    printf("%s id=%4d MIP mean=%.1f sig=%.1f adcL=%.1f H=%.1f\n",core[ibp],id,mean,sig,adcL,adcH);

	}
	cut_mipAdcL[ibp][id-1]=adcL;
	cut_mipAdcH[ibp][id-1]=adcH;
      } //Loop over tiles
    } // Loop over B/P 

    LOG_INFO<<Form("::InitRun() Load private DB, nBad BPRS=%.0f",janDb_bprsStat->GetEntries())<<endm;
 
    printf("WARN: tmp disable 2 BPRS & 1 tiles\n");
    //  tower 1075 is masked in getStat();

    janDb_bprsStat->SetBinContent(2821,99); // 2 channels overwritten by bug in mapping
    janDb_bprsStat->SetBinContent(3781,99);   
 

 } // end of JAN-DB
  

#if 0  
  // test1 of BPRS mapping
  int RDO;
  int posInFiber;
  int softId;
  int PMTbox;
  int wire;
  int A_value;
  
  for(softId=650; softId<750;softId++) {
    int newId;
    mMappB->GetPsdRDO(softId, RDO, posInFiber);      
    mMappB->GetPsdId( RDO,  posInFiber, newId,PMTbox,wire, A_value, true);
    assert(newId==softId);
    //printf("Bmap soft=%d rdo=%d posInFib=%d \n", softId,RDO,posInFiber);
  }

  // test2 of BPRS mapping after my swaps
  printf("JJJBprs softID rdo posInFib \n");

  for(softId=1; softId<=4800;softId++) {
    mMappB->GetPsdRDO(softId, RDO, posInFiber);
    int newId=(int)bprsReMap()->GetBinContent(softId);
    // fix to known channels
    if(newId==4525) { RDO=2; posInFiber=487; }
    if(newId==4526) { RDO=2; posInFiber=507; }
    printf("JJJBprs %4d %2d %4d \n", newId,RDO,posInFiber);
    janDb_bprsSoft2Hard->SetBinContent(softId, RDO*5000 + posInFiber);
  }  

  // test3 of BBTOW mapping after my swaps
  printf("JJJBtow softID rdo posInFib \n");
  for(int softId=1; softId<=4800;softId++) {
    int RDO;
    int posInFiber;
    mMappB->GetCrateFromTowerId(softId, RDO, posInFiber);
    int newId=softId;
    printf("JJJBtow %4d %2d %4d \n", newId,RDO,posInFiber);
  }  
#endif  

  LOG_INFO<<Form("::InitRun() done")<<endm;
  return StMaker::Init();
}


//____________________________________________________________
//____________________________________________________________

Int_t StJanBarrelDbMaker::Make(){
  LOG_INFO<<Form("::Make() ")<<endm;


 return kStOK;
}

//____________________________________________________________
//____________________________________________________________

int StJanBarrelDbMaker::bprsCrate(int softID) {
  assert(softID>0);
  assert(softID<=mxBtow);
  if(softID<=340 ) return 0; // PSD_1W
  else if(softID<=1540 ) return 1; // PSD_19W
  else if(softID<=2400 ) return 0; // PSD_1W
  else if(softID<=2900 ) return 2; // PSD_1E
  else if(softID<=4100 ) return 3; // PSD_20E
  return 2; // PSD_1E 
}


//____________________________________________________________
//____________________________________________________________

float StJanBarrelDbMaker::pedTile(int ibp, int softID, int capID){
   assert(softID>0 && softID<=mxBtow);
  assert(capID>=0 && capID<mxBcap);

  float ped=-991;
  int jBP=BTOW;
  if(ibp==kBPrs)jBP=BPRS;

  float pedOfl,sigPedOfl;
  mTables->getPedestal(jBP,softID,capID,pedOfl,sigPedOfl);  

  ped=pedOfl;

  // overwtite STAR db for some cases
  if(ibp==kBPrs && par_dbType==kJanDb)     
      ped=0.1*janDb_bprsPed2D->GetBinContent(softID,capID+1);
 
  return ped;
    
}

//____________________________________________________________
//____________________________________________________________

float StJanBarrelDbMaker::sigPedTile(int ibp, int softID, int capID){

  assert(softID>0 && softID<=mxBtow);
  assert(capID>=0 && capID<mxBcap);

  float sigPed=-991;
  int jBP=BTOW;
  if(ibp==kBPrs)jBP=BPRS;

  float pedOfl,sigPedOfl;
  mTables->getPedestal(jBP,softID,capID,pedOfl,sigPedOfl);  

  sigPed=sigPedOfl;

  // overwtite STAR db for some cases
  if(ibp==kBPrs && par_dbType==kJanDb)     
    sigPed=0.1*janDb_bprsSigPed2D->GetBinContent(softID,capID+1);


  return sigPed;
    
}


//____________________________________________________________
//____________________________________________________________

int StJanBarrelDbMaker::statTile(int ibp, int softID){

  assert(softID>0 && softID<=mxBtow);

  int jBP=BTOW;
  if(ibp==kBPrs)jBP=BPRS;
  int statOflPed;
  mTables->getStatus(jBP, softID, statOflPed,"pedestal");
  int stat=0;
  if(statOflPed!=1) stat=1; // flip meanung of bad flag, i.e. Jan's convention
 

  if(ibp==kBTow ){  // tmp
    if( softID==1075) stat=1; 
    if( softID==2969) stat=1; 
    if( softID==3289) stat=1; 
  }

  // overwtite STAR db for some cases
  if(ibp==kBPrs && par_dbType==kJanDb)     
    stat=(int)janDb_bprsStat->GetBinContent(softID);
 
  return stat;
    
}


//____________________________________________________________________
//____________________________________________________________________

void  StJanBarrelDbMaker::initBprsGeometry(){
  printf("\nggg BPRS geometry initialization\n");
  // getEta(const Int_t softId, Float_t &eta) 

  for( int id0=0; id0< mxBTetaBin; id0++) {
    int id=id0+1;
    float eta;
    assert(mBprsGeom->getEta(id,eta)==0);
    float eta2;
    assert(mBtowGeom->getEta(id,eta2)==0);
    printf("id=%d, BPRS eta=%f BTOW eta=%f\n",id,eta,eta2);
    // printf("%f\n",eta);
  }
}

// $Log: StJanBarrelDbMaker.cxx,v $
// Revision 1.5  2010/04/15 19:13:30  mattheww
// fixed some future gcc issues
//
// Revision 1.4  2009/08/25 16:08:04  fine
// fix the compilation issues under SL5_64_bits  gcc 4.3.2
//
// Revision 1.3  2009/04/17 18:44:43  mattheww
// fixed a bug in the last comment
//
// Revision 1.2  2009/02/04 20:33:32  ogrebeny
// Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
//
// Revision 1.1  2008/11/24 23:06:37  balewski
// start
//

 #if 0
//Some combination of these two methods:

///Get PSD id. Also returns PMTBox,  wire number and FEE value
int       GetPsdId(int RDO, int posInFiber, int &softId, int &PMTbox, int &wire, int &A_value, bool print=false) const;

///Get PSD fiber and position from id
int       GetPsdRDO(int softId, int &RDO, int &posInFiber) const;

//I believe RDO == CRATE-1.  I'm not sure how "wire" and "A_value" translate into "pixel" in the MAPMT.  But these are the methods you want.

//Adam

#endif
     
