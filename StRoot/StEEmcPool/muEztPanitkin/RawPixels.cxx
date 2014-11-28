#include <stdio.h>
#include <assert.h>
#include <string.h>


#include <TObjArray.h>
#include <TClonesArray.h>

#include <TH2.h>
#include <TF2.h>
#include <TFile.h>

#include "StMuDSTMaker/EZTREE/EztEmcRawData.h"

#include "StEEmcUtil/EEfeeRaw/EEname2Index.h"
#include "StEEmcUtil/EEfeeRaw/EEdims.h"

#include "StEEmcUtil/database/EEmcDbItem.h"
#include "StEEmcUtil/database/StEEmcDb.h"

#include "RawPixels.h"

//-------------------------------------------
//-------------------------------------------
RawPixels::RawPixels(TObjArray*L,StEEmcDb*dbx) {

  // clear histo pointers and all other stuff 
  HList=L; assert(HList);
  hPix=new TH1F * [EEindexMax];
  memset(hPix,0,sizeof(void*)*EEindexMax);

  hSmd=new TH2F * [MaxSmdPlains*MaxSectors];
  memset(hSmd,0,sizeof(void*)*MaxSmdPlains*MaxSectors);
  
  eeDb=dbx;
  setLimits(0,0);
}


//-------------------------------------------
//-------------------------------------------
void RawPixels::initHisto() {
  assert(eeDb);
  // eeDb->print();
  assert(c_x1>=0);
  assert(c_x2>c_x1);
  
  TString mode="";
  switch(c_convMode) {
  case kRawAdc: mode="rawAdc"; break;
  case kPedSub: mode="pedSub"; break;
  case kPedAndGain: mode="pedAndGain"; break;
  default:
    assert(2==3);
  }
  // ...................  histo for individual pixels ...
  int i,k=0;
  for(i=0;i<EEindexMax;i++) { 
    const  EEmcDbItem *x=eeDb->getByIndex(i);
    if(x==0) continue;
    // initialize histos only for pixels acquired from DB
    k++;
    char  tt1[100],tt2[200];
    sprintf(tt1,"a%s",x->name);
    sprintf(tt2,"ADC for %s,  cr/chan=%3.3d/%3.3d,  tube=%s; ADC (mode=%s)",x->name,
	    x->crate,x->chan,x->tube,mode.Data());
    TH1F* h=new TH1F(tt1,tt2,c_x2-c_x1+1,c_x1-0.5,c_x2+0.5);
    hPix[i]=h;
    HList->Add(h);
    // printf("k=%d -->%s\n",k,tt1);
  }  

  // ..... 2D histos for SMD
  int sec1=eeDb->getFirstSecID();
  int sec2=eeDb->getLastSecID();
  int secID;
  for(secID=sec1; secID<=sec2; secID++) {
    int iuv;
    for(iuv=0;iuv<MaxSmdPlains;iuv++) {
      char tt1[100], tt2[100];
      sprintf(tt1,"a%2.2d%c",secID,iuv+'U');
      sprintf(tt2,"%2.2d%c-SMD strip vs. raw ADC",secID,iuv+'U');
      TH2F* h2=new TH2F(tt1,tt2,200,0,4000,MaxSmdStrips,0.5,MaxSmdStrips+0.5);
      int key1=secID-1+iuv*MaxSectors;
      hSmd[key1]=h2;
      HList->Add(h2);
      //printf("init 2D %d '%s'\n",key1,tt1);
    }
  }


  {   // additional info
    char  tt2[100];
    sprintf(tt2," stat info ");
    
    hInfo=new TH1F("info",tt2,20,-0.5,19.5);
    HList->Add(hInfo);
  }

  printf("RawPixels: Initialized histos for %d  pixels\n",k);  
}  

//-------------------------------------------
//-------------------------------------------
void RawPixels::sort(EztEmcRawData  *eRaw){

  assert(eeDb);

  hInfo->Fill(0); // tot eve
  if(eRaw==0) return;
  
  int icr;
  for(icr=0;icr<eRaw->getNBlocks();icr++) {
    if(eRaw->isCrateVoid(icr)) continue;
    const UShort_t* data=eRaw->data(icr);
    int crateID=eRaw->getCrateID(icr);    
    hInfo->Fill(crateID); // this crate per eve 
    int nd=eRaw->sizeData(icr);
    // printf("ic =%d cr=%d \n",icr, crateID);

    int chan;
    for(chan=0;chan<nd;chan++) {
      const  EEmcDbItem  *x=eeDb->getByCrate(crateID,chan);
      if(x==0) continue;
      // process only pixels acquired from DB
      
      float value=data[chan]; // raw ADC 
      switch(c_convMode) {
      case kRawAdc: break;
      case kPedSub:
	value-=x->ped;	break;
      case kPedAndGain:
	value-=x->ped;
	value/=x->gain;
	//printf("kPedAndGain %s %s %f %f\n",x->name,x->tube,x->gain,value);
	break;
      default:
	assert(2==3);
      }
      
      hPix[x->key]->Fill(value);
      // printf("cr=%2d ch=%3d val=%.1f ped=%4.1f '%s' \n",crateID,chan,value,x->ped,x->name);

      if(x->isSMD()) {
        int key=(x->plane-'U')*MaxSectors + x->sec-1;
        //x->print();
        //printf("key=%d\n",key);
#if 0	
	if(key<=0 || key>=MaxSmdPlains*MaxSectors) {
	  printf("cr=%2d ch=%3d val=%.1f ped=%4.1f '%s' \n",crateID,chan,value,x->ped,x->name);
	  x->print();
	  printf("key=%d\n",key);
	}
#endif

        assert(key>=0 && key<MaxSmdPlains*MaxSectors);
	assert(hSmd[key]);
        hSmd[key]->Fill(value,x->strip);
      }


    } // channels
  } // blocks  

}
