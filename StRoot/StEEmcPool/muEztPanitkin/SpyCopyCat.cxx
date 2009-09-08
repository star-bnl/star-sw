#include <stdio.h>
#include <cassert>

#include <TH1.h>
#include "StMuDSTMaker/EZTREE/EztEmcRawData.h"

#include "SpyCopyCat.h"

//------------------------------
//------------------------------
SpyCopyCat:: SpyCopyCat(char x, int y){
  type=x;
  icr=y; 

  int crateID=0;
  int nb=0;
  switch (type) {
  case 'T': crateID=1+icr; nb=128; 
    text="ETOW crate="; break;
  case 'M': crateID=64+icr; nb=192; 
    text="ESMD crate="; break;
  default: ;
  }
  text+=crateID;

  TString txt;
  txt="SpyCopyCat"; txt+=crateID;
  h=new TH1F(txt,txt,nb,-0.5,nb-0.5);
  nEve=0;
};

//------------------------------
//------------------------------
void
SpyCopyCat::accumulate(EztEmcRawData *e, EztEmcRawData *m){
  int nCCthresh=3;

  nEve++;

  EztEmcRawData *eRaw=0; 
  int crateID=0;
  switch (type) {
  case 'T': eRaw=e; crateID=1+icr; break;
  case 'M': eRaw=m; crateID=64+icr; break;
  default: ;
  }
  assert(eRaw);  

  assert( icr>=0 && icr<eRaw->getNBlocks());
  if(eRaw->isCrateVoid(icr)) return;
  
  const UShort_t* data=eRaw->data(icr);
  int i0=-1, i1=-2;
  int nCC=0;
  int i;
  for(i=1;i<eRaw->sizeData(icr);i++) {
    int del=data[i-1]-data[i];
    //test   if(crateID==5 && i>30 && i<40) del=0;
    // printf("i=%d  adc=%d del=%4d nCC=%d\n",i,data[i],del,nCC);
    
    if(del!=0  ) { // different values
      if(nCC>=nCCthresh) {
	incrHist(i0,i1);
	//printf(" CC-series crID=%d [%d,%d] nCC=%d\n",crateID,i0,i1,nCC); // new sequence starts
      }
      nCC=0; // clear old sequence
      continue;
    } else { //identical values
      if(nCC==0) i0=i-1; // new sequence starts
      i1=i;
      nCC++;
    }
    //    const  EEmcDbItem  *x=eeDb->getByCrate(crateID,chan);
    //if(x==0) continue; // noDB info
    //if(x->fail ) continue;  // drop broken channels
   } // end of loop over hist
   
  //closing tests
  if(nCC>=nCCthresh) {
    incrHist(i0,i1);
    // printf("end CC-series crID=%d [%d,%d] nCC=%d\n",crateID,i0,i1,nCC); // new sequence starts
  }

}

//------------------------------
//------------------------------
void
SpyCopyCat::  incrHist(int i0, int i1){
  int i;
  for(i=i0; i<=i1;i++) h->Fill(i);
}


//------------------------------
//------------------------------
bool
SpyCopyCat::sense(FILE *fd) {
  // params ....
  float thres=0.5;
  float minEve=10;

  float nThres=nEve*thres;
  if(nThres<minEve) nThres=minEve;

  int nMax=(int)h->GetMaximum();
  bool isBad=nMax>nThres;
  // fprintf(fd,"\nSpyCopyCat: %s examined ",text.Data());

  // h->Draw();
  if (isBad) {
    fprintf(fd,"\nSpyCopyCat: %s , bad channels: ",text.Data());
    int i;
    int k=0;
    for(i=1;i<=h->GetNbinsX();i++) {
      if(h->GetBinContent(i) <nThres)   continue; 
      fprintf(fd,"%d,",i);
      k++;
    } 
    fprintf(fd," --> nChan=%d\n",k);
  }// end of error message

  nEve=0; // clear
  h->Reset();
  return isBad;
}
  
