// *-- Author : Rene Fatemi
// 
// $Id: MuEzCorrMaker.cxx,v 1.3 2005/01/09 04:46:47 balewski Exp $

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <StMessMgr.h>

#include "MuEzCorrMaker.h"

#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

#include "StMuDSTMaker/EZTREE/EztEventHeader.h"
#include "StMuDSTMaker/EZTREE/EztTrigBlob.h"
#include "StMuDSTMaker/EZTREE/EztEmcRawData.h"


ClassImp(MuEzCorrMaker)

//________________________________________________
//_____________________________________lis___________
MuEzCorrMaker::MuEzCorrMaker( const char* self ,const char* muDstMakerName) : StMaker(self){
  mMuDstMaker = (StMuDstMaker*)GetMaker(muDstMakerName);
  assert(mMuDstMaker);

  nInpEve=0;
  HList=0;
  mode=0;
}


//___________________ _____________________________
//________________________________________________
MuEzCorrMaker::~MuEzCorrMaker(){
 
}

//___________________ _____________________________
//________________________________________________
void MuEzCorrMaker::saveHisto(TString fname){
  TString outName=fname+".hist.root";
  TFile f( outName,"recreate");
  assert(f.IsOpen());
  printf("%d histos are written  to '%s' ...\n",HList->GetEntries(),outName.Data());
  HList->Write();
  f.Close();

}
 
//________________________________________________
//________________________________________________
Int_t MuEzCorrMaker::Init(){

  assert(HList);

  h[0]=new TH1F("tkn","Token frequency",4098,-0.5,4097.5);
  h[1]=new TH1F("twCh","Tower chi2 vs. events ID",5000,0.5,5000.5);
  h[2]=new TH1F("twCn","Tower channels with delAdc<10 vs. events ID",5000,0.5,5000.5);
  h[3]=( TH1F*) new TH2D("twChTm","Tower chi2/DOF vs. time ; time (minutes); chi2/DOF",400,0,80,20,0,10);
  
  hTow[0]=new TH2F("crate1","Crate 1 Tower ADC",4096,0,4096,128,0,128);
  hTow[1]=new TH2F("crate1","Crate 1 Tower ADC",4096,0,4096,128,0,128);
  hTow[2]=new TH2F("crate1","Crate 1 Tower ADC",4096,0,4096,128,0,128);
  hTow[3]=new TH2F("crate1","Crate 1 Tower ADC",4096,0,4096,128,0,128);
  hTow[4]=new TH2F("crate1","Crate 1 Tower ADC",4096,0,4096,128,0,128);
  hTow[5]=new TH2F("crate1","Crate 1 Tower ADC",4096,0,4096,128,0,128);

  for(int i=0;i<=6;i++)
    HList->Add(hTow[i]);

  for(int i=0;i<=3;i++)
    HList->Add(h[i]);

  return StMaker::Init();
}

//________________________________________________
//________________________________________________
Int_t MuEzCorrMaker::Finish(){
  return kStOK;
}


//________________________________________________
//________________________________________________
Int_t MuEzCorrMaker::Make(){
  nInpEve++;
  CorrAna::clear();

  gMessMgr->Message("","D") <<GetName()<<"::Make() is called "<<endm;
  //..........  acuire EztHeader
  eHead= mMuDstMaker->muDst()->eztHeader();
  if(eHead==0) {
    gMessMgr->Message("","E") <<GetName()<<"::Make() no  EztEventHeader, skip event  "<<endm;
    return kStOK;
  }
  eETow=mMuDstMaker->muDst()->eztETow();
  eESmd=mMuDstMaker->muDst()->eztESmd();
  if(mode==0) {
    printf("%s::Make() mode=%d unknown, chose what you want to do\n",GetName(),mode);
    assert(mode);
  }

  switch (mode) {
  case 1:  
    printf("______________________________________Event ID %d____inpEve #%d______________________________________\n",eHead->getEventNumber(),nInpEve);
    //scanESmdCorrupt();
    scanETowCorrupt();
    printCorrupt();
    break;
  default:
     printf("%s::Make() mode=%d unknown, doing nothing\n",GetName(),mode);
  }

  //test1(eETow,1);
  return kStOK;
#if 0
  int token= header->getToken();

  h[0]->Fill(token);
  EztEmcRawData* etow=mMuDstMaker->muDst()->eztETow();
  assert(etow);


  //
  test1(mMuDstMaker->muDst()->eztESmd(),0);
  header->print();
  // ............. acuire TRIGGER data 
  unpackTrigEzt();
  //..........  do the job
  return kStOK;

#endif  
}

//________________________________________________
//________________________________________________
void MuEzCorrMaker::test1(EztEmcRawData* tw, int flag){
  int nOk=0;
  int ib;
  for(ib=0;ib<tw->getNBlocks();ib++) {
    if( tw->sizeHeader(ib)<=0) continue;
    printf("ib=%d sizeH=%d sizeD=%d\n",ib,tw->sizeHeader(ib),tw->sizeData(ib));
    
    nOk++;
    const UShort_t* head=tw->header(ib); 
    int i;
    for(i=0;i<tw->sizeHeader(ib);i++) printf("  head[%d]=0x%04x ",i,head[i]);
    printf("\n");
    if(flag==0) continue;
    const UShort_t* data=tw->data(ib);
    int nd=tw->sizeData(ib);
    for(int chan=0;chan<nd;chan++) {
      if(chan>17) {printf(" .... etc ...\n"); break; }
      int adc=data[chan];
      printf("ib=%d ch=%d adc=%d\n",ib,chan,adc);
    }
  }
  
  return ;

}

//________________________________________________
//________________________________________________
void MuEzCorrMaker::scanETowCorrupt(){
  int headToken=eHead->getToken(); 
  int lenCount=0xa4;
  int errFlag=0;
  int trigComm=0x4; // physics, 9=laser/LED, 8=??

  int ib;
  for(ib=0;ib<eETow->getNBlocks();ib++) {
    int crID=ib+1;
    if( eETow->sizeHeader(ib)<=0) continue;
    // printf("ib=%d sizeH=%d sizeD=%d\n",ib,eETow->sizeHeader(ib),eETow->sizeData(ib));
    eETow->tagHeadValid(ib,headToken, crID,lenCount,trigComm,errFlag);
    UShort_t  sanity  = eETow->getCorruption(ib);
 
    
    int i;
    for(i=0;i<4;i++) {// examin all sanity bits
      if(sanity&(1<<i)) Elist[ib][i]=1;
    }

    if(sanity& 1) {
      Elist[ib][5]=-1;
      Elist[ib][6]=-1;
      Elist[ib][7]=-1;
      continue; 
    }
    //Use this if HV on
    const  UShort_t* data=eETow->data(ib);
    int nd=eETow->sizeData(ib);
    for (int chan=120;chan<nd;chan++) {
      if (data[chan]> 60){
	//printf("Crate #%d, chan #%d had ghost pedestal!\n",crateID,chan);
	Elist[ib][5]=1;
      }
    }

    for (int chan=0;chan<nd;chan++) {
	//printf("Crate #%d, chan #%d had ghost pedestal!\n",crateID,chan);
	hTow[ib]->Fill(data[chan],chan);
    }      
    /*use this if no HV
    for (int chan=0;chan<nd;chan++) {
      if ((data[chan]<100)&&(data[chan]>60)){
	//printf("Crate #%d, chan #%d had ghost pedestal!\n",crateID,chan);
	Elist[ib][5]=1;
      }
      if (((data[chan]%256)==0)&&(data[chan]!=0)){
	//printf("Crate #%d, chan #%d had nx256 error!\n",crateID,chan);
	Elist[ib][6]=1;
      }
      ESum+=data[chan];
    }
      
    int thres=MaxTwCrateCh*40;
    if (ESum>thres){
      Elist[ib][7]=1;
      //printf("Crate #%d has Sum ADC = %d which is greater than Threshold of 40*160 channels\n",crateID,BSum);
    }
    */
  }
}

//________________________________________________
//________________________________________________
void MuEzCorrMaker::scanESmdCorrupt(){
  int headToken=eHead->getToken(); 
  int lenCount=0x28;
  int errFlag=0;
  int trigComm=0x4; // physics, 9=laser/LED, 8=??

  int ib;
  for(ib=0;ib<eESmd->getNBlocks();ib++) {
    int crID=ib+1;
    if( eESmd->sizeHeader(ib)<=0) continue;
    printf("ib=%d sizeH=%d sizeD=%d\n",ib,eESmd->sizeHeader(ib),eESmd->sizeData(ib));

    eESmd->tagHeadValid(ib,headToken, crID,lenCount,trigComm,errFlag);
    UShort_t  sanity  =eESmd->getCorruption(ib);

    int i;   
    for(i=0;i<4;i++) {// examin all sanity bits
      if(sanity&(1<<i)) ESlist[ib][i]=1;
    }

    if(sanity& 1) {
      ESlist[ib][5]=-1;
      ESlist[ib][6]=-1;
      ESlist[ib][7]=-1;
      continue; 
    }
    //Use this if HV on
    //const  UShort_t* data=eESmd->data(ib);
    //int nd=eESmd->sizeData(ib);
    //for (int chan=0;chan<nd;chan++) {
    // if (data[chan]> 500){
	//printf("Crate #%d, chan #%d had ghost pedestal!\n",crateID,chan);
    //	ESlist[ib][5]=1;
    // }
    //}
  }
}

//________________________________________________
//________________________________________________
void MuEzCorrMaker::printCorrupt(){
  printf("DataBlock   Crate#   token  length   TrgCom   ErrFlag  Ghost/nx256\n");
  for(int Bic=0;Bic<MaxTwCrates;Bic++) {printf("     %2d        %d        %d       %d       %d          %d       %d\n",Bic,Elist[Bic][0],Elist[Bic][1],Elist[Bic][2],Elist[Bic][3],Elist[Bic][4],Elist[Bic][5]);}
  for(int Bic=0;Bic<MaxEsmdCrate;Bic++) {
    printf("     %2d        %d        %d       %d       %d          %d       %d\n",Bic,ESlist[Bic][0],ESlist[Bic][1],ESlist[Bic][2],ESlist[Bic][3],ESlist[Bic][4],ESlist[Bic][5]);
  }

} 


//---------------------------------------------------
// $Log: MuEzCorrMaker.cxx,v $
// Revision 1.3  2005/01/09 04:46:47  balewski
// sync w/  raw->tagHeadValid()
//
// Revision 1.2  2004/11/29 19:37:14  balewski
// fix to match EZTREE evolution
//
// Revision 1.1  2004/11/19 15:51:14  rfatemi
// Maker to check corruption in ezTree branch of MuDst
//
// Revision 1.2  2004/11/10 03:20:30  balewski
// trig fixed
//
// Revision 1.1  2004/11/02 14:37:11  balewski
// exampl eof stale data monitor
//
 
