// $Id: EzCorrEventLoop.cxx,v 1.4 2009/02/04 20:33:15 ogrebeny Exp $
// modified by rfatemi 6-22-04 added barrel loop
 
#include <assert.h>
#include <stdlib.h>


#include <TClonesArray.h>

#include <TObjArray.h> 

#include "EzCorrEventLoop.h"

#include "StEEmcUtil/EEfeeRaw/EEfeeRawEvent.h"
#include "StEEmcUtil/EEfeeRaw/EEstarTrig.h"
#include "StEEmcUtil/EEfeeRaw/EEmcEventHeader.h"
#include "StEEmcUtil/EEfeeRaw/EEfeeDataBlock.h"
#include "StEEmcUtil/EEfeeRaw/EEname2Index.h"

#include <iostream>

#include "CorrAna.h"

ClassImp(EzCorrEventLoop)
//--------------------------------------------------
//--------------------------------------------------
EzCorrEventLoop::EzCorrEventLoop(){
  printf("EzCorrEventLoop() constructed\n");
  eHead=0;
  eEve=0;
  eTrig=0;
  mode=0;

}

//--------------------------------------------------
//--------------------------------------------------
void EzCorrEventLoop::init (){
  printf("EzCorrEventLoop() init\n");
  evt=0;
  CorrAna::init();

}

//--------------------------------------------------
//--------------------------------------------------
EzCorrEventLoop::~EzCorrEventLoop() {/* noop */}


//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
void EzCorrEventLoop::setMode(int hold){
  mode=hold;
  //  printf("Hold=%d,Mode=%d\n",hold,mode);
}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
void EzCorrEventLoop::make(){

  evt++;
  CorrAna::clear();

  if (mode==0){
    printf("______________________________________Event #%d__________________________________________\n",evt);
    printCorrupt();
  }
  

  if (mode==1){
    unpackEzTreeHisto();
    if (BState==2) taskBbad();
    if (BState==1) taskBgood();
    if (EState==2) taskEbad();
    if (EState==1) taskEgood();
    if (EsmdState==2) taskESbad();
    if (EsmdState==1) taskESgood();
    if ((BState==2)&&(EsmdState==2)) allBad=1;
    if ((EState==2)&&(EsmdState==2)) allBad=1;
    if ((BState==2)&&(EState==2)) allBad=1;
    if (((BState==2)&&(EState==2))&&(EsmdState==2)) allBad=2;
    taskDiag();
  }

  //print();
}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
void EzCorrEventLoop::unpackEzTreeHisto(){
  nInpEve++;
  printf(" EzCorrEventLoop::getData()is called ......\n");

  // check CTB multiplicity 
  int ctbSum=0;
  int isl;
  for ( isl = 0; isl < 240; isl++ ) {
    ctbSum+= eTrig -> CTB[isl];
  }
  //printf("ctbSum=%d \n",ctbSum);
    
  int nb=eEve->block->GetEntries();
  printf("Number of datablocks is:%d\n",nb);
  if (nb==15){//header corruption testing when only BTOW in .daq file
    
    int nBcr=0;
    for(int Bic=0;Bic<nb;Bic++) {// BEMC data blocks 0 - 14
      EEfeeDataBlock *b=(EEfeeDataBlock *)eEve->block->At(Bic);
      int crateID=b->getCrateID();//crate ID 16-30 so subtract 16 
      
      if( !b->isValid() ) {
	if ((crateID>29)||(crateID<27)){
	  badBcrate[MaxBTwCrate]=1;
	}else {
	  badBcrate[crateID-BCrNumMin]=1;
	}
	Bsanity=b->getSanity();
      }else {
	nBcr++;
      };
    } 
    BState=0;
    if (nBcr!=MaxBTwCrate) {
      BState=2;
    } else {
      BState=1;
    };
    
    for(int Bic=0;Bic<=14;Bic++){
      EEfeeDataBlock *b=(EEfeeDataBlock *)eEve->block->At(Bic);
      int crateID=b->getCrateID();//crate ID 16-30 so subtract 16 
      if ((crateID>29)||(crateID<27)) continue;
      //printf("BEMC\n");
      //b->print(0);
      UShort_t* Bdata=b->getData();
      int nd=b->getValidDataLen(); 
      
      for (int chan=0;chan<nd;chan++) {
	if( BState==2 ) {
	  corruptB[crateID-16][chan]=Bdata[chan];
	}
	if (BState==1) {
	  crateB[crateID-16][chan]=Bdata[chan];
	}
      }
    }
    
  }
  else { 
    //EEMC tower crate extraction 
    //EEMC tower crates are in blocks 0 -5 
    //Use 2 loops  - 1st decides if event if is good -- 2nd stores ADCs
    int nEcr=0;
    for(int ic=EDataBkMin;ic<=EDataBkMax;ic++) {
      EEfeeDataBlock *b=(EEfeeDataBlock *)eEve->block->At(ic);
      int crateID=b->getCrateID();
      //printf("ETOW crate# = %d\n",crateID);
      //b->print(0);
      if( !b->isValid() ) { 
	if ((crateID<ECrNumMin)||(crateID>ECrNumMax)){ 
	  badEcrate[MaxTwCrates]=1;
	}else {
	  badBcrate[crateID-ECrNumMin]=1;
	};
	Esanity=b->getSanity(); 
      }else {
	nEcr++;
      };
    }
    EState=0;
    if (nEcr!=MaxTwCrates) {
      EState=2;
    } else {
      EState=1;
    };
    
    for(int ic=EDataBkMin;ic<=EDataBkMax;ic++) {
      EEfeeDataBlock *b=(EEfeeDataBlock *)eEve->block->At(ic);
      int crateID=b->getCrateID(); 
      if ((crateID<ECrNumMin)||(crateID>ECrNumMax)) continue;   
      UShort_t* Edata=b->getData();
      int nd=b->getValidDataLen(); 
      //printf("EEMC\n");
      
      
      for(int chan=0;chan<nd;chan++) {
	if ( EState==2 ) { 
	  corruptE[crateID][chan]=Edata[chan];
	}
	if ( EState==1) {
	  crateE[crateID][chan]=Edata[chan];
	}
      }
    }
    
    
    //ESMD crate extraction 
    //ESMD crates are in blocks 6-21 
    //Use 2 loops  - 1st decides if event if is good -- 2nd stores ADCs
    int nEScr=0;
    for(int ic=ESDataBkMin;ic<=ESDataBkMax;ic++) {
      EEfeeDataBlock *b=(EEfeeDataBlock *)eEve->block->At(ic);
      int crateID=b->getCrateID();
      //    printf("ESMD crate# = %d\n",crateID);
      //b->print(0);
      if( !b->isValid() ) {
	if ((crateID<ESCrNumMin)||(crateID>ESCrNumMax)){
	  badEScrate[MaxEsmdCrate]=1;
	}else {
	  badEScrate[crateID-ESCrNumMin]=1;
	};
	ESsanity=b->getSanity(); 
      }else {
	nEScr++;
      };
    }
    EsmdState=0;
    if (nEScr!=16) {
      EsmdState=2;
    } else {
      EsmdState=1;
  };
    
    for(int ic=ESDataBkMin;ic<=ESDataBkMax;ic++) {
      EEfeeDataBlock *b=(EEfeeDataBlock *)eEve->block->At(ic);
      int crateID=b->getCrateID();  
      if ((crateID<ESCrNumMin)||(crateID>ESCrNumMax)) continue;
      UShort_t* ESdata=b->getData();
      int nd=b->getValidDataLen(); 
      //printf("EEMC\n");
      //b->print(0);
      
      for(int chan=0;chan<nd;chan++) {
	if ( EsmdState==2 ) {
	  corruptES[crateID-84][chan]=ESdata[chan];
	}
	if ( EsmdState==1) {
	  crateES[crateID-84][chan]=ESdata[chan];
	}
      }
    }
    
    //BEMC tower crate extraction 
    //BEMC tower crates are in blocks 22 - 36
    //Use 2 loops  - 1st decides if event if is good -- 2nd stores ADCs
    int nBcr=0;
    for(int Bic=BDataBkMin;Bic<=BDataBkMax;Bic++) {// BEMC data blocks start at 22
      EEfeeDataBlock *b=(EEfeeDataBlock *)eEve->block->At(Bic);
      int crateID=b->getCrateID();//crate ID 16-30 so subtract 16 
      
      if( !b->isValid() ) {
	if ((crateID>BCrNumMax)||(crateID<BCrNumMin)){
	  badBcrate[MaxBTwCrate]=1;
	}else {
	  badBcrate[crateID-BCrNumMin]=1;
	}
	Bsanity=b->getSanity();
      }else {
	nBcr++;
      };
    } 
    BState=0;
    if (nBcr!=MaxBTwCrate) {
      BState=2;
    } else {
      BState=1;
    };
    
    for(int Bic=BDataBkMin;Bic<=BDataBkMax;Bic++){
      EEfeeDataBlock *b=(EEfeeDataBlock *)eEve->block->At(Bic);
      int crateID=b->getCrateID();//crate ID 16-30 so subtract 16 
      if ((crateID>BCrNumMax)||(crateID<BCrNumMin)) continue;
      //printf("BEMC\n");
      //b->print(0);
      UShort_t* Bdata=b->getData();
      int nd=b->getValidDataLen(); 
      
      for (int chan=0;chan<nd;chan++) {
	if( BState==2 ) {
	  corruptB[crateID-16][chan]=Bdata[chan];
	}
	if (BState==1) {
	  crateB[crateID-16][chan]=Bdata[chan];
	}
      }
    }
  }
  
  return ;
}
 

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
void EzCorrEventLoop::printCorrupt(){
  int nb=eEve->block->GetEntries();
  printf("#datablocks=%d\n",nb);
  long timeStamp=eHead->getTimeStamp();
  eEve->maskWrongCrates(timeStamp,eHead->getToken()); 
  UChar_t test;

  if (nb==0)  printf("NO DATA BLOCKS -- ABORT!\n");

  if (nb==15){//header corruption testing when only BEMC in .daq file

    for(int Bic=0;Bic<nb;Bic++) {// BEMC data blocks 0 - 14
      if (Bic>=MaxBTwCrate) break;
      EEfeeDataBlock *b=(EEfeeDataBlock *)eEve->block->At(Bic);
      // int crateID=b->getCrateID();//crate ID 16-30 so subtract 16 
      //b->print(0);
      
      if( !b->isValid() ) {//check for corrupted header
	Bsanity=b->getSanity();
	test=2;
	if (test&Bsanity) {
	  //printf("DataBlock #%d  has incorrect token#!\n",Bic);
	  Blist[Bic][1]=1;
	}
	test=4;
	if (test&Bsanity) {
	  //printf("DataBlock #%d  has incorrect length#!\n",Bic);
	  Blist[Bic][2]=1;
	}
	test=8;
	if (test&Bsanity){
	  //printf("DataBlock #%d  has incorrect TrgCmd#!\n",Bic);
	  Blist[Bic][3]=1;
	}
	test=16;
	if (test&Bsanity){
	  //printf("DataBlock #%d has incorrect ErrFlag#!\n",Bic);
	  Blist[Bic][4]=1;
	}
	test=1;
	if (test&Bsanity){
	  //printf("DataBlock #%d has incorrect crate#!\n",Bic);
	  Blist[Bic][0]=1;
	  Blist[Bic][5]=-1;
	  Blist[Bic][6]=-1;
	  Blist[Bic][7]=-1;
	  continue;
	}
      }
      
      UShort_t* Bdata=b->getData();
      int nd=b->getValidDataLen(); 
      int BSum=0;
      for (int chan=0;chan<nd;chan++) {
	if ((Bdata[chan]<100)&&(Bdata[chan]>60)){
	  //printf("Crate #%d, chan #%d had ghost pedestal!\n",crateID,chan);
	  Blist[Bic][5]=1;
	}
	if ((Bdata[chan]%256)==0){
	  //printf("Crate #%d, chan #%d had nx256 error!\n",crateID,chan);
	  Blist[Bic][6]=1;
	}
	BSum+=Bdata[chan];
      }
      
      int thres=MaxBTwCrateCh*40;
      if (BSum>thres){
	Blist[Bic][7]=1;
	//printf("Crate #%d has Sum ADC = %d which is greater than Threshold of 40*160 channels\n",crateID,BSum);
      }
    }
    
    printf("DataBlock   Crate#   token  length   TrgCom   ErrFlag  Ghost  nx256    Sum\n");
    for(int Bic=0;Bic<MaxBTwCrate;Bic++) {
      printf("     %2d        %d        %d       %d       %d          %d       %d      %d      %d\n",Bic,Blist[Bic][0],Blist[Bic][1],Blist[Bic][2],Blist[Bic][3],Blist[Bic][4],Blist[Bic][5],Blist[Bic][6],Blist[Bic][7]);
    }  
  }
  else {

    for(int Bic=EDataBkMin;Bic<EDataBkMax;Bic++) {// ETOW data blocks 0 - 5
      EEfeeDataBlock *b=(EEfeeDataBlock *)eEve->block->At(Bic);
      //int crateID=b->getCrateID(); 
      b->print(0);
      
      if( !b->isValid() ) {//check for corrupted header
	Esanity=b->getSanity();
	test=2;
	if (test&Esanity) {
	  //printf("DataBlock #%d  has incorrect token#!\n",Bic);
	  Elist[Bic][1]=1;
	}
	test=4;
	if (test&Esanity) {
	  //printf("DataBlock #%d  has incorrect length#!\n",Bic);
	  Elist[Bic][2]=1;
	}
	test=8;
	if (test&Esanity){
	  //printf("DataBlock #%d  has incorrect TrgCmd#!\n",Bic);
	  Elist[Bic][3]=1;
	}
	test=16;
	if (test&Esanity){
	  //printf("DataBlock #%d has incorrect ErrFlag#!\n",Bic);
	  Elist[Bic][4]=1;
	}
	test=1;
	if (test&Esanity){
	  //printf("DataBlock #%d has incorrect crate#!\n",Bic);
	  Elist[Bic][0]=1;
	  Elist[Bic][5]=-1;
	  Elist[Bic][6]=-1;
	  Elist[Bic][7]=-1;
	  continue;
	}
      }    
      
      UShort_t* Edata=b->getData();
      int nd=b->getValidDataLen(); 
      int ESum=0;
      for (int chan=0;chan<nd;chan++) {
	if ((Edata[chan]<100)&&(Edata[chan]>60)){
	  //printf("Crate #%d, chan #%d had ghost pedestal!\n",crateID,chan);
	  Elist[Bic][5]=1;
	}
	if ((Edata[chan]%256)==0){
	  //printf("Crate #%d, chan #%d had nx256 error!\n",crateID,chan);
	  Elist[Bic][6]=1;
	}
	ESum+=Edata[chan];
      }
      
      int thres=MaxTwCrateCh*40;
      if (ESum>thres){
	Elist[Bic][7]=1;
	//printf("Crate #%d has Sum ADC = %d which is greater than Threshold of 40*160 channels\n",crateID,BSum);
      }
    }

    for(int Bic=ESDataBkMin;Bic<ESDataBkMax;Bic++) {// ESMD data blocks 6-21
      EEfeeDataBlock *b=(EEfeeDataBlock *)eEve->block->At(Bic);
      //int crateID=b->getCrateID(); 
      //b->print(0);
      
      if( !b->isValid() ) {//check for corrupted header
	ESsanity=b->getSanity();
	test=2;
	if (test&ESsanity) {
	  //printf("DataBlock #%d  has incorrect token#!\n",Bic);
	  ESlist[Bic][1]=1;
	}
	test=4;
	if (test&ESsanity) {
	  //printf("DataBlock #%d  has incorrect length#!\n",Bic);
	  ESlist[Bic][2]=1;
	}
	test=8;
	if (test&ESsanity){
	  //printf("DataBlock #%d  has incorrect TrgCmd#!\n",Bic);
	  ESlist[Bic][3]=1;
	}
	test=16;
	if (test&ESsanity){
	  //printf("DataBlock #%d has incorrect ErrFlag#!\n",Bic);
	  ESlist[Bic][4]=1;
	}
	test=1;
	if (test&ESsanity){
	  //printf("DataBlock #%d has incorrect crate#!\n",Bic);
	  ESlist[Bic][0]=1;
	  ESlist[Bic][5]=-1;
	  ESlist[Bic][6]=-1;
	  ESlist[Bic][7]=-1;
	  continue;
	}
      }    
      
      UShort_t* ESdata=b->getData();
      int nd=b->getValidDataLen(); 
      int ESSum=0;
      for (int chan=0;chan<nd;chan++) {
	if ((ESdata[chan]<100)&&(ESdata[chan]>60)){
	  //printf("Crate #%d, chan #%d had ghost pedestal!\n",crateID,chan);
	  ESlist[Bic][5]=1;
	}
	if ((ESdata[chan]%256)==0){
	  //printf("Crate #%d, chan #%d had nx256 error!\n",crateID,chan);
	  ESlist[Bic][6]=1;
	}
	ESSum+=ESdata[chan];
      }
      
      int thres=MaxTwCrateCh*300;
      if (ESSum>thres){
	ESlist[Bic][7]=1;
	//printf("Crate #%d has Sum ADC = %d which is greater than Threshold of 40*160 channels\n",crateID,BSum);
      }
    }

     for(int Bic=BDataBkMin;Bic<BDataBkMax;Bic++) {// BEMC data blocks 0 - 14
      EEfeeDataBlock *b=(EEfeeDataBlock *)eEve->block->At(Bic);
      //int crateID=b->getCrateID();
      //b->print(0);
      
      if( !b->isValid() ) {//check for corrupted header
	Bsanity=b->getSanity();
	test=2;
	if (test&Bsanity) {
	  //printf("DataBlock #%d  has incorrect token#!\n",Bic);
	  Blist[Bic][1]=1;
	}
	test=4;
	if (test&Bsanity) {
	  //printf("DataBlock #%d  has incorrect length#!\n",Bic);
	  Blist[Bic][2]=1;
	}
	test=8;
	if (test&Bsanity){
	  //printf("DataBlock #%d  has incorrect TrgCmd#!\n",Bic);
	  Blist[Bic][3]=1;
	}
	test=16;
	if (test&Bsanity){
	  //printf("DataBlock #%d has incorrect ErrFlag#!\n",Bic);
	  Blist[Bic][4]=1;
	}
	test=1;
	if (test&Bsanity){
	  //printf("DataBlock #%d has incorrect crate#!\n",Bic);
	  Blist[Bic][0]=1;
	  Blist[Bic][5]=-1;
	  Blist[Bic][6]=-1;
	  Blist[Bic][7]=-1;
	  continue;
	}
      }    
      
      UShort_t* Bdata=b->getData();
      int nd=b->getValidDataLen(); 
      int BSum=0;
      for (int chan=0;chan<nd;chan++) {
	if ((Bdata[chan]<100)&&(Bdata[chan]>60)){
	  //printf("Crate #%d, chan #%d had ghost pedestal!\n",crateID,chan);
	  Blist[Bic][5]=1;
	}
	if ((Bdata[chan]%256)==0){
	  //printf("Crate #%d, chan #%d had nx256 error!\n",crateID,chan);
	  Blist[Bic][6]=1;
	}
	BSum+=Bdata[chan];
      }
      
      int thres=MaxBTwCrateCh*40;
      if (BSum>thres){
	Blist[Bic][7]=1;
	//printf("Crate #%d has Sum ADC = %d which is greater than Threshold of 40*160 channels\n",crateID,BSum);
      }
    }   


    if (nb>0) {
      printf("DataBlock   Crate#   token  length   TrgCom   ErrFlag  Ghost  nx256    Sum\n");
      for(int Bic=0;Bic<MaxTwCrates;Bic++) {
	printf("     %2d        %d        %d       %d       %d          %d       %d      %d      %d\n",Bic,Elist[Bic][0],Elist[Bic][1],Elist[Bic][2],Elist[Bic][3],Elist[Bic][4],Elist[Bic][5],Elist[Bic][6],Elist[Bic][7]);
      }
      for(int Bic=0;Bic<MaxEsmdCrate;Bic++) {
	printf("     %2d        %d        %d       %d       %d          %d       %d      %d      %d\n",Bic,Blist[Bic][0],Blist[Bic][1],Blist[Bic][2],Blist[Bic][3],Blist[Bic][4],Blist[Bic][5],Blist[Bic][6],Blist[Bic][7]);
      }
      for(int Bic=0;Bic<MaxBTwCrate;Bic++) {
	printf("     %2d        %d        %d       %d       %d          %d       %d      %d      %d\n",Bic,ESlist[Bic][0],ESlist[Bic][1],ESlist[Bic][2],ESlist[Bic][3],ESlist[Bic][4],ESlist[Bic][5],ESlist[Bic][6],ESlist[Bic][7]);
      }
    } 
  }



    return ;
  } 

   /*!This is barrel Decoder Data
      look at StRoot/StEmcUtil/database/StEmcDecoder.h
      \param crate is the crate number(1-30)
      \param crate_sequency is the position of the tower inside the crate(0-159)
      \param TowerId is the software id for towers
      // initial position in the crate
      int Init_Crate_tmp[]={2260,2420,2580,2740,2900,3060,3220,3380,3540,3700,
      3860,4020,4180,4340,4500,2180,2020,1860,1700,1540,
      1380,1220,1060,900,740,580,420,260,100,2340};
      for(int i=0;i<30;i++) Init_Crate[i]=Init_Crate_tmp[i];
      //--------------------------------------------------------
      int StEmcDecoder::Getjose_towerWest(int start,int crate_seq)
      {
      int card=crate_seq/32;
      int card_seq=31-(crate_seq%32);
      int channel_seq=card_seq/4;
      int channel=card_seq-(channel_seq*4)+1;
      int jose_tower=start+channel_seq*20+card*4+(5-channel);
      if(jose_tower>2400)jose_tower-=2400;
      return jose_tower;
      }
      //--------------------------------------------------------
      int StEmcDecoder::Getjose_towerEast(int start,int crate_seq)
      {
      int card=crate_seq/32;
      int card_seq=31-(crate_seq%32);
      int channel_seq=card_seq/4;
      int channel=card_seq-(channel_seq*4)+1;
      int jose_tower=start+channel_seq*20+card*4+(5-channel);
      if(jose_tower<=2400)jose_tower+=2400;
      return jose_tower;
      }
      
      int StEmcDecoder::GetTowerIdFromCrate(int crate,int crate_sequency, int& TowerId)
      {
       if(crate>15 && crate<31)
        {
         int start=Init_Crate[crate-1];//Gets some number of position in crate
	 TowerId=Getjose_towerWest(start,crate_sequency);//Get Software ID for West size towers
	 return 1;
	}
	if(crate>0 && crate<16)
         {
	  int start=Init_Crate[crate-1];
	  TowerId=Getjose_towerEast(start,crate_sequency);
	  return 1;
         }
        return 0;
       }
    */

/*****************************************************************
 * $Log: EzCorrEventLoop.cxx,v $
 * Revision 1.4  2009/02/04 20:33:15  ogrebeny
 * Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
 *
 * Revision 1.3  2004/07/26 23:02:29  rfatemi
 * Corruption update without comments
 *
 * Revision 1.2  2004/07/26 22:54:26  rfatemi
 * Corruption Update
 *
 * Revision 1.1  2004/07/24 22:51:08  balewski
 * first
 *
 * Revision 1.1  2004/06/06 04:54:08  balewski
 * dual analyzis
 *
 *
 ********************************************************************/
