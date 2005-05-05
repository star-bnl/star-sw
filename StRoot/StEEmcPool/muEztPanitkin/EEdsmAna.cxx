#include <iostream>
#include <assert.h>

#include <TObjArray.h>
#include <TH2.h>
#include <TFile.h>

#ifdef IN_PANITKIN
  #include "daqFormats.h"
  #include "trgReader.h"
#endif 

  
#include "EEdsmAna.h"

#define EEmapTP_USE // trick instattiates data only in the cxx
#include "StEEmcUtil/EEdsm/EEmapTP.h" 
#undef EEmapTP_USE

#include "StEEmcUtil/EEdsm/EEdsm0.h"
#include "StEEmcUtil/EEdsm/EEdsm1.h"
#include "StEEmcUtil/EEdsm/EEdsm2.h"
#include "StEEmcUtil/EEdsm/EEdsm3.h"


#ifndef IN_PANITKIN
ClassImp(EEdsmAna)
#endif

//--------------------------------------------------
//--------------------------------------------------
EEdsmAna ::  EEdsmAna( TObjArray *L, TString nm ) {
  HList=L;
  myName=nm;
  nTot=0;

  //=====================  DSM 0 ===============
  Nee0=9; 
  ee0=new EEdsm0[Nee0];
  Nee0out=12;
  ee0outTPadc=new int [Nee0out];
  ee0outHTadc=new int [Nee0out];
  
  // set pedestals initially to 0
  ped0=new int *[Nee0];
  int ibr;
  for(ibr=0;ibr<Nee0; ibr++) {
    ped0[ibr]=new int [ee0[ibr].getNc()];
    int ch;
    for(ch=0;ch<ee0[ibr].getNc();ch++) 
      ped0[ibr][ch]=0;
  }

  //=====================  DSM 1 ===============
  Nee1=2; 
  ee1=new EEdsm1[Nee1];
  ee1out3JPadc=new int [Nee1];
  // ee1outHT=new int [Nee1];
  ee1[0].setType(1);
  ee1[1].setType(2);
  
  //=====================  DSM 2 ===============
  ee2=new EEdsm2;

  //=====================  DSM 3 ===============
  ee3=new EEdsm3;

  clear();
}

//--------------------------------------------------
//--------------------------------------------------
EEdsmAna ::  ~EEdsmAna() { }

//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna :: clear() {
  
  int i;  
  for (i=0;i<Nee0;i++) ee0[i].clear();
  for (i=0;i<Nee0out;i++) { //# boards !=  # outputs
    ee0outTPadc[i]=0;
    ee0outHTadc[i]=0;
  }

  for (i=0;i<Nee1;i++) { // # boards ==# outputs
    ee1[i].clear();
    ee1out3JPadc[i]=0;
    // ee1outHT[i]=0;
  }

  for (i=0;i<EEnJetPatch;i++) { 
    ee1outJPadc[i]=0;
    ee1outHT[i]=0;
    AdjJPsum[i]=0;
    JPtrig[i]=0;
  }

  ee2->clear();
  ee2outHT=0;

  ee3->clear();
}

//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna ::initHisto(){
  //  assert(fd);
  //assert(fd->IsOpen());
  //TDirectory *dir=  fd->mkdir(myName);

  int iJ;
  char tt1[100], tt2[100];

  //printf("EEdsmAna initHisto");
  
  // .................level-0 input, 2D, HT ..............
  for(iJ=0;iJ<EEnJetPatch;iJ++) {
    sprintf(tt1,"dsm0inJP%d_HT",iJ+1);
    sprintf(tt2,"HT input, DSM layer-0, Jet Patch %d (Steve), trig=%s",iJ+1,myName.Data());
    TH2F*h2=new TH2F(tt1,tt2,64,-0.5,63.5,EEnTPinJP,0.5,EEnTPinJP+0.5);
    h2->GetXaxis()->SetTitle("input 0-DSM ADC value");
    h2->GetYaxis()->SetTitle("Trig Patch  ID");
  
    H0inHT[iJ]=h2;
    HList->Add(h2); 
  }

  sprintf(tt2,"HT input, DSM layer-0, all trig Patches, trig=%s; Hanks ID=10*brd+inpID",myName.Data());
  H0inHTall=new TH2F("dsm0inJPall_HT",tt2,90,-0.5,89.5,64,-0.5,63.5);
  
  HList->Add( H0inHTall);

  // ........level-0 input, 2D, TP sum
  for(iJ=0;iJ<EEnJetPatch;iJ++) {
    char tt1[100], tt2[100];
    sprintf(tt1,"dsm0inJP%d_TP",iJ+1);
    sprintf(tt2,"TP input, DSM layer-0, Jet Patch %d (Steve), trig=%s",iJ+1,myName.Data());
    TH2F*h2=new TH2F(tt1,tt2,64,-0.5,63.5,EEnTPinJP,0.5,EEnTPinJP+0.5);
    h2->GetXaxis()->SetTitle("input to level 0");
    h2->GetYaxis()->SetTitle("Trig Patch  ID");
    
    H0inTP[iJ]=h2;
    HList->Add(h2);
  }
  
  sprintf(tt2,"TP input, DSM layer-0, all trig Patches, trig=%s",myName.Data());
  H0inTPall=new TH2F("dsm0inJPall_TP",tt2,90,-0.5,89.5,64,-0.5,63.5);

  HList->Add( H0inTPall);
  

  // .................level-1 input, 2D
  //================================ TP 

  for(iJ=0;iJ<EEnHalfJetPatch;iJ++) {
    sprintf(tt1,"dsm1HJP%d_TP",iJ+1);
    sprintf(tt2,"TP input vs. emulated, DSM layer-1, brd=%d  ch=%d, trig=%s",iJ/6,iJ%6,myName.Data());
    //printf("iJ=%d tit=%s\n",iJ,tt1);
    TH2F*h2=new TH2F(tt1,tt2,40,0.,400,40,0,400);
    h2->GetXaxis()->SetTitle("emulated TP from level-0");
    h2->GetYaxis()->SetTitle("input to level-1");
    H1inTPvEmu[iJ]=h2;
    HList->Add(h2); 
  }


  //================================ HT 
  for(iJ=0;iJ<EEnHalfJetPatch;iJ++) {
    sprintf(tt1,"dsm1HJP%d_HT",iJ+1);
    sprintf(tt2,"HT input vs.emu, DSM-1, Half Patch %d (Falk), trig=%s",iJ+1,myName.Data());
    // printf("iJ=%d tit=%s\n",iJ,tt1);
    TH2F*h2=new TH2F(tt1,tt2,64,-0.5,63.5,4,-0.5,3.5);
    h2->GetXaxis()->SetTitle("emulated HT from level-0");
    h2->GetYaxis()->SetTitle("input to level-1");
    H1inHTvEmu[iJ]=h2;
    HList->Add(h2); 
  }

  // .................level-2 input
  //================================ TP
  for(iJ=0;iJ<EEnJetPatch;iJ++) {
    sprintf(tt1,"dsm2JP%d_TP",iJ+1);
    sprintf(tt2,"TP inp vs. emu, layer-2, JP_Falk(%d), trig=%s",iJ,myName.Data());
    //printf("iJ=%d tit=%s\n",iJ,tt1);
    TH2F*h2=new TH2F(tt1,tt2,130,-0.5,129.5,4,-0.5,3.5);
    h2->GetXaxis()->SetTitle("EMU from level-1");
    h2->GetYaxis()->SetTitle("REAL level-2");
    H2inTPvEmu[iJ]=h2;
    HList->Add(h2); 
  }

  //================================ HT 
  for(iJ=0;iJ<EEnHalf;iJ++) {
    sprintf(tt1,"dsm2Half%d_HT",iJ+1);
    sprintf(tt2,"HT inp vs. emu, layer=2, JP_Falk(%d+%d+%d), trig=%s",3*iJ,3*iJ+1,3*iJ+2,myName.Data());
    //printf("iJ=%d tit=%s\n",iJ,tt1);
    TH2F*h2=new TH2F(tt1,tt2,4,-0.5,3.5,4,-0.5,3.5);
    h2->GetXaxis()->SetTitle("EMU TP level-1");
    h2->GetYaxis()->SetTitle("REAL level-2");
    H2inHTvEmu[iJ]=h2;
    HList->Add(h2); 
  }

  // .................level-3 input
  //================================ HT 
  sprintf(tt2,"HT inp vs. emu, layer=3, trig=%s",myName.Data());
  TH2F*h2=new TH2F("dsm3_HT",tt2,4,-0.5,3.5,4,-0.5,3.5);
  h2->GetXaxis()->SetTitle("EMU TP level-2");
  h2->GetYaxis()->SetTitle("REAL level-3");
  H3inHTvEmu=h2;
  HList->Add(h2); 
 
  //..................Jet Patch sums
  //=================================summed patch spectra
  for(iJ=0;iJ<EEnJetPatch;iJ++) {
    int steve_jp=((iJ+2)%EEnJetPatch)+1;  //stored by dsm input number 0-5 Want by Steves jp #
    sprintf(tt1,"JP%d_sum",steve_jp);
    sprintf(tt2,"Emulated Sum Jet Patch %d (DSMin %d)",((iJ+2)%EEnJetPatch)+1,iJ);
    //printf("iJ=%d name=%s tit=%s\n",iJ,tt1,tt2);
    TH1F*h1=new TH1F(tt1,tt2,600,-.5,599.5);
    h1->GetXaxis()->SetTitle("Jet Patch Emu Sum");
    h1->GetYaxis()->SetTitle("Freq");
    H4jpSums[iJ]=h1;
    HList->Add(h1);
  }

  //=================================Jet patch over threshold
  int thr[4];             //label plots with sw thresholds
  thr[0]=0;
  thr[1]=EEjpTh0def;
  thr[2]=EEjpTh1def;
  thr[3]=EEjpTh2def;
  for(iJ=0;iJ<EEnThresh;iJ++) {
    sprintf(tt1,"JPsumTh%d",iJ);
    if (iJ == 0) sprintf(tt2,"Emu Jet Patch sum < %d",thr[iJ]);
    else if( iJ <EEnThresh-1) 
      sprintf(tt2,"Emu Jet Patch sum in [%d,%d]",thr[iJ],thr[iJ+1]-1);
    else  
      sprintf(tt2,"Emu Jet Patch sum >%d",thr[iJ]);
    //printf("iJ=%d name=%s tit=%s\n",iJ,tt1,tt2);
    TH1F*h1=new TH1F(tt1,tt2,8,-.5,7.5);
    h1->GetXaxis()->SetTitle("Jet Patch");
    h1->GetYaxis()->SetTitle("Freq");
    H4jpFreq[iJ]=h1;
    HList->Add(h1);
  }

  //=================================summed adjacent patch spectra
  for(iJ=0;iJ<EEnJetPatch;iJ++) {
    int steve_jp=((iJ+2)%EEnJetPatch)+1;  //stored by dsm input number 0-5 Want by Steves jp #
    int steve_jp2=((iJ+3)%EEnJetPatch)+1;
    sprintf(tt1,"JP%d%d_sum",steve_jp,steve_jp2);
    sprintf(tt2,"Emulated Sum Jet Patches %d+%d",steve_jp,steve_jp2);
    //printf("iJ=%d name=%s tit=%s\n",iJ,tt1,tt2);
    TH1F*h1=new TH1F(tt1,tt2,250,-.5,999.5);
    h1->GetXaxis()->SetTitle("Jet Patch Emu Sum");
    h1->GetYaxis()->SetTitle("Freq");
    H4adjpSums[iJ]=h1;
    HList->Add(h1);
  }

 //=================================adjacent patch correlation
  for(iJ=0;iJ<EEnJetPatch;iJ++) {
    int steve_jp=((iJ+2)%EEnJetPatch)+1;  //stored by dsm input number 0-5 Want by Steves jp #
    int steve_jp2=((iJ+3)%EEnJetPatch)+1;
    sprintf(tt1,"JP%d%d_cor",steve_jp,steve_jp2);
    sprintf(tt2,"Emulated Sum Jet Patches %d vs %d",steve_jp,steve_jp2);
    //printf("iJ=%d name=%s tit=%s\n",iJ,tt1,tt2);
    TH2F*h2=new TH2F(tt1,tt2,60,39.5,99.5,60,39.5,99.5);
    sprintf(tt1,"Jet Patch %d Sum",steve_jp);
    h2->GetXaxis()->SetTitle(tt1);
    sprintf(tt1,"Jet Patch %d Sum",steve_jp2);
    h2->GetYaxis()->SetTitle(tt1);
    H4adjPcor[iJ]=h2;
    HList->Add(h2);
  }

  { //====================freq summed adjacent patch spectra over thresh
    TH1F*h1=new TH1F("JPadjTh","Adjacent Jet patches both over thr0",8,-.5,7.5);
    h1->GetXaxis()->SetTitle("DSM Adjacent Jet Patch Emu Sum (FEE crate add 2)");
    h1->GetYaxis()->SetTitle("Freq");
    H4adjpFreq=h1;
    HList->Add(h1);
  }

  // added in 2005 
  H5jpPed =new TH2F("JPpedZoom","Zoom in of 1x1 JP pedestals; Setev's JP ID",EEnJetPatch,0.5,EEnJetPatch+0.5,40,34.5,74.5);
  HList->Add(H5jpPed); 

  sprintf(tt2,"JP DSM sum > %d; JP ID",EEjpTh1def);
  H5jpFreq =new TH1F("JPtotFreq",tt2,EEnJetPatch,0.5,EEnJetPatch+0.5); 
  H5jpFreq->SetFillColor(kGreen);
  HList->Add(H5jpFreq);

  H5jpHot =new TH1F("JPpedHot","Hot towers per JP; JP ID",EEnJetPatch,0.5,EEnJetPatch+0.5); 
  H5jpHot->SetFillColor(kRed);
  HList->Add(H5jpHot); // is _not_ filled by sorter

  printf("EEdsmAna(%s)::initHisto() done  \n", myName.Data());
 
}


//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna ::sort( const unsigned char * dsm0inp, 
		      const unsigned short int  * dsm1inp,
		      const unsigned short int  * dsm2inp,   
		      const unsigned short int  * dsm3inp){
  
  nTot++;
  // printf("EEdsmAna:: sort()  ,nTot=%d\n",nTot);

#ifndef IN_PANITKIN 
  readDsm0(dsm0inp);
  // print();
  readDsm1(dsm1inp);
  if(dsm2inp) readDsm2(dsm2inp);
  if(dsm3inp) readDsm3(dsm3inp);
#else
  readDsm0(trg.EEMC);
  readDsm1(trg.EEMC_l1);
  if(trg.trg_sum) {
    /* actual structures depend on the version
       of trgStructures.h, can be NULL!
    */
    TrgSumData *trg_sum=( TrgSumData *)trg.trg_sum; 
    readDsm2(trg_sum->DSMdata.EMC);
    readDsm3(trg_sum->DSMdata.lastDSM);
  }
#endif

  emulDsm0();    
  emulDsm1();    
  emulDsm2();    
  
  histoDsm0();
  histoDsm1();
  histoDsm2();
  histoDsm3();

  return;
  printf("==================================\n");
  printf("==================================\n");
  print();
  // print2();// for Renee to compare w/ muDst
}


//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna ::emulDsm0(){
  
  int i,j;
  // printf("emulDsm0 (%s) \n      ",myName.Data());
  for(i=0;i<EEnTPphi; i++) {
     for(j=0;j< EEnTPeta;j++) {
       EEmapTP *y=&eeMapTP[i][j];
       int ibr=y->brdIn-1;
       int ch=y->chIn;
       int chOut=y->chOut;
       EEdsm0 * br=ee0+ibr;
       // printf(" ibr=%d ch=%d chOut=%d  val=%d sum=%d\n",ibr,ch,chOut,ee0[ibr].getTP(ch), ee0out[chOut]);
       
       // ........ TP ..........
       ee0outTPadc[chOut]+=br->getTP(ch);

       //.......... HT ..............
       int val= br->getHT(ch);
       if(ee0outHTadc[chOut]< val)ee0outHTadc[chOut]=val;
     }
  }
  
}

//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna ::emulDsm1(){
  
  int i,j;
  for (i=0;i<Nee1;i++) {
    EEdsm1 *br=ee1+i;           //Select data for board 1 or 2
    for(j=0;j<br->getNc(); j++) {
      int ix=j+i*br->getNc();
      int ijp=ix/2;
      ee1outJPadc[ijp]+=br->getTPsum(j);  /*Note jet patches are numbered
					    in DSM input order.  So ijp=0
					    is FEE crate 3.  */
      ee1out3JPadc[i]+=br->getTPsum(j);
      int val= br->getHTthr(j);
      if(ee1outHT[ijp]< val)ee1outHT[ijp]=val;
    }
  }

  //Add some further processing to check jet patch sums for passing thresholds
  //and form adjacent patch sums.
  for (i=0;i<EEnJetPatch;i++) {
    AdjJPsum[i]=ee1outJPadc[i]+ee1outJPadc[(i+1)%EEnJetPatch]; //sum adj patches
    //printf("patch=%d  adc1=%d adc2=%d sum=%d  \n",i,ee1outJPadc[i],ee1outJPadc[(i+1)%EEnJetPatch],AdjJPsum[i]);


    //set trigger word for each patch
    if (ee1outJPadc[i]>EEjpTh2def)     
      JPtrig[i]=3;
    else if (ee1outJPadc[i]>EEjpTh1def) 
      JPtrig[i]=2;
    else if (ee1outJPadc[i]>EEjpTh0def)
      JPtrig[i]=1;
    else
      JPtrig[i]=0;

    //printf("trig=%d \n",JPtrig[i]);
  }
  //printf("done processing jet patch sums \n");
}


//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna ::emulDsm2(){
  
  ee2outHT=ee2->get3JPHTthr(0);
  if(ee2outHT<ee2->get3JPHTthr(1))ee2outHT=ee2->get3JPHTthr(1);

}



//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna ::readDsm0(const unsigned char *EEMC){
  
  /*Gets level 0 DSM inputs and reorders them.  They
    are stored 7 -> 0 followed by 15 -> 8.  Output is
    ordered as physically input to boards, 0 -> 15.
  */

  int ibr; // index, boards ID= ibr+1

  //--------------------- initialize input to DSM level-0
  for(ibr=0;ibr<Nee0; ibr++) {
    int k=16*ibr; // begin of data for given board
    int i;
    // fill in lower 8 words [ 0,1...,7] 
    for(i=0;i<8;i++) ee0[ibr].setBite(7-i,EEMC[k+i]);
    
    // fill in higher 8 words [8,9,....,15]
    for(i=8;i<16;i++) ee0[ibr].setBite(23-i,EEMC[k+i]);
  }
}

//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna ::readDsm1(const  unsigned short *EEMC_l1){

  /* Gets level 1 DSM inputs (output of level 0) and
     reorders them.  They are stored 3 ->0 followed by
     7 -> 4 followed by 11 -> 8 and 15 -> 12.  Only 0-5
     and 8-13 are used, the first group for the first 
     board.  Output is ordered as physically input to
     the board.  For first board the order is crate 3,
     4 then 5, and for second board crate 6, 1 then 2.
  */

  int i;
  for(i=0;i<Nee1;i++) {
    ee1[i].setWord(0,EEMC_l1[3+i*8]);
    ee1[i].setWord(1,EEMC_l1[2+i*8]);
    ee1[i].setWord(2,EEMC_l1[1+i*8]);
    ee1[i].setWord(3,EEMC_l1[0+i*8]);
    ee1[i].setWord(4,EEMC_l1[7+i*8]);
    ee1[i].setWord(5,EEMC_l1[6+i*8]);
  }
}


//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna ::readDsm2( const unsigned short *EMC){
  ee2->setWord(0,EMC[5]);
  ee2->setWord(1,EMC[4]);

}


//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna ::readDsm3( const unsigned short *lastDSM){
  ee3->setWord(0,lastDSM[0]);

}


//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna::histoDsm0(){
  int i,j;
  for(i=0;i<EEnTPphi; i++) {
    for(j=0;j< EEnTPeta;j++) {
      EEmapTP *y=&eeMapTP[i][j];
      int ibr=y->brdIn-1;
      int ch=y->chIn;
      int iJP=y->JPid-1;
      assert(iJP>=0 && iJP<EEnJetPatch);
      H0inHT[iJP]->Fill( ee0[ibr].getHT(ch),y->TPid);
      H0inTP[iJP]->Fill( ee0[ibr].getTP(ch),y->TPid);

      H0inHTall->Fill( 10*ibr+ch,ee0[ibr].getHT(ch));
      H0inTPall->Fill( 10*ibr+ch,ee0[ibr].getTP(ch));

      // printf("fill JP=%d TP=%d ADC=%d\n", y->JPid,y->TPid, ee0[ibr].getTP(ch));
    }
  }

}

//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna::histoDsm1(){

  int i,j;
  for (i=0;i<Nee1;i++) {
    EEdsm1 *br=ee1+i;
    for(j=0;j<br->getNc(); j++) {
      int ix=j+i*br->getNc();
      // .... TP ..................
      H1inTPvEmu[ix]->Fill(ee0outTPadc[ix],br->getTPsum(j));
      //...........  HT ............
      H1inHTvEmu[ix]->Fill(ee0outHTadc[ix],br->getHTthr(j));
    }
  }

  //printf("done with dsm1 histos on to jet patch histos \n");
  for (i=0;i<EEnJetPatch;i++) {     //histos for jet patch sums
    int steve_jp=((i+2)%EEnJetPatch)+1;  //stored by dsm input number 0-5 Want by Steves jp #
    H4jpSums[i]->Fill(ee1outJPadc[i]);
    H4jpFreq[JPtrig[i]]->Fill(steve_jp);
    H5jpPed ->Fill(steve_jp,ee1outJPadc[i]);
    if(ee1outJPadc[i]>EEjpTh1def)  H5jpFreq ->Fill(steve_jp); 
    j=(i+1)%EEnJetPatch;
    //printf("histog i=%d j=%d  \n",i,j);
    if (JPtrig[i]>0 && JPtrig[j]>0){  //Both patches over th0
      H4adjpFreq->Fill(steve_jp);
      H4adjPcor[i]->Fill(ee1outJPadc[i],ee1outJPadc[j]);
      H4adjpSums[i]->Fill(AdjJPsum[i]);
    }
  }
}



//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna::histoDsm2(){

  //------------ HT ---------------
  int iJ;
  for(iJ=0;iJ<EEnHalf;iJ++) {
    int max=0;
    int i;
    // .... HT ..................
    for(i=3*iJ; i<3*iJ+3; i++) 
      if(max<ee1outHT[i]) max=ee1outHT[i];
    H2inHTvEmu[iJ]->Fill(max,ee2->get3JPHTthr(iJ));
  }

  //------------------ TP---------------------
  for(iJ=0;iJ<EEnJetPatch;iJ++) {
     H2inTPvEmu[iJ]->Fill(ee1outJPadc[iJ],ee2->getJPthr(iJ));
  }
}

//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna::histoDsm3(){

  H3inHTvEmu->Fill(ee2outHT,ee3->getHTthr());
}

//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna ::printDsm0map(){

  printf("TP Map level-0 \n      ");

  int i,j;
  for(j=0;j< EEnTPeta;j++)  
    printf("        iEta=%d        ",j);

  printf("\n              ");
  for(j=0;j< EEnTPeta;j++)  
    printf("JP/TP brd:ch->outCh   ");


  for(i=0;i<EEnTPphi; i++) {
    int phi=111-i*12;
    if(phi<0) phi+=360;
    printf("\nPhi/deg=%3d   ",phi);
    for(j=0;j< EEnTPeta;j++) {
       EEmapTP *y=&eeMapTP[i][j];
      printf("%d/%2d %2d:%2d -> %2d      ",y->JPid,y->TPid,y->brdIn,y->chIn,y->chOut);
    }
  }
  printf("\n");
}



//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna :: print( int k) {
  printf("EEdsmAna(%s)::print()  \n", myName.Data());

  int i;
  for(i=0;i<Nee0;i++) {
    printf("\n----------- level-0 Board %2d ",i+1); 
    ee0[i].print();
  }
 
  printf("\n----------- level-0 emulated output \n ch =");
  for(i=Nee0out-1;i>=0; i--) printf("  %4d ",i);
  printf("\n TP =");
  for(i=Nee0out-1;i>=0; i--) printf("  %4d ",ee0outTPadc[i]);
  printf("\nHTadc =");
  for(i=Nee0out-1;i>=0; i--) printf("  %4d ",ee0outHTadc[i]);

  printf("\n\n----------- level-1 ----------------- "); 

  for(i=0;i<Nee1;i++) {
    printf("\n----------- level-1 Board %2d , ",i+1); 
    ee1[i].print();
    printf("emulated 3x.9 JP_Falk(%d+%d+%d) energy/dec: 13bit=%d  8bit=%d\n",3*i,3*i+1,3*i+2,ee1out3JPadc[i],ee1out3JPadc[i]>>5);
  }

  printf("\n----------- level-1 emulated output \n JP_Falk =");
  int Njp=EEnJetPatch;
  for(i=Njp-1;i>=0; i--) printf("  %4d ",i);
  printf("\n JP_Steve=");
  for(i=Njp-1;i>=0; i--) printf("  %4d ",(i+2)%6+1);
  printf("\n JPsum   =");
  for(i=Njp-1;i>=0; i--) printf("  %4d ",ee1outJPadc[i]);
  printf("\n HTthr   =");
  for(i=Njp-1;i>=0; i--) printf("  %4d ",ee1outHT[i]);
  printf("\n AdjJPsum=");
  for(i=Njp-1;i>=0; i--) printf("  %4d ",AdjJPsum[i]);
  printf("\n JPtrig  =");
  for(i=Njp-1;i>=0; i--) printf("  %4d ",JPtrig[i]);
  printf("\n");


  printf("\n\n----------- level-2 ----------------- "); 
  ee2->print();

  printf("\n\n----------- level-3 ----------------- "); 
  ee3->print();

}

//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna :: print2( int k) {// for Renee to compare w/ muDst
  printf("EEdsmAna(%s)::print2()  \n", myName.Data());
  //#Check#  0 0 0 1 5
  int layer,i;


  layer=0;
  for(i=0;i<Nee0;i++) {
    int board=i;
    int j;
    for(j=0;j<ee0[i].getNc();j++)
      printf("#Check#  %d %d %d %d %d\n",layer,board,j,ee0[i].getHT(j),ee0[i].getTP(j));
  }

  layer=1;
  for(i=0;i<Nee1;i++) {
    int board=i;
    int j;
    for(j=0;j<ee1[i].getNc();j++)
      printf("#Check#  %d %d %d %d %d\n",layer,board,j,ee1[i].getHTthr(j),ee1[i].getTPsum(j));
  }

  layer=2;
  int board=0;
  int j;
  for(j=0;j<ee2->getNc();j++)                                                
    printf("#Check#  %d %d %d %d %d\n",layer,board,j,ee2->get3JPHTthr(j),ee2->get3JPsum(j));

  
  layer=3;
  board=0;
  j=0;
  printf("#Check#  %d %d %d %d %d\n",layer,board,j,ee3->getHTthr(),ee3->getJPthr());
 


  ee2->print();

  printf("\n\n----------- level-3 ----------------- "); 
  ee3->print();


}


//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna ::usePed( TString fName){
  //......................... load ped for L-0
  printf("EEdsmAna(%s) reads pedestals from %s\n",myName.Data(),fName.Data());

  FILE *fd=fopen(fName.Data(),"r");
  assert(fd);
  int ibr;
  for(ibr=0;ibr<Nee0; ibr++) {
    int ch;
    for(ch=0;ch<ee0[ibr].getNc();ch++) {
      int a,b,c;
      int ret=fscanf(fd,"%d %d %d",&a,&b,&c);
      assert(ret==3);
      assert(a==ibr+1);
      assert(b==ch);
      ped0[ibr][ch]=c;
    }
  } 
}
 
