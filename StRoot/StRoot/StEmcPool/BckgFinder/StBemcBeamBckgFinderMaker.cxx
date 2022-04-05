/******************************************************************************
 * $Id: StBemcBeamBckgFinderMaker.cxx,v 1.13 2010/04/15 19:13:29 mattheww Exp $
 * \author Issam Qattan , IUCF, 2006 
 ******************************************************************************
 * Description:
 * Pattern recognition of the Barrel Beam Background on an event-by-event basis.
 *******************************************************************************
 */

#include <TH2.h>
#include <TCanvas.h>
#include <TStyle.h>
#include <stdio.h>

#include "StBemcBeamBckgFinderMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StDAQMaker/StDAQReader.h"

#include <StMessMgr.h>
#include <StEmcUtil/geometry/StEmcGeom.h>
#include <StEmcUtil/database/StBemcTables.h>

#include "StEmcUtil/database/StEmcDecoder.h"
#include "StEmcRawMaker/defines.h"

//MuDst
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTriggerIdCollection.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"


ClassImp(StBemcBeamBckgFinderMaker)

StBemcBeamBckgFinderMaker::StBemcBeamBckgFinderMaker(const char *name):StMaker(name){

  mGeomB = 0;
  mHList = 0;
  mMappB = 0; 
  memset(mhisto,  0,sizeof(mhisto));
  memset(mevtH,   0,sizeof(mevtH));
  mAccEve=mInpEve=0; 
  mTrigId=0;
  mAdcThreshold=0;
  mAdcSumThreshold=0;
  mpattern=0;
  
}
/*=============================================================================*/
/*=============================================================================*/

StBemcBeamBckgFinderMaker::~StBemcBeamBckgFinderMaker(){
  // Nothing for now
}

/*=============================================================================*/
/*=============================================================================*/

Int_t StBemcBeamBckgFinderMaker::Init(){

  mGeomB = StEmcGeom::instance("bemc");

  assert(mHList);
  mhisto[0]= new  TH1F("Frequency1","Towers Frequency (status =1); Tower SoftId; Frequency",mxSoftId,0.5,mxSoftId+0.5);
  mhisto[1]= new  TH1F("Frequency2","Towers Frequency (status !=1); Tower SoftId; Frequency",mxSoftId,0.5,mxSoftId+0.5);
  mhisto[2]= new  TH1F("Frequency3","bin1= All, bin3= Accept, bin5= !Backgnd, bin7=Backgnd bin9=East bin11=Center bin13=West; Bin No",15,0.5,15.5);
  mhisto[3]= new  TH1F("StartingphiVspattern","Pattern Starting #phi Weighted By Pattern Length; #phi[deg]",360,0.5,360.5);
  mhisto[4]= new  TH1F("StartingphiVsadcsum","Pattern Starting #phi Weighted By Pattern Summed Adc; #phi[deg]",360,0.5,360.5);

  mhisto[5]= new  TH1F("Allbunchcrossing7","All Bunch Crossings; Bunch Crossing No (7bit)",128,-0.50,127.5);
  mhisto[6]= new  TH1F("Trigbunchcrossing7","Trigger Passing Bunch Crossings; Bunch Crossing No (7bit)",128,-0.50,127.5);
  mhisto[7]= new  TH1F("Bckgbunchcrossing7","Background Passing Bunch Crossings; Bunch Crossing No (7bit)",128,-0.50,127.5);

  mevtH[0]= new TH2F("BtowphiVseta","Barrel Towers: ADC-PED; #eta Bin; #phi Bin",mxEta,-0.5,mxEta-0.5,mxPhi,-0.5,mxPhi-0.5);
  mevtH[1]= new TH2F("StartingphiVseta","Pattern Starting #phi Vs. Pattern Starting #eta; #eta; #phi(deg)",10,-1,1,30,0.5,360.5);
  mevtH[2]= new TH2F("AvgWeightedPhiVsEta","Pattern #phi Vs. Pattern Weighted Average #eta; <#eta>; #phi(deg)",20,-1,1,60,0.5,360.5);
 
  for(int i=0; i<mMaxH; i++){
    if(mhisto[i])
    mHList->Add(mhisto[i]);
  }
  
  for(int i=0; i<mMaxevtH; i++){
    if(mevtH[i])
    mHList->Add(mevtH[i]);
  }
   
  LOG_INFO<< Form("%s::Init() accept TrigID=%d\n",GetName(),mTrigId)<<endm;

  return StMaker::Init();
}

/*=============================================================================*/
/*=============================================================================*/

void StBemcBeamBckgFinderMaker::Clear(const Option_t* option){

  mevtH[0]->Reset();    //keep only eta-phi-adc histogram for the last event processed.
  memset(mAdcArray,   0,sizeof(mAdcArray));   //array cleared for each event
  memset(mPattSoftId, 0,sizeof(mPattSoftId)); //array cleared for each event
  
  mDecision=0;
  metaBegin=0;
  metaEnd=0;
  mphiBegin=0;
  mpatternLength=0;
  msumAdc=0;
  mAvgEtaAdc=0;
  mSearchDone=0;

  StMaker::Clear(option);
}

/*=============================================================================*/
/*=============================================================================*/

// ***** Make: this method is called in loop for each event

Int_t StBemcBeamBckgFinderMaker::Make(){

 
  mInpEve++;
  printf("\n");
  //printf("In%s::Make by Issam Qattan: Working on Event=%d\n",GetName(),mInpEve);
  StMuDstMaker *muMk = (StMuDstMaker*)GetMaker("MuDst");
  assert(muMk);

  StMuEmcCollection *emc = muMk->muDst()->muEmcCollection();

  //========process 7-bit Bunch-Crossing (bx7)==================================
  StMuDst *dst = muMk->muDst();
  StMuEvent *muEve = dst->event();
  StL0Trigger &trig = muEve->l0Trigger();
  StEventInfo &info = muEve->eventInfo();

  int bx7=trig.bunchCrossingId7bit(info.runId());
  mhisto[5]->Fill(bx7);  //fill 7bit bunch crossing for every event we processed. 

  //printf("eve=%d bx7=%d\n",mInpEve,bx7);
  
  
  //========process trigger info==================================================
 
  assert(muEve);
  StMuTriggerIdCollection ticB = muEve -> triggerIdCollection();
  const StTriggerId *nominal = &ticB.nominal();
   
  mhisto[2]->Fill(1);     //bin=1 gets filled once for every event we processed. 

  
  if(mTrigId<0) { // print all triggers
    vector<unsigned int> nominalL=nominal->triggerIds();
    printf("trigL len=%d totEve=%d\n",nominalL.size(),mInpEve);
    uint ii;
    for(ii=0;ii<nominalL.size();ii++)printf("trgID=%d, ",nominalL[ii]);
    printf("\n");
  }

  if(mTrigId>0 && !nominal->isTrigger(mTrigId))  return kStOK; // discard events
  mAccEve++;
 
  
  //...........................................................................//
  //.........................  B T O W   ......................................//

  int id;                        // id = this is soft ID

  for (id=0; id<mxSoftId; id++) {
    
    int ietabin = mdb_btowetaBin[id];
    int iphibin = mdb_btowphiBin[id]; 

    // int irdo = mdb_btowRdo[id]; 
    int isoftid = mdb_btowSoftId[id];
    // int istatus = mdb_btowStat[id];
    float rawAdc = emc->getTowerADC(id+1);
 
    if(mdb_btowStat[id] != 1) continue;  //added to skip failed towers (status != 1)
  
    float ped = mdb_btowPed[id];
    float adc = (rawAdc - ped);

    //printf("counter id=%d softId=%d Rdo=%d status=%d etabin=%d phibin=%d rawadc=%d ped=%f adc=%f\n",id,isoftid,irdo,istatus,ietabin,iphibin,rawAdc,ped,adc); 
    
    if(adc>mAdcThreshold) {
      //mhisto[0]->Fill(id+1);
      FillAdc(adc,ietabin,iphibin,isoftid);
      mevtH[0]->Fill(ietabin,iphibin,adc);
    }
    
  }
  
  mhisto[2]->Fill(3);     //bin=3 gets filled once for every event we accepted (satisfied trigger condition). 
  mhisto[6]->Fill(bx7);  //fill 7bit bunch crossing for every event we accepted (satisfied trigger condition).
 
  int myetapattern,myphipattern,myetaendpattern,mypatternlength;
  float myadcsum,myavgetaadc;

  
  bool IsItBackGround = CheckPatternType3(myetapattern,myphipattern,myetaendpattern,mypatternlength,myadcsum,myavgetaadc);

  mSearchDone=true; //set to true when search for background is done.

  if(IsItBackGround==1){
    mDecision=1; 
    metaBegin=myetapattern;
    mphiBegin=myphipattern;
    metaEnd=myetaendpattern;
    mpatternLength=mypatternlength;
    msumAdc=myadcsum;
    mAvgEtaAdc=myavgetaadc;
    
    //printf("Yes: Event=%d IS a Background\n",mInpEve);
    mhisto[2]->Fill(7);  //bin=7 gets filled for every background event we processed. 
    mevtH[1]->Fill((myetapattern-20.)/20.,myphipattern*3);
    mevtH[2]->Fill((myavgetaadc-20.)/20.,myphipattern*3);
    mhisto[3]->Fill(myphipattern*3,mypatternlength);
    mhisto[4]->Fill(myphipattern*3,myadcsum); 
    mhisto[7]->Fill(bx7); //gets filled for every background event we processed.

    

    if(metaEnd<20){
      mhisto[2]->Fill(9);                // EastSide Background
      sprintf(mLocation,"East");
    } 
    
    if((metaBegin<20)&&(metaEnd>=20)){   
      mhisto[2]->Fill(11);               // CentralSide Background
      sprintf(mLocation,"Central");
    } 
    
    if(metaBegin>19){
      mhisto[2]->Fill(13);               // WestSide Background
      sprintf(mLocation,"West");
    }
	
     //printf("Finally my background pattern starts at: phi=%d eta=%d pattern length=%d sum adc=%f\n", myphipattern,myetapattern, mypatternlength, myadcsum);
  }

  else {
    mDecision=0;
    //printf("Finally No: Event=%d IS NOT a Background\n",mInpEve);
    mhisto[2]->Fill(5);  //bin=5 gets filled once for every event that is not background.
  }

  //printf("**********************************************************************************\n");
 
  if(IsItBackGround==1) {
    if((mMaxYesPlots--)>0) PlotOneEvent(); 
  } else {
  if((mMaxNoPlots--)>0) PlotOneEvent();
  }

  return kStOK;
}

/*=============================================================================*/
/*=============================================================================*/

Int_t StBemcBeamBckgFinderMaker::InitRun(int runNo){

  LOG_INFO << GetName()<<"::InitRun()  run=" <<runNo<<endm;
  mRunNumber=runNo;
  
  // this is how the BTOW mapping is accesible
  assert(mMappB==0);        
  StEmcDecoder *mMappB = new StEmcDecoder(GetDate(),GetTime()); 
  
  StBemcTables *myTable=new StBemcTables;
  myTable->loadTables(this );


  memset(mdb_btowPed,     0,sizeof(mdb_btowPed));
  memset(mdb_btowStat,    0,sizeof(mdb_btowStat));

  memset(mdb_btowetaBin,  0,sizeof(mdb_btowetaBin));
  memset(mdb_btowphiBin,  0,sizeof(mdb_btowphiBin));


  memset(mdb_btowRdo,     0,sizeof(mdb_btowRdo));
  memset(mdb_btowSoftId,  0,sizeof(mdb_btowSoftId));

   
  int softID;

  for(softID=1; softID<=BTOWSIZE; softID++) {
 
   //...................................................//
  //........... querry BTOW DB/geom ...................//

    int status;
    myTable->getStatus(BTOW, softID, status);
    int m,e,s;
    mGeomB->getBin(softID,m,e,s);

    float etaF,phiF;
    mGeomB->getEta(m,e,etaF);
    mGeomB->getPhi(m,s,phiF);  // -pi <= phi < pi
    if(phiF<0) phiF+=2*C_PI; // I want phi in [0,2pi]

    float ped,sig;
    myTable->getPedestal(BTOW, softID, 0, ped,sig);

    float gain;
    myTable->getCalib(BTOW, softID, 1, gain);

    mdb_btowPed[softID-1]=ped;

    int Rdo;                                         
    assert(mMappB->GetDaqIdFromTowerId(softID,Rdo)==1); 

    mdb_btowRdo[softID-1] = Rdo;
    mdb_btowSoftId[softID-1] = softID;

    status= GetNewStatus(softID,Rdo,mRunNumber);
    mdb_btowStat[softID-1]=status;    

    // printf("back in InitRun: id=%d status=%d\n",softID,status);

    if(status != 1) mhisto[1]->Fill(softID);

    int kEta = int ((etaF+1.)*20.);     
    int kPhi = int (phiF*19.10);        // New Mapping == 19.10=(120/2pi) 
    //int kPhi = 24- (int) (phiF/C_PI*60.); // Old Jan mapping
    //if(kPhi<0) kPhi+=120;                 // Old Jan mapping

    mdb_btowetaBin[softID-1] = kEta;
    mdb_btowphiBin[softID-1] = kPhi; 

    mSoftId[kPhi][kEta] = softID;
  
    //printf("soft=%4d Rdo=%d ped=%f eta=%.2f phi(rad)=%f Etabin=%d Phibin=%d\n",softID,Rdo,ped,etaF,phiF,kEta,kPhi);

  }
 
  return kStOk;

}

/*=============================================================================*/
/*=============================================================================*/

Int_t StBemcBeamBckgFinderMaker::Finish(){
  //
  LOG_INFO << Form("%s::Finish() accepted %d eve of %d input events,  TrigID=%d\n",GetName(),mAccEve, mInpEve,mTrigId)<<endm;
 return kStOK;
}

/*=============================================================================*/
/*=============================================================================*/

Int_t StBemcBeamBckgFinderMaker::GetNewStatus(int id, int rdo, int run){

  const int db_btowAddMaskA[] = {4505,533,577,578,579,580,4020,4019,816,839,1612,2916,2897,1773,1774,1775,1776,2085,2105,2257,0}; 
  const int db_btowAddMaskB[] = {3287,3309,3407,3674,0}; 
  const int db_btowAddMaskC[] = {0};

  const int *db_btowAddMask;
  int xMask;

  if(run>7000000) {
    db_btowAddMask=db_btowAddMaskA;
    xMask=20;
  } else {
    if(run==6169089) {
      db_btowAddMask=db_btowAddMaskB; 
      xMask=4;
    } else {
      db_btowAddMask=db_btowAddMaskC;
      xMask=0;
    }
  }
  

  for(int i=0; i<xMask; i++) {
    
    //printf("db_btowAddMask =%d\n",db_btowAddMask[i]);
    
    if(db_btowAddMask[i]<=0) {
      printf("Error Error Error: your masked array db_btowAddMask=0 for this run number\n");  
      assert(1==2);
    }   
  
    if(db_btowAddMask[i]==id){
      printf("===================================================================\n");
      printf("===================================================================\n");
      printf("RunNo=%d :: tower with softid=%d and Rdo=%d will be masked\n",run,id,rdo);
      return (0);
    }
  }
  
  return (1);
}

/*=============================================================================*/
/*=============================================================================*/

void StBemcBeamBckgFinderMaker::FillAdc(float adc,int ieta,int iphi,int isoft){
  assert(ieta>=0);
  assert(ieta<mxEta);

  assert(iphi>=0);
  assert(iphi<mxPhi);

  mAdcArray[iphi][ieta] = adc;
 
  //printf("In FillAdcUsed(): ieta=%d iphi=%d isoftid=%d mAdcArray[iphi][ieta]=%f\n",ieta,iphi,isoft,adc);

}

/*=============================================================================*/
/*=============================================================================*/

void  StBemcBeamBckgFinderMaker::PlotOneEvent(){

  char Decision[10];
  char HistoTitle_Out[2000]; //title of the histogram. Replacing the original
  char FileTitle_Out[1000]; // mevtH[0] histograms will be saved for each accepted event in a ps and/or gif files.


  if(mDecision==1){
    sprintf(Decision,"Yes");
    sprintf(HistoTitle_Out,"R=%d E=%05d T=%d Bg=%s #eta1=%d #phi=%d #eta2=%d L=%d Adc+=%f Loct=%s",mRunNumber,mInpEve,mTrigId,Decision,metaBegin,mphiBegin,metaEnd,mpatternLength,msumAdc,mLocation);
  }

  if(mDecision==0){
    sprintf(Decision,"No");
    sprintf(HistoTitle_Out,"Run=%d, Eve=%05d, TrgId=%d, Bckg=%s",mRunNumber,mInpEve,mTrigId,Decision);
  }


  sprintf(FileTitle_Out,"event%05d_%s.ps",mInpEve,Decision); // name of ps file
  //sprintf(FileTitle_Out,"event%05d_%s.gif",mInpEve,Decision); // name of gif file if needed
  TCanvas mycanvas("aa","aa",600,800); //create a canvas
  mevtH[0]->SetTitle(HistoTitle_Out);
  mevtH[0]->GetXaxis()->CenterTitle();
  mevtH[0]->GetYaxis()->CenterTitle();
  gStyle->SetPalette(1,0); 
  gStyle->SetOptStat(0);
  gStyle->SetTitleFontSize(0.08);
  mevtH[0]->Draw("colz");
  mycanvas.Print(FileTitle_Out);
  //printf("Out of ::PlotOneEvent(), just finished printing to a file=%s\n",FileTitle_Out);
}

/*=============================================================================*/
/*=============================================================================*/


Int_t StBemcBeamBckgFinderMaker::CheckPatternType3(int &etaBegin, int &phiBegin, int &etaEnd, int &patternLength, float &sumAdc, float &AverageWeightedEta){

  int maxLength = 0;   // initial value for number of adjacent towers found (in eta direction).
  int i,j,k,n; 
  
  int FirstEtaFound;
  float FirstEtaAdc,sum=0.;
  float sumEtaAdc=0.;    

  //printf("**********************************************************************************\n");
  //printf("In ::CheckPatternType3(): These towers are used in pattern searched\n");
    
  for(i=0; i<mxPhi; i++){        //loop over phi
    float *adc = mAdcArray[i];
    n = 0;                      //counter over adjacent towers
    maxLength = 0;  
    memset(mPattSoftId,  0,sizeof(mPattSoftId));
   

    for(j=0; j<mxEta; j++){    //loop over eta
      if(adc[j]<=0) continue;  

      n = 1;  
      FirstEtaFound=j;
      FirstEtaAdc = adc[j];
      sum = FirstEtaAdc;
      sumEtaAdc=(FirstEtaFound*sum);  
      mPattSoftId[n-1] = mSoftId[i][j]; //[n-1] to make sure array starts from zero 

      //printf("start mPattSoftId[%d]=%d\n",n,mPattSoftId[n-1]);
      //printf("(Start probing from: phi=%d eta=%d adc=%f n=%d\n",i,j,adc[j],n);
      
      for(k=j+1; k<mxEta; k++){
	
	if((adc[k]<mAdcThreshold)&&(adc[k+1]<mAdcThreshold)) {
	  
	  break;  //exist for(k=j+1; k<mxEta; k++) loop.
	} 
	
	sum = sum + adc[k];
        sumEtaAdc=sumEtaAdc+(k*adc[k]); 
	n++;
	mPattSoftId[n-1] = mSoftId[i][k];

	//printf("next mPattSoftId[%d]=%d\n",n,mPattSoftId[n-1]);
	//printf("(k=eta+1 i.e, looping over adjacent towers if any) In k loop: phi=%d eta=%d k=%d adc=%f n=%d\n",i,j,k,adc[k],n);
      }

      maxLength=0;
      
      if(maxLength<n) maxLength=n;
      
      j=k;
      
      
      if((maxLength>=mpattern)&&(sum>=mAdcSumThreshold)){
        //printf("**********************************************************************************\n");
        //printf("Found The First BACKGROUND Pattern. No Need To Search Further: Will Stop Here\n");
	//printf("Number of adjacent towers: maxLength = %d adjacent towers >= background pattern length=%d\n",maxLength,pattern);
        //printf("******************** SUMMARY SUMMARY SUMMARY *************************************\n");
        //printf("phi=%d eta=%d pattern length=%d sum adc=%f\n",i,FirstEtaFound,maxLength,sum);
        //printf("Yes %d %d %d %f\n",i,FirstEtaFound,maxLength,sum);
        //printf("**********************************************************************************\n");
      
	phiBegin=i;
        etaBegin=FirstEtaFound; 
	patternLength=maxLength;
        etaEnd=(etaBegin + (patternLength-1));
        sumAdc=sum;
        AverageWeightedEta=(sumEtaAdc/sum); 
        //printf("my average weighted eta for this pattern=%f\n",AverageWeightedEta);
	return(true);
	break;
      }
    }
  }
  //printf("Not Background pattern\n");
  return(false);
}

/*=============================================================================*/
/*=============================================================================*/

void StBemcBeamBckgFinderMaker::GetDecision(int &fDecision, int &eta1, int &phi1, int &eta2, int &patternleng, float &Adcsum){
  
  if((mSearchDone)&&(mDecision)){
    
    fDecision=mDecision;
    eta1=metaBegin;
    eta2=metaEnd;
    phi1=mphiBegin;
    patternleng=mpatternLength;
    Adcsum=msumAdc;
  } else {

    if((mSearchDone)&&(!mDecision)){      
      fDecision=mDecision; 
    } else {
      fDecision=-1;
    } 
    eta1=0;
    eta2=0;
    phi1=0;
    patternleng=0;
    Adcsum=0;
    
  }
}

/*=============================================================================*/
/*=============================================================================*/
    
/**********************************************************************
  $Log: StBemcBeamBckgFinderMaker.cxx,v $
  Revision 1.13  2010/04/15 19:13:29  mattheww
  fixed some future gcc issues

  Revision 1.12  2009/02/04 20:33:30  ogrebeny
  Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388

  Revision 1.11  2007/08/22 14:03:50  kocolosk
  #included some additional headers that used to be included implicitly from StBemcTables

  Revision 1.10  2006/06/27 16:54:47  qattan
  *** empty log message ***

  Revision 1.9  2006/06/27 15:41:30  qattan
  *** empty log message ***

  Revision 1.8  2006/06/13 21:50:36  qattan
  *** empty log message ***

  Revision 1.7  2006/06/13 21:42:42  qattan
  *** empty log message ***

  Revision 1.6  2006/06/13 21:26:25  qattan
  *** empty log message ***

  Revision 1.5  2006/05/30 23:05:23  qattan
  check5

  Revision 1.4  2006/05/30 22:40:29  qattan
  check4

  Revision 1.3  2006/05/30 22:21:41  qattan
  *** empty log message ***

  Revision 1.2  2006/05/30 20:12:56  qattan
  fix 1

  Revision 1.1  2006/05/30 20:08:03  qattan
  start1

 
*/
