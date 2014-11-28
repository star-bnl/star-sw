/*************************************************
 *
 * StPmdCalibConstMaker.cxx$
 * Author: Raghunath Sahoo, IOP, Bhubaneswar, Subhasis Chattopadhyay, VECC, Kolkata.
 *************************************************
 *
 * Description: Base class for Pmd Calibration Constant Maker
 *
 *************************************************
 *
 * Initial version : 28th aug 2003
 *  The code is tested to work in simulation chain, and tested with 
 *  data taken with PMD last year. Numbers are written to DB, and tested
 *  to read them back. 
 *
 *   Some changes are forseen in data storing once a large amount 
 *   PMD data will be run to generate the numbers.
 *
 *************************************************/

/* !
 
This Maker obtains calibrations constants for all cells in PMD and CPV.
This collects MIP spectra for each cell and at the end it fits them with 
Landau function and then generates the calibration constants. these calibration 
constants are then stored in calibration database
*/

#include"Stsstream.h"
#include<assert.h>
#include<math.h>
#include"TROOT.h"
#include"TMath.h"
#include<TRandom.h>
#include<StMessMgr.h>
#include<TFile.h>
#include<TF1.h>
#include "StPmdUtil/StPmdGeom.h"
#include "StPmdCalibConstMaker.h"
#include "StPmdUtil/StPmdDetector.h" //! will be obtained 
#include "StPmdUtil/StPmdModule.h" //! will be obtained 
#include "StPmdUtil/StPmdCollection.h" //! will be obtained 
#include "StPmdUtil/StPmdHit.h"         //! will be obtained 
//...........................................................
#include "TStopwatch.h"
#include "StarClassLibrary/SystemOfUnits.h"
#include "time.h"
#include "TDatime.h"

#include "StDbLib/StDbManager.hh"
#include "StDbLib/StDbTable.h"
#include "StDbLib/StDbConfigNode.hh"

#ifndef ST_NO_NAMESPACES
using units::tesla;
#endif 



ClassImp(StPmdCalibConstMaker)

  /*!
    These arrays are used to store calibrations constants to be stored in
    database.PMD(and CPV) calibrations numbers are stored in terms of board_no,
    channel no, so it is necessary to convert [sm_no][row][col] into
    [board][channel]
  */

  Double_t MipArray[PMD_CRAMS_MAX*2][PMD_ROW_MAX][PMD_COL_MAX][MIP_CH_MAX];

  //___________________------------
  //! constants used for calculating coordinate of neighbouring cells
Int_t StPmdCalibConstMaker::neibx[PMD_CELL_NEIGHBOUR] = {1,0,-1,-1,0,1};

Int_t StPmdCalibConstMaker::neiby[PMD_CELL_NEIGHBOUR] = {0,1,1,0,-1,-1};

Int_t StPmdCalibConstMaker::imax[2*PMD_CRAMS_MAX] = {48,72,48,72,72,72,48,48,48,72,72,96,48,72,48,72,72,72,48,48,48,72,72,96};
//for col
Int_t StPmdCalibConstMaker::jmax[2*PMD_CRAMS_MAX] = {48,48,72,72,48,48,48,72,48,72,48,72,48,48,72,72,48,48,48,72,48,72,48,72};
//for row

//-------------------- 
StPmdCalibConstMaker::StPmdCalibConstMaker(const char *name):StMaker(name)
{
  // mSaveCalibToDB = kFALSE;  //To be checked
  mSaveCalibToDB = kTRUE; 
  mOptHist = kTRUE; 
  mDate=0;
  mTime=0;
  InitMipParams();
  mPmdGeom=new StPmdGeom();
  mPmdDbUtil=new StPmdDBUtil();
}
//----------------------

StPmdCalibConstMaker::~StPmdCalibConstMaker()
{
  cout << "**** I am in StPmdCalibConstMaker::~StPmdCalibConstMaker()"<< endl;
  
  if(mPmdGeom){mPmdGeom=0;delete mPmdGeom;}
  if(mPmdDbUtil){mPmdDbUtil=0;delete mPmdDbUtil;}
  if(mOptHist)ClearHists();
}

//---------------------
Int_t StPmdCalibConstMaker::Init()
{
  //Initialize date and time
  if(mOptHist)BookHistograms();
  
  //---------------------------------------------------
  // Newly included from this
  
  /*! get the mapping for boards. In pmdUtil, we have mapping for 
    'chain_no,channel_no to Sm_no,row,col', what we need here is
    from Sm_no,row,col, we need to convert to board_no,channel_no. 
    The numbering of board no is done using the scheme that,
    board_no=(chain_no)*27+(channel_no/64),chain_no and channelNo 
    goes from 0 to 24 and 0 to 1727 respectively.
  */
  ClearMipArray();

  return StMaker::Init();
}

//--------------------------------------
void StPmdCalibConstMaker::BookHistograms()
{

  for(Int_t sm=0;sm<2*PMD_CRAMS_MAX;sm++){//! SM goes from 0 to 23
    for(Int_t i =0;i<imax[sm];i++){
      for(Int_t j =0;j<jmax[sm];j++){
	char text[40],title[80];
	sprintf(text,"calib_%02d_%02d_%02d",sm,i,j);
	sprintf(title,"MIP plot for %02d_%02d_%2d",sm,i,j);
	mMipEnergy[sm][i][j]=new TH1F(text,title,200,0.,150.);
      }
    }
  }
}


//-------------------------------------------

Int_t StPmdCalibConstMaker::Make() 
{
  if (mDate==0) {mDate = GetDate();}
  if (mTime==0) {mTime = GetTime();}

  TDataSet* CalibIn = GetDataSet("pmdReader");
  if (!CalibIn){CalibIn = GetDataSet("PmdSimulator");}
  if (CalibIn){
    StPmdCollection *CalibHit = (StPmdCollection*)CalibIn->Find("PmdCollection");
    if(CalibHit){
      StPmdDetector * cpv_det = CalibHit->detector(Int_t(1));
      StPmdDetector * pmd_det = CalibHit->detector(Int_t(0));
      GetIsoHit(pmd_det,cpv_det);
    }
  }
  else{cout<<"Could not find Simulator/Reader"<<endl; return kStOK;}
  return kStOK;
}//! loop for make ends here.


void StPmdCalibConstMaker::GetIsoHit(StPmdDetector* pmd_det, 
				     StPmdDetector* cpv_det)
{
  /*! Finds Isolated hits for each cell to accumulate MIP spectra */
  
  Int_t xpad,ypad,gsuper,idet;
  Float_t edep;
  Float_t d0[PMD_ROW_MAX][PMD_COL_MAX];
  
  if(cpv_det){  //! getting CPV detector
    for(Int_t id=0;id<PMD_CRAMS_MAX;id++){   //! loop for supermodule
      //!  id has to be 0 to 11
      for(Int_t j=0;j<PMD_ROW_MAX;j++){
	for(Int_t k=0;k<PMD_COL_MAX;k++){
	  d0[j][k]=0;  //! Initialize CPV arrays
	}
      }
      
      
      StPmdModule * cpv_mod=cpv_det->module(id);
      if(cpv_det->module_hit(id)>0){
	Int_t nmh=cpv_det->module_hit(id);  //! total no.of hits in 
	TIter next(cpv_mod->Hits());
	StPmdHit *spmhit0;   //! pointer for hits
	for(Int_t im=0; im<nmh; im++){
	  spmhit0 = (StPmdHit*)next();
	  if(spmhit0)
	    {
	      ypad=spmhit0->Row();          //! row of the hit
	      xpad=spmhit0->Column();       //! column of the hit
	      edep=spmhit0->Edep();         //! edep
	      Int_t adc=spmhit0->Adc();     //! adc
	      gsuper = spmhit0->Gsuper();   //! supermodule (for CPV 0-11)
	      idet=spmhit0->SubDetector();  //! detector(= 2) for Cpv
	      if(idet==2){
if((xpad>0 && xpad<=PMD_ROW_MAX) && (ypad>0 && ypad <=PMD_COL_MAX))d0[xpad-1][ypad-1]=adc;  //! edep added for each cell
	      }
	    }
	}
	
	//! Identifying neighbouring cells : for isolated hits.
	
	for(Int_t i=0;i<imax[id];i++){
	  for(Int_t j=0;j<jmax[id];j++){
	    if(d0[i][j] > 0.){
	      Int_t count = 0;
	      for(Int_t ii=0;ii<PMD_CELL_NEIGHBOUR;ii++){
		Int_t id1 = i + neibx[ii];
		Int_t jd1 = j + neiby[ii];
		if(d0[id1][jd1]==0.){
		  count++;
		  if(count == PMD_CELL_NEIGHBOUR){//finding isolated cell
		    gsuper = spmhit0->Gsuper(); 
		    gsuper = gsuper-1;
		    if(mOptHist)mMipEnergy[gsuper][i][j]->Fill(d0[i][j]);
		    Int_t channel=(Int_t)d0[i][j];
		    if(channel<MIP_CH_MAX)MipArray[gsuper][i][j][channel]++;
		    
		  }
		}
	      }
	    }
	  }
	}
	
      }// if-- module hit
    }// for supermodule
  }// if---CPV detector



  Float_t d1[PMD_ROW_MAX][PMD_COL_MAX];
  
  if(pmd_det){  //! getting Pmd detector
    for(Int_t id=0;id<PMD_CRAMS_MAX;id++){   //! loop for supermodule
      //!  id has to be 0 to 11
      for(Int_t j=0;j<PMD_ROW_MAX;j++){
	for(Int_t k=0;k<PMD_COL_MAX;k++){
	  d1[j][k]=0;  //! Initialize Pmd arrays
	}
      }
      
      StPmdModule * pmd_mod=pmd_det->module(id);
      if(pmd_det->module_hit(id)>0){
	Int_t nmh=pmd_det->module_hit(id);  //! total no.of hits in 
	TIter next(pmd_mod->Hits());
	StPmdHit *spmhit1;   //! pointer for hits
	for(Int_t im=0; im<nmh; im++){
	  spmhit1 = (StPmdHit*)next();
	  if(spmhit1)
	    {
	      ypad=spmhit1->Row();          //! row of the hit
	      xpad=spmhit1->Column();       //! column of the hit
	      edep=spmhit1->Edep();         //! edep
	      Int_t adc=spmhit1->Adc();     //! adc
	      gsuper = spmhit1->Gsuper();   //! supermodule
	      idet=spmhit1->SubDetector();  //! detector(= 1) for Pmd
	      if(idet==1){
if((xpad>0 && xpad<=PMD_ROW_MAX) && (ypad>0 && ypad <=PMD_COL_MAX))d1[xpad-1][ypad-1]=adc;  //! edep added for each cell
	      }
	    }
	}

	//! Identifying neighbouring cells : for isolated hits.
	
	for(Int_t i=0;i<imax[id];i++){
	  for(Int_t j=0;j<jmax[id];j++){
	    if(d1[i][j] > 0.){
	      Int_t count = 0;
	      for(Int_t ii=0;ii<PMD_CELL_NEIGHBOUR;ii++){
		Int_t id1 = i + neibx[ii];
		Int_t jd1 = j + neiby[ii];
		if(d1[id1][jd1]==0.){
		  count++;
		  if(count == PMD_CELL_NEIGHBOUR){//finding isolated cell
		    gsuper = spmhit1->Gsuper();//! gives  supermodules 1 to 12
		    gsuper = gsuper + 11;
		    if(mOptHist)mMipEnergy[gsuper][i][j]->Fill(d1[i][j]);
		    Int_t channel=(Int_t)d1[i][j];
		    if(channel<MIP_CH_MAX)MipArray[gsuper][i][j][channel]++;;
		 
		  }
		}
	      }
	    }
	  }
	}
	
      }// if-- module hit
    }// for supermodule
  }// if---PMD detector
}//void

Int_t StPmdCalibConstMaker::FindMipParameters()
{
  Float_t MipFitParam[3];
  Float_t MipFitChiSqr, MipPeakPosition, MipPeakWidth;
  TF1 LandauFunction("LandauFunction","landau",0,100);
  LandauFunction.SetParameters(1,1,1);
  LandauFunction.SetParNames("Constant","MPV","Sigma");
  Float_t MPV_Sum   = 0;
  Int_t   MPV_Count = 0;
  TH1F * miphist;
  Stat_t entryFlag=0;
  for(Int_t sm=0;sm<2*PMD_CRAMS_MAX;sm++){
    for(Int_t i =0;i<imax[sm];i++){
      for(Int_t j =0;j<jmax[sm];j++){
	if(!mOptHist){
	 miphist=new TH1F("miphist","MipHist",200,0.,140.);
       for(Int_t k =0;k<10;k++){
	      miphist->SetBinContent(k+1,(Stat_t)MipArray[sm][i][j][k]);
       }
	entryFlag = miphist->GetEntries();
	}
	if(mOptHist)entryFlag = mMipEnergy[sm][i][j]->GetEntries();

	//if(entryFlag > 5){
	 if(entryFlag > MIP_MIN_ENTRY){ // new 
	 if(mOptHist)mMipEnergy[sm][i][j]->Fit("LandauFunction","q","r");
	 if(!mOptHist)miphist->Fit("LandauFunction","q","r");
	  for(Int_t m=0; m < 3; m++){
	    MipFitParam[m]    = LandauFunction.GetParameter(m);
	  }

	  if(MipFitParam[1] > 0.){
	    MPV_Sum             = MPV_Sum + MipFitParam[1];
	    MPV_Entry[sm][i][j] = MipFitParam[1]; 
	    MPV_Count++;
	  }
	  MipFitChiSqr        = LandauFunction.GetChisquare();
	  //Float_t ndf       = func->GetNDF();
	  if(mOptHist){
		MipPeakPosition     = mMipEnergy[sm][i][j]->GetMean();
	  	MipPeakWidth        = mMipEnergy[sm][i][j]->GetRMS();
	  }
	  else{
	        MipPeakPosition     = miphist->GetMean();
	        MipPeakWidth        = miphist->GetRMS();
	  }
	  
          Int_t BrdNo=0;
	  mPmdDbUtil->BoardNumber(sm,i,j,BrdNo);
          Int_t BrdCh=0;
	  mPmdDbUtil->ChannelInBoard(sm,i,j,BrdCh);
	  
	  //mMipPeak[BrdNo][BrdCh]=MipPeakPosition;//!new
          mMipWidth[BrdNo][BrdCh]=MipPeakWidth;
	  
	}//! if for entryFlag
	if(!mOptHist)delete miphist;
      } // ! col
    }  //! row
  } //! Sm
  
  Float_t MPV_Av = MPV_Sum/MPV_Count;
  //  Float_t MPV_Av = 4.5;
  
  for(Int_t sm=0;sm<2*PMD_CRAMS_MAX;sm++){
    for(Int_t i =0;i<imax[sm];i++){
      for(Int_t j =0;j<jmax[sm];j++){
	if(MPV_Entry[sm][i][j] > 0.)
	  normFactor[sm][i][j] = (Float_t) MPV_Av/(Float_t) MPV_Entry[sm][i][j];
	Int_t BrdNo=0;
	mPmdDbUtil->BoardNumber(sm,i,j,BrdNo);
	Int_t BrdCh=0;
	mPmdDbUtil->ChannelInBoard(sm,i,j,BrdCh);
	
	mMipPeak[BrdNo][BrdCh]=normFactor[sm][i][j];//!new
      }
    }
  }
  return kStOK;
}

Int_t StPmdCalibConstMaker::Finish()
{
  cout << " *** I am in StPmdCalibConstMaker::Finish() " << endl;
  
  TStopwatch clock;
  clock.Start();
  
  FindMipParameters(); 
  if(mSaveCalibToDB)SaveCalibration(); 
  
  gMessMgr->Info("StPmdCalibConstMaker::Finish()");
  return StMaker::Finish();
  //return kStOK;
}

//_____________________________________________________________________________
void StPmdCalibConstMaker::SaveCalibration()
{
  
  cout <<" ***** I am now saving the Calibration to DB : Wait !!" << endl;
  //  TDatime *tt = new TDatime(date,time);
  //  TString timestamp = tt->AsSQLString();
  //  delete tt;
  
  //  if(!mSaveCalibToDB) return;
  /* Presently it is storing some numbers , coming from simulated files,
     it is tested with daq file, and time stamp works., so once we have good 
     amount fo data , it should work*/
  
  
  StDbManager* mgr=StDbManager::Instance();
  StDbConfigNode* node=mgr->initConfig("Calibrations_pmd");
  StDbTable* tab1=node->addDbTable("pmdBrdMipCalib");
  StDbTable* tab2=node->addDbTable("pmdCalSummary");
  pmdBrdMipCalib_st pbd[PMD_BOARD_MAX];
  memset(pbd,0,PMD_BOARD_MAX*sizeof(pmdBrdMipCalib_st));
  for(Int_t ism=0;ism<2*PMD_CRAMS_MAX;ism++){
    for(Int_t irow=0;irow<jmax[ism];irow++){
      for(Int_t icol=0;icol<imax[ism];icol++){
	Int_t brd=0;
	Int_t brd_ch=0;
	mPmdDbUtil->BoardNumber(ism,irow,icol,brd);
	mPmdDbUtil->ChannelInBoard(ism,irow,icol,brd_ch);
	pbd[brd].FEEBoardNumber = brd;
	//filling peak and width from arrays
	
	pbd[brd].MipPeakPosition[brd_ch] = mMipPeak[brd][brd_ch];
	pbd[brd].MipPeakWidth[brd_ch] = mMipWidth[brd][brd_ch];
      }
    }
  }
  tab1->SetTable((char*)pbd,PMD_BOARD_MAX);
  
  pmdCalSummary_st pcs[1];
  memset(pcs,0,sizeof(pmdCalSummary_st));
  // Summary table will be filled here. 
  
  tab2->SetTable((char*)pcs,1);
  
  //storing timestamp from the first event in the chain
  char timestamp[15]; 
  sprintf(timestamp,"%08d%06d",mDate,mTime);
  mgr->setStoreTime(timestamp);
  //mgr->setStoreTime("2003-12-29 11:11:15");
  cout<<"PMD Calib: storing in DB"<<endl;
  
  mgr->storeDbTable(tab1);
  mgr->storeDbTable(tab2);
  cout<<"PMD Calib: Stored in DB "<<endl;
  
}


void StPmdCalibConstMaker::InitMipParams()
{
  for(Int_t i=0;i<PMD_BOARD_MAX;i++){
    for(Int_t j=0;j<PMD_BOARD_CH_MAX;j++){
      mMipPeak[i][j]=0.;
      mMipWidth[i][j]=0.;
      
    }
  }
}

void StPmdCalibConstMaker::ClearHists()
{
  cout << " *** Let me to clear the Histograms in PmdCalib" << endl;
  
  for(Int_t sm=0;sm<2*PMD_CRAMS_MAX;sm++){
    for(Int_t i =0;i<imax[sm];i++){
      for(Int_t j =0;j<jmax[sm];j++){
	
	if(mMipEnergy[sm][i][j]){
	  mMipEnergy[sm][i][j]=0;
	  delete mMipEnergy[sm][i][j];
	}
      }
    }
  }
}
void StPmdCalibConstMaker::ClearMipArray()
{
  for(Int_t sm=0;sm<2*PMD_CRAMS_MAX;sm++){
    for(Int_t i =0;i<imax[sm];i++){
      for(Int_t j =0;j<jmax[sm];j++){
       for(Int_t k =0;k<MIP_CH_MAX;k++){
	  MipArray[sm][i][j][k]=0;
	}
      }
    }
  }
}











