// 
// $Id: StEmcADCtoEMaker.cxx,v 1.67 2004/04/05 20:06:46 suaide Exp $
// $Log: StEmcADCtoEMaker.cxx,v $
// Revision 1.67  2004/04/05 20:06:46  suaide
// added feature to print maps
//
// Revision 1.66  2004/04/05 18:56:15  suaide
// raw data structure is filled in StEvent
//
// Revision 1.65  2004/03/31 23:56:25  jeromel
// Initialize mPedRMS, mPed and added a value to an empty return statement in
// a non-void context (ANSI C++ forbids this). Should fix valgrind messages.
//
// Revision 1.64  2004/03/24 00:09:50  suaide
// small bug fixed and PSD is added to the maker
//
// Revision 1.63  2004/01/26 22:50:13  perev
// WarnOff
//
// Revision 1.62  2003/10/14 13:36:41  suaide
// small change
//
// Revision 1.61  2003/10/14 13:27:30  suaide
// new methods added in order to select either energy of pedestal cut for the SMD
//
// Revision 1.60  2003/10/10 14:26:12  suaide
// small fix
//
// Revision 1.59  2003/10/10 13:38:28  suaide
// change to allow RMS or Energy CUT. Default is now RMS.
//
// Revision 1.58  2003/10/03 21:12:59  suaide
// some histograms are created only in debug mode to save memory
//
// Revision 1.57  2003/10/03 16:02:57  suaide
// version changed
//
// Revision 1.56  2003/10/03 14:02:23  suaide
// NULL points initialization fixed
//
// Revision 1.55  2003/10/02 15:51:13  suaide
// possible memory leak removed
//
// Revision 1.54  2003/09/19 14:34:39  suaide
// Removed muDST option from StEmcADCtoEMaker. Will find another solution for that
//
// Revision 1.53  2003/09/18 17:15:53  suaide
// Modifications in the way StEmcADCtoEMaker handles the database requests,
// fixed a bug related to GetDate() that was making the maker extremely slow,
// added the option of reading muDst events directly (need to use StEmcPreDbMaker
// in order to set the correct timestamp for database)
//
// StEmcPreDbMaker just sets the correct timestamp for St_db_Maker when reading
// muDst files without convertion to StEvent. Should run before St_db_Maker
//
// Revision 1.52  2003/09/13 00:42:28  perev
// XDF obsolete + small fixes
//
// Revision 1.51  2003/09/12 22:03:54  jeromel
// Bug fix : data member not zeroed (later delete)
//
// Revision 1.50  2003/09/11 20:39:57  suaide
// Removed SMD capacitors 124 and 125 from data for dAu and pp Y2003 runs only.
// It is timestamp flagged so it will work only for this data.
// The point is the fact the the ped subtracted SMD data looks strange for these
// two capacitors values
//
// Revision 1.48  2003/09/08 20:56:41  suaide
// Patch to fix problem with SMD-phi pedestals saved on database
//
// Revision 1.45  2003/08/29 19:34:22  suaide
// small modification in the histogram binning
//
// Revision 1.44  2003/08/29 13:01:18  suaide
// QA Histograms fixed. Histogram bin size fixed
//
// Revision 1.43  2003/04/03 13:17:49  suaide
// option to turn off message log was included
//
// Revision 1.42  2003/02/19 23:15:10  suaide
// small modifications
//
// Revision 1.41  2003/02/19 22:28:32  suaide
// small modifications on QA histograms
//
// Revision 1.40  2003/02/10 16:29:20  suaide
// small modifications
//
// Revision 1.39  2003/02/06 16:34:58  suaide
// changes on data validation
//
// Revision 1.38  2003/02/04 18:33:19  suaide
// small modifications
//
// Revision 1.37  2003/01/23 03:09:05  jeromel
// Include modif
//
// Revision 1.36  2003/01/21 22:40:29  suaide
// small modification to save all tower hits
//
// Revision 1.35  2003/01/17 23:02:25  suaide
// small modification
//
// Revision 1.34  2002/12/02 21:20:12  suaide
// modifications for new DB scheme
//
// Revision 1.33  2002/09/30 14:03:00  suaide
// small bugs fixed
//
// Revision 1.32  2002/09/19 21:32:23  suaide
// Modifications to use a new internal data format
//
// Revision 1.31  2002/05/22 22:04:23  suaide
// small bug fixed to reconstruct micro DST's
//
// Revision 1.30  2002/05/15 15:05:28  suaide
// bugs fixed to recalibrate EMC after production
//
// Revision 1.29  2002/02/25 16:23:24  suaide
// reduced SMD threshold
//
// Revision 1.28  2002/02/24 21:19:21  suaide
// clean up and modifications on the settings that allow to save only hits
// above a given threshold that can be defined for each sub detector.
//
// Revision 1.27  2001/12/28 15:03:09  suaide
// fixed documentation
//
// Revision 1.26  2001/12/27 17:45:36  suaide
// removed obsolete files and updated documentation
//
// Revision 1.24  2001/12/26 19:25:34  suaide
// Added documentation and few modifications
//
// Revision 1.23  2001/12/06 17:50:08  suaide
// changes to save ADC without pedestal subtraction
//
// Revision 1.22  2001/12/05 22:31:12  suaide
// Modifications to include SMD
//
// Revision 1.21  2001/12/04 22:05:50  suaide
// new QA histogram for tower
//
// Revision 1.20  2001/11/05 17:09:11  suaide
// small changes
//
// Revision 1.19  2001/10/31 22:24:17  suaide
// modified Finish() method
//
// Revision 1.11  2001/10/24 23:06:54  suaide
// log messages included for easier debug
//
// Revision 1.8  2001/10/24 14:47:16  suaide
// type correction
//
// Revision 1.7  2001/10/24 14:41:44  suaide
// huge change on StEmcADCtoEMaker to a different software.
// The other version is kept as *.old for future debug 
//
// Revision 1.13  2000/05 16:07:01  
// Add README
//
#include "StEmcADCtoEMaker.h"
#include "StEventTypes.h"
#include "StEvent.h"
#include "Stiostream.h"
#include <math.h>
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "TStopwatch.h"
// DAQ Libraries
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/EMC/EMC_Reader.hh"
#include "StDAQMaker/StDAQReader.h"
#include "StDaqLib/EMC/StEmcDecoder.h"
#include "StBemcData.h"

#define STATUS_OK 1
#define CAP1 124
#define CAP2 125

ClassImp(StEmcADCtoEMaker)

//_____________________________________________________________________________
/* 
   Default constructor. Set Initial values for some variables
*/
StEmcADCtoEMaker::StEmcADCtoEMaker(const char *name):StMaker(name)
{
  mDecoder       = 0;
  mControlADCtoE = new controlADCtoE_st();
  mSMDPidMinus1Bug = kFALSE;
  Int_t   calib[]      = {1, 1, 1, 1, 0, 0, 0, 0};
  Int_t   pedSub[]     = {1, 1, 1, 1, 0, 0, 0, 0};
  Float_t cut[]        = {-1, 1.5, 1.5, 1.5, -1, -1, -1, -1};
  Int_t   cutType[]    = {0, 1, 1, 1, 0, 0, 0, 0};
  Int_t   onlyCal[]    = {0, 0, 0, 0, 0, 0, 0, 0};
  
  int i,j;
  for(i=0; i<MAXDETBARREL; i++)
  {
    mControlADCtoE->DeductPedestal[i]=pedSub[i];  
    mControlADCtoE->Calibration[i]=calib[i];
    mControlADCtoE->CutOff[i]=cut[i];
    mControlADCtoE->CutOffType[i]=cutType[i];
    mControlADCtoE->OnlyCalibrated[i]=onlyCal[i];
    mHits[i] = NULL;   //!
    mAdc[i] = NULL;    //!
    mEnergyHist[i] = NULL; //!
    mEnergySpec[i][0] = NULL; //!
    mEnergySpec[i][1] = NULL; //!
    mEnergySpec[i][2] = NULL; //!
    mAdc1d[i] = NULL;  //!           
    mEn1d[i] = NULL;  //!           
    mADCSpec[i] = NULL;          //!         
    mGeo[i] = NULL; 
    for (j=0 ; j < 18001 ; j++){
      mPed[i][j][0]    = 0.0;
      mPed[i][j][1]    = 0.0;
      mPed[i][j][2]    = 0.0;
      mPedRMS[i][j][0] = 0.0;
      mPedRMS[i][j][1] = 0.0;
      mPedRMS[i][j][2] = 0.0;
    }
  } 
  mNhit = NULL;           //! 
  mEtot = NULL;           //!
  mSmdTimeBinHist = NULL; //!
  mValidEvents = NULL;    //!
           
  mEmbedd=kFALSE;
  mPrint = kTRUE;  
  mDb = NULL;
  mEmc = NULL;            
  mDecoder = NULL;          
  mData = NULL;       
  mDBRunNumber = 0;
  mFillHisto = kTRUE;
  mDebug = kFALSE;
  mSaveAllStEvent = kFALSE;
  mRawData = NULL;               

}
//_____________________________________________________________________________
/* 
   Default destructor
*/
StEmcADCtoEMaker::~StEmcADCtoEMaker()
{
}
//_____________________________________________________________________________
/* 
Init function. Initializes the histograms and all other variables used by the
program
*/
Int_t StEmcADCtoEMaker::Init()
{     
  mData = new StBemcData(); 
  mDecoder = new StEmcDecoder();
  for (Int_t i=0; i<MAXDETBARREL; i++) 
	{
		mGeo[i]=StEmcGeom::getEmcGeom(detname[i].Data());
    cout <<"det = "<<detname[i].Data()
         <<"  DeductPedestal = "<<mControlADCtoE->DeductPedestal[i]
         <<"  Calibration = "<<mControlADCtoE->Calibration[i]
         <<"  CutOff = "<<mControlADCtoE->CutOff[i]
         <<"  CutOffType = "<<mControlADCtoE->CutOffType[i]
         <<"  OnlyCalibrated = "<<mControlADCtoE->OnlyCalibrated[i]<<endl;
  }
  
  if(!mFillHisto) return StMaker::Init();
  //Making QA histgrams
	
	int nbins[] = {4800,4800,18000,18000};
  mValidEvents = new TH2F("ValidEvents","Valid events for each detector (1=good, 2= bad)",4,-0.5,3.5,8,0.5,8.5);
 
  mNhit = new TH2F("EmcNHitsVsDet" ,"Number of hit with energy > 0 .vs. Detector #",1000,0.0,18000,8,0.5,8.5);
  mEtot = new TH2F("EmcEtotVsDet" ,"Total energy(log10) .vs. Detector #",500,-4.0,15.0,8,0.5,8.5);
 
  //tower spectra for gain monitoring  
  int a[]={4800,4800,18000,18000};
  float b[]={4096,1024,1024,1024};
  for (Int_t i=0; i<MAXDETBARREL; i++) 
	{
    char name[40];
    sprintf(name,"ADCSpec-%s",detname[i].Data());
    if(mDebug) mADCSpec[i]=new TH2F(name,name,a[i],0.5,a[i]+0.5,1024,-64,b[i]);
  }     
  // SMD time bin
  mSmdTimeBinHist = new TH2F("SmdTimeBin","SMD Time bin",8,-0.5,7.5,128,0.5,128.5);
  
  for (Int_t i=0; i<MAXDETBARREL; i++) if(mControlADCtoE->Calibration[i]==1)
  {
    Int_t nEta=mGeo[i]->NEta();
    Int_t nSub=mGeo[i]->NSub();
    
    TArrayF EtaB(nEta+1,mGeo[i]->EtaB());
    TArrayF PhiB(nSub+1,mGeo[i]->PhiB());
    
    TArrayF EtaBins(2*nEta+2);
    for(Int_t j=0;j<2*nEta+2;j++)
    {
      if (j<nEta+1) EtaBins[j]=-EtaB[nEta-j];
      else EtaBins[j]=EtaB[j-nEta-1];
    } 
    
    TArrayF PhiBins1(60*(nSub+1));
    TArrayF PhiBins(60*(nSub+1));
    Int_t j=0;
    for(Int_t m=1;m<=60;m++)
      for(Int_t s=1;s<=nSub+1;s++)
      {
        Float_t center;
        mGeo[i]->getPhiModule(m,center);
        PhiBins1[j]=center-PhiB[s-1];
        j++;
      }
      
    Bool_t again=kTRUE;
    j=0;
    do
    {
      again=kFALSE;
      Float_t phitmp=6.4;
      Int_t ktmp=-1;
      for(Int_t k=0;k<60*(nSub+1);k++)
      {
        if(PhiBins1[k]<phitmp)
        {
          phitmp=PhiBins1[k];
          ktmp=k;
        }
      }
      if(ktmp!=-1)
      {
        PhiBins[j]=phitmp;
        again=kTRUE;
        PhiBins1[ktmp]=999;
        j++;
      }      
    } while(again);
      
      
    TString name_h = detname[i] + "_Hits";
    TString name_e = detname[i] + "_Energy";
    TString name_s = detname[i] + "_Spec";
    TString name_a = detname[i] + "_ADC";
    TString name_a1= detname[i] + "_ADC1D";
    TString name_e1= detname[i] + "_Energy1D";
    TString title_h= detname[i] + " Hits distribution with energy > 0";
    TString title_e= detname[i] + " Energy distribution";
    TString title_s= detname[i] + " Spectra";
    TString title_a= detname[i] + " ADC distribution";
    TString title_a1= detname[i] +" ADC distribution (log10)";
    TString title_e1= detname[i] +" Energy distribution";
    mHits[i]   = new TH2F(name_h,title_h,2*nEta+2-1,EtaBins.GetArray(),60*(nSub+1)-1,PhiBins.GetArray());
    mEnergyHist[i] = new TH2F(name_e,title_e,2*nEta+2-1,EtaBins.GetArray(),60*(nSub+1)-1,PhiBins.GetArray());
    if(i<2)
    {
      if(mDebug) mEnergySpec[i][0] = new TH2F(name_s.Data(),title_s,nbins[i],0.5,(float)nbins[i]+0.5,500,0,5);
      if(mDebug) mEnergySpec[i][1] = mEnergySpec[i][0];
      if(mDebug) mEnergySpec[i][2] = mEnergySpec[i][0];
    }
    else for(int k=0;k<3;k++)
    {
      char name[40];
      sprintf(name,"%s for capacitor %d",name_s.Data(),k);
      if(mDebug) mEnergySpec[i][k] = new TH2F(name,title_s,nbins[i],0.5,(float)nbins[i]+0.5,500,0,5);
    }
    mAdc[i]    = new TH2F(name_a,title_a,2*nEta+2-1,EtaBins.GetArray(),60*(nSub+1)-1,PhiBins.GetArray());
    mAdc1d[i]  = new TH1F(name_a1,title_a1,1000,0,8);   
    mEn1d[i]   = new TH1F(name_e1,title_e1,1000,-200,2000);   
  }
  return StMaker::Init();
}
//_____________________________________________________________________________
/*!
This method creates mean ADC and RMS histograms. It runs only in the end of the job
*/
Int_t StEmcADCtoEMaker::Finish()
{
  return kStOk;
}
//_____________________________________________________________________________
void StEmcADCtoEMaker::zeroAll()
{ 
  mData->zeroAll();
  clearStEventStaf();
  mRawData = NULL;
} 
//_____________________________________________________________________________
/*!
Process the event. Basicaly it get the status database and makes a loop over
EMC subdetectors. For each sub detector it gets the calibration tables, subtract
pedestals and apply calibration constants. In the end, update StEvent with
calibrated hits
*/
Int_t StEmcADCtoEMaker::Make()
{  
  TStopwatch clock;
  clock.Start();
  if(mPrint) cout <<"\n\nStEmcADCtoEMaker::Make()******************************************************************\n";
  zeroAll(); 
			
	Bool_t dataOk = getEmc(); 
  if(mRunNumber!=mDBRunNumber) getTables(); 
	
  if(dataOk) for(Int_t det=0;det<MAXDETBARREL;det++)
  {
		Bool_t present = kFALSE;
		if(det==0 && mData->TowerPresent) present = kTRUE;
		if(det==1 && mData->PSDPresent) present = kTRUE;
		if(det==2 && mData->SMDPresent) present = kTRUE;
		if(det==3 && mData->SMDPresent) present = kTRUE;
		if(present && mControlADCtoE->Calibration[det]==1)
		{
		  Int_t nCh = 0;
			if(det==0) nCh = mData->NTowerHits;
			if(det==1) nCh = mData->NPsdHits;
			if(det==2) nCh = mData->NSmdHits;
			if(det==3) nCh = mData->NSmdHits;
			if(nCh>0) calibrate(det);				
		}
  }
	if(mFillHisto)fillHistograms();
	fillStEvent();
  
  clock.Stop();
  if(mPrint) cout <<"Time to run StEmcADCtoEMaker::Make() real = "<<clock.RealTime()<<"  cpu = "<<clock.CpuTime()<<" \n";
  if(mPrint) cout <<"*******************************************************************************************\n\n\n";

  return kStOK;
}

//_____________________________________________________________________________
/*!
This method gets all the tables from the database
*/
Bool_t StEmcADCtoEMaker::getTables()
{
  mDBRunNumber = mRunNumber;
  if(mDecoder) delete mDecoder;
  mDecoder=new StEmcDecoder(GetDate(),GetTime());
	emcCalib_st       *emccalibst = NULL;
	smdCalib_st       *smdcalibst = NULL;
	emcGain_st        *emcgainst = NULL;
	smdGain_st        *smdgainst = NULL;
	emcPed_st         *emcpedst = NULL;
  smdPed_st         *smdpedst = NULL;
  int date = GetDate();
  for(int det=0;det<MAXDETBARREL;det++)
  {
    getStatus(det);
    mHasCalib[det] = kFALSE;
    mHasPed[det] = kFALSE;
    mHasGain[det] = kFALSE;
    mDb=NULL;
	  TString DbName = "Calibrations/emc/y3"+detname[det];
    mDb=GetInputDB(DbName.Data());
    if(mDb)
    {  	  
	    //getting tables ////////////////////////////////////////////////////
	    TString TableName;
	    if(det<2)//bemc and bprs
	    {
		    TableName = detname[det]+"Calib";
		    St_emcCalib* a = (St_emcCalib*) mDb->Find(TableName.Data());
		    if(a) 
        {
          emccalibst = a->GetTable();
          if(emccalibst)
          {
            mHasCalib[det] = kTRUE;
            for(int i=0;i<4800;i++) for(int j=0;j<5;j++) mCalib[det][i][j] = emccalibst[0].AdcToE[i][j];
          }
          else for(int i=0;i<4800;i++) for(int j=0;j<5;j++) mCalib[det][i][j] = 0;
        } 
		
		    TableName = detname[det]+"Gain";
		    St_emcGain* b = (St_emcGain*) mDb->Find(TableName.Data());
		    if(b) 
        {
          emcgainst = b->GetTable();
          if(emcgainst)
          {
            mHasGain[det] = kTRUE;
            for(int i=0;i<4800;i++) mGain[det][i] = emcgainst[0].Gain[i];
          }
          else for(int i=0;i<4800;i++) mGain[det][i] = 1;
          
		    }
		    if(mControlADCtoE->DeductPedestal[det]==1)
		    {
			    TableName = detname[det]+"Ped";
			    St_emcPed* ped=(St_emcPed*)mDb->Find(TableName.Data());
    	    if(ped) 
          {
            emcpedst=ped->GetTable();
            if(emcpedst)
            {
              mHasPed[det] = kTRUE;
              for(int i=0;i<4800;i++) 
              {
                mPed[det][i][0] = ((Float_t)emcpedst[0].AdcPedestal[i])/100.;
                mPedRMS[det][i][0] = ((Float_t)emcpedst[0].AdcPedestalRMS[i])/100.;
              }
            }
          }
		    }
	    }
	    else// SMD
	    {
		    TableName = detname[det]+"Calib";
		    St_smdCalib* a = (St_smdCalib*) mDb->Find(TableName.Data());
		    if(a) 
        {
          smdcalibst = a->GetTable();
          if(smdcalibst)
          {
            mHasCalib[det] = kTRUE;
            for(int i=0;i<18000;i++) for(int j=0;j<5;j++) mCalib[det][i][j] = smdcalibst[0].AdcToE[i][j];
          }
          else for(int i=0;i<18000;i++) for(int j=0;j<5;j++) mCalib[det][i][j] = 0;
          
		    }
		    TableName = detname[det]+"Gain";
		    St_smdGain* b = (St_smdGain*) mDb->Find(TableName.Data());
		    if(b) 
        {
          smdgainst = b->GetTable();
          if(smdgainst)
          {
            mHasGain[det] = kTRUE;
            for(int i=0;i<18000;i++) mGain[det][i] = smdgainst[0].Gain[i];
          }
          else for(int i=0;i<18000;i++) mGain[det][i] = 1;
        }
		
		    if(mControlADCtoE->DeductPedestal[det]==1)
		    {
			    TableName = detname[det]+"Ped";
			    St_smdPed* ped=(St_smdPed*)mDb->Find(TableName.Data());
    	    if(ped) 
          {
            smdpedst=ped->GetTable();
            if(smdpedst)
            {
              mHasPed[det] = kTRUE;
              /////////////////////////////////////////////////////////////////////
              /////////////////////////////////////////////////////////////////////
              // need to include a correction because the SMD-phi
              // pedestals for Y2003 run were not saved in the right position
              // in the database. Instead of doing id-1 for SMD-phi, we need
              // do only id. This is valid only for Y2003 data
              //
              // by AAPSUAIDE 20030908
              //
              int shift = 0;
              if(date>20021101 && date<20030601) if(det==3 && mSMDPidMinus1Bug) shift = 1;
              /////////////////////////////////////////////////////////////////////
              /////////////////////////////////////////////////////////////////////
             
              for(int i=0;i<18000-shift;i++) for(int j=0;j<3;j++) 
              {
                mPed[det][i][j] = ((Float_t)smdpedst[0].AdcPedestal[i+shift][j])/100.;
                mPedRMS[det][i][j] = ((Float_t)smdpedst[0].AdcPedestalRMS[i+shift][j])/100.;
                // This lines ZERO the PEDESTAL values for capacitors 124 and 125 for Y2003
                // run. Once the pedestal is zero the energy of the corresponding hit
                // will be set also as zero.
                // This will remove the SMD hits with these capacitor values and
                // remove the pedestal problem we are having with them
                //
                // by AAPSUAIDE 20030910
                if(date>20021101) if(j>0) mPed[det][i][j] = 0;
                /////////////////////////////////////////////////////////////////////
                /////////////////////////////////////////////////////////////////////
              }
            }
          }
        }
		  }
	  }
  }
  return kTRUE;
}
//_____________________________________________________________________________
/*!
This method gets the status tables for a given detector and stores it in the
array mStatus[det][index].
*/
Bool_t StEmcADCtoEMaker::getStatus(Int_t det)
{
	mDb=NULL;
	TString DbName = "Calibrations/emc/y3"+detname[det];
  mDb=GetDataBase(DbName.Data());
  if(!mDb) 
  { 
    if(mPrint) cout <<"StEmcADCtoEMaker::Make() - Can not get new status tables.... for "<<detname[det].Data()<<"\n"; 
    return kFALSE;
  }
  
	if(mPrint) cout <<"Getting status table for detector "<<detname[det].Data()<<endl;
  TString TableName=detname[det]+"Status";
	
	Int_t NGOOD = 0; 
  Int_t date = GetDate();
  
  if(det==0 || det==1)
	{
		St_emcStatus* run = NULL;
		if(mDb) run = (St_emcStatus*)mDb->Find(TableName.Data());
		if(run)
		{
			emcStatus_st* runst=run->GetTable();
			
      for(Int_t i=1;i<=4800;i++) 
			{
				if(det==0)
        {
          mData->TowerStatus[i-1] = runst[0].Status[i-1];
				  if(mData->TowerStatus[i-1]==STATUS_OK) NGOOD++;
        }
        else
        {
          mData->PsdStatus[i-1] = runst[0].Status[i-1];
				  if(mData->PsdStatus[i-1]==STATUS_OK) NGOOD++;
        }
			}
		} 
	}
  
	if(det==2 || det==3)
  {
    St_smdStatus* run = NULL;
    if(mDb) run = (St_smdStatus*)mDb->Find(TableName.Data());
    if(run)
    {
      smdStatus_st* runst=run->GetTable(); 
      for(Int_t i=1;i<=18000;i++)
      {
        if(det==2) mData->SmdeStatus[i-1]=runst[0].Status[i-1];
				if(det==3) mData->SmdpStatus[i-1]=runst[0].Status[i-1];
        if(date<20030601) if(i>9000)
        {
          if(det==2) mData->SmdeStatus[i-1]=0;
				  if(det==3) mData->SmdpStatus[i-1]=0;
        }
        if(det==2 &&mData->SmdeStatus[i-1]==STATUS_OK) NGOOD++;
        if(det==3 &&mData->SmdpStatus[i-1]==STATUS_OK) NGOOD++;
      }
    }
  }
	if(mPrint) cout <<"Detector: "<<detname[det].Data()<<"  STATUS_OK = "<<NGOOD<<"  channels\n";
  return kTRUE;

}
//_____________________________________________________________________________
/*!
This method gets EMC collection from DAQ dataset. It also gets the capacitor number
for SMD and saves it in the calibrationType member of StEmcRawHit. 
*/
Bool_t StEmcADCtoEMaker::getEmcFromDaq(TDataSet* daq)
{
  if(mPrint) cout <<"Decoding EMC information from DAQ structure "<<endl;

	StDAQReader* TheDataReader=(StDAQReader*)(daq->GetObject());
  if(!TheDataReader->EMCPresent()) return kFALSE;

  StEMCReader* TheEmcReader=TheDataReader->getEMCReader();
  if(!TheEmcReader) return kFALSE;
	
	EMC_Reader* reader = TheEmcReader->getBemcReader();
	if(!reader) return kFALSE;
  if(!mRawData) mRawData = new StEmcRawData();
	  
  // tower data
	mData->TowerPresent = reader->isTowerPresent();
	if(reader->isTowerPresent())
	{
		Bank_BTOWERADCR& tower = reader->getBTOWERADCR();
		mData->EventNumber = tower.EventNumber;
		mData->TowerByteCount = tower.ReceivedByteCount;
		mData->NTowerHits = tower.NTowerHits;
		mData->TDCErrorFlag = tower.TDCErrorFlag;
		mData->NTDCChannels = tower.NTDCChannels;
    mRawData->createBank(0,120,4800);
    for(int i = 0;i<120;i++) mRawData->setHeader(0,i,tower.TDCHeader[i]);
		for(Int_t i=0;i<30;i++)
		{
			mData->TDCError[i] = tower.TDCError[i];
			mData->TDCToken[i] = tower.TDCToken[i];
			mData->TDCTrigger[i] = tower.TDCTrigger[i];
			mData->TDCCrateId[i] = tower.TDCCrateId[i];
			mData->TDCCount[i] = tower.TDCCount[i];
		}
		for(Int_t id=1;id<=4800;id++)
		{
			Int_t m,e,s;
			mGeo[0]->getBin(id,m,e,s);
			mData->TowerADC[id-1] = tower.TowerMatrix[m-1][e-1][s-1];
      mRawData->setData(0,id-1,tower.TowerADCArray[id-1]);
		}
	}	
	
	// smd data
	mData->SMDPresent=reader->isSmdPresent();
	if(reader->isSmdPresent())
	{
		Bank_BSMDADCR& smd =  reader->getSMD_ADCR();
		mData->SMDErrorFlag = smd.SMDErrorFlag;
		mData->SMDByteCount = smd.ReceivedByteCount;
		mData->NSmdHits = smd.NSmdHits;
    int NSMD = 8;
    // there is only 4 SMD Crates before that data and some
    // of them are PSD crates. For Y2004 AuAu runs PSD do
    // not have its own data format and it is being read as 
    // SMD
    if(mData->EventDate<20040701) NSMD = 4;
    for(int i = 0;i<NSMD;i++)
    {      
      if(smd.HasData[i]==1)
      {
        mData->TimeBin[i]=(Char_t)smd.TimeBin[i];
        int m,e,s,det,id;
        mRawData->createBank(i+1,128,4800);
        for(int j=0;j<128;j++) mRawData->setHeader(i+1,j,smd.SmdHeader[i][j]);
        for(int j=0;j<4800;j++) 
        {
          int stat = mDecoder->GetSmdCoord(i,j,det,m,e,s);
          if(stat==1 && det==3)
          {
            mGeo[2]->getId(m,e,s,id);
            mData->SmdeADC[id-1] =  smd.SmdE_ADCMatrix[m-1][e-1];
          }
          if(stat==1 && det==4)
          {
            mGeo[3]->getId(m,e,s,id);
            mData->SmdpADC[id-1] =  smd.SmdP_ADCMatrix[m-1][e-1][s-1];
          }
          mRawData->setData(i+1,j,smd.SMDADCArray[i][j]);
        }
      }
    }
    /////////////////////////////////////////////////////////////////////
    // read Pre Shower data for Y2004 AuAu data. This year, the PSD data
    // is read as SMD data for fibers 4 and 5.
    // 
    // This is a temporary solution while the PSD data format is not
    // decided by Tonko. He needs to have a decision on some
    // hardware issues before the data format is decided
    //
    // AAPSUAIDE 20040318
    //
    if(mData->EventDate>20040101&&mData->EventDate<20040701)
    {
      for(int RDO = 0;RDO<2;RDO++)
      {
        int SMDRDO = RDO+4;
        if(smd.HasData[SMDRDO]==1) 
        {
          mRawData->createBank(RDO+9,128,4800);
          for(int i=0;i<128;i++) mRawData->setHeader(RDO+9,i,smd.SmdHeader[SMDRDO][i]);
          for(int index = 0;index<4800;index++)
          {
            int adc = smd.SMDADCArray[SMDRDO][index];
            int cap = smd.TimeBin[SMDRDO];
            mRawData->setData(RDO+9,index,smd.SMDADCArray[SMDRDO][index]);
            if(adc>0)
            {
              int id;
              int stat = mDecoder->GetPsdId(RDO,index,id,false);
              if(stat==1)
              {
                mData->PsdADC[id-1] = adc;
							  mData->ValidPSDEvent=kTRUE; 
							  mData->PSDPresent = kTRUE;
							  mData->NPsdHits++;
              }
              // remove CAPACITOR 1 and 2 from the Pre-Shower
              if(cap==CAP1 || cap == CAP2) mData->PsdADC[id-1] = 0;
            }
          }
        }
      }
    }
    /////////////////////////////////////////////////////////////////////
	}
	mData->validateData();
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcADCtoEMaker::getEmcFromStEvent(StEmcCollection *emc)
{
  if(mPrint) cout <<"Decoding EMC information from StEmcCollection "<<endl;
	mData->NTowerHits = 0;
	mData->NSmdHits = 0;
	mData->ValidTowerEvent = kFALSE; 
	mData->TowerPresent = kFALSE;
	mData->ValidSMDEvent = kFALSE;
	mData->SMDPresent = kFALSE;
  if(!emc) return kFALSE;
  
  for(Int_t det=0;det<MAXDETBARREL;det++)
  {
    StDetectorId id = static_cast<StDetectorId>(det+kBarrelEmcTowerId);
    StEmcDetector* detector=emc->detector(id);
    if(detector)
    {
      for(UInt_t j=1;j<=120;j++)
      {
        StEmcModule* module = detector->module(j);
        if(module)
        {
          StSPtrVecEmcRawHit& rawHit=module->hits();
          for(UInt_t k=0;k<rawHit.size();k++) if(rawHit[k])
          {
            Int_t m=rawHit[k]->module();
            Int_t e=rawHit[k]->eta();
            Int_t s=abs(rawHit[k]->sub());
            Short_t adc=(Short_t) rawHit[k]->adc();
            Int_t cal=rawHit[k]->calibrationType();
            if(cal>127) cal-=128;
       
            Int_t idh=0;
            Int_t stat = mGeo[det]->getId(m,e,s,idh);
            if(det==0 && stat==0) 
						{
							mData->TowerADC[idh-1] = adc; 
							mData->ValidTowerEvent=kTRUE; 
							mData->TowerPresent = kTRUE;
							mData->NTowerHits++;
						}
            if(det==1 && stat==0) 
						{
							mData->PsdADC[idh-1] = adc; 
							mData->ValidPSDEvent=kTRUE; 
							mData->PSDPresent = kTRUE;
							mData->NPsdHits++;
						}
						if(det==2 && stat==0) {mData->SmdeADC[idh-1] = adc;mData->NSmdHits++;}
						if(det==3 && stat==0) {mData->SmdpADC[idh-1] = adc;mData->NSmdHits++;}
            if(det==2 && stat==0) 
            {
              Int_t RDO,index;
              mDecoder->GetSmdRDO(det+1,m,e,s,RDO,index);
              mData->TimeBin[RDO]=cal;
							mData->ValidSMDEvent=kTRUE;
							mData->SMDPresent=kTRUE;
            }
          }
        }
      }
    } 
  }
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcADCtoEMaker::getEmcFromStEventRaw(StEmcRawData *raw)
{
  if(mPrint) cout <<"Decoding EMC information from StEmcRawData "<<endl;
	mData->NTowerHits = 0;
	mData->NSmdHits = 0;
	mData->ValidTowerEvent = kFALSE; 
	mData->TowerPresent = kFALSE;
	mData->ValidSMDEvent = kFALSE;
	mData->SMDPresent = kFALSE;
  if(!raw) return kFALSE;
  
  unsigned short *header = NULL;
  unsigned short *data   = NULL;
  
  // tower stuff
  
  header = raw->header(0);
  data   = raw->data(0);
  if(header && data) // tower present
  {
		for(Int_t i=0;i<30;i++)
		{
			mData->TDCCount[i] = header[i];
			mData->TDCError[i] = header[i+30];
			mData->TDCToken[i] = header[i+60];
			mData->TDCTrigger[i] = (header[i+90] & 0xF00) >> 8;
			mData->TDCCrateId[i] = (header[i+90] & 0x0FF);
		}
		for(Int_t daqId=0;daqId<4800;daqId++) if(data[daqId]>0)
		{
			int id=-1;
      int stat=mDecoder->GetTowerIdFromDaqId(daqId,id);
      if(stat && id>0)
      {
        mData->TowerADC[id-1] = data[daqId];
				mData->TowerPresent = kTRUE;
				mData->NTowerHits++;
      }
		}    
  }
  
  // SMD stuff
  for(int RDO = 0;RDO<8;RDO++)
  { 
    header = raw->header(RDO+1);
    data   = raw->data(RDO+1);
    if(header && data)
    {
      mData->TimeBin[RDO] = header[32];
      int m,e,s,det,id;
      for(int j=0;j<4800;j++) if(data[j]>0)
      {
        int stat = mDecoder->GetSmdCoord(RDO,j,det,m,e,s);
        if(stat==1 && det==3)
        {
          mGeo[2]->getId(m,e,s,id);
          mData->SmdeADC[id-1] = data[j];
        }
        if(stat==1 && det==4)
        {
          mGeo[3]->getId(m,e,s,id);
          mData->SmdpADC[id-1] = data[j];
        }
				if(stat==1)
        {
          mData->ValidSMDEvent=kTRUE; 
					mData->SMDPresent = kTRUE;
          mData->NSmdHits++;
        }
      }
    }
  }
  
  // PSD stuff
  for(int RDO = 0;RDO<4;RDO++)
  { 
    header = raw->header(RDO+9);
    data   = raw->data(RDO+9);
    if(header && data)
    {
      int id;
      for(int j=0;j<4800;j++) if(data[j]>0)
      {
        int stat = mDecoder->GetPsdId(RDO,j,id,false);
        if(stat==1)
        {
          mData->PsdADC[id-1] = data[j];
					mData->ValidPSDEvent=kTRUE; 
					mData->PSDPresent = kTRUE;
					mData->NPsdHits++;
        }
      }
    }
  }
	mData->validateData();
  return kTRUE;
}
//_____________________________________________________________________________
/*!
This method gets EMC hits from different sources. First it looks for DAQ datasets.
if Not present, looks for StEvent hits to recalibrate.
*/
Bool_t StEmcADCtoEMaker::getEmc()
{  
// check if there is event from DAQ
  if(mPrint) cout <<"Decoding EMC information. Looking for available data format "<<endl;
  mData->EventDate = GetDate();
	mData->EventTime = GetTime();
	mFromDaq = kFALSE;
  if(!mDecoder) mDecoder=new StEmcDecoder(mData->EventDate,mData->EventTime);
	
  TDataSet* TheData   = GetDataSet("StDAQReader");
  if(TheData) 
	{
  	mFromDaq = kTRUE;
		StDAQReader* TheDataReader=(StDAQReader*)(TheData->GetObject());
  	if(!TheDataReader) return kFALSE;
		mData->RunNumber = TheDataReader->getRunNumber();
		mData->TriggerWord = TheDataReader->getTrigWord();
		mRunNumber = mData->RunNumber;
		if(getEmcFromDaq(TheData)) return kTRUE; 
		return kFALSE;
	}
     
  // check if there is event from StEvent
  StEvent* event=NULL;
  if(!mEmbedd) event = (StEvent*)GetInputDS("StEvent");
  else 
  {
    StMaker *m = GetMaker("embedIO");
    if(!m) 
    { 
      if(mPrint) cout<<"No embedIO maker"<<endl; 
      return kFALSE; 
    }
    event = (StEvent*)m->GetInputDS("StEvent");  
  }
  if(event)
	{
  	mData->RunNumber = event->runId();
		mData->TriggerWord = event->triggerMask();
		mRunNumber = mData->RunNumber;
		
		StEmcCollection* emctemp=event->emcCollection();
  	if(!emctemp)  return kFALSE;
    if(emctemp->bemcRawData()) return getEmcFromStEventRaw(emctemp->bemcRawData());
  	return getEmcFromStEvent(emctemp);
	}  
  
  if(mPrint) cout <<"No EMC data structure was found. Can not do anything. "<<endl;
  return kFALSE;
}
//_____________________________________________________________________________
/*!
This method applies the calibration constants to get the hit energy. The calibration
is applied only to the hits which tower/strip status is set to 1 (mStatus[det][index==1).
It also checks if the calibration is done for that bin
*/
Bool_t StEmcADCtoEMaker::calibrate(Int_t det)
{
  if(mPrint) cout <<"Calibrating detector "<<detname[det].Data()<<endl;
  mSave[det] = kFALSE;
	
	Bool_t Valid = kTRUE;
	if(det==0) Valid = mData->ValidTowerEvent;
	if(det==1) Valid = mData->ValidPSDEvent;
	if(det==2 || det==3) Valid = mData->ValidSMDEvent;	
	if(!Valid) 
  {
    if(mPrint) cout <<"Not a valid event for detector "<<detname[det].Data()<<endl;
    return kFALSE;
	}
  
  //if(!mHasCalib[det]) return kFALSE;
  if(mControlADCtoE->DeductPedestal[det]==1) if(!mHasPed[det]) return kFALSE;
			
	//loop over all channels ///////////////////////////////////////////////

  Int_t MAX = 18000;
	if(det<2) MAX = 4800;
	Float_t TOTALE=0;
	Int_t   NHITS=0;
	Int_t cap = 0;
	for(Int_t id=1;id<=MAX;id++)
	{
		Float_t ADC = 0;
		Float_t PED = 0;
		Char_t status = 0;
		if(det==0) { ADC = (Float_t)mData->TowerADC[id-1]; status = mData->TowerStatus[id-1]; }
		if(det==1) { ADC = (Float_t)mData->PsdADC[id-1];   status = mData->PsdStatus[id-1]; }
		if(det==2) { ADC = (Float_t)mData->SmdeADC[id-1];  status = mData->SmdeStatus[id-1];  }
		if(det==3) { ADC = (Float_t)mData->SmdpADC[id-1];  status = mData->SmdpStatus[id-1];  }
		if(status==STATUS_OK && ADC>0)
		{
      if(mControlADCtoE->DeductPedestal[det]==1)
		  {
        if(det<2) PED = mPed[det][id-1][0];
			  else
			  {
      	  Int_t RDO,index,m,e,s;
				  mGeo[det]->getBin(id,m,e,s);
      	  mDecoder->GetSmdRDO(det+1,m,e,s,RDO,index);
      	  Int_t capacitor=mData->TimeBin[RDO];
				  cap = 0;
				  if(capacitor == CAP1) cap = 1;
				  if(capacitor == CAP2) cap = 2;
          PED = mPed[det][id-1][cap];
		    }
		  }
			PED =(Float_t)((Int_t)(PED)); // calibration is done this way because of histograms
      Float_t ADCSUB=ADC-PED;
			Float_t EN=0;
			Float_t ADCPOWER=1;
			//calibrating ..............
      for(Int_t i=0;i<5;i++) 
      {
        Float_t c = 0;
        c = mCalib[det][id-1][i];
				EN+=c*ADCPOWER; 
        ADCPOWER*=ADCSUB;
      }
      if(PED<=0) EN = 0;
			Float_t gain = 1;
			if(mHasGain[det]) gain = mGain[det][id-1];
			EN*=gain;
			
			if(det==0) mData->TowerEnergy[id-1] = EN;
			if(det==1) mData->PsdEnergy[id-1] = EN;
			if(det==2) mData->SmdeEnergy[id-1] = EN;     
			if(det==3) mData->SmdpEnergy[id-1] = EN;     
			if(saveHit(det,id,cap)) 
			{
				TOTALE+=EN;
				NHITS++;
        if(mFillHisto) if(mEnergySpec[det][cap]) mEnergySpec[det][cap]->Fill(id,EN);			  
        if(!mSave[det]) mSave[det] = kTRUE;
        //if(det==0) cout <<"id = "<<id<<"  ADC = "<<ADC<<"  PED = "<<PED<<"  CAP = "<<cap<<"  ADCSUB = "<<ADCSUB<<"  E = "<<EN<<endl;
			}
      else
      {
 			  if(det==0) mData->TowerEnergy[id-1] = 0;
 			  if(det==1) mData->PsdEnergy[id-1] = 0;
			  if(det==2) mData->SmdeEnergy[id-1] = 0;     
			  if(det==3) mData->SmdpEnergy[id-1] = 0;        
      }
		}
    else
    {
 			if(det==0) mData->TowerEnergy[id-1] = 0;
 			if(det==1) mData->PsdEnergy[id-1] = 0;
			if(det==2) mData->SmdeEnergy[id-1] = 0;     
			if(det==3) mData->SmdpEnergy[id-1] = 0;        
    }
	}
	if(mPrint) cout <<"detector: "<<detname[det].Data()<<"  NHITS = "<<NHITS<<"  TOTALE = "<<TOTALE<<endl;
  return kTRUE;
}
//_____________________________________________________________________________
/*!
This method fills QA histograms
*/
Bool_t StEmcADCtoEMaker::fillHistograms()
{
  
  for(Int_t det=0;det<MAXDETBARREL;det++)
	{
		Int_t nHits = 0;
		Float_t totalE = 0;
		Float_t totalADC = 0;
		Int_t MAXCHANNEL = 18000;
		if(det<2) MAXCHANNEL = 4800;
    Int_t date = mData->EventDate;
    if(date<20030501 && det>=2) MAXCHANNEL=9000; //just to save CPU time
		Bool_t valid = kTRUE;
		if(det==0) valid = mData->ValidTowerEvent;
		if(det==1) valid = mData->ValidPSDEvent;
		if(det==2) valid = mData->ValidSMDEvent;
		if(det==3) valid = mData->ValidSMDEvent;
		if(valid) mValidEvents->Fill(1,det+1); else mValidEvents->Fill(2,det+1);
		for(Int_t i=0;i<MAXCHANNEL;i++)
		{
			Char_t status = 0;
			if(det==0) status = mData->TowerStatus[i];
			if(det==1) status = mData->PsdStatus[i];
			if(det==2) status = mData->SmdeStatus[i];
			if(det==3) status = mData->SmdpStatus[i];
			if(status==STATUS_OK && saveHit(det,i+1))
			{
				Float_t ADC = 0;
				Float_t E = 0;
				if(det==0) { ADC = (Float_t)mData->TowerADC[i]; E = mData->TowerEnergy[i]; }
				if(det==1) { ADC = (Float_t)mData->PsdADC[i]; E = mData->PsdEnergy[i]; }
				if(det==2) { ADC = (Float_t)mData->SmdeADC[i]; E = mData->SmdeEnergy[i]; }
				if(det==3) { ADC = (Float_t)mData->SmdpADC[i]; E = mData->SmdpEnergy[i]; }
			  if(ADC!=0) if(mADCSpec[det]) mADCSpec[det]->Fill(i+1,ADC-mPed[det][i][0]);
        //if(det==2) cout <<"id = "<<i+1<<"  ADC = "<<ADC<<"  E = "<<E<<endl;
				totalE+=E;
				totalADC+=ADC;
				nHits++;
				Float_t eta,phi;
				mGeo[det]->getEtaPhi(i+1,eta,phi);
        //mEnergySpec[det]->Fill(i+1,E);
				if(ADC!=0) mHits[det]->Fill(eta,phi);
        if(ADC!=0) mAdc[det]->Fill(eta,phi,ADC);
				if(E!=0) mEnergyHist[det]->Fill(eta,phi,E);
			}
		}
    if(mPrint) cout <<"HISTOGRAM: det = "<<det+1<<"  NHits = "<<nHits<<"  totalE = "<<totalE<<endl;
		if(nHits>0)    mNhit->Fill((Float_t)nHits,(Float_t)det+1);
		if(totalE>0)   mEtot->Fill(log10(totalE),(Float_t)det+1);
		if(mEn1d[det]) mEn1d[det]->Fill(totalE);
		if(totalADC>0) mAdc1d[det]->Fill(log10(totalADC));
  	if(det==2) for(Int_t RDO=0;RDO<8;RDO++) mSmdTimeBinHist->Fill(RDO,mData->TimeBin[RDO]);
	}
  return kTRUE;
}
//_____________________________________________________________________________
/*!
This method makes a clean up of StEvent before store it in the .data
*/
Bool_t StEmcADCtoEMaker::fillStEvent()
{  
  // first need to clean hits with adc = 0
  StEvent* event=NULL;
  if(!mEmbedd) event = (StEvent*)GetInputDS("StEvent");
  else
  {
    StMaker *m = GetMaker("embedIO");
    if(!m) 
    { 
      if(mPrint) cout<<"No embedIO maker"<<endl; 
      return kFALSE; 
    }
    event = (StEvent*)m->GetInputDS("StEvent");  
  }
  
  mEmc = NULL;	
	if(event) mEmc = event->emcCollection();
	if(mEmc) clearOldEmc();
  
  bool save = false;
  for(Int_t det=0;det<MAXDETBARREL;det++) if(mSave[det]) save = true;
  if(!save) return kFALSE; 
  if(!mEmc) mEmc =new StEmcCollection();
  
  if(mRawData)
  {
    if(mEmc->bemcRawData())
    {
      // there is already a StEmcRawData object defined... Need 
      // to copy this information into the other and delete this
      // object
      for(int i = 0;i<12;i++)
      {
        UShort_t* header = mRawData->header(i);
        UShort_t* data   = mRawData->data(i);
        Int_t sh = mRawData->sizeHeader(i);
        Int_t sd = mRawData->sizeData(i);
        mEmc->bemcRawData()->createBank(i,sh,sd);
        mEmc->bemcRawData()->setHeader(i,header);
        mEmc->bemcRawData()->setData(i,data);    
      }
      delete mRawData; 
      mRawData = NULL;
    }
    else mEmc->setBemcRawData(mRawData);
  }
    
  for(Int_t det=0;det<MAXDETBARREL;det++) 
	{
		if(mControlADCtoE->Calibration[det]==1)
  	{
    	Bool_t Valid = kTRUE;
			Int_t NHITS = 0;
    	Int_t NGOOD = 0;
      Int_t NSTEVENT = 0;
	  	if(det==0) Valid = mData->ValidTowerEvent;
	  	if(det==1) Valid = mData->ValidPSDEvent;
	  	if(det==2 || det==3) Valid = mData->ValidSMDEvent;	
			if(Valid)
			{
				Int_t Max = 18000;
        if(det<2) Max = 4800;    
    		// first check if there is at least one valid hit to save
    		if(mSave[det])
    		{
      		StDetectorId id = static_cast<StDetectorId>(det+kBarrelEmcTowerId);
      		StEmcDetector* detector=mEmc->detector(id);
					if(!detector)
					{
						detector = new StEmcDetector(id,120); 
						mEmc->setDetector(detector);
					}
    
      		for(Int_t idh=1;idh<=Max;idh++)
      		{      
            int cap =0;
            int timebin=0;
          	Int_t m,e,s;
          	mGeo[det]->getBin(idh,m,e,s);
            if(det==2 || det==3) 
          	{
            	Int_t RDO,index;
            	mDecoder->GetSmdRDO(det+1,m,e,s,RDO,index);
            	timebin=mData->TimeBin[RDO];
              if(timebin==CAP1) cap=1;
              if(timebin==CAP2) cap=2;
          	}
            int calib = timebin;
          	Int_t ADC = 0;
						Float_t E = 0;
						if(det==0) { ADC = mData->TowerADC[idh-1]; E = mData->TowerEnergy[idh-1]; }
						if(det==1) { ADC = mData->PsdADC[idh-1]; E = mData->PsdEnergy[idh-1]; }
						if(det==2) { ADC = mData->SmdeADC[idh-1]; E = mData->SmdeEnergy[idh-1]; }
						if(det==3) { ADC = mData->SmdpADC[idh-1]; E = mData->SmdpEnergy[idh-1]; }
						if(ADC>0)  NHITS++;
            
            bool go = saveHit(det,idh,cap);
            if(!go) calib+=128;            
            if(mSaveAllStEvent) go = true;           
            if(go && ADC>0)
            {
              StEmcRawHit* hit=new StEmcRawHit(id,m,e,s,(UInt_t)ADC);
          	  hit->setEnergy(E);
          	  hit->setCalibrationType(calib);
              detector->addHit(hit);
              NSTEVENT++;
              if(calib<128) NGOOD++;
            }
      		}
    		}
			}
			if(mPrint) cout <<"NHITS for detector "<<detname[det].Data()<<" = "<<NHITS<<"  GOOD to StEvent = "<<NSTEVENT<<"  muDST = "<<NGOOD<<endl;
  	} 
	} 
  // finished clean up
  
  return kTRUE;
}
//_____________________________________________________________________________
/*!
Check if this hit is ok to be saved on StEvent
*/
Bool_t StEmcADCtoEMaker::saveHit(Int_t det,Int_t idh, Int_t cap)
{  
  Float_t ADC = 0;
  Float_t E   = 0;
  Char_t  S   = 0;
  Float_t PED = mPed[det][idh-1][cap];
  Float_t RMS = mPedRMS[det][idh-1][cap];

  if(det==0) {ADC = mData->TowerADC[idh-1]; E = mData->TowerEnergy[idh-1]; S = STATUS_OK; } // save all for towers
  if(det==1) {ADC = mData->PsdADC[idh-1]; E = mData->PsdEnergy[idh-1]; S = mData->PsdStatus[idh-1]; } 
  if(det==2) {ADC = (Float_t)mData->SmdeADC[idh-1]; E = mData->SmdeEnergy[idh-1]; S = mData->SmdeStatus[idh-1]; }
  if(det==3) {ADC = (Float_t)mData->SmdpADC[idh-1]; E = mData->SmdpEnergy[idh-1]; S = mData->SmdpStatus[idh-1]; }
  
  Bool_t save = kTRUE;  
  if(ADC<=0) save = kFALSE;   
	if(S!=STATUS_OK) save = kFALSE;
  if(mControlADCtoE->CutOff[det]>0) 
  {
    if(mControlADCtoE->CutOffType[det]==1)
    {
      if(RMS>0 && PED>0)
      {
        Float_t x = (ADC-PED)/RMS;
        if(x<mControlADCtoE->CutOff[det]) save = kFALSE;
      } else save = kFALSE;
    } 
    else
    {
      if(E<mControlADCtoE->CutOff[det]) save = kFALSE;
    }
  }
  return save;
}
//_____________________________________________________________________________
/*!
Clear old EMC stuff
*/
Bool_t StEmcADCtoEMaker::clearOldEmc()
{  
	if(!mEmc) return kFALSE;
	StSPtrVecEmcPoint& pvec = mEmc->barrelPoints();
  if(pvec.size()>0)  pvec.clear(); 
 
  for(Int_t i=0; i<4; i++)
  {
    StDetectorId id = static_cast<StDetectorId>(i+kBarrelEmcTowerId);
    StEmcDetector* detector=mEmc->detector(id);
    if(detector)
    {
      if(detector->cluster())
			{
      	StSPtrVecEmcCluster& cluster=detector->cluster()->clusters();
      	if(cluster.size()>0) cluster.clear();  
      }
      for(UInt_t j=1;j<=detector->numberOfModules() ;j++)
			{
				StEmcModule *module = detector->module(j);
				if(module)
				{
					StSPtrVecEmcRawHit&  hits=module->hits();
					hits.clear();
				}
			}
    }
  }
	return kTRUE;
}
void StEmcADCtoEMaker::setSMDEnergyCut(Float_t a, Float_t b)
{
    mControlADCtoE->CutOff[2]=a;
    mControlADCtoE->CutOffType[2]=0;
    mControlADCtoE->CutOff[3]=b;
    mControlADCtoE->CutOffType[3]=0;
}
void StEmcADCtoEMaker::setSMDRmsCut(Float_t a, Float_t b)
{
    mControlADCtoE->CutOff[2]=a;
    mControlADCtoE->CutOffType[2]=1;
    mControlADCtoE->CutOff[3]=b;
    mControlADCtoE->CutOffType[3]=1;
}
void StEmcADCtoEMaker::printMap(Int_t detector,char*file)
{
    if(!mDecoder) return;
    ofstream f(file);
    if(detector==1) mDecoder->PrintTowerMap(&f);
    if(detector==2) mDecoder->PrintPsdMap(&f);
    if(detector==3) mDecoder->PrintSmdMap(&f);
    if(detector==4) mDecoder->PrintSmdMap(&f);
    f.close();
    return;
}

