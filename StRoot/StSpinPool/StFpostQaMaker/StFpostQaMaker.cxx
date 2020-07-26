/*
 *
 * \class StFpostQaMaker
 *
 */

#include "StFpostQaMaker.h"

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/StEvent/StTriggerData.h"
#include "StRoot/StEvent/StFmsCollection.h"
#include "StRoot/StEvent/StFmsHit.h"
#include "StRoot/StFmsDbMaker/StFmsDbMaker.h"
#include "StRoot/StSpinPool/StFpsRawDaqReader/StFpsRawDaqReader.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TString.h"
#include "TFile.h"
#include "TCanvas.h"

#include <string.h>
#include <time.h>


//Default Constructor
StFpostQaMaker::StFpostQaMaker(const Char_t* name) : StMaker(name),mFmsDbMkr(0),mFmsCollectionPtr(0), mRun(0) {};

//Defatult Destructor
StFpostQaMaker::~StFpostQaMaker(){};

//Function that will initailze variables and histograms
Int_t StFpostQaMaker::Init()
{
  //Opening the FMS database
  mFmsDbMkr = static_cast< StFmsDbMaker*>(GetMaker("fmsDb"));
  if( !mFmsDbMkr )
    {
      LOG_FATAL << "Error finding StFmsDbMaker"<< endm;
      return kStFatal;
    }
  
  int yday=mRun/1000;//The usual 'yday' variable which looks like year followed by day

  //Making the root file that data will be written to
  sprintf(mFilename,"%d/%d.root",yday,mRun);
  printf("StFpostQaMaker::Init - Opening %s\n",mFilename);
  mFile=new TFile(mFilename,"RECREATE");

  char name[100];//variable to hold names of histograms

  //Histograms for the Data size
  mDataSize[0] = new TH1F("TotalSize","TotalSize",100,-1.0,4.0);
  mDataSize[1] = new TH1F("DataSize","DataSize",100,-1.0,3.0);

  mRccDiff[0] = new TH1F("RccDiff_Full","RccDiff_Full",100,0,120000);
  mRccDiff[1] = new TH1F("RccDiff_Zoom","RccDiff_Zoom",100,0,10000);
  
  //Histograms for Crossings
  for(int i=0; i<mNPREPOST*2+1; i++)
    {
      int x=i-mNPREPOST;
      sprintf(name,"Xing=%d",x);
      mXing[i] = new TH1F(name,name,100,0.0,3000.0);
    }

  //Histograms for the ADCs
  if(mPed==0)//physics
    {
      mAdc2[0] = new TH2F("Adc2", "Adc2", 252,0.0,252.0,64,0.0,4096.0);
      mAdc2[1] = new TH2F("Adc2z","Adc2z",252,0.0,252.0,50,0.0,200.0);
      for(int i=0; i<mNID; i++){
	sprintf(name,"ADC%03d",i);    
	mAdc[i][0]=new TH1F(name,name,128,0.0,4096.0);
	sprintf(name,"ADC%03dz",i);    
	mAdc[i][1]=new TH1F(name,name,150,0.0,300.0);
      }
    }
  else//not physics (pedestal)
    {
      mAdc2[0] = new TH2F("Adc2", "Adc2", 252,0.0,252.0, 100,64.0,4096.0);
      mAdc2[1] = new TH2F("Adc2z","Adc2z",252,0.0,252.0, 100,0.0,200.0);
      for(int i=0; i<mNID; i++){
	sprintf(name,"ADC%03d",i);    
	mAdc[i][0]=new TH1F(name,name,128,0.0,4096.0);
	sprintf(name,"ADC%03dz",i);    
	mAdc[i][1]=new TH1F(name,name,100,0.0,200.0);
      }
    }

  //Histograms for number of hits and hits per slat
  /*
  for( int q = 0; q <= 2 )
    {
      for( int l = 0; l <= 2; l++ )
	{
	  //if( q == 2 && l == 1 ){mNHit[q][l]=0;continue;}
	  sprintf(name,"NHIT_Q%1dL%1d",q+1,l+1);
	  mNHit[q][l]=new TH1F(name,name,22,0.0,22.0);
	  sprintf(name,"HIT_Q%1dL%1d",q+1,l+1);
	  mHit[q][l]=new TH1F(name,name,14,0.5,14.5);
	}

      //Layer 4
      sprintf(name,"NHIT_Q%1dL%1d",q+1,4);
      mNHit[q][3]=new TH1F(name,name,22,0.0,22.0);
      sprintf(name,"HIT_Q%1dL%1d",q+1,4);
      mHit[q][3]=new TH1F(name,name,25,0.5,25.5);

      //Layer 5 Top
      sprintf(name,"NHIT_Q%1dL%1dT",q+1,5);
      mNHit[q][4]=new TH1F(name,name,22,0.0,22.0);
      sprintf(name,"HIT_Q%1dL%1dT",q+1,5);
      mHit[q][4]=new TH1F(name,name,21,0.5,21.5);

      //Layer 5 Bottom
      sprintf(name,"NHIT_Q%1dL%1dB",q+1,5);
      mNHit[q][5]=new TH1F(name,name,22,0.0,22.0);
      sprintf(name,"HIT_Q%1dL%1dB",q+1,5);
      mHit[q][5]=new TH1F(name,name,21,21.5,43.5);
      }
      
  */
  
  for(int q=0; q<mNQ; q++)
    {
      for(int l=0; l<mNL; l++)
	{
	  sprintf(name,"NHIT_Q%1dL%1d",q+1,l+1);
	  mNHit[q][l]=new TH1F(name,name,22,0.0,22.0);
	  sprintf(name,"HIT_Q%1dL%1d",q+1,l+1);
	  mHit[q][l]=new TH1F(name,name,43,0.5,43.5);
	}
    }
 
  //Histograms for the triggers
  for(int t=0; t<mNTRG+1; t++)
    {
      sprintf(name,"NHIT_TRG%02d",t);
      mNHitTrg[t]=new TH1F(name,name,241,0.0,241.0);
    }
  sprintf(name,"NHIT_TRG");
  mNHitTrg2=new TH2F(name,name,241,0.0,241.0,64,0.0,64.0);

  return kStOK;
};

//Function that will use the database and the collection pointer to fill the histograms
Int_t StFpostQaMaker::Make()
{
  //cout << "In Make" << endl;
  StEvent* eventPtr=0;
  mFmsCollectionPtr=0;
  
  eventPtr= (StEvent*)GetInputDS("StEvent");
  if(!eventPtr) { LOG_INFO << "No StEvent found" << endm;}
  else{ mFmsCollectionPtr=eventPtr->fmsCollection();}
  if(!mFmsCollectionPtr)
    {
      LOG_INFO << "No StFmsCollection found" << endm;
      return kStErr;
    }
  
  unsigned int nhit=mFmsCollectionPtr->numberOfHits();
  StSPtrVecFmsHit hits = mFmsCollectionPtr->hits(); 
  printf("StFpostQaMaker found %d hits\n",nhit);
  /*
  if( nhit == 0 )
    {
      printf("There were no hits skipping this iteration\n");
      return kStOK;
    }
  */
  int nfpostdata=0;   //counter for number of fpost data points for xing=0
  int nfpostdatatot=0;//counter for total number of fpost data points
  int nh[mNQ][mNL]; memset(nh,0,sizeof(nh));//number of hits array
  int nhtot=0;//total number of hits over all quadrands and layers

  //Loop through all the hits and fill adcs, xing, and hits
  //cout << "Beginning for loop for hits" << endl;
  for (unsigned int i=0; i<nhit; i++)
    {
      //cout << "inside for loop" << endl;
      int det = hits[i]->detectorId();
      //Check if the detector id is the same as for the fpost
      //cout << "Detector ID: " << det << endl;
      if( det == 15 )//See StRoot/StEvent/StEnumerations.h to see what is the correct ID
	{
	  ran_fpost = true;
	  nfpostdatatot++;
	  int xing   = hits[i]->tdc(); if(xing>65536/2){xing-=65536;}
	  int adc    = hits[i]->adc();
	  int qt     = hits[i]->qtSlot();
	  int ch     = hits[i]->qtChannel();
	  int slatid = mFmsDbMkr->fpostSlatidFromQT(qt,ch);

	  int q=0;int l=0;int s=0;
	  mFmsDbMkr->fpostQLSfromSlatId(slatid,&q,&l,&s);

          //cout << "Begin outside Check: SlatID="<<slatid << "|xing="<<xing << "|adc="<<adc << "|qt="<<qt << "|ch="<<ch << "|q="<<q <<",l="<<l << ",s="<<s << endl;

	  if( q>0 && l>0 && s>0 && abs(xing) <= mNPREPOST )
	    {
	      mXing[xing+mNPREPOST]->Fill((float)adc);
	      if( xing == 0 || mPed != 0 )//Collision for this event
		{
		  nfpostdata++;
		  mAdc2[0]->Fill((float)slatid,(float)adc);
		  mAdc2[1]->Fill((float)slatid,(float)adc);
		  
                  //cout << "Begin inside Check: slatID=" << slatid << "|xing="<<xing << "|adc="<<adc << "|qt="<<qt << "|ch="<<ch << "|q="<<q <<",l="<<l << ",s="<<s << endl;

		  mAdc[slatid][0]->Fill((float)adc);
		  mAdc[slatid][1]->Fill((float)adc);
		  if( adc>50 )
		    {
		      nhtot++;
		      nh[q-1][l-1]++;
		      mHit[q-1][l-1]->Fill(float(s));
		    }//if( adc>50) )
		}//if( xing == 0 )
	    }//if(q>0 && l>0 && s>0 && abs(xing)<=mNPREPOST)
	  //hits[i]->print();      
	}//if( det == 15 )
    }//for (unsigned int i=0; i<nhit; i++)
  
  //cout << "Finished first for loop" << endl;

  //Filling number of hits
  mDataSize[0]->Fill(log10(nfpostdatatot));
  mDataSize[1]->Fill(log10(nfpostdata));
  //Filling number of hits per quadrant and layer
  for(int q=0; q<mNQ; q++)
    {
      for(int l=0; l<mNL; l++)
	{
	  mNHit[q][l]->Fill(float(nh[q][l]));
	}
    } 

  //cout << "Finshed second for loop" << endl;
 
  //total multiplicity by triggers
  mNHitTrg[64]->Fill(float(nhtot));//cout << "mNHitTrg->Fill" << endl;
  unsigned long long one=1;//cout << "long long one" << endl;
  StFpsRawDaqReader* fpsraw=(StFpsRawDaqReader*)GetMaker("fpsRawDaqReader");//cout <<"pointer: " << fpsraw <<endl;
  if( fpsraw == 0 ){return kStOK;}
  //cout << "Checking: " << endl; cout << fpsraw->trgMask() << endl;
  //cout << "value" << endl;
  unsigned long long tmask = fpsraw->trgMask();//cout << "long long tmask" << endl;
  //cout << "Beginning for loop" << endl;
  for(int t=0; t<mNTRG; t++)
    {
      //cout << "Inside third for loop" << endl;
      if( tmask & (one<<t) )
	{
	  mNHitTrg[t]->Fill(float(nhtot));
	  mNHitTrg2->Fill(float(nhtot),float(t));
	}
    }

  //cout << "Finished third for loop" << endl;
  //printf("NFMSHIT=%4d NFPSHITTOT=%4d NFPSHIT(xing=0)=%d\n",nhit,nfpsdatatot,nfpsdata); 
  

  StTriggerData* trg = fpsraw->trgdata();
  if(trg)
    {
      unsigned int tcu=trg->tcuCounter();
      unsigned int rfpo=fpsraw->rccFpost();
      unsigned int dfpo=rfpo-tcu;
      if(rfpo==0)
	{
	  dfpo=0;
	}
      else if(rfpo < tcu)
	{
	  const long long one=1;
	  const long long m=one<<32;
	  long long r=rfpo;
	  long long t=tcu;
	  dfpo=(unsigned int)(r+m-t);
	}
      mRccDiff[0]->Fill(dfpo);
      mRccDiff[1]->Fill(dfpo);
      //printf("FPOST RCC=%10d  TCU=%10d  DIFF=%10d\n",rfpo,tcu,dfpo);
    }
  else
    {
      printf("No StTriggerData found\n");
    }

  return kStOK;
};

Int_t StFpostQaMaker::Finish(){
  /*
  mDataSize[0]->Write();
  mDataSize[1]->Write();
  for(int i=0; i<mNPREPOST*2+1; i++) mXing[i]->Write();
  mAdc2->Write();
  for(int i=0; i<mNID; i++) mAdc[i]->Write();
  */
  
  if( ran_fpost )//fpost was run so write the root file
    {
      cout << "This run has FPOST" << endl;
      mFile->Write();
      mFile->Close();
      printf("StFpostQaMaker::Finish - Closing %s\n",mFilename);
      return kStOK;
    }
  else
    {
      cout << "This run has no FPOST" << endl;
      //This will create an empty file instead of a root file so that I don't waste time plotting
      int yday = mRun/1000;//The usual 'yday' variable which looks like year followed by day
      stringstream outFile_name;
      outFile_name << "/ldaphome/dkap7827/fpost_onl_monitoring/" << yday << "/" << mRun << ".empty.txt";
      ofstream outFile( outFile_name.str().c_str() );
      if( !outFile.is_open() )
	{
	  cout << "Could not open file: " << outFile_name.str() << endl;
	  exit(0);
	}
      outFile << endl;
      mFile->Close();
      outFile.close();
      printf("StFpostQaMaker::Finish - Closing %s and %s\n",outFile_name.str().c_str(),mFilename);
      return kStOK;
      }
};

ClassImp(StFpostQaMaker);

/*
 * $Id: StFpostQaMaker.cxx,v 1.1 2017/02/22 07:14:44 akio Exp $
 * $Log: StFpostQaMaker.cxx,v $
 * Revision 1.1  2017/02/22 07:14:44  akio
 * Initial version from David
 *
 *
 * Revision 1.1  2015/02/01 23:22:56  dkap7827
 * new fpost qa maker
 *
 */

