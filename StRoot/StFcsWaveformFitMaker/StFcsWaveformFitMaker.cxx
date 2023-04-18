// $Id: StFcsWaveformFitMaker.cxx,v 1.2 2021/05/30 21:26:52 akio Exp $
// $Log: StFcsWaveformFitMaker.cxx,v $
// Revision 1.2  2021/05/30 21:26:52  akio
// Added mFitDrawOn=2 for resetting mHitIdx for end of page, instead of each event
// Increased accepted tb range as hit, need further tuning
//
// Revision 1.1  2021/03/30 13:40:13  akio
// FCS code after peer review and moved from $CVSROOT/offline/upgrades/akio
//
// Revision 1.18  2021/02/25 21:56:28  akio
// Int_t -> int
//
// Revision 1.17  2021/02/25 19:24:33  akio
// Modified for STAR code review (Hongwei)
//
// Revision 1.16  2021/02/13 13:43:09  akio
// Some cleanups
//
// Revision 1.15  2021/02/13 03:11:44  akio
// Added TGraphAsymmErrorsWithReset and separate resetGraph() and simplify getGraph(idx=-1)
//
// Revision 1.14  2021/02/11 18:45:00  akio
// fix getGraph(0,n) to getGraph(mHitIdx,n)
//
// Revision 1.13  2021/02/10 04:14:41  akio
// And Davids new draw functions
//
// Revision 1.12  2021/02/10 04:11:24  akio
// Code from David on improving TGraphArray housekeeping
//
// Revision 1.11  2021/01/26 18:43:14  akio
// Include David's fitter & holding of TGA for the event. Separate drawFit().
//
// Revision 1.10  2021/01/25 19:28:15  akio
// few changes to speed up
//
// Revision 1.9  2021/01/11 14:46:18  akio
// Adding another tail functional form, adding fit drawing to PDF
//
// Revision 1.8  2021/01/02 21:54:05  akio
// fix a typo
//
// Revision 1.7  2021/01/02 21:38:03  akio
// Fix some minor issues
//
// Revision 1.6  2021/01/02 21:04:53  akio
// added 11 for ped sub
//
// Revision 1.5  2020/09/17 19:01:52  akio
// fix bugs David reported
//
// Revision 1.4  2020/09/17 15:54:51  akio
// Add parameter limits so that fit won't go crazy, esp when main peak is large and 2nd peak from the main pulse found to form a peak
//
// Revision 1.3  2020/09/16 14:52:46  akio
// Use of TGraphAsymmErrors to deal with adc saturation
// Impelment gausFit() for gaussian fit
//
// Revision 1.2  2020/09/03 20:15:49  akio
// Adding res array for peak height/position/sigma and chi2
//
// Revision 1.1  2020/08/19 19:51:08  akio
// Initial version
//

#include "StFcsWaveformFitMaker.h"

ClassImp(StFcsWaveformFitMaker)

#include "StMessMgr.h"
#include "StEvent/StEventTypes.h"
#include "StEvent/StFcsHit.h"
#include "StFcsDbMaker/StFcsDb.h"
#include "StFcsDbMaker/StFcsDbPulse.h"

#include <cmath>
#include <chrono>
#include "TMath.h"
#include "TF1.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TGraph.h"
#include "TGraphAsymmErrors.h"
#include "TGraphAsymmErrorsWithReset.h"
#include "TCanvas.h"
#include "TColor.h"
#include "TStyle.h"
#include "TROOT.h"


StFcsWaveformFitMaker::StFcsWaveformFitMaker(const char* name) : StMaker(name) {
    mChWaveData.SetClass("TGraphAsymmErrors"); //Initialize with only one graph at first
    mPulseFit = 0;

    mOutFile = 0;
    
    mEnergySelect[0]=13; //default PulseFit2 for Ecal
    mEnergySelect[1]=13; //default PulseFit2 for Hcal
    mEnergySelect[2]=1;  //default sum8 for Pres

    mAnaWaveform = true; //default is to compute integral for the waveform

    for( UShort_t i=0; i<7; ++i ){
      if( i<3 ){
	mH2_Dep0DepMod[i]=0;
	mH2_Sum8Dep0[i]=0;
	mH2_Sum8DepMod[i]=0;
      }
      if( i<6 ){
	mH2F_AdcTbAkio[i] = 0;
	mH2F_AdcTbMine[i] = 0;
	mH2F_SumFitvSumWin[i] = 0;
	mH2F_APeakvMPeak[i] = 0;
	mH1F_PeakStart[i] = 0;
	mH1F_PeakEnd[i] = 0;
	mH2F_NOvsId[i] = 0;
      }
      mH2F_AdcTbValidPeak[i] = 0;
      mH1F_NPeaks[i] = 0;
      mH1F_NPeaksFiltered[i] = 0;
      mH1F_Res0[i] = 0;
      mH1F_Res0Zoom[i] = 0;
      mH1F_Sum8Res0[i] = 0;
      mH1F_Sum8Res0Zoom[i] = 0;
      mH1F_FitRes0[i] = 0;
      mH1F_FitRes0Zoom[i] = 0;
      mH2F_Sum8vFit[i] = 0;
    }
}

StFcsWaveformFitMaker::~StFcsWaveformFitMaker() {
  mChWaveData.Delete();
  delete mPulseFit;
  for( UShort_t i=0; i<7; ++i ){
    if( i<3 ){
      delete mH2_Dep0DepMod[i];
      delete mH2_Sum8Dep0[i];
      delete mH2_Sum8DepMod[i];
    }
    if( i<6 ){
      delete mH2F_AdcTbAkio[i];
      delete mH2F_AdcTbMine[i];
      delete mH2F_SumFitvSumWin[i];
      delete mH2F_APeakvMPeak[i];
      delete mH1F_PeakStart[i];
      delete mH1F_PeakEnd[i];
      delete mH2F_NOvsId[i];
    }
    delete mH2F_AdcTbValidPeak[i];
    delete mH1F_NPeaks[i];
    delete mH1F_NPeaksFiltered[i];
    
    delete mH1F_Res0[i];
    delete mH1F_Res0Zoom[i];
    delete mH1F_Sum8Res0[i];
    delete mH1F_Sum8Res0Zoom[i];
    delete mH1F_FitRes0[i];
    delete mH1F_FitRes0Zoom[i];
    delete mH2F_Sum8vFit[i];
  }
  delete mTime;

  delete mH1_NPeaksAkio;
  delete mH1_NPeaksFilteredAkio;
  
  delete mH1_PeakTiming;

  delete mH2_NPeakvsPeakXdiff;
  delete mH2_NPeakvsPeakYratio;
  delete mH1_VOverlap;
  delete mH2_NOvsNPeaks;
  delete mH2_VvsNOverlap;

  delete mH1_TimeFitPulse;

  delete mH2_HeightvsSigma;
  delete mH2_HeightvsSigmaTrig;
  delete mH1_ChiNdf;
  delete mH2_HeightvsChiNdf;
  delete mH2_MeanvsChiNdf;
  delete mH2_SigmavsChiNdf;

  delete mH1_PeakTimingGaus;
  delete mH1_PeakTimingPuls;
  delete mH2_PeakTimingCompare;

  if( mOutFile!=0 ){ mOutFile->Close(); delete mOutFile; }
}

void StFcsWaveformFitMaker::writeFile(std::string filename){
  if( filename.size()==0 ){return;}
  if( mOutFile!=0 ){ mOutFile->Close(); delete mOutFile; mOutFile=0; }
  mOutFile = new TFile(filename.c_str(),"RECREATE");
}

void StFcsWaveformFitMaker::setTest(int v)
{
  mTest=v;
  if( mOutFile==0 ){ writeFile("test.root"); }//All test levels need to have a file output to write to
  if( mTest==1 ){//Test level 1 is for testing the DEP triggering algorithm to currently selected energy method. If method is zero switch on most basic which is sum 8
    if( mEnergySelect[0]==0 ){mEnergySelect[0]=1; mEnergySelect[1]=1; mEnergySelect[2]=1; }
  }
  if( mTest==2 ){//Test level 2 is for testing *StFcsPulseAna* to gausFit() which requires either energy select 10 or 11.
    if( mEnergySelect[0]!=10 || mEnergySelect[0]!=11 ){ mEnergySelect[0]=10;}
    if( mEnergySelect[1]!=10 || mEnergySelect[1]!=11 ){ mEnergySelect[1]=10;}
    if( mEnergySelect[2]!=10 || mEnergySelect[2]!=11 ){ mEnergySelect[2]=10;}
  }
  if( mTest==3 ){//Test level 3 is for testing the steering code in PulseFit1() which is energy select 12
    if( mEnergySelect[0]!=12 ){ mEnergySelect[0]=12; }
    if( mEnergySelect[1]!=12 ){ mEnergySelect[1]=12; }
    if( mEnergySelect[2]!=12 ){ mEnergySelect[2]=12; }
  }
  if( mTest==4 ){//Test level 4 is for testing fitting all signals using my pulsefit code
    if( mEnergySelect[0]!=12 ){ mEnergySelect[0]=12; }
    if( mEnergySelect[1]!=12 ){ mEnergySelect[1]=12; }
    if( mEnergySelect[2]!=12 ){ mEnergySelect[2]=12; }
  }
  if( mTest==5 ){//Test level 5 is for testing timing on gausFit() and PulseFit1() so all is needed is that the energy select is not zero and drawing is off otherwise it will double up the hits in mChWaveData
    if( mEnergySelect[0]==0  ){ mEnergySelect[0]=1; mEnergySelect[1]=1; mEnergySelect[2]=1; mFitDrawOn=0; }
  }
  if( mTest==6 ){//Test level 6 is for testing the steering code in PulseFit2()
    if( mEnergySelect[0]!=13  ){ mEnergySelect[0]=13; }
    if( mEnergySelect[1]!=13  ){ mEnergySelect[1]=13; }
    if( mEnergySelect[2]!=13  ){ mEnergySelect[2]=13; }
  }
  if( mTest==7 ){//Test level 7 is for testing overall quality of PulseFit2()
    if( mEnergySelect[0]!=13  ){ mEnergySelect[0]=13; }
    if( mEnergySelect[1]!=13  ){ mEnergySelect[1]=13; }
    if( mEnergySelect[2]!=1   ){ mEnergySelect[2]=1;  }
  }
  if( mTest==8 ){//Test level 8 is for testing fitting quality from PulseFit2() from gausFit()
    if( mEnergySelect[0]!=13  ){ mEnergySelect[0]=13; }
    if( mEnergySelect[1]!=13  ){ mEnergySelect[1]=13; }
    if( mEnergySelect[2]!=1   ){ mEnergySelect[2]=1;  }
  }
}

void StFcsWaveformFitMaker::setTail(int v){mTail=v;}

void StFcsWaveformFitMaker::Clear(Option_t* option) {
  if(mFitDrawOn>=1) mHitIdx=0;
  StMaker::Clear(option);
}

int StFcsWaveformFitMaker::Init()
{
  if(mFilename){
    mCanvas=new TCanvas("FCSWaveFormFit","FCSWaveFormFit",10,10,2000,2000);
    gStyle->SetOptStat(0);
    char file[100];
    if( mFitDrawOn>=0 ){
      sprintf(file,"%s.pdf[",mFilename);//Opens fileA but does not save anything
      mCanvas->Print(file);
    }
  }
  if(mMeasureTime){
    mTime=new TH1F("FitTime","FitTime;msec",1000,0,1000);
  }
  if(mFilter && mFilter[0]=='0'){
    mTimeIntg[0]=new TH2F("Noboth",  "No both;  PeakTB; Integral",100,47.0,54.0,400,0.0,2000);
    mTimeIntg[1]=new TH2F("NoFall",  "No Fall;  PeakTB; Integral",100,47.0,54.0,400,0.0,2000);
    mTimeIntg[2]=new TH2F("NoRise",  "No Rise;  PeakTB; Integral",100,47.0,54.0,400,0.0,2000);
    mTimeIntg[3]=new TH2F("Accepted","Accepted; PeakTB; Integral",100,47.0,54.0,400,0.0,2000);
  }
  if( mOutFile!=0 ){
    for( UShort_t i=0; i<7; ++i ){
      std::stringstream ss;
      ss << i;
      if( i<3 ){
	if( mTest==1 ){
	  mH2_Dep0DepMod[i]=new TH2F( ("H2_Dep0DepMod_"+ss.str()).c_str(), ("Sum Dep0 vs DepMod "+ss.str()).c_str(),100,0,1000,100,0,1000);
	  mH2_Sum8Dep0[i]=new TH2F( ("H2_Sum8Dep0_"+ss.str()).c_str(), ("Sum 8 vs Dep0 "+ss.str()).c_str(),100,0,1000,100,0,1000);
	  mH2_Sum8DepMod[i]=new TH2F( ("H2_Sum8DepMod_"+ss.str()).c_str(), ("Sum 8 vs DepMod "+ss.str()).c_str(),100,0,1000,100,0,1000);
	}
      }
      if( i<6 ){
	if( mTest==2 ){
	  mH2F_AdcTbAkio[i] = new TH2F( ("H2_AdcTbAkio_"+ss.str()).c_str(),"Akio Peaks Adc vs. Tb",102,-1.5,100.5,4097,-0.5,4096.5);
	  mH2F_AdcTbMine[i] = new TH2F( ("H2_AdcTbMine_"+ss.str()).c_str(),"My Peaks Adc vs. Tb",102,-1.5,100.5,4097,-0.5,4096.5);
	  mH2F_SumFitvSumWin[i] = new TH2F( ("H2_SumFitvSumWin_"+ss.str()).c_str(),"Fitted Sum vs. Peak Window Sum",100,0,2000,100,0,2000);
	  mH2F_APeakvMPeak[i] = new TH2F( ("H2_APeakvMPeak_"+ss.str()).c_str(),"Peak Locations Akio v. Mine",80,-0.5,79.5,80,-0.5,79.5);
	  mH1F_PeakStart[i] = new TH1F( ("H1_PeakStart_"+ss.str()).c_str(),"PeakWindow start times",102,-1.5,100.5);
	  mH1F_PeakEnd[i] = new TH1F( ("H1_PeakEnd_"+ss.str()).c_str(),"PeakWindow end time",102,-1.5,100.5);
	}
	if( mTest==3 ){
	  mH2F_NOvsId[i] = new TH2F( ("H2_NOvsId_"+ss.str()).c_str(),"Number of overlaps vs. Ch Id", 748,-0.5,747.5, 11,-0.5,10.5);
	}
      }
      //i<7 histograms
      if( mTest==2 ){
	if( i<1 ){  //Test=2 only needs 1 histogram
	  mH1F_NPeaks[i] = new TH1F( ("H1_NPeaks_"+ss.str()).c_str(),"Number of peaks from finder", 11,-0.5,10.5);
	  mH1F_NPeaksFiltered[i] = new TH1F( ("H1_NPeaksFiltered_"+ss.str()).c_str(),"Number of peaks from finder when a valid peak was found", 11,-0.5,10.5);
	}
	mH2F_AdcTbValidPeak[i] = new TH2F( ("H2_AdcTbValidPeak_"+ss.str()).c_str(),"Valid Peaks Adc vs. Tb",102,-1.5,100.5,4097,-0.5,4096.5);
      }
      if( mTest==3 || mTest==6 || mTest==7 || mTest==8 ){
	mH1F_NPeaks[i] = new TH1F( ("H1_NPeaks_"+ss.str()).c_str(),"Number of peaks from finder", 11,-0.5,10.5);
	mH1F_NPeaksFiltered[i] = new TH1F( ("H1_NPeaksFiltered_"+ss.str()).c_str(),"Number of peaks from finder when a valid peak was found", 11,-0.5,10.5);
	mH1F_Res0[i] = new TH1F( ("H1_Res0_"+ss.str()).c_str(),"All ADC sums", 100,0,2000 );
	mH1F_Res0Zoom[i] = new TH1F( ("H1_Res0Zoom_"+ss.str()).c_str(),"All ADC sums", 201,-0.5,200.5 );
      }
      if( mTest==3 || mTest==6 ){
	mH1F_Sum8Res0[i] = new TH1F( ("H1_Sum8Res0_"+ss.str()).c_str(),"All ADC sums using sum 8", 100,0,2000 );
	mH1F_Sum8Res0Zoom[i] = new TH1F( ("H1_Sum8Res0Zoom_"+ss.str()).c_str(),"All ADC sums using sum 8", 201,-0.5,200.5 );
      }
      if( mTest==3 || mTest==6 || mTest==8 ){
	mH1F_FitRes0[i] = new TH1F( ("H1_FitRes0_"+ss.str()).c_str(),"All ADC sums using fitting", 100,0,2000 );
	mH1F_FitRes0Zoom[i] = new TH1F( ("H1_FitRes0Zoom_"+ss.str()).c_str(),"All ADC sums using fitting", 201,-0.5,200.5 );
	mH2F_Sum8vFit[i] = new TH2F(("H2_Sum8vFit_"+ss.str()).c_str(),"sum 8 vs fit sum", 100,0,2000, 100,0,2000);
      }
    }
    
    if( mTest==2 ){
      mH1_PeakTiming = new TH1F("H1_PeakTiming","Timing to just find peak",200,0,5);
    }
    if( mTest==2 || mTest==8 ){
      mH1_NPeaksAkio = new TH1F("H1_NPeaksAkio","Number of peaks from current method", 11,-0.5,10.5);
      mH1_NPeaksFilteredAkio = new TH1F("H1_NPeaksFilteredAkio","Number of peaks from current method when a valid peak is found", 11,-0.5,10.5);
    }
    if( mTest==3 ){
      mH2_NPeakvsPeakXdiff = new TH2F("H2_NPeakvsPeakXdiff","NPeak vs. Peak X difference", 41,-20.5,20.5, 11,-0.5,10.5);
      mH2_NPeakvsPeakYratio = new TH2F("H2_NPeakvsPeakYratio","NPeak vs. Peak Y ratio", 100,0,10, 11,-0.5,10.5);
      mH1_VOverlap = new TH1F("H1_VOverlap","Peak Compare values",4,-0.5,3.5 );
      mH2_NOvsNPeaks = new TH2F("H2_NOvsNPeaks","Number of Overlapping peaks vs. Number of Peaks", 11,-0.5,10.5, 11,-0.5,10.5);
      mH2_VvsNOverlap = new TH2F("H2_VvsNOverlap","Value of Comparison vs. Peak index", 21,-10.5,10.5, 4,-0.5,3.5);
    }
    if( mTest==3 || mTest==6 || mTest==8 ){
      mH1_TimeFitPulse = new TH1F("H1_TimeFitPulse","FitTime;msec",1000,0,1000);
    }
    if( mTest==4 ){
      mH2_HeightvsSigma = new TH2F("H2_HeightvsSigma","Fitted Peak Height vs. Sigma;Sigma;Height", 31,-0.5,30.5, 4000,-0.5,3999.5);
      mH2_HeightvsSigmaTrig = new TH2F("H2_HeightvsSigmaTrig","Fitted Peak Height vs. Sigma Triggered Xing;Sigma;Height", 31,-0.5,30.5, 4000,-0.5,3999.5);
      mH1_ChiNdf = new TH1F("H1_ChiNdf","Chi^2/NDF for all fits;Chi^2/NDF", 50,-0.5,99.5);
      mH2_HeightvsChiNdf = new TH2F("H2_HeightvsChiNdf", "Peak Height TXing vs. Chi^2/NDF;Chi^2/NDF;Height", 50,-0.5,99.5, 500,-0.5,499.5);
      mH2_MeanvsChiNdf = new TH2F("H2_MeanvsChiNdf", "Peak Mean TXing vs. Chi^2/NDF;Chi^2/NDF;Mean", 50,-0.5,99.5, 21,39.5,60.5 );
      mH2_SigmavsChiNdf = new TH2F("H2_SigmavsChiNdf", "Peak Sigma TXing vs. Chi^2/NDF;Chi^2/NDF;Sigma", 50,-0.5,99.5, 21,-0.5,20.5 );
    }
    if( mTest==5 ){
      mH1_PeakTimingGaus = new TH1F("H1_PeakTimingGaus","gausFit timing;msec", 1000,0,1000 );
      mH1_PeakTimingPuls = new TH1F("H1_PeakTimingPuls","PulseFit timing;msec", 1000,0,1000 );
      mH2_PeakTimingCompare = new TH2F("H2_PeakTimingCompare","PulseFit vs. gausFit;ms;ms", 200,0,6, 200,0,6 );
    }
  }
  return StMaker::Init(); 
}

int StFcsWaveformFitMaker::InitRun(int runNumber) {
    LOG_DEBUG << "StFcsWaveformFitMaker initializing run" << endm;
    mDb = static_cast<StFcsDb*>(GetDataSet("fcsDb"));
    if (!mDb) {
        LOG_ERROR << "StFcsWaveformFitMaker initializing failed due to no StFcsDb" << endm;
        return kStErr;
    }
    mDbPulse = static_cast<StFcsDbPulse*>(GetDataSet("fcsPulse"));
    if (!mDbPulse) {
        LOG_ERROR << "StFcsWaveformFitMaker initializing failed due to no StFcsDbPulse" << endm;
        return kStErr;
    }
    mDbPulse->setTail(mTail);
    if( mPulseFit==0 ){ mPulseFit = new StFcsPulseAna(); SetupDavidFitterMay2022(); mPulseFit->setDbPulse(mDbPulse); }
    mPulseFit->setDbPulse(mDbPulse);
    
    return StMaker::InitRun(runNumber);
}

int StFcsWaveformFitMaker::Finish(){
  if(mFilename && mPad>=0){
    char file[200];
    sprintf(file,"%s.pdf]",mFilename);
    mCanvas->Print(file);
  }
  if(mMeasureTime){
    printf("WFFit Time Mean=%f  RMS=%f\n",mTime->GetMean(),mTime->GetRMS());
    TCanvas *c= new TCanvas("FCSWFFTime","FCSWFFTime",10,10,800,800);
    gStyle->SetOptStat(111110);
    gStyle->SetLabelSize(0.02,"xy");
    c->cd(1)->SetLogy();
    mTime->Draw();
    c->SaveAs(mMeasureTime);
    delete c;
  }
  if(mFilter && mFilter[0]=='0'){
    TCanvas *c= new TCanvas("Stage0","Stage0",10,10,800,800);
    gStyle->SetOptStat(111110);
    gStyle->SetLabelSize(0.02,"xy");
    c->Divide(2,2);
    c->cd(1)->SetLogy(); mTimeIntg[0]->Draw("colz");
    c->cd(2)->SetLogy(); mTimeIntg[1]->Draw("colz");
    c->cd(3)->SetLogy(); mTimeIntg[2]->Draw("colz");
    c->cd(4)->SetLogy(); mTimeIntg[3]->Draw("colz");
    c->SaveAs("stage0.png");
    delete c;
  }
  if( mOutFile!=0 ){
    mOutFile->cd();
    
    for( UShort_t i=0; i<7; ++i ){
      if( i<3 ){
	if( mH2_Dep0DepMod[i]!=0 ){mH2_Dep0DepMod[i]->Write();}
	if( mH2_Sum8Dep0[i]!=0 )  {mH2_Sum8Dep0[i]->Write();}
	if( mH2_Sum8DepMod[i]!=0 ){mH2_Sum8DepMod[i]->Write();}
      }
      if( i<6 ){
	if( mH2F_AdcTbAkio[i]!=0 )     { mH2F_AdcTbAkio[i]->Write();}
	if( mH2F_AdcTbMine[i]!=0 )     { mH2F_AdcTbMine[i]->Write();}
	if( mH2F_SumFitvSumWin[i]!=0 ) { mH2F_SumFitvSumWin[i]->Write();}
	if( mH2F_APeakvMPeak[i]!=0 )   { mH2F_APeakvMPeak[i]->Write();}
	if( mH1F_PeakStart[i]!=0 )     { mH1F_PeakStart[i]->Write();}
	if( mH1F_PeakEnd[i]!=0 )       { mH1F_PeakEnd[i]->Write();}
	if( mH2F_NOvsId[i]!=0    )     { mH2F_NOvsId[i]->Write();}
      }
      if( mH2F_AdcTbValidPeak[i]!=0 ){ mH2F_AdcTbValidPeak[i]->Write();}
      if( mH1F_NPeaks[i]!=0 )        { mH1F_NPeaks[i]->Write();}
      if( mH1F_NPeaksFiltered[i]!=0 ){ mH1F_NPeaksFiltered[i]->Write();}
      
      if( mH1F_Res0[i]!=0      )     { mH1F_Res0[i]->Write();}
      if( mH1F_Res0Zoom[i]!=0  )     { mH1F_Res0Zoom[i]->Write();}
      if( mH1F_Sum8Res0[i]!=0  )     { mH1F_Sum8Res0[i]->Write();}
      if( mH1F_Sum8Res0Zoom[i]!=0 )  { mH1F_Sum8Res0Zoom[i]->Write();}
      if( mH1F_FitRes0[i]!=0      )  { mH1F_FitRes0[i]->Write();}
      if( mH1F_FitRes0Zoom[i]!=0  )  { mH1F_FitRes0Zoom[i]->Write();}
      if( mH2F_Sum8vFit[i]!=0     )  { mH2F_Sum8vFit[i]->Write();}
    }
    
    if( mTime!=0 ){ mTime->Write(); }
    
    if( mH1_NPeaksAkio!=0 ){ mH1_NPeaksAkio->Write(); }
    if( mH1_NPeaksFilteredAkio!=0 ){ mH1_NPeaksFilteredAkio->Write(); }
    if( mH1_PeakTiming!=0 ){mH1_PeakTiming->Write();}
    
    if( mH2_NPeakvsPeakXdiff!=0 )      {mH2_NPeakvsPeakXdiff->Write();}
    if( mH2_NPeakvsPeakYratio!=0)      {mH2_NPeakvsPeakYratio->Write();}
    if( mH1_VOverlap!=0         )      {mH1_VOverlap->Write();}
    if( mH2_NOvsNPeaks!=0       )      {mH2_NOvsNPeaks->Write();}
    if( mH2_VvsNOverlap!=0      )      {mH2_VvsNOverlap->Write();}
    if( mH1_TimeFitPulse!=0     )      {mH1_TimeFitPulse->Write();}
    
    if( mH2_HeightvsSigma!=0    )      {mH2_HeightvsSigma->Write();}
    if( mH2_HeightvsSigmaTrig!=0)      {mH2_HeightvsSigmaTrig->Write();}
    if( mH1_ChiNdf!=0           )      {mH1_ChiNdf->Write();}
    if( mH2_HeightvsChiNdf!=0   )      {mH2_HeightvsChiNdf->Write();}
    if( mH2_MeanvsChiNdf!=0     )      {mH2_MeanvsChiNdf->Write();}
    if( mH2_SigmavsChiNdf!=0    )      {mH2_SigmavsChiNdf->Write();}

    if( mH1_PeakTimingGaus!=0   )      {mH1_PeakTimingGaus->Write();}
    if( mH1_PeakTimingPuls!=0   )      {mH1_PeakTimingPuls->Write();}
    if( mH2_PeakTimingCompare!=0)      {mH2_PeakTimingCompare->Write();}
      
    //In case you want to save a particular graph needs mFitDrawOn==1
    //TGraphAsymmErrors* g = getGraph(0,161);
    //std::cout << "found graph:"<<g << std::endl;
    //if( g!=0 ){ g->Write("GAE_det0id161"); }
  }
  return kStOK;
}

int StFcsWaveformFitMaker::Make() {
    LOG_DEBUG << "StFcsWaveformFitMaker Make!!!" << endm;

    //Get FCS hits from StEvent
    StEvent* event = static_cast<StEvent*>(GetInputDS("StEvent"));
    mFcsCollection=0;
    if (event) mFcsCollection = event->fcsCollection();
    if(!mFcsCollection) {
	LOG_WARN << "StFcsWaveformFitMaker did not find fcsCollection in StEvent" << endm;
	return kStWarn;	
    }
    
    if(mEnergySelect[0]==0) return kStOK;  // don't touch energy, directly from MC

    //Loop over all hits and run waveform analysis of the choice
    float res[8] = {0};
    TF1* func=0;
    for(int det=0; det<kFcsNDet; det++) {      
	StSPtrVecFcsHit& hits = mFcsCollection->hits(det);
	int ehp = det/2;
	int nhit=hits.size();
	for(int i=0; i<nhit; i++){ //loop over all hits  	    
	    memset(res,0,sizeof(res));
	  auto start=std::chrono::high_resolution_clock::now();

	  //if we are geting pedestal from data
	  if(mPedMin>=0){
	    AnaPed( hits[i], res[6], res[7] );
	    mDb->setPedestal(hits[i]->ehp(), hits[i]->ns(), hits[i]->dep(), hits[i]->channel(), res[6] );
	    if(GetDebug()>1){
	      char name[100];
	      mDb->getName(hits[i]->ehp(), hits[i]->ns(), hits[i]->dep(), hits[i]->channel(),name);
	      LOG_DEBUG << name << " Pedestal ("<<mPedMax<<"-"<<mPedMin<<"+1)="<< res[6] <<std::endl;
	    }
	  }
	  
	  //run waveform analysis of the choice and store as AdcSum
	  float integral = hits[i]->adcSum();
	  if( mAnaWaveform ){
	    integral = analyzeWaveform(mEnergySelect[ehp],hits[i],res,func,res[6]);
	    hits[i]->setAdcSum(integral);
	  }
	  hits[i]->setFitPeak(res[2]);	    
	  hits[i]->setFitSigma(res[3]);	    
	  hits[i]->setFitChi2(res[4]);	    
	  hits[i]->setNPeak(res[5]);	    
	  //apply gain and update energy
	  float gain = mDb->getGain(hits[i]);
	  float gaincorr = mDb->getGainCorrection(hits[i]);
	  hits[i]->setEnergy(integral*gain*gaincorr);
	  if(GetDebug()>0) printf("det=%1d id=%3d integ=%10.2f peak=%8.2f, sig=%8.4f chi2=%8.2f npeak=%2d\n",
				  det,hits[i]->id(),integral,res[2],res[3],res[4],int(res[5]));
	  if(mMeasureTime){
	    auto stop=std::chrono::high_resolution_clock::now();
	    long long usec = chrono::duration_cast<chrono::microseconds>(stop-start).count();
	    //printf("Fit Time = %lld usec\n",usec);
	    mTime->Fill(float(usec)/1000.0);
	  }
	  if( mTest==5 ){
	    auto startg = std::chrono::high_resolution_clock::now();
	    integral = analyzeWaveform(10,hits[i],res,func,res[6]);
	    auto stopg = std::chrono::high_resolution_clock::now();
	    long long usecg = chrono::duration_cast<chrono::microseconds>(stopg-startg).count();
	    auto startp = std::chrono::high_resolution_clock::now();
	    integral = analyzeWaveform(12,hits[i],res,func,res[6]);
	    auto stopp = std::chrono::high_resolution_clock::now();
	    long long usecp = chrono::duration_cast<chrono::microseconds>(stopp-startp).count();
	    mH1_PeakTimingGaus->Fill(float(usecg)/1000.0);
	    mH1_PeakTimingPuls->Fill(float(usecp)/1000.0);
	    mH2_PeakTimingCompare->Fill(float(usecg)/1000.0,float(usecp)/1000.0);
	    if( usecp>200000.0 ){ std::cout << getGraph()->GetName() << std::endl; }
	  }
	}
    }
    return kStOk;
}

TGraphAsymmErrors* StFcsWaveformFitMaker::resetGraph(){
  TGraphAsymmErrorsWithReset* gae = (TGraphAsymmErrorsWithReset*)mChWaveData.ConstructedAt(mHitIdx);
  gae->Reset();
  if(mFitDrawOn) mHitIdx++;
  return (TGraphAsymmErrors*)gae;
}

TGraphAsymmErrors* StFcsWaveformFitMaker::getGraph(int idx){
    if(idx<0){ 
      if(mHitIdx>0) {idx=mHitIdx-1;}
      else          {idx=0;}
    }
    TGraphAsymmErrors* gae = (TGraphAsymmErrors*)mChWaveData.ConstructedAt(idx);
    return gae;
}

TGraphAsymmErrors* StFcsWaveformFitMaker::getGraph(int det, int id)
{
  //Since mHitIdx may have been reset by call to clear use size of array instead this way you can find graphs even if Clear() has been called
  for( Int_t i=0; i<mChWaveData.GetEntriesFast(); ++i ){
    int det0 = -1;
    int id0  = -1;
    TGraphAsymmErrors* gae = (TGraphAsymmErrors*)mChWaveData.At(i);
    StFcsDb::getFromName(gae->GetName(),det0,id0);
    //std::cout << "|det0:"<<det0 << "|id0:"<<id0 << std::endl;
    if( det0>=0 && det0==det ){
      if( id0>=0 && id0==id ){
	return gae;
      }
    }
  }
  LOG_ERROR << "Unable to find det:"<<det << " id:"<<id << " in graph array" << std::endl;
  return 0;
}

TGraphAsymmErrors* StFcsWaveformFitMaker::makeTGraphAsymmErrors(int n, double* tb, double* adc){
    TGraphAsymmErrors* gae = resetGraph();
    for(int i=0; i<n; ++i){
        gae->SetPoint(i,tb[i],adc[i]);
	StFcsDbPulse::setTGraphAsymmErrors(gae, i, adc[i],mError,mErrorSaturated);
    }
    return gae;
}

TGraphAsymmErrors* StFcsWaveformFitMaker::makeTGraphAsymmErrors(TGraph* g){
    int n = g->GetN();
    double *t = g->GetX();
    double *a = g->GetY();
    return makeTGraphAsymmErrors(n, t, a);
}

TGraphAsymmErrors* StFcsWaveformFitMaker::makeTGraphAsymmErrors(TH1* hist){
  int NValues = hist->GetNbinsX();
  TGraphAsymmErrors* gae = resetGraph();
  for(int i=1; i<=NValues; ++i ){
      double adc = hist->GetBinContent(i);
      gae->SetPoint(i-1,hist->GetBinCenter(i),adc);
      StFcsDbPulse::setTGraphAsymmErrors(gae,i-1,adc,mError,mErrorSaturated);
  }
  return gae;
}

TGraphAsymmErrors* StFcsWaveformFitMaker::makeTGraphAsymmErrors(StFcsHit* hit){  
    //int N = mMaxTB - mMinTB +1;
    int n = hit->nTimeBin();
    TGraphAsymmErrors* gae = resetGraph();
    mDb->getName(hit->detectorId(),hit->id(),mDetName);
    gae->SetName(mDetName);
    int j=0;
    for(int i=0; i<n; i++){
        int tb=hit->timebin(i);
        if(tb>=mMinTB){
	    int adc=hit->adc(i);
	    gae->SetPoint(j,tb,adc);
	    StFcsDbPulse::setTGraphAsymmErrors(gae,j,adc,mError,mErrorSaturated);
	    j++;
	}
	if(tb>=mMaxTB) break;
    }
    //printf("GetN()=%d gae=%d\n",gae->GetN(),gae);
    return gae;
}


float StFcsWaveformFitMaker::AnaPed( TGraphAsymmErrors* g, float& ped, float& pedstd )
{
  int n = g->GetN();
  double *t = g->GetX();
  double *a = g->GetY();    
  int p=0;
  double sumsq = 0;//For variance
  for(int i=0; i<n; i++){
    int tb1=t[i];
    if(mPedMin<=tb1 && tb1<=mPedMax){    
      p+=a[i];
      sumsq += a[i]*a[i];
    }
  }
  ped = float(p)/(mPedMax-mPedMin+1.0);
  pedstd = sqrt( ( sumsq-((double(p)*double(p))/(mPedMax-mPedMin+1.0)) )/(mPedMax-mPedMin) );//Variance/StdDev using naive algorithm from https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
  return ped;
}

float StFcsWaveformFitMaker::AnaPed( StFcsHit* hit, float& ped, float& pedstd )
{
  int p=0;
  int n = hit->nTimeBin();
  double sumsq = 0;//For variance
  for(int i=0; i<n; i++){
    int tb=hit->timebin(i);
    if(mPedMin<=tb && tb<=mPedMax ){
      int adc = hit->adc(i);
      p+=adc;
      sumsq += adc*adc;
    }
    if(tb>=mPedMax) break;
  }
  ped = p/(mPedMax-mPedMin+1.0);
  pedstd = sqrt( ( sumsq-((p*p)/(mPedMax-mPedMin+1.0)) )/(mPedMax-mPedMin) );//Variance/StdDev using naive algorithm from https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
  return ped;
}

float StFcsWaveformFitMaker::analyzeWaveform(int select, TGraphAsymmErrors* g, float* res, TF1*& func, float ped){
    if(func) delete func;
    func=0;
    float integral=0.0;
    switch(select){
    case  1: integral = sum8(g, res); break;
    case  2: integral = sum16(g, res); break;
    case  3: integral = highest(g, res); break;
    case  4: integral = highest3(g, res); break;
    case 10: integral = gausFit(g, res, func, ped); break;
    case 11: integral = gausFitWithPed(g, res, func); break;
    case 12: integral = PulseFit1(g,res,func,ped); break;
    case 13: integral = PulseFit2(g,res,func,ped); break;
    case 14: integral = PulseFitAll(g,res,func,ped); break;
    case 15: integral = PulseFit2WithPed(g, res, func); break;
    case 16: integral = PulseFitAllWithPed(g, res, func); break;
    case 17: integral = PedFitPulseFit(g, res, func); break;
    default: 
      LOG_WARN << "Unknown fit/sum method select=" << select << endm;
    }
    //if(func && (mFitDrawOn || mFilter ) && mFilename && mPage<=mMaxPage) drawFit(g,func);
    int flag=1;
    if(mFilter && mFilter[0]=='0'){
      flag=0;
      if(integral>0 && res[2]>47.0 && res[2]<54.0){	
	int peak=0;
	double* t=g->GetX();
	double* a=g->GetY();
	for(int i=0; i<g->GetN()-1; i++){
	  int t0=t[i];
	  int t1=t[i+1];
	  int a0=a[i];
	  int a1=a[i+1];
	  int dt=t1-t0;
	  int da=a1-a0;
	  if(GetDebug()) printf("AAA t0=%4d t1=%4d a0=%4d a1=%4d dt=%4d da=%5d ",t0,t1,a0,a1,dt,da);
	  if(t0==mCenterTB-3 && dt==1){
	    if(da>0) {
	      peak+=1;	
	      if(GetDebug()) printf("R"); 
	    } 
	    else {if(GetDebug()) printf("X");}
	  }
	  if(t0==mCenterTB+3 && dt==1){
	    if(da<0) {
	      peak+=2; 
	      if(GetDebug()) printf("F"); 
	    }
	    else {if(GetDebug()) printf("X");}
	  }
	  if(GetDebug()) printf("\n");
	}
	printf("BBB Intg=%8.2f peak=%6.2f Raise/Fall=%1d\n",integral,res[2],peak); 
	if(peak<3 && integral>50 && res[2]>49) flag=1;
	mTimeIntg[peak]->Fill(res[2],integral);
      }
    }else if(mFilter){
      flag=0;
      TString dname(mDetName);
      if(integral>50 && dname.Contains(mFilter)) flag=1;
    }
    if(mFitDrawOn && flag && mFilename && mPage<=mMaxPage) {
      printf("hit:%u det=%s func=%p mFitDrawOn=%d mFilter=%s mFilename=%s mPage=%d mMaxPage=%d integral=%f\n",
	     mHitIdx,mDetName,(void*)func,mFitDrawOn,mFilter,mFilename,mPage,mMaxPage,integral);	
      drawFit(g,func);
    }

    if( mOutFile!=0 ){
      if( mTest==1 ){
	int det = 0; int ch=0;
	mDb->getFromName( g->GetName(), det,ch );
	int sumdep0 = StFcsPulseAna::SumDep0(g,centerTB()-3);
	int sumdepmod = StFcsPulseAna::SumDep0Mod(g,centerTB()-3);
	if( sumdep0>sumdepmod ){std::cout << " - StFcsWaveformFitMaker::analyzeWaveform|SumDep0:"<<sumdep0 << "|SumDepMod:"<<sumdepmod << endl;}
	mH2_Dep0DepMod[det/2]->Fill(sumdepmod,sumdep0);
	mH2_Sum8Dep0[det/2]->Fill(sumdep0,integral);
	mH2_Sum8DepMod[det/2]->Fill(sumdepmod,integral);
      }
      if( mTest==7 ){
	int det0 = 0; int ch0=0;
	mDb->getFromName( g->GetName(), det0,ch0 );
	mH1F_Res0[det0]->Fill(res[0]);
	mH1F_Res0Zoom[det0]->Fill(res[0]);
	mH1F_Res0[6]->Fill(res[0]);
	mH1F_Res0Zoom[6]->Fill(res[0]);
      }
    }
    return integral;
}

float StFcsWaveformFitMaker::analyzeWaveform(int select, int n, double* tb, double* adc, float* res, TF1*& func, float ped){
    TGraphAsymmErrors* gae = makeTGraphAsymmErrors(n,tb,adc);
    return analyzeWaveform(select, gae, res, func,ped);
}

float StFcsWaveformFitMaker::analyzeWaveform(int select, TGraph* g, float* res, TF1*& func, float ped){
    TGraphAsymmErrors* gae = makeTGraphAsymmErrors(g);
    return analyzeWaveform(select,gae,res,func,ped);
}

float StFcsWaveformFitMaker::analyzeWaveform(int select, StFcsHit* hit, float* res, TF1*& func, float ped){
    TGraphAsymmErrors* gae = makeTGraphAsymmErrors(hit);
    return  analyzeWaveform(select, gae, res, func, ped);
}

float StFcsWaveformFitMaker::sum8(TGraphAsymmErrors* g, float* res){
    int min = mCenterTB-3;
    int max = mCenterTB+4;
    int n = g->GetN();
    double *t = g->GetX();
    double *a = g->GetY();
    float sum=0;
    float tsum=0;
    for(int i=0; i<n; i++){
	double tb=t[i];
	if(tb>=min && tb<=max) {
	  sum += a[i];
	  tsum += a[i] * tb;
	}
    }
    //res[0]/=1.226; //Compensate for fitting sum in ECal. Only turn on for testing comparisons
    res[0]=sum;
    if(sum>0) res[2]=tsum/sum;
    return sum;
}

float StFcsWaveformFitMaker::sum16(TGraphAsymmErrors* g, float* res){
    int min = mCenterTB-3-4; // 16 timebin sum means from center - 7 
    int max = mCenterTB+4+4; //                      tp   center + 8
    int n = g->GetN();
    double *t = g->GetX();
    double *a = g->GetY();
    float sum=0;
    float tsum=0;
    for(int i=0; i<n; i++){
	double tb=t[i];
	if(tb>=min && tb<=max) {
	  sum += a[i];
	  tsum += a[i] * tb;
	}
    }
    res[0]=sum;
    if(sum>0) res[2]=tsum/sum;
    return sum;
}

float StFcsWaveformFitMaker::highest(TGraphAsymmErrors* g, float* res){
    int min = mCenterTB-3;
    int max = mCenterTB+4;
    int n = g->GetN();
    double *t = g->GetX();
    double *a = g->GetY();
    float maxadc=0;
    int maxtb=0; 
    for(int i=0; i<n; i++){ //find max adc witin triggered xing
	float tb=t[i];
	if(tb>=min && tb<=max) {	    
	    float adc=a[i];
	    if(adc>maxadc){
		maxadc=adc;
		maxtb=tb;
	    }
	}
    }
    //! https://www.star.bnl.gov/protected/spin/akio/fcs/pulse/waveformRes.png
    res[0]=maxadc / 0.2;   //this is estimated "full integral" 
    res[1]=maxadc;         //this is peak height	       
    res[2]=maxtb;          //this is peak position [timebin]   
    //res[3]=0.0;          //no width from this method	       
    //res[4]=0.0;          //no chi2 from this		       
    //res[5]=0.0;          //no # of peaks		       
    return maxadc;         //this is the highest               
}

float StFcsWaveformFitMaker::highest3(TGraphAsymmErrors* g, float* res){
    int min = mCenterTB-3;
    int max = mCenterTB+4;
    int n = g->GetN();
    double *t = g->GetX();
    double *a = g->GetY();
    float sum=0, tsum=0, maxadc=0;
    int maxtb=0; 
    for(int i=0; i<n; i++){ //find max adc witin triggered xing
	float tb=t[i];
	if(tb>=min && tb<=max) {	    
	    float adc=a[i];
	    if(adc>maxadc){
		maxadc=adc;
		maxtb=tb;
	    }
	}
    }
    for(int i=0; i<n; i++){ //sum 3TB around max within 8 timebins
        float tb=t[i];
        if(tb>=min && tb<=max && tb>=maxtb-1 && tb<=maxtb+1) {
	    sum  += a[i];
	    tsum += a[i] * tb;
	}
    }    
    //! https://www.star.bnl.gov/protected/spin/akio/fcs/pulse/waveformRes.png
    res[0]= sum / 0.555;  //this is estimated "full integral" 
    res[1]= maxadc;	  //this is peak height	       
    if(sum>0) res[2]= tsum/sum; //this is averaged peak position [timebin]   
    //res[3]= 0.0;	  //no sigma from this	       
    //res[4]= 0.0;	  //no chi2 from this		       
    //res[5]= 0.0;        //no # of peak
    return sum;		  //this is the 3 timebin sum
}

float StFcsWaveformFitMaker::gausFit(TGraphAsymmErrors* g, float* res, TF1*& func, float ped){
    char Opt[10]="QN  ";
    if(GetDebug()>2) sprintf(Opt,"  ");
    if(GetDebug()>3) sprintf(Opt,"V ");
    //find peaks and set parameters
    int trgmin = mCenterTB-4.5;
    int trgmax = mCenterTB+5.5;
    int n = g->GetN();
    double *t = g->GetX();
    double *a = g->GetY();    
    double para[100];
    int npeak=0;
    int trgx = -1;
    double mindt = 1000;
    double tb0,tb1,tb2;
    double adc0,adc1,adc2;

    if(mFilter && GetDebug()>0){
      TString dname(mDetName);
      if(! dname.Contains(mFilter)) return res[0];
      printf("%s mMinTB=%d mMaxTB=%d : ",
	     mDetName,mMinTB+8,mMaxTB-20);
      for(int i=1; i<n-1; i++){
	tb1=t[i];
	if(tb1>=mMinTB+8 && tb1<=mMaxTB-20){
	  printf("%4d ",int(a[i]));
	}
      }
      printf("\n");
    }

    for(int i=1; i<n-1; i++){
	tb1=t[i];
	if(tb1>=mMinTB && tb1<=mMaxTB){	    
	    adc1=a[i];
	    if(adc1-ped<mMinAdc) continue;
	    tb0=t[i-1];
	    if(tb1-tb0>1.1) {adc0 = 0;}
	    else            {adc0 = a[i-1];}
	    tb2=t[i+1];
	    if(tb2-tb1>1.1) {adc2 = 0;}
	    else            {adc2 = a[i+1];}
	    //	    printf("i=%3d  tb0=%4.0lf  tb1=%4.0lf  tb2=%4.0lf  adc0=%5.0lf adc1=%5.0lf adc2=%5.0lf\n",
	    //             i,tb0,tb1,tb2,adc0,adc1,adc2);
            if( (adc1>adc0 || tb1==mMinTB) && adc1>=adc2){  //if falling from starting point, make it peak
		para[1+npeak*3+1] = adc1;
		para[1+npeak*3+2] = tb1;
		para[1+npeak*3+3] = mDbPulse->GSigma();
		double dt  = fabs(tb1 - mCenterTB); 
		if(trgmin<tb1 && tb1<trgmax && dt < mindt) { //find closest to center within trigger xing
		    mindt = dt;
		    trgx  = npeak;
		}		    
		if(GetDebug()>1) printf("peak npeak=%2d tb=%4.0lf adc=%5.0lf\n",npeak,tb1,adc1);
		npeak++;
	    }
        }
    }

    //For comparing with pulse fit analysis class
    if( mPulseFit==0 ){mPulseFit = new StFcsPulseAna(g); SetupDavidFitterMay2022(); }//Setup only needs to be called once as those values will not get reset when data changes.
    else{ mPulseFit->SetData(g); }
    Int_t compidx = mPulseFit->FoundPeakIndex();
    int myNpeaks = -1;
    int npeakidx = -1;
    int mypeakidx = -1;
    if( mTest==2 || mTest==8 ){
      mH1_NPeaksAkio->Fill(npeak);
      if( trgx>=0 ){ mH1_NPeaksFilteredAkio->Fill(npeak); }//peaks inside triggered crossing
      if( mTest==2 ){
	auto start=std::chrono::high_resolution_clock::now();
	if( compidx<0 ){ compidx = mPulseFit->AnalyzeForPeak(); }
	auto stop=std::chrono::high_resolution_clock::now();
	long long usec = chrono::duration_cast<chrono::microseconds>(stop-start).count();
	//printf("Fit Time = %lld usec\n",usec);
	mH1_PeakTiming->Fill(float(usec)/1000.0);
	
	myNpeaks = mPulseFit->NPeaks();
	mH1F_NPeaks[0]->Fill(myNpeaks);
	if( 0<=compidx && compidx<myNpeaks ){ mH1F_NPeaksFiltered[0]->Fill(myNpeaks); }
	npeakidx = npeak<=4?npeak:5;
	mypeakidx = myNpeaks<=4?myNpeaks:5;
	//std::cout << "|npeakidx:"<< npeakidx << "|mypeakidx:"<< mypeakidx << std::endl;
	//bool checkpeak = false; //This is to check a zero peak with a large adc value in triggered crossing
	for( int i=0; i<g->GetN(); ++i ){
	  mH2F_AdcTbAkio[npeakidx]->Fill(t[i],a[i]);
	  mH2F_AdcTbMine[mypeakidx]->Fill(t[i],a[i]);
	  //if( mypeakidx==0 ){ if( 40<t[i] && t[i]<=60 ){ if( a[i]>20 ){ checkpeak=true; } } }
	  if( mPulseFit->ValidPeakIdx() ){mH2F_AdcTbValidPeak[mypeakidx]->Fill(t[i],a[i]);}
	  else{ mH2F_AdcTbValidPeak[6]->Fill(t[i],a[i]); }
	  //std::cout << "- |i:"<<i << "|t:"<<t[i] << "|a:"<< a[i] << std::endl;
	}
      }
    }
    //Continue with rest of peak finding
    
    if(npeak>0 && npeak<mMaxPeak){
      //func = new TF1("waveform",this,&StFcsWaveformFitMaker::multiPulseShape,mMinTB,mMaxTB,2+npeak*3);//Old way
      //func = new TF1("waveform",mDbPulse,&StFcsDbPulse::multiPulseShape,mMinTB,mMaxTB,2+npeak*3);//For reference
      func = mDbPulse->createPulse(mMinTB,mMaxTB,2+npeak*3);//Needs to have name "waveform"
      func->SetLineColor(6);
      func->SetParameters(para);
      func->FixParameter(0,npeak);
      func->FixParameter(1,ped);
      for(int i=0; i<npeak; i++){
	func->SetParLimits(1+i*3+1,0.0,40000.0);       //limit peak not to go negative
	int j=1+i*3+2;
	    func->SetParLimits(j,para[j]-2.0,para[j]+2.0); //limit peak position to +- 2TB
	    func->SetParLimits(1+i*3+3,0.5,10.0);          //limit sigma to go too narrow or wide
      }	
      //TFitResultPtr result = g->Fit("waveform",Opt,"",mMinTB,mMaxTB);
      TFitResultPtr result = g->Fit(func,Opt,"",mMinTB,mMaxTB);
      if(trgx>=0){ // return pulse closest to center of triggered xing
	res[1] = func->GetParameter(trgx*3 + 2);
	res[2] = func->GetParameter(trgx*3 + 3);
	res[3] = func->GetParameter(trgx*3 + 4);
	res[4] = func->GetChisquare()/func->GetNDF();    
	res[5] = npeak;
	res[0] = res[1]*res[3]*StFcsDbPulse::sqrt2pi();
      }else{  // no peak found in triggered xing
	//res[1] = 0.0;  
	//res[2] = 0.0;
	//res[3] = 0.0;
	res[4] = func->GetChisquare()/func->GetNDF();    
	res[5] = npeak;
	//res[0] = 0.0;
      }	
    }else if(npeak>=mMaxPeak){
      res[5] = npeak;
      sum8(g, res); // too many peak, taking sum8
      res[0]/=1.21; // normalized by 1/1.21
      LOG_INFO << Form("%s Finding too many peaks npeak=%d. Skip fitting. Taking Sum8",mDetName,npeak)<<endm;
    }
    //printf("func=%d res=%f\n",func,res[0]);
    
    //For comparing to my peak finder
    if( mTest==2 ){
      if( npeakidx==mypeakidx){mH2F_SumFitvSumWin[npeakidx]->Fill(mPulseFit->SumWindow(),res[0]);}
      if( npeakidx==mypeakidx){
	Double_t peakloc = 0;
	if( 0<=compidx && compidx<myNpeaks ){ peakloc=mPulseFit->GetPeak(compidx).mPeakX; }
	mH2F_APeakvMPeak[npeakidx]->Fill(peakloc,res[2]);
      }
      mH1F_PeakStart[mypeakidx]->Fill(mPulseFit->PeakStart());
      mH1F_PeakEnd[mypeakidx]->Fill(mPulseFit->PeakEnd());
    }
    return res[0];
}

void StFcsWaveformFitMaker::drawFit(TGraphAsymmErrors* g, TF1* func){
  const int MAXPAD=4*4;
  TGraphAsymmErrors* gg = getGraph();
  if(gg==0){
    LOG_WARN<<"Found no TGraphAsymmErrors at mHitIdx="<<mHitIdx<<endm;
    return;
  }

  static int skip=0;
  //printf("skip=%d mSkip=%d mPage=%d mMaxPage=%d mPad=%d\n",skip,mSkip,mPage,mMaxPage,mPad);
  if(skip>mSkip){
    skip=0;
  }
  if(skip>0){
    skip++;
  }else{
    skip++;
    if(mPad==0) { mCanvas->Clear(); mCanvas->Divide(4,4);}
    mPad++;
    mCanvas->cd(mPad);
    gg->SetTitle(mDetName);
    gg->Draw("ALP");
    if( func!=0 ){
      func->SetLineColor(kRed);
      func->SetLineWidth(2);
      func->DrawCopy("LSAME");
    }
    if( mPulseFit!=0 ){
      int det=-1;
      int ch=-1;
      StFcsDb::getFromName(gg->GetName(),det,ch);
      char post[50];
      sprintf(post,"_D%dC%d",det,ch);
      StFcsPulseAna* ana = (StFcsPulseAna*)mPulseFit->DrawCopy("LP;A",post,gg);//Sets 'kCanDelete' so canvas will delete the copy
      ana->GetData()->SetLineColor(kBlue);
      ana->GetData()->SetMarkerStyle(kBlue);
      ana->GetData()->SetMarkerStyle(4);
      ana->GetData()->SetMarkerSize(0.5);
    }
    
    if(mPad==MAXPAD){
      char file[100];
      sprintf(file,"%s.pdf",mFilename);
      //if(mMaxPage==0)          {sprintf(file,"%s.pdf",mFilename);}
      //else if(mPage==0)        {sprintf(file,"%s.pdf(",mFilename);}
      //else if(mPage==mMaxPage) {sprintf(file,"%s.pdf",mFilename); mPad=-1; mFitDrawOn=0; mHitIdx=0;}
      //else                {sprintf(file,"%s.pdf",mFilename);}
      printf("Saving pdf with [%s] mPage=%d mPad=%d\n",file,mPage,mPad);
      mCanvas->Print(file);
      //      for(int i=0; i<MAXPAD; i++) if(gg[i]) delete gg[i];
      if(mPage==mMaxPage){ mPad=-9999; /*mPad=-1;*/ mFitDrawOn=0; mHitIdx=0; }
      else               { mPad=0; }
      mPage++;	
      if(mFitDrawOn==2) mHitIdx=0;
    }
  }
}

void StFcsWaveformFitMaker::drawCh(UInt_t detid, UInt_t ch) const
{
  for( UInt_t i=0; i<mHitIdx; ++i ){
    int det = -1;
    int id  = -1;
    TGraphAsymmErrors* ggdraw = (TGraphAsymmErrors*)mChWaveData.At(i);
    StFcsDb::getFromName(ggdraw->GetName(),det,id);
    if( det>=0 && det==static_cast<int>(detid) ){
      if( id>=0 && id==static_cast<int>(ch) ){
	ggdraw->Draw("APL");
	ggdraw->SetLineColor(kBlack);
	ggdraw->SetMarkerStyle(kBlack);
	ggdraw->SetMarkerStyle(4);
	ggdraw->SetMarkerSize(0.5);
	ggdraw->GetXaxis()->SetTitle("timebin");
	ggdraw->GetYaxis()->SetTitle("ADC");
	if( mPulseFit!=0 ){
	  int det=-1;
	  int ch=-1;
	  StFcsDb::getFromName(ggdraw->GetName(),det,ch);
	  char post[50];
	  sprintf(post,"_D%dC%d",det,ch);
	  StFcsPulseAna* ana = mPulseFit->DrawCopy("LP;P",post,ggdraw);//Sets 'kCanDelete' so an external canvas will delete this object when "Clear" is called
	  ana->GetData()->SetLineColor(kBlue);
	  ana->GetData()->SetMarkerStyle(kBlue);
	  ana->GetData()->SetMarkerStyle(4);
	  ana->GetData()->SetMarkerSize(0.5);
	  //ana->ResetPeak();
	  //ana->SetDebug(5);
	  //mPulseFit->Print();
	  //ana->Print();
	}
      }
    }
  }
}

void StFcsWaveformFitMaker::drawDualFit(UInt_t detid, UInt_t ch)
{
  for( UInt_t i=0; i<mHitIdx; ++i ){
    int det = -1;
    int id  = -1;
    TGraphAsymmErrors* ggdraw = (TGraphAsymmErrors*)mChWaveData.At(i);
    StFcsDb::getFromName(ggdraw->GetName(),det,id);
    if( det>=0 && det==static_cast<int>(detid) ){
      if( id>=0 && id==static_cast<int>(ch) ){
	char post[50];
	sprintf(post,"_D%dC%d",detid,ch);
	ggdraw->SetTitle(post);
	ggdraw->Draw("APL");
	ggdraw->SetLineColor(kBlack);
	ggdraw->SetMarkerStyle(kBlack);
	ggdraw->SetMarkerStyle(4);
	ggdraw->SetMarkerSize(0.5);
	ggdraw->GetXaxis()->SetTitle("timebin");
	ggdraw->GetYaxis()->SetTitle("ADC");
	if( mPulseFit!=0 ){
	  StFcsPulseAna* ana = mPulseFit->DrawCopy("LP;P",post,ggdraw);//Sets 'kCanDelete' so an external canvas will delete this object when "Clear" is called
	  ana->GetData()->SetLineColor(kBlue);
	  ana->GetData()->SetMarkerStyle(kBlue);
	  ana->GetData()->SetMarkerStyle(4);
	  ana->GetData()->SetMarkerSize(0.5);
	  Int_t compidx = ana->FoundPeakIndex();
	  if( compidx<0 ){ compidx = ana->AnalyzeForPeak(); }
	  int npeaks = ana->NPeaks();
	  //std::cout << "|det:"<<det << "|ch:"<<ch << std::endl;
	  //std::cout << " + |B::npeaks:"<<npeaks << "|compidx:"<<compidx << std::endl;
	  if( compidx<npeaks ){
	    int trigfitidx = compidx;//this is starting condition is needed to pick out the right index
	    //Hack to replicate NPeaksPrePost function
	    //make initial window 1 RHIC crossing
	    Double_t xmin = mCenterTB - mDbPulse->TBPerRC()/2;
	    Double_t xmax = mCenterTB + mDbPulse->TBPerRC()/2;
	    //@[August 25, 2022](David Kapukchyan)>In future use varaiables as opposed to constants??
	    int prexing = 3*mDbPulse->TBPerRC();//Pre-crossing more important than post-crossing so use wider range for pre-crossing
	    int postxing = 2*mDbPulse->TBPerRC();
	    bool foundtrigidx = false;
	    std::vector<int> validpeakidx;  //store all valid indexes for peaks around the triggered crossing
	    for( int i=0; i<ana->NPeaks(); ++i ){
	      if( (mCenterTB-prexing) < ana->GetPeak(i).mPeakX && ana->GetPeak(i).mPeakX < (mCenterTB+postxing) ){
		if( !foundtrigidx && trigfitidx==i ){ foundtrigidx = true; trigfitidx = validpeakidx.size(); }//do before adding i to vector so that when i does get added the triggered crossing index matches
		if( !validpeakidx.empty() && validpeakidx.back()!=(i-1) ){ LOG_ERROR << "StFcsWaveformFitMaker::dualFit: Found indices are not linear" << endm;  }
		validpeakidx.push_back(i);
		if( xmin > ana->GetPeak(i).mStartX ){ xmin = ana->GetPeak(i).mStartX; }
		if( xmax < ana->GetPeak(i).mEndX ){ xmax = ana->GetPeak(i).mEndX; }
	      }
	    }
	    if( !foundtrigidx ){ LOG_ERROR << "StFcsWaveformFitMaker::dualFit: Unable to find a matching triggered crossing possibly due to improper use of function" << endm; }
	    //End of Hack
	    npeaks = validpeakidx.size();
	    if( npeaks>0 && npeaks<mMaxPeak ){
	      TF1* func_PulseFit = mDbPulse->createPulse(xmin,xmax,2+npeaks*3);//only fit inside the range of valid peaks
	      ana->SetFitPars(func_PulseFit,validpeakidx.front(),validpeakidx.back());
	      if( func_PulseFit->GetParameter(0)==0 ){ LOG_ERROR << "StFcsWaveformFitMaker::drawDualFit: Unable to set function parameters, bad peak indicies" << endl; }	      
	      //ana->SetSignal(func);
	      TFitResultPtr result = ggdraw->Fit(func_PulseFit,"BNRQ");
	      //compidx no longer equal to index to triggered crossing peak
	      func_PulseFit->SetLineColor(kBlack);
	      func_PulseFit->SetLineWidth(1);
	      func_PulseFit->SetBit(kCanDelete);
	      func_PulseFit->Draw("same");
	    }
	  }
	}
	TF1* func_gaus = 0;
	float gausres[8] = {0};
	gausFit( ggdraw , gausres, func_gaus );
	if( func_gaus!=0 ){
	  func_gaus->SetLineColor(kGreen);
	  func_gaus->SetLineWidth(1);
	  func_gaus->SetBit(kCanDelete);
	  func_gaus->SetParameter(1,0.5);
	  func_gaus->Draw("same");
	}
      }//ch check
    }//detid check
  }//hit loop
}

float StFcsWaveformFitMaker::gausFitWithPed(TGraphAsymmErrors* g, float* res, TF1*& func){
  AnaPed( g, res[6], res[7] );
  LOG_DEBUG << "Pedestal ("<<mPedMax<<"-"<<mPedMin<<"+1)=" << res[6]<<" +- "<<res[7] << endm;
  return gausFit(g, res, func, res[6]);
}

void StFcsWaveformFitMaker::drawRegion(int det, int col_low, int row_low, int col_high, int row_high, int event){
  const int MAXPAD = 16;
  int NumDrawnPad[MAXPAD]={0};//To keep track of how much was drawn on each pad
  //Keep track of minimum and maximum y for each pad x should be same so leave as is
  Double_t MinY[MAXPAD] = {0};
  Double_t MaxY[MAXPAD] = {0};
  int MaxDrawPad=0;
  //int MaxCol = mDb->nColumn(det);
  //int MaxRow = mDb->nRow(det);

  if( det<2 ){MaxDrawPad=6;}//Maximum to draw on one pad for Ecal
  else if( det>1 && det<4 ){MaxDrawPad=4;}//Maximum to draw on one pad for Hcal
  else{ LOG_ERROR<< "StFcsWaveformFitMaker::drawRegion(): This function only works for det<4"<<endm; return; }
  
  if( mCanvas==0 ){ mCanvas = new TCanvas("mCanvas","FCSWaveFormFit",10,10,2000,2000); }
  //gStyle->SetOptStat(0);
  else{
    //mCanvas->Clear("nodelete");
    mCanvas->Clear();
  }
  mCanvas->Divide(4,4);

  //Need to loop to mHitIdx since that is the most hits for a given event and this function should be only used after all data is read in. This prevents reading old event that may have had more hits.
  for( unsigned int iCh=0; iCh<mHitIdx; ++iCh ){
    TGraphAsymmErrors* gTemp = (TGraphAsymmErrors*)mChWaveData.ConstructedAt(iCh);
    if( gTemp->GetN()==0 ){continue;}
    //std::cout << "DEBUG::drawRegion|Name:"<<gTemp->GetName()<<"|size:"<<gTemp->GetN() << std::endl;
    int Chdet,Chid;
    StFcsDb::getFromName(gTemp->GetName(),Chdet,Chid);
    if( det!=Chdet ){continue;}
    int col = mDb->getColumnNumber(Chdet,Chid);
    int row = mDb->getRowNumber(Chdet,Chid);
    if( !(col_low<=col && col<=col_high) || !(row_low<=row && row<=row_high) ){continue;}//Skip columns and rows not in range

    mPad = StFcsDbPulse::PadNum4x4(det,col,row);//Won't return zero unless invalid column/row
    TPad* padTemp = (TPad*)mCanvas->cd( mPad-- );//Post-incrment should return old value
    //Since mPad is a number from 1-16 need to offset by one to get 0-15 for array
    int color = 100-((static_cast<double>(NumDrawnPad[mPad])/static_cast<double>(MaxDrawPad))*(100-51));//Some suitable rainbow root colors (100=red, 51=purple)
    gTemp->SetLineColor(color);
    gTemp->SetMarkerColor(color);
    gTemp->SetMarkerStyle(NumDrawnPad[mPad]+24);//Some suitable root styles
    gTemp->SetMarkerSize(0.5);
    //std::cout << " - |Pad:"<<mPad<<"|Num:"<<NumDrawnPad[mPad] << std::endl;
    if( NumDrawnPad[mPad]==0 ){
      padTemp->cd();
      gTemp->Draw("APL");
      MinY[mPad]=padTemp->GetUymin();
      MaxY[mPad]=padTemp->GetUymax();
    }
    else{
      gTemp->Draw("PL");
      Double_t YminNew=5000; Double_t YmaxNew=-5;
      StFcsDbPulse::getYMinMax(gTemp,YminNew,YmaxNew);
      if( YminNew<MinY[mPad] ){MinY[mPad]=YminNew;}
      if( YmaxNew>MaxY[mPad] ){MaxY[mPad]=YmaxNew;}
      ((TGraphAsymmErrors*)padTemp->GetListOfPrimitives()->At(1))->GetYaxis()->SetRangeUser(MinY[mPad],MaxY[mPad]);//For this function first graph in list of primitives is the graph with axis object (First object of list is TFrame)
      padTemp->Draw();//Update axes
    }
    if( (++NumDrawnPad[mPad]) > MaxDrawPad ){LOG_WARN << "StFcsWaveformFitMaker::drawRegion(): Drawing too many on one pad"<<endm;}
  }
  std::stringstream SS_name;
  SS_name << "Det"<<det << "_Cl"<<col_low << "Rl"<<row_low << "_Ch"<<col_high << "Rh"<<row_high << "_Event"<<event <<".png";
  mCanvas->SaveAs( SS_name.str().c_str() );
}

void StFcsWaveformFitMaker::drawEvent(int det, int event){
  if( det<=1 )//Ecal
    {
      drawRegion(det, 1,1,  8,12,  event);//top left
      drawRegion(det, 9,1,  16,12, event);//top middle
      drawRegion(det, 17,1, 24,12, event);//top right

      drawRegion(det, 1,13,  8,24,  event);//middle left
      drawRegion(det, 9,13,  16,24, event);//middle middle
      drawRegion(det, 17,13, 24,24, event);//middle right

      drawRegion(det, 1,25,  8,36,  event);//bottom left
      drawRegion(det, 9,25,  16,36, event);//bottom middle
      drawRegion(det, 17,25, 24,36, event);//bottom right
    }
  if( det>1 && det<4 )//Hcal
    {
      drawRegion(det, 1,1, 8,8,  event);//top left
      drawRegion(det, 9,1, 16,8, event);//top right

      drawRegion(det, 1,9, 8,16,  event);//middle left
      drawRegion(det, 9,9, 16,16, event);//midle right

      drawRegion(det, 1,17, 8,24, event);//bottom left
      drawRegion(det, 9,17, 16,24,event);//bottom right
    }
}

void StFcsWaveformFitMaker::printArray() const{
  //int Nentries = mChWaveData.GetEntries();//Just to prevent multiple evaluation since ROOT loops through array to count elements
  for( unsigned int iCh=0; iCh<mHitIdx; ++iCh ){
    TGraphAsymmErrors* gTemp = (TGraphAsymmErrors*)mChWaveData.At(iCh);
    std::cout << "|Index:"<<iCh<<"|Name:"<<gTemp->GetName()<<"|Size:"<<gTemp->GetN() << std::endl;
  }
}

float StFcsWaveformFitMaker::PulseFit1(TGraphAsymmErrors* gae, float* res, TF1*& func, float ped)
{
  if( mPulseFit==0 ){mPulseFit = new StFcsPulseAna(gae); SetupDavidFitterMay2022();}
  else{ mPulseFit->SetData(gae); }//Resets finder
  if( fabs(res[7]) > 0.000001 ){ mPulseFit->SetBaseline(ped,res[7]); }//only change pedestal if standard deviation is greater than 0, which only happens if a pedestal is calculated
  if( GetDebug()>2 ){mPulseFit->SetDebug(1);}

  Int_t compidx = mPulseFit->FoundPeakIndex();
  if( compidx<0 ){ compidx = mPulseFit->AnalyzeForPeak(); }
  int npeaks = mPulseFit->NPeaks();

  int det0 = 0; int ch0=0;
  mDb->getFromName( gae->GetName(), det0,ch0 );

  if( mTest==4 ){
    if( compidx==npeaks ){ res[0] = 0; return res[0]; }
    func = mDbPulse->createPulse(mMinTB,mMaxTB,2+npeaks*3);
    mPulseFit->SetFitPars(func);
    TFitResultPtr result = gae->Fit(func,"BNRQ");
    for( int i=0; i<npeaks; ++i ){
      mH2_HeightvsSigma->Fill( func->GetParameter(1+i*3+3),func->GetParameter(1+i*3+1) );
    }
    res[5] = npeaks;
    res[1] = func->GetParameter(compidx*3 + 2);
    res[2] = func->GetParameter(compidx*3 + 3);
    res[3] = func->GetParameter(compidx*3 + 4);
    res[4] = func->GetChisquare()/func->GetNDF();
    res[0] = res[1]*res[3]*StFcsDbPulse::sqrt2pi();
    mH2_HeightvsSigmaTrig->Fill(res[3],res[1]);
    mH1_ChiNdf->Fill(res[4]);
    mH2_HeightvsChiNdf->Fill(res[4],res[1]);
    mH2_MeanvsChiNdf->Fill(res[4],res[2]);
    mH2_SigmavsChiNdf->Fill(res[4],res[3]);

    return res[0];
  }

  //std::cout << "|compidx:"<<compidx << "|npeaks:"<<npeaks <<std::endl;
  if( mTest==3 ){
    mH1F_NPeaks[det0]->Fill(npeaks);
    mH1F_NPeaks[6]->Fill(npeaks);
  }
  if( compidx==npeaks ){ res[0] = 0; }//no found peak case sum is 0
  else if( npeaks<=1 ){ //0 or 1 peak case with a valid peak;  do sum 8
    if( mTest==3 ){
      mH1F_NPeaksFiltered[det0]->Fill(npeaks);
      mH1F_NPeaksFiltered[6]->Fill(npeaks);
    }
    res[0] = sum8(gae,res);
    //Scale sum8 to match fitted sum (These may need to be confirmed by data year by year)
    if( det0==0 || det0==1 ){res[0]/=1.226;}
    if( det0==2 || det0==3 ){res[0]/=1.195;}
    if( det0==4 || det0==5 ){res[0]/=1.29;}
    res[1] = mPulseFit->GetPeak(compidx).mPeakY;
    res[2] = mPulseFit->GetPeak(compidx).mPeakX;
    res[3] = mDbPulse->GSigma();
    res[4] = -1;
  }
  else{ //More than 1 peak and valid peak found
    if( mTest==3 ){
      mH1F_NPeaksFiltered[det0]->Fill(npeaks);
      mH1F_NPeaksFiltered[6]->Fill(npeaks);
      //int mypeakidx = (npeaks<=5)?(npeaks-2):3;
      double xfound = mPulseFit->GetPeak(compidx).mPeakX;
      double yfound = mPulseFit->GetPeak(compidx).mPeakY;
      for( Int_t i=0; i<npeaks; ++i ){
	if( i==compidx ){ continue; }
	double x = mPulseFit->GetPeak(i).mPeakX;
	double y = mPulseFit->GetPeak(i).mPeakY;
	mH2_NPeakvsPeakXdiff->Fill(xfound-x,npeaks);
	mH2_NPeakvsPeakYratio->Fill(yfound/y,npeaks);
      }
    }
    int numoverlaps = 0;
    for( Int_t i=0; i<npeaks; ++i ){
      if( i==compidx ){ continue; }
      if( mTest==3 ){
	int comp = PeakCompare( mPulseFit->GetPeak(compidx),mPulseFit->GetPeak(i) );
	mH1_VOverlap->Fill(comp);
	mH2_VvsNOverlap->Fill(i-compidx,comp);
	if( comp!=0 ){ ++numoverlaps; }
      }
      else{ if(PeakCompare( mPulseFit->GetPeak(compidx),mPulseFit->GetPeak(i) )!=0  ){ ++numoverlaps; } }
    }
    if( numoverlaps>0 ){
      //res[0] = mPulseFit->FitSignal(mMinTb,mMaxTb);
      auto start=std::chrono::high_resolution_clock::now();//for timing studies can be commented out as long as not testing
      func = mDbPulse->createPulse(mMinTB,mMaxTB,2+npeaks*3);
      mPulseFit->SetFitPars(func);
      //mPulseFit->SetSignal(func);
      TFitResultPtr result = gae->Fit(func,"BNRQ");
      res[1] = func->GetParameter(compidx*3 + 2);
      res[2] = func->GetParameter(compidx*3 + 3);
      res[3] = func->GetParameter(compidx*3 + 4);
      res[4] = func->GetChisquare()/func->GetNDF();
      res[0] = res[1]*res[3]*StFcsDbPulse::sqrt2pi();
      if( mTest==3 ){
	auto stop=std::chrono::high_resolution_clock::now();
	long long usec = chrono::duration_cast<chrono::microseconds>(stop-start).count();
	mH1_TimeFitPulse->Fill(float(usec)/1000.0);
	mH2F_NOvsId[det0]->Fill(ch0,numoverlaps);
	mH2_NOvsNPeaks->Fill(npeaks,numoverlaps);
	//printf("Fit Time = %lld usec\n",usec);
      }
    }
    else{//Don't need to fit as other peaks don't contribute to found peak
      res[0] = sum8(gae,res);
      //Scale sum8 to match fitted sum (These may need to be confirmed by data year by year)
      if( det0==0 || det0==1 ){res[0]/=1.226;}
      if( det0==2 || det0==3 ){res[0]/=1.195;}
      if( det0==4 || det0==5 ){res[0]/=1.29;}
      res[1] = mPulseFit->GetPeak(compidx).mPeakY;
      res[2] = mPulseFit->GetPeak(compidx).mPeakX;
      res[3] = mDbPulse->GSigma();
      res[4] = -1;
    }
  }
  res[5] = npeaks;//change to number of peaks in pre2 and post1 (number of peaks fitted)
  if( mTest==3 ){
    mH1F_Res0[det0]->Fill(res[0]);
    mH1F_Res0Zoom[det0]->Fill(res[0]);
    mH1F_Res0[6]->Fill(res[0]);
    mH1F_Res0Zoom[6]->Fill(res[0]);
    float sum8res[8] = {0};
    sum8( gae ,sum8res );
    mH1F_Sum8Res0[det0]->Fill(sum8res[0]);
    mH1F_Sum8Res0Zoom[det0]->Fill(sum8res[0]);
    mH1F_Sum8Res0[6]->Fill(sum8res[0]);
    mH1F_Sum8Res0Zoom[6]->Fill(sum8res[0]);
    float savesum8 = sum8res[0];
    //Scale sum8 to match fitted sum
    //if( det0==0 || det0==1 ){savesum8/=1.226;}
    //if( det0==2 || det0==3 ){savesum8/=1.195;}
    //if( det0==4 || det0==5 ){savesum8/=1.29;}

    if( npeaks>=1 ){//No found peak skip fitting
      if( func==0 ){//No fit performed above so do a fit now
	//Here I will reuse the sum8res array
	func = mDbPulse->createPulse(mMinTB,mMaxTB,2+npeaks*3);
	mPulseFit->SetFitPars(func);
	TFitResultPtr result = gae->Fit(func,"BNRQ");
	sum8res[5] = npeaks;
	sum8res[1] = func->GetParameter(compidx*3 + 2);
	sum8res[2] = func->GetParameter(compidx*3 + 3);
	sum8res[3] = func->GetParameter(compidx*3 + 4);
	sum8res[4] = func->GetChisquare()/func->GetNDF();
	sum8res[0] = sum8res[1]*sum8res[3]*StFcsDbPulse::sqrt2pi();
	mH1F_FitRes0[det0]->Fill(sum8res[0]);
	mH1F_FitRes0Zoom[det0]->Fill(sum8res[0]);
	mH1F_FitRes0[6]->Fill(sum8res[0]);
	mH1F_FitRes0Zoom[6]->Fill(sum8res[0]);
	
	mH2F_Sum8vFit[det0]->Fill(sum8res[0],savesum8);
	mH2F_Sum8vFit[6]->Fill(sum8res[0],savesum8);
      }
      else{//Fit was done above so just save that result
	mH1F_FitRes0[det0]->Fill(res[0]);
	mH1F_FitRes0Zoom[det0]->Fill(res[0]);
	mH1F_FitRes0[6]->Fill(res[0]);
	mH1F_FitRes0Zoom[6]->Fill(res[0]);
	mH2F_Sum8vFit[det0]->Fill(res[0],savesum8);
	mH2F_Sum8vFit[6]->Fill(res[0],savesum8);
      }
    }
  }

  return res[0];
}

float StFcsWaveformFitMaker::PulseFit2(TGraphAsymmErrors* gae, float* res, TF1*& func, float ped)
{
  if( mPulseFit==0 ){mPulseFit = new StFcsPulseAna(gae); SetupDavidFitterMay2022();}
  else{ mPulseFit->SetData(gae); }//Resets finder
  if( fabs(res[7]) > 0.000001 ){ mPulseFit->SetBaseline(ped,res[7]); }//only change pedestal if standard deviation is greater than 0, which only happens if a pedestal is calculated
  if( GetDebug()>2 ){mPulseFit->SetDebug(1);}

  Int_t compidx = mPulseFit->FoundPeakIndex();
  if( compidx<0 ){ compidx = mPulseFit->AnalyzeForPeak(); }
  int npeaks = mPulseFit->NPeaks();

  int det0 = 0; int ch0=0;
  mDb->getFromName( gae->GetName(), det0,ch0 );

  if( mTest==6 || mTest==8 ){
    mH1F_NPeaks[det0]->Fill(npeaks);
    mH1F_NPeaks[6]->Fill(npeaks);
  }
  if( compidx==npeaks ){ res[0] = 0; }//no found peak case sum is 0
  else if( (mTest==8?(npeaks<1):(npeaks<=1)) ){ //0 or 1 peak case with a valid peak;  do sum 8; if test==8 then do for all peaks
  //else if( npeaks<=1 ){
    res[0] = sum8(gae,res);
    //Scale sum8 to match fitted sum (These may need to be confirmed by data year by year)
    if( det0==0 || det0==1 ){res[0]/=1.226;}
    if( det0==2 || det0==3 ){res[0]/=1.195;}
    if( det0==4 || det0==5 ){res[0]/=1.29;}
    res[1] = mPulseFit->GetPeak(compidx).mPeakY;
    res[2] = mPulseFit->GetPeak(compidx).mPeakX;
    res[3] = mDbPulse->GSigma();
    res[4] = -1;
  }
  else{ //More than 1 peak and valid peak found
    Double_t xmin=0;
    Double_t xmax=1;
    int trigfitidx = compidx;//this is starting condition is needed to pick out the right index
    std::vector<int> validindex = NPeaksPrePost(trigfitidx,xmin,xmax);//Check for peaks near triggered crossing (counts triggered crosing peak)
    npeaks = validindex.size();
    if( mTest==6 || mTest==7 || mTest==8 ){
      mH1F_NPeaksFiltered[det0]->Fill(npeaks);
      mH1F_NPeaksFiltered[6]->Fill(npeaks);
    }
    if( ((mTest==8)?(npeaks>=1 && npeaks<mMaxPeak):(npeaks>1)) ){//If equal to one then still don't need to fit, unless mTest==8 then fit 1 peak cases
      auto start=std::chrono::high_resolution_clock::now();//for timing studies can be commented out as long as not testing
      func = mDbPulse->createPulse(xmin,xmax,2+npeaks*3);//only fit inside the range of valid peaks
      mPulseFit->SetFitPars( func, validindex.front(), validindex.back() );
      if( func->GetParameter(0)==0 ){ LOG_ERROR << "StFcsWaveformFitMaker::PulseFit2: Unable to set function parameters, bad peak indicies" << endl; }
      //mPulseFit->SetSignal(func);
      TFitResultPtr result = gae->Fit(func,"BNRQ");
      //compidx no longer equal to index to triggered crossing peak
      res[1] = func->GetParameter(trigfitidx*3 + 2);
      res[2] = func->GetParameter(trigfitidx*3 + 3);
      res[3] = func->GetParameter(trigfitidx*3 + 4);
      res[4] = func->GetChisquare()/func->GetNDF();
      res[0] = res[1]*res[3]*StFcsDbPulse::sqrt2pi();
      if( mTest==6 || mTest==8 ){
	auto stop=std::chrono::high_resolution_clock::now();
	long long usec = chrono::duration_cast<chrono::microseconds>(stop-start).count();
	mH1_TimeFitPulse->Fill(float(usec)/1000.0);
	//printf("Fit Time = %lld usec\n",usec);
      }
    }
    else{//Don't need to fit as other peaks don't contribute to found peak
      res[0] = sum8(gae,res);
      //Scale sum8 to match fitted sum (These may need to be confirmed by data year by year)
      if( det0==0 || det0==1 ){res[0]/=1.226;}
      if( det0==2 || det0==3 ){res[0]/=1.195;}
      if( det0==4 || det0==5 ){res[0]/=1.29;}
      res[1] = mPulseFit->GetPeak(compidx).mPeakY;
      res[2] = mPulseFit->GetPeak(compidx).mPeakX;
      res[3] = mDbPulse->GSigma();
      res[4] = -1;
    }
  }
  res[5] = npeaks;//number of peaks in pre2 and post1 (number of fitted peaks)
  
  if( mTest==6 ){
    mH1F_Res0[det0]->Fill(res[0]);
    mH1F_Res0Zoom[det0]->Fill(res[0]);
    mH1F_Res0[6]->Fill(res[0]);
    mH1F_Res0Zoom[6]->Fill(res[0]);
    float sum8res[8] = {0};
    sum8( gae ,sum8res );
    mH1F_Sum8Res0[det0]->Fill(sum8res[0]);
    mH1F_Sum8Res0Zoom[det0]->Fill(sum8res[0]);
    mH1F_Sum8Res0[6]->Fill(sum8res[0]);
    mH1F_Sum8Res0Zoom[6]->Fill(sum8res[0]);
    float savesum8 = sum8res[0];
    //if( det0==0 || det0==1 ){savesum8/=1.226;}
    //if( det0==2 || det0==3 ){savesum8/=1.195;}
    //if( det0==4 || det0==5 ){savesum8/=1.29;}
    
    if( mPulseFit->NPeaks()>=1 ){//No found peak skip fitting
      TF1* testfunc = mDbPulse->createPulse(mMinTB,mMaxTB,2+(mPulseFit->NPeaks())*3);//Want to compare when doing fit to all peaks
      //Here I will reuse the sum8res array
      mPulseFit->SetFitPars(testfunc);
      TFitResultPtr result = gae->Fit(testfunc,"BNRQ");
      sum8res[5] = mPulseFit->NPeaks();
      sum8res[1] = testfunc->GetParameter(compidx*3 + 2);
      sum8res[2] = testfunc->GetParameter(compidx*3 + 3);
      sum8res[3] = testfunc->GetParameter(compidx*3 + 4);
      sum8res[4] = testfunc->GetChisquare()/testfunc->GetNDF();
      sum8res[0] = sum8res[1]*sum8res[3]*StFcsDbPulse::sqrt2pi();
      mH1F_FitRes0[det0]->Fill(sum8res[0]);
      mH1F_FitRes0Zoom[det0]->Fill(sum8res[0]);
      mH1F_FitRes0[6]->Fill(sum8res[0]);
      mH1F_FitRes0Zoom[6]->Fill(sum8res[0]);

      mH2F_Sum8vFit[det0]->Fill(sum8res[0],savesum8);
      mH2F_Sum8vFit[6]->Fill(sum8res[0],savesum8);
    }
  }
  if( mTest==8 ){
    mH1F_Res0[det0]->Fill(res[0]);
    mH1F_Res0Zoom[det0]->Fill(res[0]);
    mH1F_Res0[6]->Fill(res[0]);
    mH1F_Res0Zoom[6]->Fill(res[0]);
    TF1* testfunc = 0;
    float gausres[8] = {0};
    gausFit( gae , gausres, testfunc );
    mH1F_FitRes0[det0]->Fill(gausres[0]);
    mH1F_FitRes0Zoom[det0]->Fill(gausres[0]);
    mH1F_FitRes0[6]->Fill(gausres[0]);
    mH1F_FitRes0Zoom[6]->Fill(gausres[0]);
    
    mH2F_Sum8vFit[det0]->Fill(gausres[0],res[0]);
    mH2F_Sum8vFit[6]->Fill(gausres[0],res[0]);
  }
  
  return res[0];
}

float StFcsWaveformFitMaker::PulseFitAll(TGraphAsymmErrors* gae, float* res, TF1*& func, float ped)
{
  if( mPulseFit==0 ){mPulseFit = new StFcsPulseAna(gae); SetupDavidFitterMay2022();}
  else{ mPulseFit->SetData(gae); }//Resets finder
  if( fabs(res[7]) > 0.000001 ){ mPulseFit->SetBaseline(ped,res[7]); }//only change pedestal if standard deviation is greater than 0, which only happens if a pedestal is calculated
  if( GetDebug()>2 ){mPulseFit->SetDebug(1);}

  Int_t compidx = mPulseFit->FoundPeakIndex();
  if( compidx<0 ){ compidx = mPulseFit->AnalyzeForPeak(); }
  int npeaks = mPulseFit->NPeaks();

  int det0 = 0; int ch0=0;
  mDb->getFromName( gae->GetName(), det0,ch0 );

  //std::cout << "det0:"<<det0 << "|ch0:"<<ch0 << "|npeaks:"<<npeaks << std::endl;
  if( compidx==npeaks || npeaks==0 ){
    res[0] = sum8(gae,res);
    //Scale sum8 to match fitted sum (These may need to be confirmed by data year by year)
    if( det0==0 || det0==1 ){res[0]/=1.226;}
    if( det0==2 || det0==3 ){res[0]/=1.195;}
    if( det0==4 || det0==5 ){res[0]/=1.29;}
    res[1] = mPulseFit->Baseline()+mPulseFit->BaselineSigma()*mPulseFit->BaselineSigmaScale();
    res[2] = mCenterTB;
    res[3] = mDbPulse->GSigma();
    res[4] = -1;
    res[5] = npeaks;
    return res[0];
  }
  Double_t xmin=mPulseFit->GetPeak(0).mStartX;
  Double_t xmax=mPulseFit->GetPeak(npeaks-1).mEndX;

  func = mDbPulse->createPulse(xmin,xmax,2+npeaks*3);//only fit inside the range of valid peaks
  mPulseFit->SetFitPars( func );
  if( func->GetParameter(0)==0 ){ LOG_ERROR << "StFcsWaveformFitMaker::PulseFitAll: Unable to set function parameters, bad peak indicies" << endl; }
  TFitResultPtr result = gae->Fit(func,"BNRQ");
  //compidx no longer equal to index to triggered crossing peak
  res[1] = func->GetParameter(compidx*3 + 2);
  res[2] = func->GetParameter(compidx*3 + 3);
  res[3] = func->GetParameter(compidx*3 + 4);
  res[4] = func->GetChisquare()/func->GetNDF();
  res[0] = res[1]*res[3]*StFcsDbPulse::sqrt2pi();

  res[5] = npeaks;//number of fitted peaks
  
  return res[0];
}

float StFcsWaveformFitMaker::PulseFit2WithPed(TGraphAsymmErrors* gae, float* res, TF1*& func)
{
  AnaPed(gae,res[6],res[7]);
  return PulseFit2(gae,res,func,res[6]);
}

float StFcsWaveformFitMaker::PulseFitAllWithPed(TGraphAsymmErrors* gae, float* res, TF1*& func)
{
  AnaPed(gae,res[6],res[7]);
  return PulseFitAll(gae,res,func,res[6]);
}

float StFcsWaveformFitMaker::PedFitPulseFit(TGraphAsymmErrors* gae, float* res, TF1*& func)
{
  if( mPulseFit==0 ){mPulseFit = new StFcsPulseAna(gae); SetupDavidFitterMay2022();}
  else{mPulseFit->SetData(gae);}//Resets finder
  if( GetDebug()>2){mPulseFit->SetDebug(1);}

  mPulseFit->AnalyzeForPedestal();
  res[6] = mPulseFit->Baseline();
  res[7] = mPulseFit->BaselineSigma();

  return PulseFitAll(gae,res,func,res[6]);
}

int StFcsWaveformFitMaker::GenericPadPos(int value, int Nvals, int PadNums )
{
  if( value<=0 ){return ceil( static_cast<double>(value+(Nvals*PadNums))/static_cast<double>(Nvals) );}
  else{ return GenericPadPos(value-(Nvals*PadNums), Nvals, PadNums); }
}

int StFcsWaveformFitMaker::PadNum4x4(int det, int col, int row)
{
  int ncol = 0;
  int nrow = 0;
  if( det<=1 ){
    ncol = 2;
    nrow = 3;
  }
  else if( 1<det && det<=3 ){
    ncol = 2;
    nrow = 2;
  }
  else{ LOG_ERROR << "StFcsWaveformFitMaker::PadNum4x4: This only works for Ecal and Hcal" << endm; return 0;}
  int padcol = GenericPadPos(col,ncol,4);
  int padrow = GenericPadPos(row,nrow,4);
  return 4*(padrow-1)+padcol;
}

int StFcsWaveformFitMaker::PeakCompare(const PeakWindow& pwin1, const PeakWindow& pwin2 )
{
  //return true;
  double xdiff = fabs(pwin1.mPeakX-pwin2.mPeakX);
  double yratio = pwin1.mPeakY/pwin2.mPeakY;
  unsigned int result = 0;//Bit vector to store results of checks
  // + First bit xdiff cut
  // + Second bit yratio cut
  //@[June 22, 2022](David Kapukchyan)>After looking at some plots from Test==3 an xdiff>10 seems like a good cutoff for no overlap between peaks and a y-ratio>2.0
  //check peak-y difference if greater than some value no overlap
  //check sum8 of both and if both small no overlap
  if( xdiff<10.0 ){ result |= 1<<0; }
  if( yratio<2.0 ){ result |= 1<<1; }
  return result;
}

std::vector<int> StFcsWaveformFitMaker::NPeaksPrePost(int& trigidx,Double_t& xmin, Double_t &xmax) const
{
  //make initial window 1 RHIC crossing
  xmin = mCenterTB - mDbPulse->TBPerRC()/2;
  xmax = mCenterTB + mDbPulse->TBPerRC()/2;
  //@[August 25, 2022](David Kapukchyan)>In future use varaiables as opposed to constants??
  int prexing = 3*mDbPulse->TBPerRC();//Pre-crossing more important than post-crossing so use wider range for pre-crossing
  int postxing = 2*mDbPulse->TBPerRC();
  bool foundtrigidx = false;
  std::vector<int> valididx;  //store all valid indexes for peaks around the triggered crossing
  //std::cout << "|prexing:" << mCenterTB-prexing << "|postxing:"<<mCenterTB+postxing << std::endl;
  for( int i=0; i<mPulseFit->NPeaks(); ++i ){
    //std::cout << "-|i:" << i << "|peakx:"<<mPulseFit->GetPeak(i).mPeakX << std::endl;
    if( (mCenterTB-prexing) < mPulseFit->GetPeak(i).mPeakX && mPulseFit->GetPeak(i).mPeakX < (mCenterTB+postxing) ){
      //if pre crossing peak hits zero then don't need to fit pre-crossing (this can be done by checking if pre-endx==trigB-startx??
      if( !foundtrigidx && trigidx==i ){ foundtrigidx = true; trigidx = valididx.size(); }//do before adding i to vector so that when i does get added the triggered crossing index matches
      if( !valididx.empty() && valididx.back()!=(i-1) ){ LOG_ERROR << "StFcsWaveformFitMaker::NPeaksPrePost: Found indices are not linear" << endm;  }
      valididx.push_back(i);
      if( xmin > mPulseFit->GetPeak(i).mStartX ){ xmin = mPulseFit->GetPeak(i).mStartX; }
      if( xmax < mPulseFit->GetPeak(i).mEndX ){ xmax = mPulseFit->GetPeak(i).mEndX; }
    }
  }
  if( !foundtrigidx ){ LOG_ERROR << "StFcsWaveformFitMaker::NPeaksPrePost: Unable to find a matching triggered crossing possibly due to improper use of function" << endm; }
  return valididx;
}

StFcsPulseAna* StFcsWaveformFitMaker::InitFitter(Double_t ped)
{
  if( mPulseFit==0 ){ mPulseFit = new StFcsPulseAna(); }
  //Need to set baseline to zero, or greater than zero, to prevent baseline finding.
  //This is good for zero-suppressed data but should be left unset otherwise.  Also can be used to adjust adc threshold
  //for acceptance by changing the 0.75 to some other number (default ADC acceptance = 4.0*0.75)
  mPulseFit->SetBaseline(ped,0.39);
  mPulseFit->SetBaselineSigmaScale(5);
  mPulseFit->SetRange(-4,0,2000,5000);
  mPulseFit->SetSearchWindow(centerTB(),4);//Check +- 4tb around triggered crossing; some suitable starting search parameter may need to be adjusted based on data
  mPulseFit->SetContinuity(1.0);
  return mPulseFit;
}

void StFcsWaveformFitMaker::SetupDavidFitterMay2022(Double_t ped)
{
  if( mPulseFit==0 ){return;}
  InitFitter(ped);
  //mPulseFit->SetFilter(1,1);//Mean filter with "radius" 1
  //mPulseFit->SetFilter(1,2);//Mean filter with "radius" 2
  //mPulseFit->SetFilter(2,1,0.5);//Gaus filter with "radius" 1 and sigma 0.5.
  //mPulseFit->SetFilter(2,3,1);//Gaus filter with "radius" 2 and sigma 1.
  //Parameters for peak tunneling method,
  mPulseFit->SetTunnelScale(1.0);
  mPulseFit->SetTunnelSigma(1.5);
  mPulseFit->SetTunnelThreshold(-0.001);//Negative value <=-1 means do merging after peak finding, positive value <=1 means do merging wiht peak finding
  //Below are some other thresholds for testing
  //mPulseFit->SetTunnelThreshold(-0.9);//Negative value >=-1 means do merging after peak finding
  //mPulseFit->SetTunnelThreshold(0.1);//Negative value >=-1 means do merging after peak finding
  //mPulseFit->SetTunnelThreshold(-2.0);//Negative value >=-1 means do merging after peak finding
}

