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
#include "StEventTypes.h"
#include "StEvent/StFcsHit.h"
#include "StFcsDbMaker/StFcsDb.h"

#include <cmath>
#include <chrono>
#include "TMath.h"
#include "TF1.h"
#include "TGraph.h"
#include "TGraphAsymmErrors.h"
#include "TGraphAsymmErrorsWithReset.h"
#include "TCanvas.h"
#include "TColor.h"
#include "TStyle.h"
#include "TROOT.h"

namespace {
  static const double sqrtpi    = sqrt(3.141592654);
  static const double sqrt2pi   = sqrt(2.0 * 3.141592654);
  static const double TBPerRC   = 8;
  static const double nsecPerTB = 107.0/TBPerRC;
  
  //pulse shape
  double GSigma=24.5/nsecPerTB;
  double A1=0;    
  double A2=0;    
  double Xoff1=0; 
  double Xoff2=0; 
  double Tau1=1;  
  double Tau2=1;  
  double P1=1;  
  double P2=1;    
  
  //Data from Gerard 2020 summer
  static const double GSigma_1    = 24.5/nsecPerTB;
  static const double A1_1        = 1.0/0.154/GSigma_1;
  static const double A2_1        = 0.2/0.154/GSigma_1;
  static const double Xoff1_1     = (70 - 129)/nsecPerTB;
  static const double Xoff2_1     = (220 - 129)/nsecPerTB;
  static const double Tau1_1      = 200.0/nsecPerTB;
  static const double Tau2_1      = 40.0/nsecPerTB;
  static const double P1_1        = 1.0;
  static const double P2_1        = 1.0;
  
  //Data from WAH with real detector/LED system 2021 Jan
  static const double GSigma_2    = 2.347;
  static const double A1_2        = 2543.0/854.0/GSigma_2;
  static const double A2_2        = 0.0;
  static const double Xoff1_2     = 211.3-215.7;
  static const double Xoff2_2     = 0.0;
  static const double Tau1_2      = 4.375;
  static const double Tau2_2      = 0.0;
  static const double P1_2        = 1.0;
  static const double P2_2        = 0.0;  
}

StFcsWaveformFitMaker::StFcsWaveformFitMaker(const char* name) : StMaker(name) {
    mChWaveData.SetClass("TGraphAsymmErrors"); //Initialize with only one graph at first
    mPulseFit = new StFcsPulseFit( (TGraphAsymmErrors*)mChWaveData.ConstructedAt(0) );
}

StFcsWaveformFitMaker::~StFcsWaveformFitMaker() { 
    mChWaveData.Delete();//Completely clear all graphs (Put in Finish??)
    delete mPulseFit;
}

void StFcsWaveformFitMaker::Clear(Option_t* option) {
  if(mFitDrawOn==1) mHitIdx=0;
  StMaker::Clear(option);
}

int StFcsWaveformFitMaker::InitRun(int runNumber) {
    LOG_DEBUG << "StFcsWaveformFitMaker initializing run" << endm;
    mDb = static_cast<StFcsDb*>(GetDataSet("fcsDb"));
    if (!mDb) {
        LOG_ERROR << "StFcsWaveformFitMaker initializing failed due to no StFcsDb" << endm;
        return kStErr;
    }
    
    if(mFilename){
      mCanvas=new TCanvas("FCSWaveFormFit","FCSWaveFormFit",10,10,2000,2000);
      gStyle->SetOptStat(0);
    }
    if(mMeasureTime){
      mTime=new TH1F("FitTime","FitTime; msec",200,0,6);
    }

    if(mTail==1){
      GSigma = GSigma_1;
      A1     = A1_1;        
      A2     = A2_1;        
      Xoff1  = Xoff1_1;  
      Xoff2  = Xoff2_1;  
      Tau1   = Tau1_1;    
      Tau2   = Tau2_1;   
      P1     = P1_1;      
      P2     = P2_1;        
    }else if(mTail==2){
      GSigma = GSigma_2;
      A1     = A1_2;        
      A2     = A2_2;        
      Xoff1  = Xoff1_2;  
      Xoff2  = Xoff2_2;  
      Tau1   = Tau1_2;    
      Tau2   = Tau2_2;   
      P1     = P1_2;      
      P2     = P2_2;              
    }

    return StMaker::InitRun(runNumber);
}

int StFcsWaveformFitMaker::Finish(){
  if(mFilename && mPad>=0){
    char file[200];
    sprintf(file,"%s.pdf)",mFilename);
    mCanvas->Print(file,"pdf");
  }
  if(mMeasureTime){
    printf("WFFit Time Mean=%f  RMS=%f\n",mTime->GetMean(),mTime->GetRMS());
    TCanvas *c= new TCanvas("FCSWFFTime","FCSWFFTime",10,10,800,800);
    gStyle->SetOptStat(111110);
    gStyle->SetLabelSize(0.02,"xy");
    c->cd(1)->SetLogy();
    mTime->Draw();
    c->SaveAs(mMeasureTime);
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
    
    if(mEnergySelect==0) return kStOK;  // don't touch energy, directly from MC

    //Loop over all hits and run waveform analysis of the choice
    float res[8];
    TF1* func=0;
    for(int det=0; det<kFcsNDet; det++) {
	StSPtrVecFcsHit& hits = mFcsCollection->hits(det);
	int nhit=hits.size();
	for(int i=0; i<nhit; i++){ //loop over all hits  	    
	  
	  auto start=std::chrono::high_resolution_clock::now();

	  //if we are geting pedestal from data
	  float ped=0.0;
	  if(mPedMin>=0){
	    StFcsHit* hit = hits[i];
	    int p=0;
	    int n = hit->nTimeBin();
	    for(int i=0; i<n; i++){
	      int tb=hit->timebin(i);
	      if(tb>=mPedMin) p+=hit->adc(i);
	      if(tb>=mPedMax) break;
	    }       
	    ped = float(p)/(mPedMax-mPedMin+1.0);
	    mDb->setPedestal(hit->ehp(), hit->ns(), hit->dep(), hit->channel(), ped);
	    if(GetDebug()>1){
	      char name[100];
	      mDb->getName(hit->ehp(), hit->ns(), hit->dep(), hit->channel(),name);
	      printf("%s Pedestal=%d/(%d-%d+1)=%8.2f\n",name,p,mPedMax,mPedMin,ped);
	    }
	  }
	  
	  //run waveform analysis of the choice and store as AdcSum	  
	  memset(res,0,sizeof(res));
	  float integral = analyzeWaveform(mEnergySelect,hits[i],res,func,ped);
	  hits[i]->setAdcSum(integral);	    
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

void StFcsWaveformFitMaker::setTGraphAsymmErrors(TGraphAsymmErrors* gae, const int &i, const double &adc){
    double HighY = 0;
    if(adc<mAdcSaturation) { HighY=mError; }
    else                   { HighY=mErrorSaturated; }
    gae->SetPointError(i,0,0,mError,HighY);
}

TGraphAsymmErrors* StFcsWaveformFitMaker::makeTGraphAsymmErrors(int n, double* tb, double* adc){
    TGraphAsymmErrors* gae = resetGraph();
    for(int i=0; i<n; ++i){
        gae->SetPoint(i,tb[i],adc[i]);
        setTGraphAsymmErrors(gae, i, adc[i]);
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
      setTGraphAsymmErrors(gae,i-1,adc);
  }
  return gae;
}

TGraphAsymmErrors* StFcsWaveformFitMaker::makeTGraphAsymmErrors(StFcsHit* hit){  
    int N = mMaxTB - mMinTB +1;
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
	    setTGraphAsymmErrors(gae,j,adc);
	    j++;
	}
	if(tb>=mMaxTB) break;
    }
    //printf("GetN()=%d gae=%d\n",gae->GetN(),gae);
    return gae;
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
    case 12: integral = PulseFit(g,res,func); break;
    case 21: integral = PedFit(g, res, func); break;
    case 31: integral = LedFit(g, res, func); break;
    default: 
      LOG_WARN << "Unknown fit/sum method select=" << select << endm;
    }
    //if(func && (mFitDrawOn || mFilter ) && mFilename && mPage<=mMaxPage) drawFit(g,func);
    int flag=0;
    if(mFilter){
      TString dname(mDetName);
      if(integral>50 && dname.Contains(mFilter)) flag=1;
    }
    if(mFitDrawOn && flag && mFilename && mPage<=mMaxPage) {
      printf("det=%s func=%d mFitDrawOn=%d mFilter=%s mFilename=%s mPage=%d mMaxPage=%d mHitIdx=%d integral=%f\n",
	     mDetName,func,mFitDrawOn,mFilter,mFilename,mPage,mMaxPage,mHitIdx,integral);	
      drawFit(g,func);
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

//this one is just shower shape function                                                                  
double StFcsWaveformFitMaker::pulseShape(double* x, double* p) {
    double ret =  p[0]*exp(-0.5*pow((x[0]-p[1])/p[2],2));
    if(mTail>0){
      double x1 = x[0] - p[1] - Xoff1;
      if(x1>0){
        double a0 = p[0] * p[2];
        ret += a0*A1/Tau1/Tau1*pow(x1,P1)*exp(-x1/Tau1);
	if(A2>0){
	  double x2 = x[0] - p[1] - Xoff2;
	  if(x2>0){
	    ret += a0*A2/Tau2/Tau2*pow(x2,P2)*exp(-x2/Tau2);
	  }
	}
      }
    }
    return ret;
}

double StFcsWaveformFitMaker::multiPulseShape(double* x, double* p) {
    int npulse = p[0];
    double ret = p[1];
    for(int i=0; i<npulse; i++){
	ret += pulseShape(x, &p[2+i*3]);
    }    
    return ret;
}

float StFcsWaveformFitMaker::gausFit(TGraphAsymmErrors* g, float* res, TF1*& func, float ped){
    char Opt[10]="Q  ";
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
            if(adc1>adc0 && adc1>=adc2){		
		para[1+npeak*3+1] = adc1;
		para[1+npeak*3+2] = tb1;
		para[1+npeak*3+3] = GSigma;
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

    if(npeak>0 && npeak<mMaxPeak){
	func = new TF1("waveform",this,&StFcsWaveformFitMaker::multiPulseShape,mMinTB,mMaxTB,2+npeak*3);
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
	TFitResultPtr result = g->Fit("waveform",Opt,"",mMinTB,mMaxTB);
	if(trgx>=0){ // return pulse closest to center of triggered xing
	    res[1] = func->GetParameter(trgx*3 + 2);
	    res[2] = func->GetParameter(trgx*3 + 3);
	    res[3] = func->GetParameter(trgx*3 + 4);
	    res[4] = func->GetChisquare()/func->GetNDF();    
	    res[5] = npeak;
	    res[0] = res[1]*res[3]*sqrt2pi;
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
        printf("%s Finding too many peaks npeak=%d. Skip fitting\n",mDetName,npeak);	
    }
    //printf("func=%d res=%f\n",func,res[0]);
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
    if(mPad==MAXPAD){
      char file[100];
      if(mMaxPage==0)          {sprintf(file,"%s.pdf",mFilename);}
      else if(mPage==0)        {sprintf(file,"%s.pdf(",mFilename);}
      else if(mPage==mMaxPage) {sprintf(file,"%s.pdf)",mFilename); mPad=-1; mFitDrawOn=0; mHitIdx=0;}
      else                     {sprintf(file,"%s.pdf",mFilename);}
      printf("Saving pdf with [%s] mPage=%d mPad=%d\n",file,mPage,mPad);
      mCanvas->Print(file,"pdf");	  
      //      for(int i=0; i<MAXPAD; i++) if(gg[i]) delete gg[i];
      if(mPage==mMaxPage){ mPad=-9999; }
      else               { mPad=0; }
      mPage++;	
      if(mFitDrawOn==2) mHitIdx=0;
    }
  }
}

float StFcsWaveformFitMaker::gausFitWithPed(TGraphAsymmErrors* g, float* res, TF1*& func){
  int n = g->GetN();
  double *t = g->GetX();
  double *a = g->GetY();    
  int p=0;
  for(int i=0; i<n; i++){
    int tb1=t[i];
    if(tb1>=mPedMin && tb1<=mPedMax){    
      p+=a[i];
    }
  }
  float ped = float(p)/(mPedMax-mPedMin+1.0);
  printf("Pedestal=%6.2f/(%6.2f-%6.2f+1)=%6.2f\n",float(p),float(mPedMax),float(mPedMin),ped);
  return gausFit(g, res, func, ped);
}

void StFcsWaveformFitMaker::drawRegion(int det, int col_low, int row_low, int col_high, int row_high, int event){
  const int MAXPAD = 16;
  int NumDrawnPad[MAXPAD]={0};//To keep track of how much was drawn on each pad
  int MaxDrawPad=0;
  //int MaxCol = mDb->nColumn(det);
  //int MaxRow = mDb->nRow(det);

  if( det<2 ){MaxDrawPad=6;}//Maximum to draw on one pad for Ecal
  else if( det>1 && det<4 ){MaxDrawPad=4;}//Maximum to draw on one pad for Hcal
  else{ std::cout<< "ERROR:This function only works for det<4"<<std::endl; return; }
  
  if( mCanvas==0 ){ mCanvas = new TCanvas("mCanvas","FCSWaveFormFit",10,10,2000,2000); }
  //gStyle->SetOptStat(0);
  else{
    //mCanvas->Clear("nodelete");
    mCanvas->Clear();
  }
  mCanvas->Divide(4,4);

  //int Nentries = mChWaveData.GetEntries();//Just to prevent multiple evaluation since ROOT loops through array to count elements (not needed Feb 05, 2021)
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

    mPad = PadNum4x4(det,col,row);//Won't return zero unless invalid column/row
    TPad* padTemp = (TPad*)mCanvas->cd( mPad );
    
    //Since mPad is a number from 1-16 need to offset by one to get 0-15 for array
    int color = 100-((static_cast<double>(NumDrawnPad[mPad-1])/static_cast<double>(MaxDrawPad))*(100-51));//Some suitable rainbow root colors (100=red, 51=purple)
    gTemp->SetLineColor(color);
    gTemp->SetMarkerColor(color);
    gTemp->SetMarkerStyle(NumDrawnPad[mPad-1]+24);//Some suitable root styles
    gTemp->SetMarkerSize(0.5);

    //std::cout << "DEBUG::drawRegion|Pad:"<<mPad<<"|Num:"<<NumDrawnPad[mPad-1] << std::endl;
    if( NumDrawnPad[mPad-1]==0 ){gTemp->Draw("APL");}
    else{ gTemp->Draw("PLsame"); }
    if( (++NumDrawnPad[mPad-1]) > MaxDrawPad ){std::cout << "WARNING:Drawing too many on one pad"<<std::endl;}
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

float StFcsWaveformFitMaker::PulseFit(TGraphAsymmErrors* gae, float* res, TF1*& func){
  //StFcsPulseFitter Fitter(gae);
  mPulseFit->SetSignal(gae);//Resets finder
  mPulseFit->SetSearchWindow(mCenterTB,5);//Some suitable starting search parameter may need to be adjusted based on data
  mPulseFit->SetZS();//This will set baseline to zero with some "good" guess for the pedestal sigma
  //mPulseFit->SetBaseline(0,0.75);//Need to set baseline to zero, or greater than zero, to prevent baseline finding.  
     //This is good for zero-suppressed data but should be left unset otherwise.  Also can be used to adjust adc threshold 
     //for acceptance by changing the 0.75 to some other number (default ADC acceptance = 4.0*0.75)
  mPulseFit->SetSearchWindow(mCenterTB,3);//Check +- 3tb around triggered crossing
  //Many more options to set but excluded for now to see how default values perform
  res[0]=mPulseFit->SumAdcFit();            //this is full integral (pedestal subtracted)
  //int xlow = mPulseFit->SignalStart();
  //int xhigh = mPulseFit->SignalEnd();
  func = mPulseFit->SignalFit();     //Fitted function
  //mPulseFit->GetXYMax(xlow,xhigh); //Get x and y max from graph not fit
  res[1]=func->GetParameter(0);  //this is peak height from signal fit
  //res[1] = mPulseFit->MaxAdc();
  res[2]=func->GetParameter(1);  //this is peak position from signal fit [timebin]
  //res[2] = mPulseFit->MaxTb();
  res[3]=func->GetParameter(2);  //this is width from signal fit
  res[4]=func->GetChisquare()/func->GetNDF();   //chi2 from signal fit  
 return res[0];
}

float StFcsWaveformFitMaker::PedFit(TGraphAsymmErrors* gae, float* res, TF1*& func){
  //StFcsPulseFit Fitter(gae);
  mPulseFit->SetSignal(gae);
  mPulseFit->AnalyzeForPedestal();//Must be called before anything else otherwise crash
  func = mPulseFit->BaselineFit();
  res[0] = mPulseFit->Baseline();
  res[1] = mPulseFit->BaselineSigma();
  res[2] = func->GetChisquare()/func->GetNDF();   //chi2 from signal fit
  return res[0];
}

float StFcsWaveformFitMaker::LedFit(TGraphAsymmErrors* gae, float* res, TF1*& func)
{
  //StFcsPulseFit Fitter(gae);
  mPulseFit->SetSignal(gae);
  mPulseFit->SetSearchWindow(mCenterTB,5);//Some suitable starting search parameter may need to be adjusted based on data
  //Since LED data is raw (not pedestal subtracted) determine baseline first
  mPulseFit->AnalyzeForPedestal();//Must be called before anything else otherwise crash
  func = mPulseFit->BaselineFit();
  res[5] = mPulseFit->Baseline();
  res[6] = mPulseFit->BaselineSigma();
  res[7] = func->GetChisquare()/func->GetNDF();   //chi2 from signal fit
  
  mPulseFit->SetSearchWindow(mCenterTB,4);//Check +- 4tb around triggered crossing
  //Many more options to set but excluded for now to see how default values perform
  res[0]=mPulseFit->SumAdcFit();     //this is full integral (pedestal subtracted)
  func = mPulseFit->SignalFit();     //Fitted function
  res[1]=func->GetParameter(0);  //this is peak height from signal fit
  res[2]=func->GetParameter(1);  //this is peak position from signal fit [timebin]
  res[3]=func->GetParameter(2);  //this is width from signal fit
  res[4]=func->GetChisquare()/func->GetNDF();   //chi2 from signal fit
  
  return res[0];
}

int StFcsWaveformFitMaker::GenericPadPos(int value, int Nvals, int PadNums )
//PadNums is not total pads but number pads in the row or column, Nvals is number of stuff per column or row to put on same pad
{
  if( value<=0 ){return ceil( static_cast<double>(value+(Nvals*PadNums))/static_cast<double>(Nvals) );}
  else{ return GenericPadPos(value-(Nvals*PadNums), Nvals, PadNums); }
}

int StFcsWaveformFitMaker::PadNum4x4(int det, int col, int row)
{
  int ncol = 0;
  int nrow = 0;
  if( det<=1 )
    {
      ncol = 2;
      nrow = 3;
    }
  else if( det<=3 )
    {
      ncol = 2;
      nrow = 2;
    }
  else{ std::cout << "This only works for Ecal and Hcal" << std::endl; return 0;}
  int padcol = GenericPadPos(col,ncol,4);
  int padrow = GenericPadPos(row,nrow,4);
  return 4*(padrow-1)+padcol;
}

