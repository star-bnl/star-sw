#include "StEmcCalibMaker.h"

#include <TFile.h>
#include <TROOT.h>
#include <TF1.h>

#include <StEventTypes.h>
#include <StEvent.h>
#include <StEmcUtil/geometry/StEmcGeom.h>
#include <StEmcUtil/voltageCalib/VoltCalibrator.h>
#include <StDbLib/StDbManager.hh>
#include <StDbLib/StDbTable.h>
#include <StDbLib/StDbConfigNode.hh>
#include <tables/St_emcCalib_Table.h>

ClassImp(StEmcCalibMaker)

//_____________________________________________________________________________
StEmcCalibMaker::StEmcCalibMaker(const Char_t *name)
    :StMaker(name)
    {
  mCalib = NULL;
  mAccept = NULL;
  mSpec = NULL;
  mDate = 20330101;
  mNMinTracks = 0;
  mNMaxTracks = 100000;
  mDebug = false;
  setDetector(1);
  setRange(200);
  mZDCMin = -1000000;
  mZDCMax =  1000000;
  mCTBMin = -1000000;
  mCTBMax =  1000000;
  mSpecName = "mSpec";
  mAcceptName = "mAccept";
  setFile("spec.root");
}
//_____________________________________________________________________________
StEmcCalibMaker::~StEmcCalibMaker()
{
}
//_____________________________________________________________________________
Int_t StEmcCalibMaker::Init()
{
  mSpec = new TH2F(mSpecName.Data(),"Equal Spectra",mNChannel,+0.5,(float)mNChannel+0.5,(int)mRange,-0.5,mRange-0.5);
  mAccept = new TH1F(mAcceptName.Data(),"Accepted events",20,-0.5,19.5);
  return StMaker::Init();
}
//_____________________________________________________________________________
void StEmcCalibMaker::Clear(Option_t *option)              
{
}
//_____________________________________________________________________________
Int_t StEmcCalibMaker::Make()
{    
  return kStOK;
}
//_____________________________________________________________________________
bool StEmcCalibMaker::accept()
{  
  mAccept->Fill(0);
	if(!mCalib) mCalib = (StEmcCalibrationMaker*)GetMaker("Calib");
  if(!mCalib) return false;
  mAccept->Fill(1);
	
	if(!mCalib->hasDetector(getDetector())) return false;
  mAccept->Fill(2);
  
  if(mCalib->getNTracks()<mNMinTracks) return false;
  mAccept->Fill(3);
	
  if(mCalib->getNTracks()>mNMaxTracks) return false;
  mAccept->Fill(4);
  
  if(GetDate()<mDate) { mDate = GetDate(); mTime = GetTime();}
  
  if(!mCalib->isTriggerOk()) return false;
  mAccept->Fill(5);
	
	long CTB = mCalib->getCTBSum();
	long ZDC = mCalib->getZDCSum();
	
	if(ZDC<mZDCMin || ZDC > mZDCMax) return false;
  mAccept->Fill(6);
	
	if(CTB<mCTBMin || CTB > mCTBMax) return false;
  mAccept->Fill(7);
	
  
  mNEvents++;
  if(mNEvents%100==0) saveHist(mFileName.Data());
  
	//cout <<"Processing event number "<<mNEvents<<" for maker "<<GetName()<<endl;
	return true;
}
//_____________________________________________________________________________
Int_t StEmcCalibMaker::Finish()
{
  saveHist(mFileName.Data());
  return StMaker::Finish();
}
//_____________________________________________________________________________
void StEmcCalibMaker::saveHist(const Char_t *file)
{
  cout << "Saving spec to file " << file << endl;
  TFile *f = new TFile(file, "RECREATE");
  mSpec->Write();
  f->Close();
  delete f;
  return ;
}
//_____________________________________________________________________________
void StEmcCalibMaker::loadHist(const Char_t *file)
{
	TFile *f = new TFile(file);
  TString N = mSpec->GetName();
  N+=";1";
  TH2F *h = (TH2F*)f->Get(N.Data());
  if(h){ mSpec->Reset(); mSpec->Add(h,1);}
  f->Close();
  delete f;
  return ;
}
//_____________________________________________________________________________
void StEmcCalibMaker::addHist(const Char_t *file)
{
  TFile *f = new TFile(file);
  TString N = mSpec->GetName();
  N+=";1";
  TH2F *h = (TH2F*)f->Get(N.Data());
  if(h){ mSpec->Add(h,1);}
  f->Close();
  delete f;
  return ;
}
//_____________________________________________________________________________
float StEmcCalibMaker::getTimeInterval(int refDate, int refTime)
{
  struct tm t0;
  struct tm t1;
    
  Int_t year  = (Int_t)(mDate/10000);
  t0.tm_year=year-1900;
  Int_t month = (Int_t)(mDate-year*10000)/100; 
  t0.tm_mon=month-1;
  Int_t day   = (Int_t)(mDate-year*10000-month*100);
  t0.tm_mday=day;
  Int_t hour  = (Int_t)(mTime/10000);
  t0.tm_hour=hour;
  Int_t minute= (Int_t)(mTime-hour*10000)/100;
  t0.tm_min=minute;
  Int_t second= (Int_t)(mTime-hour*10000-minute*100);
  t0.tm_sec=second;
  t0.tm_isdst = -1;
  
  year  = (Int_t)(refDate/10000);
  t1.tm_year=year-1900;
  month = (Int_t)(refDate-year*10000)/100; 
  t1.tm_mon=month-1;
  day   = (Int_t)(refDate-year*10000-month*100);
  t1.tm_mday=day;
  hour  = (Int_t)(refTime/10000);
  t1.tm_hour=hour;
  minute= (Int_t)(refTime-hour*10000)/100;
  t1.tm_min=minute;
  second= (Int_t)(refTime-hour*10000-minute*100);
  t1.tm_sec=second;
  t1.tm_isdst = -1;

  time_t ttime0=mktime(&t0);
  time_t ttime1=mktime(&t1);
  double diff=difftime(ttime0,ttime1);
  
  //cout <<"startdate = "<<startDate<<" now = "<<d<<"  dt = "<<diff/60<<endl;
  //cout <<diff<<"  "<<diff/60.<<endl;
  float ddd=fabs(diff/3600.);
  return ddd;
}
//_____________________________________________________________________________
void StEmcCalibMaker::getMeanAndRms(TH1D* tmp,float amin,float amax,
                                   float* m,float* r)
{  
  float mean=0,rms=0,sum=0;
  if(tmp->Integral()>0)
  {
	  Int_t bin0 = tmp->FindBin(amin);
	  Int_t bin1 = tmp->FindBin(amax);
    for(Int_t j=bin0;j<bin1;j++)
    {
      float w = tmp->GetBinContent(j);
		  float x = tmp->GetBinCenter(j);
      mean += w*x;
      sum  += w;
      rms  += x*x*w;
    }
    if(sum>0)
    {
      mean /= sum;
      rms   = ::sqrt(rms/sum-mean*mean);
    }
  }
  *m    = mean;
  *r    = rms;
  return;

}
//_____________________________________________________________________________
void StEmcCalibMaker::getLogMeanAndRms(TH1D* tmp,float amin,float amax,
                                      float* m,float* r)
{
  float mean=0,rms=0,sum=0;
  
  if(tmp->Integral()>0)
  {
	  Int_t bin0 = tmp->FindBin(amin);
	  Int_t bin1 = tmp->FindBin(amax);
    for(Int_t j=bin0;j<bin1;j++)
    {
      float w = tmp->GetBinContent(j);
			if(w==1) w=1.5;
		  if(w>0) w = ::log(w);
		  float x = tmp->GetBinCenter(j);
      mean += w*x;
      sum  += w;
      rms  += x*x*w;
    }
    if(sum>0)
    {
	    mean /= sum;
      rms   = ::sqrt(rms/sum-mean*mean);
    }
  }
  *m    = mean;
  *r    = rms;
  return;

}
StEmcGeom* StEmcCalibMaker::getGeom()
{
	return StEmcGeom::instance(getDetector());
}
//_____________________________________________________________________________
void StEmcCalibMaker::calcVoltages(TH1F *gain, char* refFile, char* voltageInput, char* voltageOutput)
{
  char GainShift[]="HVGainShift.dat";
	char line[300];
	
	ofstream out(GainShift);
	ifstream inp(voltageInput);
	//TH1F *h = new TH1F("Gain","Current Gain Distribution",100,0,0.02);
	do
	{
	  int a,b,c,d,e,f,id;
		float v;
		inp>>a>>id>>b>>c>>d>>e>>f>>v;		
    float gainShift = gain->GetBinContent(id);
		sprintf(line,"%2d %5d  %5d  %5d  %5d  %5d  %5d  %7.5f",a,id,b,c,d,e,f,gainShift);
		out <<line<<endl;
		cout <<line<<endl;
		
	} while(!inp.eof());	
  out.close();
	
	cout <<"Calculating voltages\n";
	
  // Instantiate the voltage calibrator class
  VoltCalibrator vc;
  // Select the file that contains the Gain vs HV data
  vc.setRefFile(refFile);
  // Select the file that contains the relative gains to be
  // be achieved
  vc.setGainFile(GainShift);
  // Select the file that containe the current voltages
  vc.setVoltInputFile(voltageInput);
  // Select the file that will contain the new request voltages
  vc.setVoltOutputFile(voltageOutput);
  // Perform the calculation
  vc.process();
}
