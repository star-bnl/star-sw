#include "StEmcEqualMaker.h"
#include "TFile.h"
#include "TROOT.h"
#include "TF1.h"
#include "StEventTypes.h"
#include "StEvent.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StDbLib/StDbManager.hh"
#include "StDbLib/StDbTable.h"
#include "StDbLib/StDbConfigNode.hh"
#include "tables/St_emcCalib_Table.h"

ClassImp(StEmcEqualMaker)

//_____________________________________________________________________________
StEmcEqualMaker::StEmcEqualMaker(const char *name):StEmcCalibMaker(name)
{
  setRange(700);
}
//_____________________________________________________________________________
StEmcEqualMaker::~StEmcEqualMaker()
{
}
//_____________________________________________________________________________
Int_t StEmcEqualMaker::Init()
{
  float pi = 3.1415926;
	mA = new TH1F("mA","",getNChannel(),0.5,getNChannel()+0.5);
  mB = new TH1F("mB","",getNChannel(),0.5,getNChannel()+0.5);
  mADistr = new TH1F("mADistr","",200,0,10);
  mBDistr = new TH1F("mBDistr","",200,-10,10);
  mStatus = new TH1F("mStatus","",getNChannel(),0.5,getNChannel()+0.5);
  mRefSlopes = new TH1F("mRefSlopes","",getNChannel(),0.5,getNChannel()+0.5);
  mRefAmps = new TH1F("mRefAmps","",getNChannel(),0.5,getNChannel()+0.5);
  mSlopes = new TH1F("mSlopes","",getNChannel(),0.5,getNChannel()+0.5);
  mAmps = new TH1F("mAmps","",getNChannel(),0.5,getNChannel()+0.5);
  mSlopesTheta = new TH2F("mSlopesTheta","",40,45*pi/180,135*pi/180,100,0,100);

  mSpecName ="mSpecEqual";
  mAcceptName = "mAcceptEqual";
  return StEmcCalibMaker::Init();
}
//_____________________________________________________________________________
void StEmcEqualMaker::Clear(Option_t *option)              
{
}
//_____________________________________________________________________________
Int_t StEmcEqualMaker::Make()
{  
  if(!accept()) return kStOk;
      
  for(int i=0;i<mNChannel;i++)
  {
    int id = i+1;
    float rms = getCalib()->getPedRms(mDetector,id);
    float adc = (float) getCalib()->getADCPedSub(mDetector,id);
    if(adc!=0 && adc>3*rms) fill(id,adc);
  }
  
  return kStOK;
}
//_____________________________________________________________________________
Int_t StEmcEqualMaker::Finish()
{
  saveHist((char*)mFileName.Data());
  return StMaker::Finish();
}
//_____________________________________________________________________________
void StEmcEqualMaker::equalize(int mode,int DeltaEta,bool Et)
{
	mADistr->Reset();
	mBDistr->Reset();
	StEmcGeom *g = getGeom();
	int Neta = g->NEta();
	int Nsub = g->NSub();
	int etabin = 0;
	float avg[18000];
	float MIN = 20;
	float MAX = 500;
	TF1 *f= new TF1("slopes","22*TMath::Sin(x)");

	for(int eta0 = -Neta; eta0<Neta; eta0+=DeltaEta)
	{
		etabin++;
		float sum = 0;
		float n = 0;
		int m1 = 1;
		int m2 = 60;
		if(eta0<0) { m1 = 61; m2 = 120; }
		if(DeltaEta == 2*Neta){ m1=1; m2=120;}
		for(int j = 0;j<DeltaEta;j++)
		{
			int eta = eta0+j;
			if(eta>=0) eta+=1;
			cout <<"etabin = "<<etabin<<" eta = "<<eta<<endl;
			eta = abs(eta);
			//cout <<"Calculating averages ..."<<endl;
			for(int m = m1;m<=m2;m++)
			{
				for(int sub = 1;sub<=Nsub;sub++)
				{
				  int id;
					g->getId(m,eta,sub,id);
					TH1D * h = getSpec(id);
					if(h->Integral()>0)
					{
						float m1,r1;
						getMeanAndRms(h,MIN,MAX,&m1,&r1);
						sum+=m1;
						n++;
						avg[id-1] = m1;
					}
				}
			}
		}
		float AVG = 0;
		if(n>0) AVG=sum/n;
		if(AVG>0)
		{
			int ID = 0;
			float DCA = 99999;
			//cout <<"Looking for reference ..."<<endl;
			for(int m = m1;m<=m2;m++) for(int j = 0;j<DeltaEta;j++) for(int sub = 1;sub<=Nsub;sub++)
			{
				int id;
				int eta = eta0+j;
				if(eta>=0) eta+=1;
			  eta = abs(eta);
				g->getId(m,eta,sub,id);
				if(avg[id-1]>0 && fabs(avg[id-1]-AVG)<DCA) { DCA = fabs(avg[id-1]-AVG); ID = id;}
			}
			if(ID>0)
			{
				cout <<"Reference found id = "<<ID<<"  avg = "<<avg[ID-1]<<endl;
			  cout <<"Equalizing eta bin ..."<<endl;
				for(int m = m1;m<=m2;m++) for(int j = 0;j<DeltaEta;j++) for(int sub = 1;sub<=Nsub;sub++) 
				{
					int eta = eta0+j;
					if(eta>=0) eta+=1;
			    eta = abs(eta);
					int id;
					g->getId(m,eta,sub,id);
					if(mode!=5) equalizeRelative(ID,id,mode,Et);
					else equalizeToFunction(id,f);
					
				}
			} 
		}
	}
	delete f;
	return;
}
//_____________________________________________________________________________
void StEmcEqualMaker::equalizeRelative(int ref, int channel,int mode, bool Et)
{
  // mean and RMS modes  
  // 0 -  mean and RMS with liear average
  // 1 -  mean with linear average
  // 2 -  mean and rms with log average
  // 3 -  mean with log average 
	// 4 -  exponential fit

  TH1D *h1 = getSpec(ref,"ref");
	TH1D *h2 = getSpec(channel,"channel");

  float integral1 = h1->Integral();
	float integral2 = h2->Integral();
	
	mA->SetBinContent(channel,0);
	mB->SetBinContent(channel,0);
	mStatus->SetBinContent(channel,0);
	mRefSlopes->SetBinContent(channel,0);
	mRefAmps->SetBinContent(channel,0);
	mSlopes->SetBinContent(channel,0);
	mAmps->SetBinContent(channel,0);
	
	float MIN = 20;
	float MAX = 500;
	
	//cout <<"ref = "<<ref<<"  channel = "<<channel<<endl;
	//cout <<"h1 = "<<h1<<"  h2 = "<<h2<<endl;
	//cout <<"integral 1 = "<<integral1<<"  integral 2 = "<<integral2<<endl;
	
	if((integral1==0 || integral2==0) && h1!=h2)
	{
		delete h1;
		delete h2;
		return;
	}   
	if((integral1==0 || integral2==0) && h1==h2)
	{
		delete h1;
		return;
	}   
	
  bool EqDone=kFALSE;
  float a=0,b=0;
  
  if(mode==-1)
  {
    a = 1;
    b = 0;
    EqDone=true;
  }
  
  if(mode>=0 && mode <=3)  
  {
    float m1,r1,m2,r2;        
    if(mode==0 || mode==1)
    {
      getMeanAndRms(h1,MIN,MAX,&m1,&r1);
      getMeanAndRms(h2,MIN,MAX,&m2,&r2);
    }
    if(mode==2 || mode==3)
    {
      getLogMeanAndRms(h1,MIN,MAX,&m1,&r1);
      getLogMeanAndRms(h2,MIN,MAX,&m2,&r2);
    }
    if(mode==0 || mode==2)
    {
      a=r1/r2;
      b=m1-a*m2;
    }
    if(mode==1 || mode==3)
    {    
      a=m1/m2;
      b=0;
    }
    EqDone=kTRUE;
    cout <<"  id = "<<channel<<"  ref = "<<ref<<"  mean = "<<m1<<" , "<<m2
         <<"  rms = "<<r1<<" , "<<r2
         <<"  a = "<<a<<"  b = "<<b<<endl;
  }
  
	float m1=0,m2=0,A1=0,A2=0;
	if(mode==4)
	{
		TF1 *f=new TF1("ff","[0]*exp(-x/[1])",MIN,MAX);
    float I1 = h1->Integral(h1->FindBin(MIN),h1->FindBin(MAX));
		f->SetParameter(1,10);
		h1->Fit(f,"RQN");
		m1 = f->GetParameter(1);
		A1 = f->GetParameter(0);
	  mRefSlopes->SetBinContent(channel,m1);
	  mRefAmps->SetBinContent(channel,A1);
	
    float I2 = h2->Integral(h2->FindBin(MIN),h2->FindBin(MAX));
		h2->Fit(f,"RQN");
		m2 = f->GetParameter(1);
		A2 = f->GetParameter(0);
	  mSlopes->SetBinContent(channel,m2);
	  mAmps->SetBinContent(channel,A2);
		
		a=m1/m2;
		b=-::log((A2*I1)/(A1*I2));
		EqDone=true;
    if(!finite(a) || !finite(b) || a<=0 || b>1000) {EqDone = false; a = 0;}
    b=0;
    cout <<"  id = "<<channel<<"  ref = "<<ref<<"  slopes = "<<m2<<" , "<<m1
         <<"  a = "<<a<<"  b = "<<b<<"  EQDONE = "<<(Int_t)EqDone<<endl;
		delete f;
	}
  
  if (EqDone)
  {
	  mA->SetBinContent(channel,a);
	  mB->SetBinContent(channel,b);
	  mADistr->Fill(a);
	  mBDistr->Fill(b);
	  mStatus->SetBinContent(channel,1);
		
		float theta;
		int mod,eta,sub;
		getGeom()->getBin(channel,mod,eta,sub);
		getGeom()->getTheta(mod,eta,theta);
		mSlopesTheta->Fill(theta,m2);
	  
  }  
  else mStatus->SetBinContent(channel,2);
	
	delete h1;
	delete h2;
  return;
}
//_____________________________________________________________________________
void StEmcEqualMaker::equalizeToFunction(int channel,TF1 *func)
{
	TH1D *h2 = getSpec(channel,"channel");

	float integral2 = h2->Integral();
	
	mA->SetBinContent(channel,0);
	mB->SetBinContent(channel,0);
	mStatus->SetBinContent(channel,0);
	mRefSlopes->SetBinContent(channel,0);
	mRefAmps->SetBinContent(channel,0);
	mSlopes->SetBinContent(channel,0);
	mAmps->SetBinContent(channel,0);
	
	float MIN = 20;
	float MAX = 500;
		
	if(integral2==0) 
	{ 
	  delete h2;  	  
	  mA->SetBinContent(channel,0);
	  mB->SetBinContent(channel,0);
		return;
  }   
	
  bool EqDone=kFALSE;
  float a=0,b=0;
    
	float theta;
	int mod,eta,sub;
	getGeom()->getBin(channel,mod,eta,sub);
	getGeom()->getTheta(mod,eta,theta);

	float m1=0,m2=0,A1=0,A2=0;
	TF1 *f=new TF1("ff","[0]*exp(-x/[1])",MIN,MAX);
	f->SetParameter(1,10);
	
	m1 = func->Eval(theta);
	mRefSlopes->SetBinContent(channel,m1);
	mRefAmps->SetBinContent(channel,A1);
	
  if(channel<=2400)
	{
	  //float I2 = h2->Integral(h2->FindBin(MIN),h2->FindBin(MAX));
		h2->Fit(f,"RQN");
		m2 = f->GetParameter(1);
		A2 = f->GetParameter(0);
		mSlopes->SetBinContent(channel,m2);
		mAmps->SetBinContent(channel,A2);
		
		
		a=m1/m2;
	} //else a =1; //protection because crates in the east side are not timed
	
	EqDone=true;
  if(!finite(a) || !finite(b) || a<=0.01 || b>1000 || a > 10) { EqDone = false; a = 0;}
  b=0;
  cout <<"  id = "<<channel<<"  slopes = "<<m2<<" , "<<m1
         <<"  a = "<<a<<"  b = "<<b<<"  EQDONE = "<<(Int_t)EqDone<<endl;
	delete f;
  
  if (EqDone)
  {
	  mA->SetBinContent(channel,a);
	  mB->SetBinContent(channel,b);
	  mADistr->Fill(a);
	  mBDistr->Fill(b);
	  mStatus->SetBinContent(channel,1);
		
		mSlopesTheta->Fill(theta,m2);
	  
  }  
  else mStatus->SetBinContent(channel,2);
	
	delete h2;
  return;
}
//_____________________________________________________________________________
void StEmcEqualMaker::calcSlopes()
{

	float MIN = 20;
	float MAX = 500;
  mSlopes->Reset();
	mAmps->Reset();
	mSlopesTheta->Reset();	
	TF1 *f=new TF1("ff","[0]*exp(-x/[1])",MIN,MAX);
	StEmcGeom *g = getGeom();

	for(int channel = 1;channel<=getNChannel();channel++)
	{
		TH1D *h2 = getSpec(channel,"channel");
  	float I2 = h2->Integral(h2->FindBin(MIN),h2->FindBin(MAX));
		if(I2>0)
		{
      float theta;
		  int mod,eta,sub;
		  g->getBin(channel,mod,eta,sub);
		  g->getTheta(mod,eta,theta);

		  float m2=0,A2=0;
		
	    f->SetParameter(1,10);
		  h2->Fit(f,"RQN");
		  m2 = f->GetParameter(1);
		  A2 = f->GetParameter(0);
		  mSlopes->SetBinContent(channel,m2);
		  mAmps->SetBinContent(channel,A2);
				
		  mSlopesTheta->Fill(theta,m2);
			cout <<"Slope for channel "<<channel<<"  is "<<m2<<endl;
		}
	  delete h2;
  }  
  delete f;
 
  return;
}
//_____________________________________________________________________________
void StEmcEqualMaker::saveEqual(int date,int time)
{   
	char ts[100];
  TString n[] = {"bemcEqual","bprsEqual","bsmdeEqual","bsmdpEqual"};
	sprintf(ts,"%s.%08d.%06d.root",n[getDetector()-1].Data(),date,time);
	TFile *f = new TFile(ts,"RECREATE");
	if(getSpec()) getSpec()->Write();
	if(mA) mA->Write();
	if(mB) mB->Write();
	if(mADistr) mADistr->Write();
	if(mBDistr) mBDistr->Write();
	if(mStatus) mStatus->Write();
	if(mRefSlopes) mRefSlopes->Write();
	if(mRefAmps) mRefAmps->Write();
	if(mSlopes) mSlopes->Write();
	if(mAmps) mAmps->Write();
	if(mSlopesTheta) mSlopesTheta->Write();
	f->Close();
	delete f;
	return;
}
//_____________________________________________________________________________
void StEmcEqualMaker::loadEqual(char* file)
{   
	TFile *f = new TFile(file);
	mA->Reset();
	mB->Reset();
	mADistr->Reset();
	mBDistr->Reset();
	mStatus->Reset();
	mRefSlopes->Reset();
	mRefAmps->Reset();
	mSlopes->Reset();
	mAmps->Reset();
	mSlopesTheta->Reset();
	getSpec()->Reset();
	
  if(f->Get("mSpec;1")) getSpec()->Add((TH2F*)f->Get("mSpec;1"),1);
  if(f->Get("mA;1")) mA->Add((TH1F*)f->Get("mA;1"),1);
  if(f->Get("mB;1")) mB->Add((TH1F*)f->Get("mB;1"),1);
  if(f->Get("mADistr;1")) mADistr->Add((TH1F*)f->Get("mADistr;1"),1);
  if(f->Get("mBDistr;1")) mBDistr->Add((TH1F*)f->Get("mBDistr;1"),1);
  if(f->Get("mStatus;1")) mStatus->Add((TH1F*)f->Get("mStatus;1"),1);
  if(f->Get("mRefSlopes;1")) mRefSlopes->Add((TH1F*)f->Get("mRefSlopes;1"),1);
  if(f->Get("mRefAmps;1")) mRefAmps->Add((TH1F*)f->Get("mRefAmps;1"),1);
  if(f->Get("mSlopes;1")) mSlopes->Add((TH1F*)f->Get("mSlopes;1"),1);
  if(f->Get("mAmps;1")) mAmps->Add((TH1F*)f->Get("mAmps;1"),1);
  if(f->Get("mSlopesTheta;1")) mSlopesTheta->Add((TH2F*)f->Get("mSlopesTheta;1"),1);
	f->Close();
	delete f;
	return;
}
//_____________________________________________________________________________
TH1F* StEmcEqualMaker::getEtaBinSpec(int eta0, int eta1,TH2F* SPEC)
{   
	TH2F *spec = NULL;
	if(!SPEC) spec = getSpec(); else spec = SPEC;
	if(!spec) return NULL;
	int mi = 1;
	int mf = 60;
	if(eta0<0 && eta1<0)
	{
		mi = 61;
		mf = 120;
	}
	if(eta0*eta1<0)
	{
		mi =1;
		mf = 120;
	}
	int ns = getGeom()->NSub();
	int e0 = abs(eta0);
	int e1 = abs(eta1);
	TH1F *hist = (TH1F*)spec->ProjectionY("etabin",1,1);
	hist->Reset();
	
	for(int m=mi;m<=mf;m++)
	{
		for(int e=e0;e<=e1;e++)
		{
			for(int s=1;s<=ns;s++)
			{
				int id;
				getGeom()->getId(m,e,s,id);
				char name[30];
				sprintf(name,"Tower %d",id);
				TH1F* tmp = rebin(id,name,spec);
				if(tmp)
				{
				  hist->Add(tmp,1);
				  delete tmp; tmp = NULL;
				}
			}
		}
	}
	return hist;
}
//_____________________________________________________________________________
TH1F* StEmcEqualMaker::rebin(int id,const char *name, TH2F* SPEC)
{   
	TH2F *spec = NULL;
	if(!SPEC) spec = getSpec(); else spec = SPEC;
	if(!spec) return NULL;
	
	float seg = 50;
	float a = mA->GetBinContent(id);
	float b = mB->GetBinContent(id);
	//cout <<"A = "<<a<<"  B = "<<b<<endl;
	
  if(a<=0) return NULL;	
		
	TH1F *h = (TH1F*)spec->ProjectionY(name,id,id);
	if(a==1 && b==0) return h;
	
	float deltaBin=h->GetBinWidth(1)/seg;
	int nbins=h->GetNbinsX();	
	
	float Y[4096];
	for(int i=0;i<4096;i++) Y[i] = 0;
	for(int i=1;i<=nbins;i++) Y[i] = h->GetBinContent(i);
			
	h->Reset();
  
  for(int i=1;i<=nbins;i++)
  {
    float x = h->GetBinLowEdge(i);
		float y = Y[i];
		for(int j=0;j<(int)seg;j++)
		{
			float x1 = x+((float)j+0.5)*deltaBin;
			float x2 = x1*a+b;
      //if (a>1) cout <<position<<"  "<<a<<"  "<<x1<<"  "<<x2<<endl;
			h->Fill(x2,y/seg);
		}
  }
	return h;
}
//_____________________________________________________________________________
// this method leaks memory because it draws a lot of histograms
// use it with care
// AASPSUAIDE
void StEmcEqualMaker::drawEtaBin(int eta0, int eta1,TH2F* SPEC)
{   
	TH2F *spec = NULL;
	if(!SPEC) spec = getSpec(); else spec = SPEC;
	if(!spec) return;
	int mi = 1;
	int mf = 60;
	if(eta0<0 && eta1<0)
	{
		mi = 61;
		mf = 120;
	}
	if(eta0*eta1<0)
	{
		mi =1;
		mf = 120;
	}
	int ns = getGeom()->NSub();
	int e0 = abs(eta0);
	int e1 = abs(eta1);

  bool first = true;
		
	for(int m=mi;m<=mf;m++)
	{
		for(int e=e0;e<=e1;e++)
		{
			for(int s=1;s<=ns;s++)
			{
				int id;
				getGeom()->getId(m,e,s,id);
				char name[30];
				sprintf(name,"Tower %d",id);
				TH1F* tmp = rebin(id,name,spec);
				if(tmp)
				{
				  if(first) { tmp->Draw(); first = false;} else tmp->Draw("same");
				}
			}
		}
	}
	return;
}





