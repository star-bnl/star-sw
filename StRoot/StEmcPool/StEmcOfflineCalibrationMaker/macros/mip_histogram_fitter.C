#include <iostream>
#include <fstream>
using namespace std;

#include "CalibrationHelperFunctions.cxx"

#include "TFile.h"
#include "TMath.h"
#include "TH1.h"
#include "TF1.h"
#include "TPostScript.h"
#include "TCanvas.h"
#include "TStyle.h"
#include "TLatex.h"
#include "TString.h"
#include "TLine.h"

void drawTower(TH1* h, TF1* f, int id, int status, CalibrationHelperFunctions* helper);

float getTowerEta(int id);
TString towerQA(TH1* h, TF1* f);
bool isBadIso2005(int id);
bool isBadPoverE2005(int id);
bool isBadTower2005(int id);
bool isPmtNew(int id);
bool isBadTower2006(int id);

void mip_histogram_fitter(const char* file_list="mips.list", const char* postscript="mip.ps", const char* gainfile="mip.gains.long",const char* ofname="2009.mipfit.root",const char* gfiles="mip.gains"){
  const int ntowers = 4800;//4800;

 int mipstatus[ntowers];
	cout<<"input files:  "<<file_list<<endl;
	cout<<"plots:           "<<postscript<<endl;
	cout<<"gains:           "<<gainfile<<endl;
	
	gStyle->SetCanvasColor(10);
	gStyle->SetCanvasBorderMode(0);
	gStyle->SetStatColor(10);
	gStyle->SetOptTitle(0);
	gStyle->SetOptFit(111);
	gStyle->SetOptStat("mrie");

	TFile* outfile = new TFile(ofname,"RECREATE");
	CalibrationHelperFunctions* helper = new CalibrationHelperFunctions();
	float pi = TMath::Pi();

	TPostScript *ps = new TPostScript(postscript);
	TCanvas *c = new TCanvas("c","",100,100,600.,800.);
	int pad;	
	TH1 *mip_histo[ntowers];
	TH2F* etaphi = new TH2F("etaphi","Mip peaks",40,-1.0,1.0,120,-pi,pi);
	TH2F* statcode = new TH2F("statcode","Mip peaks",40,-1.0,1.0,120,-pi,pi);
	etaphi->SetXTitle("#eta"); etaphi->SetYTitle("#phi");
	statcode->SetXTitle("#eta"); statcode->SetYTitle("#phi");
	TH1 *dumhist;
	char file[200];
	int nfiles = 0;
	ifstream filelist(file_list);
	while(1){
		filelist >> file;
		if(!filelist.good()) break;
		cout<<file<<endl;
		TFile input_file(file,"READ");
		char name[100];
      
		for(int y = 0; y < ntowers; y++)
		  {
		    sprintf(name,"mip_histo_%i",y+1);
		    if(nfiles == 0)
		      {
			mip_histo[y] = (TH1*)input_file.Get(name);
		      }
		    else
		      {
			//cout<<y<<endl;
			dumhist = (TH1*)input_file.Get(name);
			mip_histo[y]->Add(dumhist);
			dumhist->Clear();
		      }
		  }
		nfiles++;
		input_file.Clear();
	}

	TF1 *gaussian_fit[ntowers];
	char name[100];
	
	int counter = 0;
	for(int i=0; i<ntowers; i++){		
	  mipstatus[i] = 0;
//		if(!isBadTower2005(i+1) && !isBadIso2005(i+1)) continue;
//		if(!isBadTower2006(i+1)) continue;
		if(counter%20 == 0){
			c->Update();
			ps->NewPage();
			c->Clear();
			c->Divide(4,5);
			pad = 1;
		}
		
		if(i%600==0) cout<<"finished drawing tower "<<i+1<<endl;
		
		c->cd(pad);
		

		sprintf(name,"fit_%i",i+1);
		
		gaussian_fit[i] = new TF1(name,"gaus(0) + pol0(3)",5.,250.);

		if((i+1) % 20 == 0){
		  //cout<<i+1<<endl;
		  //mip_histo[i]->Rebin(5);
		  mip_histo[i]->GetXaxis()->SetRangeUser(6.,100.);
		}else{
		  //mip_histo[i]->Rebin(2);
		  mip_histo[i]->GetXaxis()->SetRangeUser(6.,50.);
		}
		float guesspeak = mip_histo[i]->GetBinCenter(mip_histo[i]->GetMaximumBin());
		float peakmax = mip_histo[i]->GetMaximum();	
		float guessrms = mip_histo[i]->GetRMS();
		gaussian_fit[i]->SetParameter(1,guesspeak);
		gaussian_fit[i]->SetParameter(2,guessrms);
		gaussian_fit[i]->SetParLimits(0,1,peakmax*2);
		gaussian_fit[i]->SetParLimits(1,1,250);
		gaussian_fit[i]->SetParLimits(2,guessrms/8.,100);
		gaussian_fit[i]->SetParLimits(3,0,1000000);
		
		gaussian_fit[i]->SetLineColor(kBlue);
		gaussian_fit[i]->SetLineWidth(0.6);

		double histogram_top = mip_histo[i]->GetBinContent(mip_histo[i]->GetMaximumBin());
		double gaussian_mean = 0;
		
		if(mip_histo[i]->Integral() > 25){
		  mip_histo[i]->Fit(gaussian_fit[i],"rql","",guesspeak-2*guessrms,guesspeak+2*guessrms);
		  mipstatus[i]+=1;
		  double gauss_const = gaussian_fit[i]->GetParameter(0);
		  gaussian_mean = gaussian_fit[i]->GetParameter(1);
		  //if((histogram_top / gauss_const > 2) || (histogram_top / gauss_const < 0.5))mipstatus[i]+=16;
		  if(gaussian_mean < 5)mipstatus[i]+=4; //change to <6?
		  if((i+1)%20==0){
		    if(abs(gaussian_mean-guesspeak) > 10)mipstatus[i]+=8;
		    if(gaussian_fit[i]->GetParameter(2) > 20)mipstatus[i]+=2;
		  }else{
		    if(abs(gaussian_mean-guesspeak) > 5)mipstatus[i]+=8;
		    if(gaussian_fit[i]->GetParameter(2) > 15)mipstatus[i]+=2;
		  }
		  if(guesspeak > 50)mipstatus[i]+=64;
		  if(mipstatus[i]==1){
		    etaphi->Fill(helper->getEta(i+1),helper->getPhi(i+1),gaussian_mean);
		    //mipstatus[i]+=128;
		  }
		  statcode->Fill(helper->getEta(i+1),helper->getPhi(i+1),mipstatus[i]);

		}
		//if(mipstatus[i]==129)mipstatus[i] = 1;
		if(mipstatus[i]>1){
		  //cout<<i+1<<" "<<mipstatus[i]<<endl;
		  mip_histo[i]->GetFunction(name)->SetLineColor(kRed);
		}				
		drawTower(mip_histo[i],gaussian_fit[i],i+1,mipstatus[i],helper);
		
		TLine *gaussian_peak = new TLine(gaussian_mean,0.,gaussian_mean,histogram_top+15);
		gaussian_peak->SetLineColor(kBlue);
		if(mipstatus[i]!=1)gaussian_peak->SetLineColor(kRed);
		gaussian_peak->SetLineWidth(2.0);
		gaussian_peak->Draw("same");
	       


		
//		if(fit[i]->GetParameter(0) > 10. && fit[i]->GetParameter(1)>0. && fit[i]->GetParameter(1)<40. && fit[i]->GetParameter(2)>0.3 && fit[i]->GetParameter(2)<5.){
//			printf("%4i     %f     %f     %f\n",i+1,fit[i]->GetParameter(0),fit[i]->GetParameter(1),fit[i]->GetParameter(2));
//		}
		
//		TString QA = towerQA(mip_histo[i],fit[i]);
//		if(QA.Index("empty") != -1)	c->GetPad(pad)->SetFillColor(kRed);
//		else if(QA.Length() > 0)	c->GetPad(pad)->SetFillColor(kYellow);		
		
		pad++;
		counter++;
	}
	c->Update();
	ps->NewPage();
	c->Clear();
	c->Divide(1,2);
	gStyle->SetPalette(1);
	gStyle->SetOptStat("e");
	c->cd(1);
	etaphi->Draw("colz");
	c->cd(2);
	statcode->GetZaxis()->SetRangeUser(0,20);
	statcode->Draw("colz");
	ps->Close();

	ofstream gains(gainfile);
	ofstream shortgain(gfiles);
	char line[500];

	int nGood = 0;
	int nZero = 0;
	for(int i=0; i<ntowers; i++){
		double fitMaximum = gaussian_fit[i]->GetMaximum(0.,100.);
		double mipPeak = gaussian_fit[i]->GetX(fitMaximum, 0., 100.);
		double fitMean = gaussian_fit[i]->GetParameter(1);
		
		int hentries = mip_histo[i]->GetEntries();
		double hmean = mip_histo[i]->GetMean();
		double hrms = mip_histo[i]->GetRMS();
		double fNDF = gaussian_fit[i]->GetNDF();
		double fchi = gaussian_fit[i]->GetChisquare();
		double chi = 0;
		if(fNDF > 0)chi = fchi/fNDF;

		//if(TMath::Abs(mipPeak-fitMean)>0.001) cout<<i<<"  "<<fitMean<<"  "<<mipPeak<<endl;
		
		double gain = 0;
		double fSig = 0;
		double fMeanErr = 0;
		double fSigErr = 0;
		double fCon = 0;
		double fConErr = 0;
		double mInt = 0;
		if(mipPeak > 0 && fitMean > 0){
		  float eta = getTowerEta(i+1);
		  float theta=2.*TMath::ATan(TMath::Exp(-eta));
		  gain = 0.264*(1.+0.056*eta*eta)/(TMath::Sin(theta)*fitMean);
		  fSig = gaussian_fit[i]->GetParameter(2);
		  fSigErr = gaussian_fit[i]->GetParError(2);
		  fCon = gaussian_fit[i]->GetParameter(0);
		  fConErr = gaussian_fit[i]->GetParError(0);
		  fMeanErr = gaussian_fit[i]->GetParError(1);
		  mip_histo[i]->GetXaxis()->SetRangeUser(fitMean-2*fSig,fitMean+2*fSig);
		  mInt = mip_histo[i]->Integral();
		  if((i+1) % 20 == 0){
		    mip_histo[i]->GetXaxis()->SetRangeUser(6.,100.);
		  }else{
		    mip_histo[i]->GetXaxis()->SetRangeUser(6.,50.);
		  }
		}		

		sprintf(line,"%-4i,%i,%2.3f,%2.3f,%2.3f,%i,%2.3f,%2.3f,%2.3f,%2.3f,%2.3f,%2.3f,%2.3f,%2.3f",i+1,hentries,hmean,hrms,gain,mipstatus[i],chi,fCon,fConErr,fitMean,fMeanErr,fSig,fSigErr,mInt);
		if(mipstatus[i]==1)nGood++;
		if(mipstatus[i]==0)nZero++;
		gains<<line<<endl;
		shortgain<<i+1<<" "<<fitMean<<" "<<fMeanErr<<" "<<mipstatus[i]<<endl;
	}


	outfile->cd();
	for(int i = 0; i < ntowers; i++){
	  mip_histo[i]->Write();
	}
	etaphi->Write();
	statcode->Write();
	outfile->Close();

	cout<<"finished tower fits"<<endl;
	cout<<"nGood = "<<nGood<<", nZero = "<<nZero<<endl;
	//print fit stats to a file
	//cout<<"saved fit params in "<<argv[3]<<endl;
	gains.close();
	shortgain.close();
}
			
void drawTower(TH1* h, TF1* f, int id, int status, CalibrationHelperFunctions* helper){		
	//calculate a few quantities
	double peak = f->GetParameter(1);
	double mean = f->Mean(5.,200.);
	double histo_height = h->GetBinContent(h->GetMaximumBin());
	if(histo_height == 0) histo_height = 1.;
	
	int xLatexBin = 20;

	//histogram options
	h->SetXTitle("ADC");
	h->Draw("e");
	
	//draw a line through the location of the MIP peak
	char line_name[50];
	sprintf(line_name,"mip_peak_%i",id);
	TH1* line = new TH1F(line_name,line_name,h->GetNbinsX(),h->GetBinLowEdge(1),h->GetBinLowEdge(h->GetNbinsX()+1));
	line->SetLineColor(kRed);
	line->Fill(peak,1.e8);
//	line->Draw("same");
	
	//write the tower number on the plot
	char tower_title[100];
	char teta[100];
	char tphi[100];
	sprintf(teta,"e:%1.2f",helper->getEta(id));
	sprintf(tphi,"p:%1.2f",helper->getPhi(id));
	TLatex eta_latex;
	TLatex phi_latex;
	eta_latex.SetTextSize(0.1);
	phi_latex.SetTextSize(0.1);
	eta_latex.DrawTextNDC(0.6,0.5,teta);
	phi_latex.DrawTextNDC(0.6,0.3,tphi);
	sprintf(tower_title,"%i",id);//,f->GetParameter(1));
	TLatex title_latex;
	title_latex.SetTextSize(0.15);
	if(status!=1) title_latex.SetTextColor(kRed);
//	title_latex.DrawLatex(xLatexBin,0.94*histo_height,tower_title);
	title_latex.DrawTextNDC(0.13,0.78,tower_title);
	if(status!=1){
	  char tower_code[100];
	  sprintf(tower_code,"%i",status);
	  TLatex status_latex;
	  status_latex.SetTextSize(0.15);
	  status_latex.SetTextColor(kRed);
	  status_latex.DrawTextNDC(0.47,0.78,tower_code);
	}
/*	
	//write the value of the MIP peak
	char tower_peak[100];
	sprintf(tower_peak,"%2.3f",peak);
	TLatex peak_latex;
	peak_latex.SetTextSize(0.15);
//	peak_latex.DrawLatex(xLatexBin,0.70*histo_height,tower_peak);
	
	//write the width of the MIP peak
	char tower_width[100];
	sprintf(tower_width,"%2.3f",f->GetParameter(2));
	TLatex width_latex;
	width_latex.SetTextSize(0.15);
//	width_latex.DrawLatex(xLatexBin,0.46*histo_height,tower_width);
	
	//write the corresponding full-scale energy
	char tower_energy[100];
	float eta = getTowerEta(id);
	double transverse_gain = 0.261*(1.+0.056*eta*eta)/peak;
	sprintf(tower_energy,"%2.3f",4066*transverse_gain);
//	sprintf(tower_energy,"%i:%i:%i",(int)h->GetBinCenter(h->GetMaximumBin()),(int)h->GetBinContent(0),(int)h->GetBinContent(h->GetNbinsX()+1));
//	sprintf(tower_energy,"%2.2f/%i",f->GetChisquare(),f->GetNDF());
	TLatex energy_latex;
	energy_latex.SetTextSize(0.15);
//	energy_latex.DrawLatex(xLatexBin,0.22*histo_height,tower_energy);	
	
	//write if there is a problem with iso or p/E
//	char tower_probs[100];
	TLatex tower_probs;
	tower_probs.SetTextSize(0.25);
	tower_probs.SetTextColor(6);
	if(isBadPoverE2005(id))		tower_probs.DrawLatex(1,0.5*histo_height,"bad p/E");
	if(isBadIso2005(id))		tower_probs.DrawLatex(1,0.5*histo_height,"bad iso");
	else if(isBadTower2005(id) && !isPmtNew(id))	tower_probs.DrawLatex(1,0.5*histo_height,"bad 2005");
	
	if(id==3541)				tower_probs.DrawLatex(1,0.5*histo_height,"begin");
	if(id==4100)				tower_probs.DrawLatex(1,0.5*histo_height,"end");
*/
	
}

float getTowerEta(int id){
	float eta = (id%20)*0.05-0.025;
	if(eta < 0.02) eta = 0.97;
	return eta;
}

TString towerQA(TH1* h, TF1* f){
	TString QA = "";
	
	if(h->GetEntries() < 1)													QA += "empty";
	if(h->GetBinContent(0) > 5)												QA += "under";
	if(h->GetBinContent(h->GetNbinsX()+1) > 5)								QA += "over";
	if(h->GetBinCenter(h->GetMaximumBin()) < 10.)							QA += "lowmax";
	if(f->GetParameter(1) < 0.)												QA += "negfit";
	
	return QA;
}

bool isBadPoverE2005(int id){
	switch(id){
		case(30): case(95): case(108): case(162): case(308): case(533): case(555): case(750):
		case(762): case(779): case(873): case(882): case(899): case(1024): case(1130): case(1132): 
		case(1197): case(1204): case(1217): case(1237): case(1257): case(1294): case(1306): case(1375):
		case(1434): case(1487): case(1537): case(1709): case(1984): case(2043): case(2162): case(2339):
		case(2392):
			return true;
		default:
			return false;
	}
}

bool isBadIso2005(int id){
	switch(id){
		case(34): case(35): case(266): case(267): case(286): case(287): case(561): case(562):
		case(615): case(616): case(633): case(653): case(637): case(657): case(649): case(650):
		case(673): case(674): case(813): case(814): case(837): case(857): case(953): case(954): 
		case(1026): case(1046): case(1353): case(1354): case(1574): case(1575): case(1753): case(1773):
		case(1765): case(1766): case(1897): case(1898): case(2073): case(2093): case(2077): case(2097): 
		case(2440): case(2460): case(2589): case(2590): case(3070): case(3071): case(3494): case(3495):
		case(4677): case(4678):
			return true;
		default: 
			return false;
	}
}

bool isBadTower2005(int id){
	switch(id){
		case(36): case(45): case(46): case(47): case(48): case(50): case(59): case(63): case(139): case(220):
		case(297): case(303): case(389): case(411): case(426): case(446): case(448): case(485): case(492):
		case(528): case(559): case(565): case(638): case(671): case(691): case(738): case(744): case(761):
		case(846): case(855): case(875): case(897): case(916): case(1028): case(1080): case(1100): case(1104):
		case(1160): case(1176): case(1220): case(1280): case(1317): case(1397): case(1398): case(1400): 
		case(1417): case(1419): case(1420): case(1440): case(1471): case(1505): case(1612): case(1668):
		case(1676): case(1679): case(1720): case(1764): case(1856): case(1866): case(1880): case(1909): 
		case(2069): case(2074): case(2075): case(2079): case(2092): case(2161): case(2168): case(2241):
		case(2257): case(2285): case(2378): case(2394): case(2403): case(2409): case(2458): case(2460):
		case(2470): case(2504): case(2529): case(2592): case(2610): case(2658): case(2794): case(2834):
		case(2835): case(2863): case(2865): case(2897): case(2961): case(2969): case(2972): case(3069):
		case(3086): case(3093): case(3097): case(3232): case(3255): case(3283): case(3289): case(3309): 
		case(3372): case(3407): case(3515): case(4171): case(4217): case(4232): case(4240): case(4312): 
		case(4357): case(4377): case(4506): case(4507): case(4508): case(4514): case(4519): case(4543): 
		case(4558): case(4560): case(4580): case(4585): case(4639): case(4660): case(4671): case(4678): 
		case(4766): case(4768):
			return true;
		default:
			return false;
	}
}
	
bool isPmtNew(int id){
	switch(id){
		case(10): case(11): case(30): case(36): case(63): case(66): case(91): case(272): case(274): case(279):
		case(297): case(303): case(313): case(318): case(326): case(361): case(367): case(390): case(412):
		case(448): case(528): case(565): case(596): case(691): case(738): case(744): case(761): case(774): 
		case(855): case(875): case(897): case(1317): case(1400): case(1471): case(1476): case(1505): case(1507):
		case(1529): case(1721): case(1764): case(1808): case(1872): case(1873): case(1881): case(1883):
		case(1913): case(1924): case(1976): case(2020): case(2186): case(2187): case(2195): case(2238): 
		case(2255): case(2257): case(2285): case(2378):
			return true;
		default: 
			return false;
	}
}

bool isBadTower2006(int id){
	switch(id){
		//copy bad towers from 2005, remove a bunch of good ones
		case(50): case(139): case(220):
		case(389): case(411): case(426): case(446): case(485): case(492):
		case(638): case(671):
		case(846): case(855): case(875): case(916): case(1028): case(1080): case(1100):
		case(1160): case(1176): case(1220): case(1280): case(1397):  
		case(1612): case(1668):
		case(1720): case(1856): case(1880):  
		case(2074): case(2075): case(2079): case(2092): case(2168):
		case(2257): case(2394): case(2403): case(2409): case(2458):
		case(2470): case(2504): case(2529): case(2610): case(2658): case(2794): case(2834):
		case(2865): case(2897): case(2961): case(2969): case(2972):
		case(3097): case(3255): case(3289):
		case(3372): case(3515): case(4171): case(4217): case(4240):
		case(4357): case(4377): case(4506): case(4507): case(4508): case(4514): case(4543): 
		case(4560): case(4580): case(4585): case(4671):
		case(4766): case(4768):
			return true;
			
		//copy bad iso from 2005, dropped 1897-8 (should this have been 1877-8?)
		case(34): case(35): case(266): case(267): case(286): case(287): case(561): case(562):
		case(615): case(616): case(633): case(653): case(637): case(657): case(649): case(650):
		case(673): case(674): case(813): case(814): case(837): case(857): case(953): case(954): 
		case(1026): case(1046): case(1353): case(1354): case(1574): case(1575): case(1753): case(1773):
		case(1765): case(1766): case(2073): case(2093): case(2077): case(2097): 
		case(2440): case(2460): case(2589): case(2590): case(3070): case(3071): case(3494): case(3495):
		case(4677): case(4678):
			return true;
		
		//new bad towers
		case(240): case(390): case(391): case(392): case(409): case(410): case(412): case(504): case(541): case(594):
		case(629): case(639): case(647): case(681): case(749): case(760): case(839): case(840): case(844): 
		case(859): case(873): case(880): case(933): case(1018): case(1125): case(1142): case(1143): 
		case(1158): case(1159): case(1161): case(1162): case(1163): case(1171): case(1180): case(1198):
		case(1217): case(1224): case(1225): case(1237): case(1240): case(1244): case(1250):
		case(1301): case(1319): case(1321): case(1341): case(1342): case(1348): case(1375): case(1381): case(1401):
		case(1422): case(1507): case(1588): case(1608): case(1654): case(1732): case(1779): case(1838): 
		case(1892): case(1893): case(1949): case(1985): case(2005): case(2021): case(2025): case(2070): case(2085):
		case(2094): case(2095): case(2101): case(2105): case(2108): case(2116): case(2177): case(2188): case(2196):
		case(2222): case(2262): case(2301): case(2305): case(2337): case(2453): case(2580): case(2633): case(2652):
		case(2727): case(3007): case(3017): case(3154): case(3171): case(3220): case(3231): case(3287): case(3290):
		case(3296): case(3328): case(3333): case(3493): case(3508): case(3544): case(3557): case(3588): case(3604):
		case(3611): case(3653): case(3678): case(3679): case(3709): case(3715): case(3727): case(3745): case(3746):
		case(3761): case(3769): case(3795): case(3803): case(3986): case(4014): case(4105): case(4016): case(4107): 
		case(4018): case(4019): case(4020): case(4053): case(4054): case(4055): case(4056): case(4057): case(4075): 
		case(4077): case(4078): case(4080): case(4100): case(4119): case(4120): case(4130): case(4174): case(4326):
		case(4388): case(4459): case(4464): case(4505): case(4549): case(4569): case(4596): case(4672): case(4684):
		case(4765):
			return true;
			
		//a few more I missed the first time around
		case(341):  case(900):  case(1044): case(1063): case(1078): case(1850): case(1877): case(1878): case(3738):
		case(4015): case(4017): case(1048): case(3690): case(3718):
			return true;
			
		default:
			return false;
	}
}
