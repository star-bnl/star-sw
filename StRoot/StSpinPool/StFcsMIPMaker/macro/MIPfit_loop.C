#include "TH1.h"
#include "TF1.h"
#include "TROOT.h"
#include "TStyle.h"
#include "TMath.h"
#include "TFile.h"
#include "TGraph.h"
#include "TCanvas.h"
  
Double_t langaufun(Double_t *x, Double_t *par) {

      // Numeric constants
      Double_t invsq2pi = 0.3989422804014;   // (2 pi)^(-1/2)
      Double_t mpshift  = -0.22278298;       // Landau maximum location

      // Control constants
      Double_t np = 100.0;      // number of convolution steps
      Double_t sc =   5.0;      // convolution extends to +-sc Gaussian sigmas

      // Variables
      Double_t xx;
      Double_t mpc;
      Double_t fland;
      Double_t sum = 0.0;
      Double_t xlow,xupp;
      Double_t step;
      Double_t i;

      // MP shift correction
      mpc = par[1] - mpshift * par[0];

      // Range of convolution integral
      xlow = x[0] - sc * par[3];
      xupp = x[0] + sc * par[3];

      step = (xupp-xlow) / np;

      // Convolution integral of Landau and Gaussian by sum
      for(i=1.0; i<=np/2; i++) {
         xx = xlow + (i-.5) * step;
         fland = TMath::Landau(xx,mpc,par[0]) / par[0];
         sum += fland * TMath::Gaus(x[0],xx,par[3]);

         xx = xupp - (i-.5) * step;
         fland = TMath::Landau(xx,mpc,par[0]) / par[0];
         sum += fland * TMath::Gaus(x[0],xx,par[3]);
      }
      return ((par[2]*step*sum*invsq2pi/par[3]));
}

TF1 *langaufit(TH1F *his, Double_t *fitrange, Double_t *startvalues, Double_t *parlimitslo, Double_t *parlimitshi, Double_t *fitparams, Double_t *fiterrors)
{
   // Once again, here are the Landau * Gaussian parameters:
   //   par[0]=Width (scale) parameter of Landau density
   //   par[1]=Most Probable (MP, location) parameter of Landau density
   //   par[2]=Total area (integral -inf to inf, normalization constant)
   //   par[3]=Width (sigma) of convoluted Gaussian function
   //
   // Variables for langaufit call:
   //   his             histogram to fit
   //   fitrange[2]     lo and hi boundaries of fit range
   //   startvalues[4]  reasonable start values for the fit
   //   parlimitslo[4]  lower parameter limits
   //   parlimitshi[4]  upper parameter limits
   //   fitparams[4]    returns the final fit parameters
   //   fiterrors[4]    returns the final fit errors

   Int_t i;
   Char_t FunName[100];

   sprintf(FunName,"Fitfcn_%s",his->GetName());

   cout<<" NAME = "<<FunName<<endl;
   TF1 *ffitold = (TF1*)gROOT->GetListOfFunctions()->FindObject(FunName);
   if (ffitold) delete ffitold;

   TF1 *ffit = new TF1(FunName,langaufun,fitrange[0],fitrange[1],4);
   ffit->SetParameters(startvalues);
   ffit->SetParNames("Width","MP","Area","GSigma");

   for (i=0; i<4; i++) {
     ffit->SetParLimits(i, parlimitslo[i], parlimitshi[i]);
   }
   
   his->Fit(FunName,"RB0");   // fit within specified range, use ParLimits, do not plot
   
   ffit->GetParameters(fitparams);    // obtain fit parameters
   for (i=0; i<4; i++) {
     fiterrors[i] = ffit->GetParError(i);     // obtain fit parameter errors
   }
   return (ffit);
}

//Background fit funciton
Double_t background(Double_t *x, Double_t *par){
  return (par[0]*TMath::Power(x[0],par[1]));
}

//total fit funct with convolution applied
Double_t total_wconvo(Double_t *x, Double_t *par){
  return (background(x,par) + langaufun(x,&par[2]));
}                                       

void MIPfit_loop(int rebinFactor= 10, int minTow = 0, int maxTow = 63, int MIPrange=325, TString filename="minbias_checkmaker.root"){
  
  TFile *file=new TFile(filename);
  TString outputFile="mip_fits.pdf";
  TString openOutput=outputFile+"(";
  TString closeOutput=outputFile+")";
 
  cout<<" YOU'RE READING IN..."<<filename<<endl;
  cout<<" YOU'RE READING OUT..."<<outputFile<<endl;

  TH1F *h[64],*h_Clone[64];
  TH1F *MIPbgsub1[64], *MIPbg[64];
  TF1 *bg[64], *mip[64], *mipconvo, *mipconvo2;
  TString title,mip_title,bg_title;
  char mipbg_t[100],mipbg_n[100],mipbgsub_t[100],mipbgsub_n[100];
  Int_t xmin,xmax;
  Int_t fitxmin=60, fitxmax=150;
  TAxis *y,*x;

  TH1F *chisq_edge=new TH1F("chiqr2","MIP Chisquare/DOF",100,0,10);
  TH1F *chisq=new TH1F("chiqr","MIP Chisquare/DOF",100,0,10);
  
  float chisq_values[64];
  float mostprob_values[64];
  float id_list[64];

  int towids[64]={7,6,5,4,3,2,1,0,15,14,13,12,11,10,9,8,23,22,21,20,19,18,17,16,31,30,29,28,27,26,25,24,39,38,37,36,35,34,33,32,47,46,45,44,43,42,41,40,55,54,53,52,51,50,49,48,63,62,61,60,59,58,57,56};

  int edgetowids[15]={7,15,23,31,39,47,55,63,62,61,60,59,58,57,56}; 
  float edgetowids_length=sizeof(edgetowids)/sizeof(edgetowids[0]);

  for(int id=minTow; id<maxTow+1; id++){
    
    fitxmax=150;

    //give edge tows a different start fit range
    for(int i=0; i<edgetowids_length; i++){
      if(edgetowids[i]==id){
	fitxmax=700;
      }
    }

    ///Set up background fit
    bg_title="background_id";
    bg_title+=id;
    bg[id]=new TF1(bg_title, "[0]*x^[1]",fitxmin,fitxmax);
    

    //get tower MIP spectrum
    title="adcsum_det1_id";
    title+=id;
    cout<<"*****************************************"<<endl;
    cout<<"*****************************************"<<endl;
    cout<<"*********GETTING HISTOGRAM...#"<<id<<endl;
    h[id]=(TH1F*)file->Get(title);
    h[id]->Rebin(rebinFactor);
    x=h[id]->GetXaxis();                                                     
    x->SetTitle("ADC Sum");
    x->SetRangeUser(20,900);
    
    // clone histogram for first iteration of fit
    h_Clone[id]=(TH1F*)h[id]->Clone();
    
    //define histograms for MIP fit and MIP bg sub
    sprintf(mipbg_t,"mipbackground id%d",id);
    sprintf(mipbg_n,"mipbackground_id%d",id);
    
    int nbinX_hold=h[id]->GetNbinsX();
    const int nbinX=nbinX_hold;
    float binwidth=h[id]->GetBinWidth(nbinX);
    float hist_nbins=nbinX*rebinFactor;
    float hist_range=binwidth*nbinX;
    
    MIPbg[id]= new TH1F(mipbg_n,mipbg_t,hist_nbins,0,hist_range);
    MIPbg[id]->Rebin(rebinFactor);
    MIPbg[id]->Sumw2();
    
    sprintf(mipbgsub_t,"mipbackground1_sub_id%d",id);
    sprintf(mipbgsub_n,"mipbackground1_sub_id%d",id);
    MIPbgsub1[id]= new TH1F(mipbgsub_n,mipbgsub_t,hist_nbins,0,hist_range);
    MIPbgsub1[id]->Rebin(rebinFactor);
    MIPbgsub1[id]->Sumw2();
    
    //apply background fit to cloned histogram for subtraction
    h_Clone[id]->Fit(bg[id],"","",fitxmin,fitxmax);
    
    //obtain fit params for bg fit, to see if fit failed
    Double_t bgparam[4];
    bg[id]->GetParameters(&bgparam[0]);
    float bgwidth=bgparam[0];
    float bgmp=bgparam[1];
    cout<<"BG PARAMS: "<<bgwidth<<" "<<bgmp<<endl;
    
    //if bg fit failed, change fit range to find one that works
    for(int j=0; j<=14;j++){
      if(bgwidth<100){//if fit failed(small/zero fit params), try new fit max
	
	fitxmax=150+50*j;
	h_Clone[id]->Fit(bg[id],"","",fitxmin,fitxmax);
	
	cout<<"TRYING FITMAX= "<<fitxmax<<endl;
	bg[id]->GetParameters(&bgparam[0]);
	bgwidth=bgparam[0];	  
      }
    }
    
    //fill background histograms
    for(int bin=4; bin<nbinX; bin++){
      float XbinCenter=x->GetBinCenter(bin);//get x value of bin center
      float bg_at_center=bg[id]->Eval(XbinCenter);//evaluate the fit at that xval
      MIPbg[id]->SetBinContent(bin,bg_at_center);
    }
    MIPbgsub1[id]->Add(h[id],MIPbg[id],1,-1);//perform background sub 
    
    //SET UP CONVOLUTION FIT
    ////// Setting fit range and start values
    Double_t fr[2];
    Double_t sv[4], pllo[4], plhi[4], fp[4], fpe[4];
    
    ////// get most probable value to set fit range
    float ncounts[nbinX];
    for(int bin=4; bin<nbinX; bin++){
      ncounts[bin]=MIPbgsub1[id]->GetBinContent(bin);//counts in the bin      
    }
    float mostprob=ncounts[nbinX-1];//initialize mpv to last element in array
    int mostprob_bin=nbinX-1;
    float mostprob_x=0;
    mostprob_x=x->GetBinCenter(mostprob_bin);

    int flag=0;//stop searching when there are 4 points to the left of peak
    for(int i=nbinX-1; i>=0; i--){//loop backwards through array
      
      if((ncounts[i]>mostprob)&&(flag<4)){ //find local max
	mostprob=ncounts[i];//most prob value
	mostprob_bin=i;//bin of most prob value
	mostprob_x=x->GetBinCenter(mostprob_bin);

	flag=0;
      }
      if(mostprob_x>MIPrange){//MIPrange is anticipated xmax location of MIP
	flag=0;
      }
      else if((ncounts[i]<=mostprob)&&(mostprob_bin!=nbinX-1)){
	flag++;
      }
    }
    mostprob_x=x->GetBinCenter(mostprob_bin);
    fr[0]=0.6*mostprob_x; // <-- fit range xmin
    fr[1]=1.3*mostprob_x; // <-- fit range xmax
    
    cout<<"MIP FIT RANGE IS: "<< fr[0] <<" -- "<<fr[1]<<endl;
    
    pllo[0]=1; pllo[1]=0.6*mostprob_x; pllo[2]=10.0; pllo[3]=5.0;
    plhi[0]=10; plhi[1]=1.3*mostprob_x; plhi[2]=8000.0; plhi[3]=100.0;
    sv[0]=1; sv[1]=mostprob_x; sv[2]=1000.0; sv[3]=20;
    
    //fit bg subtracted MIP spectra w convolution
    mipconvo = langaufit(MIPbgsub1[id],fr,sv,pllo,plhi,fp,fpe);
    MIPbgsub1[id]->Fit(mipconvo,"R+");
    MIPbgsub1[id]->GetListOfFunctions()->Add(mipconvo);
    mipconvo->SetNpx(10000);

    Double_t param[4];
    mipconvo->GetParameters(&param[0]);
    float width=param[0];
    float mp=param[1];
    float area=param[2];
    float gsigma=param[3];

    Double_t param2[4];
    mipconvo->GetParameters(&param2[0]);
    float mpv2=param2[1];
    mostprob_values[id]=mpv2;
    id_list[id]=id;

    //get chi2/dof for each fit    
    float chi=mipconvo->GetChisquare();
    float DOF=mipconvo->GetNDF();
    float chiDOF=chi/DOF;
    chisq_values[id]=chiDOF;

    chisq->Fill(chiDOF); 
  }
  
  //plot MIPs
  gStyle->SetOptStat(0);
  TCanvas *c=new TCanvas("c","mip fit",1100,900);
  
  for (int id =minTow; id < maxTow+1; id++)
    {
      c->cd(towids[id]+1); 
      c->SetGrid(0,0);
      c->SetLogy();

      x1=MIPbgsub1[id]->GetXaxis();
      x1->SetTitle("ADC Sum");
      x1->SetRangeUser(20,900);  

      MIPbgsub1[id]->Draw();
      MIPbgsub1[id]->SetLineColor(kBlue);
      
      //print MIPs to multi-page pdf
      if(id==0){
	c->Print(openOutput, "pdf");
      }   
      else if(id==63){
	c->Print(closeOutput,"pdf");
      }
      else{
	c->Print(outputFile, "pdf");
      }
      
    }    
  
  //plot chisq/dof for each tower 
  TCanvas *d=new TCanvas("d","av chisqr",1100,900);
  chisq_edge->SetLineColor(kBlue);
  chisq->Draw();
  chisq_edge->Draw("same");
  d->Print("mip_chisqr.pdf");
  
  
  //plot most probable values of MIP for each tower
  TCanvas *e=new TCanvas("e", "mean values",1100,900);
  TGraph* mean_vs_id=new TGraph(64,id_list,mostprob_values);
  mean_vs_id->GetXaxis()->SetTitle("Ecal Tower ID");
  mean_vs_id->GetYaxis()->SetTitle("Most Prob. Value (ADCSum)");
  mean_vs_id->SetMarkerStyle(21);
  mean_vs_id->SetMarkerSize(2);
  mean_vs_id->SetMarkerColor(2);
  mean_vs_id->Draw("AP");
  e->Print("mip_mpvs.pdf");
    
}

