#include "commonmacro/common.h"
#include "commonmacro/histutil.h"
#include "common/Name.cc"

void divideSpectra(const char* inName=
		    "links/P01hi.minbias.2000.hist/hianalysis_1000.hist.root",
		    const char* psDir="ps",
		    int cut = 1,
		    const char* outDir="./",
		    const char* more = "west",
		    float extraValue = 0)
{
  cout << "--------------------------" << endl;
  cout << "in name=" << inName << endl
       << "ps dir=" << psDir << endl
       << "cut=" << cut << endl;
  cout << "--------------------------" << endl;

 

  TFile* inRoot;
  inRoot = new TFile(inName);
  if(!inRoot){
    cout << "cannot find the infile" << endl;
    return;
  }

  TH1* h1[2];
  TGraphAsymmErrors* g[2];

  TCanvas c1("c1","c1",400,500);
  float minpt=2,maxpt=6;

  const int nCharge=1;
  char* charge[] = { 0,"Plus","Minus"};
  const int nBin=1; // different binning
  float ratioMin=0.2,ratioMax=1.5;
  float ptMin=2,ptMax=6;

  char name1[100],name2[100];

  int nPair=2;
  char* namePair[][2]={
    {"h1Raw","h1EffCorrected"},
      {"h1Corrected","h1EffCorrected"}
  };
  int nx=1,ny=2;
  
  gStyle->SetOptStat(0); gStyle->SetTitleBorderSize(0);
  gStyle->SetPadTickY(1); gStyle->SetPadTickX(1);
  //---------------------------------------------------

 
  // raw/efficiency corrected

  c1.Clear();

  for(int ib=0; ib<nBin; ib++){ // loop over binning
    for(int ic=0; ic<nCharge; ic++){ // loop over charge type

      sprintf(title,"spectra ratios (binning %d, cut %d)",cut);
      Divide(&c1,nx,ny,title,inName);

      for(int ip=0; ip<nPair; ip++){ // loop over pairs of hists
	c1.cd(ip+1); gPad->SetGridx(); gPad->SetGridy();
	setName(name1,namePair[ip][0],ib,charge[ic]);
	cout << name1 << "\t";
	h1[0]=(TH1*)inRoot->Get(name1);
	setName(name2,namePair[ip][1],ib,charge[ic]);
	cout << name2 << endl;
	h1[1]=(TH1*)inRoot->Get(name2);
	
	sprintf(title,"%s / %s",name1,name2);
	cout << h1[0]->GetBinContent(5) 
	     << "\t" << h1[1]->GetBinContent(5) << endl;

	h1[0]->Divide(h1[1]);
	h1[0]->SetTitle(title);
	h1[0]->Draw("e"); 
	//	SetMinMax(h1[0],ratioMin,ratioMax);
	h1[0]->SetAxisRange(ptMin,ptMax-.00001);

	h1[0]->SetMinimum(); h1[0]->SetMaximum(1.2);
      } //ip
      
      if(ic==0)
	sprintf(title,"bin%d.divideSpectra",ib);
      else
	sprintf(title,"bin%d.%s.divideSpectra",ib,ic);
      Print(&c1,psDir,title);
    } // ic
  } // ib
}
