#include "commonmacro/common.h"
#include "commonmacro/histutil.h"
#include "common/Name.cc"

void spectra(const char* inName=
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

  TH1* h1[2]; TH1* ha[2]; TH1* hRatio;
  TGraphAsymmErrors* g;

  TCanvas c1("c1","c1",400,500);
  float minpt=2,maxpt=6;
  //---------------------------------------------------
  // histogram ratios
  const int nBin=2; const int nBase=1; const int nCharge=3;
  char* baseName[] = {"gSpecCorrected","gSpecCorrected" };
  char* baseTitle[] = { "raw","corrected"};
  char* charge[] = { 0,"Plus","Minus"};
  int markerStyle[] = {4,8,2};
  float min=1e-5,max=10.;

  gStyle->SetOptStat(0); 
  gStyle->SetTitleBorderSize(0);
  for(int iBin=0; iBin<nBin; iBin++){
    for(int iCharge=0; iCharge<nCharge; iCharge++){	
 
      sprintf(title,"bin %d %s (cut %d)",
	      iBin,charge[iCharge],cut);
      //      Divide(&c1,1,1,title,inName);
      
      c1.Clear();c1.cd(1); gPad->SetTickx(); gPad->SetTicky();
      gPad->SetGridx(); gPad->SetGridy();
      gPad->SetLogy();

      for(int iBase=0; iBase<nBase; iBase++){
	setName(name,baseName[iBase],iBin,charge[iCharge]);
	g=(TGraphAsymmErrors*)inRoot->Get(name);
	g->SetMinimum(min); g->SetMaximum(max);
	sTitle=g->GetTitle();
	g->SetTitle(title);
	g->Draw("ap");
      }
      sprintf(title,"%s",g->GetTitle());
      Print(&c1,psDir,sTitle.Data());
       
     }
  }
}
  
