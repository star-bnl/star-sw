#include "commonmacro/common.h"
#include "common/Name.cc"
#include "commonmacro/histutil.h"

void zdcCent(const char* inName=
	    "links/P01hi.minbias.2000.hist/hianalysis_cut111.hist.root",
	    const char* psDir="ps",
	    int cut = 1,
	    const char* outDir="./",
	    const char* more = "west",
	    float extraValue = 0)
{
  gSystem->Clear();

  cout << "--------------------------" << endl;
  cout << "in name=" << inName << endl
       << "ps dir=" << psDir << endl
       << "cut=" << cut << endl;
  cout << "--------------------------" << endl;
 
  inRoot = new TFile(inName);

  gSystem->Load("StHiMicroEvent");
  gSystem->Load("StHiMicroAnalysis");
  Cut::SetCut(cut);
  Cut::ShowCuts();

  if(!inRoot){
    cout << "cannot find the infile" << endl;
    return;
  }

  gStyle->SetOptStat(0);

  TCanvas *c1=new TCanvas("c1","c1",400,500);
  //-------------------------------------------------------------------
  TH3* h3=inRoot->Get("h3ZdcHMinusVtxZ");
  
  // only -95,95 vtx
  float vtxMin=-95,vtxMax=95;

  TH2 *h2,*h2Clone; TProfile* p;
  h2 = HistSlice(h3,"","",0,vtxMin,vtxMax,"xy","e");

  // function?
  TF1* f=new TF1("f","[0]*exp(-[1])");

  Divide(c1,2,2,"zdc vs h-");
  c1->cd(1);
  h2->SetMinimum(0);
  h2->Draw("colz");

  /*
  c1->cd(2);
  gStyle->SetPalette(1,0);
  h2Clone=(TH2*)h2->Clone();
  h2Clone->Draw("colz");
  */

  c1->cd(3);
  p=h2->ProfileX();
  p->Draw("e");

  //  Print(c1,psDir,"zdcVhMinus");

}

			    
    
