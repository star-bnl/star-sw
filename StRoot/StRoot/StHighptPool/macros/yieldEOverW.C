#include "commonmacro/common.h"
#include "commonmacro/histutil.h"

void yieldEOverW(TString inName=
		       "links/P01hi.minbias.2000.hist/hianalysis_1000.hist.root",
		       const char* psDir="ps",
		       int cut = 1,
		       const char* outDir="./",
		       const char* more = "west",
		       float extraValue = 0)
{
  
  if(!strstr(inName.Data(),"east")){
    cout << "input must be east" << endl; return;
  }

  TString inName2 = inName;
  inName2.ReplaceAll("east","west");

  TFile* inRoot[2];
  inRoot[0] = new TFile(inName.Data());
  inRoot[1] = new TFile(inName2.Data());

  cout << "--------------------------" << endl;
  cout << "in name east=" << inName << endl
       << "in name west=" << inName2 << endl
       << "ps dir=" << psDir << endl
       << "cut=" << cut << endl;
  cout << "--------------------------" << endl;

  if(!inRoot[0]){
    cout << "cannot find the infile" << endl;
    return;
  }

  TCanvas c1("c1","c1",400,500);
  
  TH1* h1[2][2];

  gStyle->SetOptStat(0); gStyle->SetPadGridX(1); gStyle->SetPadGridY(1);
  
  TString sName,sTitle;
  int j;
  float minpt=2,maxpt=6;
  float mindca=-1.5, maxdca=1.5;

  int n=2;
  char* baseName[] = { "h3PhiPrDcaXYGlPtPr","h3PhiGlDcaXYGlPtGl"};

  gStyle->SetPadTickY(1);

  //-------------------------------------------------------
  
  for(int i=0; i<n; i++){
    TLegend legend(.1,.1,.25,.2);
    TH3* h3;
    for(int iew=0; iew<2; iew++){
      for(int ic=0; ic<2; ic++){
	
	// get the 3d histogram
	sprintf(name,"%s.%s",sPM[ic].Data(),baseName[i]);
	h3 = (TH3*) inRoot[iew]->Get(name);
	//	cout << name << endl;
	
	// project 1d 
	h1[iew][ic] =
	  HistSlice(h3,name,"",
		    0,-165,195,0,mindca,maxdca,
		    "z","e");
	SetRange(h1[iew][ic]->GetXaxis(),minpt,maxpt);
		  
	sName=h1[iew][ic]->GetTitle();
	sName.Prepend(sEW[iew]); 
	h1[iew][ic]->SetTitle(sName.Data());

      }
      h1[iew][0]->SetMarkerStyle(4); h1[iew][1]->SetMarkerStyle(8);
      
    }
  
    // east/west
    sprintf(name,"yieldV%sew",h3->GetZaxis()->GetTitle());
    sName=name; 
    sprintf(title,"yield V %s %s (east/west)",h3->GetZaxis()->GetTitle(),
	    h1[0][0]->GetTitle());
    sTitle=title;
    
    
    Divide(&c1,1,3,title,inName);c1.SetName(sName.Data());
    
    int j=0; TLine* line=new TLine; 
    for(int ic=0; ic<2; ic++){
      h1a = (TH1*)h1[0][ic]->Clone();
      h1a->Divide(h1[1][ic]);
      h1a->SetMinimum(0.5); h1a->SetMaximum(1.6);
      ReplaceTitle(h1a,sEW[0].Data(),"");
      sprintf(title,"%s (east/west)",sPM[ic].Data());
      h1a->SetXTitle(h1[0][ic]->GetXaxis()->GetTitle());
      h1a->SetTitle(title);
      c1.cd(ic+1); h1a->Draw("e");
      
      TAxis* axis=h1a->GetXaxis();
      line->DrawLine(axis->GetBinLowEdge(axis->GetFirst()),1,
		     axis->GetBinUpEdge(axis->GetLast()),1);
    }
    h1a=(TH1*)h1[0][0]->Clone(); // sum east
    h1a->Add(h1[0][1]);
    h1b=(TH1*)h1[1][0]->Clone(); // sum west
    h1b->Add(h1[1][1]);
    h1a->Divide(h1b); h1a->SetMinimum(0.5); h1a->SetMaximum(1.6);
    sprintf(title,"plus+minus (east/west)");
    h1a->SetMinimum(0.8); h1a->SetMaximum(1.2);
    h1a->SetTitle(title); c1.cd(3); h1a->Draw("e");
    line->DrawLine(axis->GetBinLowEdge(axis->GetFirst()),1,
		   axis->GetBinUpEdge(axis->GetLast()),1);
    Print(&c1,psDir,c1.GetName());
    
  }  

  //--------------------
  // varying bins
  for(int iew=0; iew<2; iew++){
    for(int ic=0; ic<2; ic++){
      sprintf(name,"%s.%s",sPM[ic].Data(),"RawVarBin");
      h1[iew][ic]=(TH1*)inRoot[iew]->Get(name);
      h1[iew][ic]->Sumw2();
      //      cout << h1[iew][ic]->GetBinContent(4) << "\t" 
      //	   << h1[iew][ic]->GetBinError(4) << endl;
      h1[iew][ic]->SetAxisRange(minpt,maxpt-.001);
      dump(h1[iew][ic]);
    }
    h1[iew][0]->SetMarkerStyle(4); h1[iew][1]->SetMarkerStyle(8);
  }
   
  // east/west
  sprintf(name,"yieldVptVarBinew");
  sprintf(title,"yield V ptPr (var binning) (east/west)");    
  
  Divide(&c1,1,3,title,inName);c1.SetName(name);
  
  int j=0; TLine* line=new TLine; 
  for(int ic=0; ic<2; ic++){
    h1a = (TH1*)h1[0][ic]->Clone(); 
    h1a->Divide(h1[1][ic]);
    h1a->SetMinimum(0.5); h1a->SetMaximum(1.6);
    ReplaceTitle(h1a,sEW[0].Data(),"");
    sprintf(title,"%s (east/west)",sPM[ic].Data());
    h1a->SetXTitle(h1[0][ic]->GetXaxis()->GetTitle());
    h1a->SetTitle(title);
    c1.cd(ic+1); h1a->Draw("e");
    
    cout << h1a->GetBinContent(4) << "\t" << h1a->GetBinError(4) << endl;

    TAxis* axis=h1a->GetXaxis();
    line->DrawLine(axis->GetBinLowEdge(axis->GetFirst()),1,
		   axis->GetBinUpEdge(axis->GetLast()),1);
  }
  h1a=(TH1*)h1[0][0]->Clone(); // sum east
  h1a->Add(h1[0][1]);
  h1b=(TH1*)h1[1][0]->Clone(); // sum west
  h1b->Add(h1[1][1]);
  h1a->Divide(h1b); h1a->SetMinimum(0.5); h1a->SetMaximum(1.6);
  sprintf(title,"plus+minus (east/west)");
  h1a->SetMinimum(0.8); h1a->SetMaximum(1.2);
  h1a->SetTitle(title); c1.cd(3); h1a->Draw("e");
  line->DrawLine(axis->GetBinLowEdge(axis->GetFirst()),1,
		 axis->GetBinUpEdge(axis->GetLast()),1);
  Print(&c1,psDir,c1.GetName());
   

}
