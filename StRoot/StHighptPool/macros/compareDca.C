#include "commonmacro/common.h"
#include "common/Name.cc"
#include "commonmacro/histutil.h"

void compareDca(const char* inName=
		   "links/P01hi.minbias.2000.hist/hianalysis_1000.hist.root",
		   const char* psDir="ps",
		   int cut = 1,
		   const char* outDir="./",
		   const char* more = "embeddingfile",
		   float extraValue = 0) 
{
 
  gSystem->Load("StHiMicroEvent");
  gSystem->Load("StHiMicroAnalysis");
 
  TFile* inRoot = new TFile(inName);
  TFile* embedRoot;
  if(more){
    strcpy(name,more);
  }
  else if(extraValue>0){
    // attempt to find the embed file
    TString s=inName;
    char* trigger = (s.Contains("central")) ? "central" :"minbias";
    sprintf(name,"~/wrk/assoc/links/P01hi.central.HighpT_piminus_%d.hist/assoc_cut%d.hist.root",(int)extraValue,cut);
  }
  else{
    cout << "Unknown embed file" << endl; return;
  }

  embedRoot = new TFile(name);
  cout << "--------------------------" << endl;
  cout << "in name=" << inName << endl
       << "ps dir=" << psDir << endl
       << "cut=" << cut << endl
       << "embed name=" << name << endl;
  cout << "--------------------------" << endl;

  if(!inRoot){
    cout << "cannot find the infile" << endl;
    return;
  }
  if(!embedRoot){
    cout << "cannot find " << name << endl;
  }
  TH3* h3[2]; TH2* h2[2];
  TH1* h1[2]; //real,embed

  TH3* h3a;

  h3[0]=(TH3*)inRoot.Get("Minus.h3DcaGlDcaXYGlPtPr");
  //h3a=(TH3*)inRoot.Get("Plus.h3DcaGlDcaXYGlPtPr");
  //if(!h3a){cout << "cannot find h3a" << endl; return; }
  //h3[0]->Add(h3a);

  h3[1]=(TH3*)embedRoot.Get("Minus.h3DcaGlDcaXYGlPtPr");
  if(!h3[1]) { cout << "huh" << endl; return; }
  //h3a=(TH3*)embedRoot.Get("Plus.h3DcaGlDcaXYGlPtPr");
  //if(!h3a){cout << "cannot find h3a" << endl; return; }
  //h3[1]->Add(h3a);

  char* type[] = {"real","embed"}; int npt=4;
  float ptary[] = {2,3,4,5,6};

  for(int i=0;i<2;i++){ //real, embed
    sprintf(name,"%s",type[i]);
    h2[i]=HistSlice(h3[i],"","",0,0,3,"zy");
  }
  
  TCanvas c1("c1","c1",400,500);
  TText* t=new TText;
  gStyle->SetOptStat(0);

  //  Cut::SetCut(cut);
  //  int fitPtsCut=Cut::mFitPts[0];
  float dcaCutAry[] = { 3,1};
  char* dcatitle[] = { "Wide","Cut"};
  int nRebin=4;
  //-------------------------------------------------------------

  for(int id=0;id<2;id++){

    sprintf(title,"dcaXY |dca|<%.f (cut %d)",dcaary[id],cut);
    Divide(&c1,2,2,title,inName);
    for(int ipt=0;ipt<npt;ipt++){
      c1.cd(ipt+1); gPad->SetLogy();
      for(int i=0;i<2;i++){ // real and mc 
	h1[i]= HistSlice(h2[i],"","",0,ptary[ipt],ptary[ipt+1],"x");
	SetRange(h1[i]->GetXaxis(),-dcaary[id],dcaary[id]);
	if(nRebin>1)h1[i]->Rebin(nRebin);
	h1[i]->Scale(1./h1[i]->Integral());
	h1[i]->SetMaximum(.40);
	h1[i]->SetMarkerStyle(4+4*i);
	if(i==0){
	  h1[i]->SetLineColor(kBlue); h1[i]->Draw("");
	}
	else{
	  h1[i]->SetLineColor(kRed); h1[i]->Draw("same"); 
	}
	PrintMeanRms(h1[i],0.15,0.8-.1*i);
      }

      if(ipt==0){ // first pad
	 TLegend l(0.15,0.4,0.3,0.5); l.SetFillColor(kWhite); 
	 l.SetBorderSize(0);
	l.AddEntry(h1[0],"real","l"); l.AddEntry(h1[1],"mc","l");
	l.Draw();
      }
    }
    sprintf(title,"dcaXYGl%s",dcatitle[id]);
    Print(&c1,psDir,title);
  }
}
