#include "commonmacro/common.h"
#include "common/Name.cc"
#include "commonmacro/histutil.h"

void compareFitPtsVeta(const char* inName=
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
    char* trigger = (strstr(inName,"central")) ? "central" :"minbias";
    sprintf(name,"~/wrk/assoc/links/P01hi.central.HighpT_piminus_%d.hist/assoc_cut%d.hist.root",(int)extraValue,cut);
  }
  else{
    cout << "Unknown embed file" << endl; return;
  }

  TString half = "";
  if(strstr(inName,"east")){ half="east"; }
  if(strstr(inName,"west")){ half="west"; }

  embedRoot = new TFile(name);
  cout << "--------------------------" << endl;
  cout << "in name=" << inName << endl
       << "ps dir=" << psDir << endl
       << "cut=" << cut << endl
       << "embed name=" << name << endl
       << "half = " << half << endl;
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
  TProfile* p[2];

  TH3* h3a; TH2* h2a;

  h3[0]=(TH3*)inRoot.Get("Minus.h3VtxZFitPtsEtaPr");
  h3a=(TH3*)inRoot.Get("Plus.h3VtxZFitPtsEtaPr");
  if(!h3a){cout << "cannot find h3a" << endl; return; }
  h3[0]->Add(h3a);
  h3[1]=(TH3*)embedRoot.Get("Minus.h3VtxZFitPtsEtaPr");
  h3a=(TH3*)embedRoot.Get("Plus.h3VtxZFitPtsEtaPr");
  h3[1]->Add(h3a);
  
  char* type[] = {"real","mc"}; int npt=4;
  int marker[] = {4,8};
  char* opt[] = { "p","psame"};
  char* optp[] = {"e","esame"};
  char ctitle[100],cname[100];

  TCanvas c1("c1","c1",400,500);
  TText* t=new TText;
  gStyle->SetOptStat(0);

  Cut::SetCut(cut);
  float fitPtsCut=20;

  float vtxAry[]= {-150,-125,-100,-75,-50,-25,0,25,50,75,100,125,150};
  int nVtx = 12;
  float etaAry[][2] = { { .1,.3 }, {.5,.7}, {-.3,-.1}, {-.7,-.5}};
  int nEta = 4;
  
  //-------------------------------------------------------------

  
  for(int iEta=0; iEta<nEta; iEta++){
    //*** vtx z slices
    
    sprintf(title,"fitpts (range %d-45), vtx z slices (%.1f < eta < %.1f) (cut %d)",
	    fitPtsCut,etaAry[iEta][0],etaAry[iEta][1],cut);
    Divide(&c1,3,4,title,inName);
    
    
    for(int iVtx=0;iVtx<nVtx; iVtx++){
      c1.cd(iVtx+1);
      for(int i=0; i<2; i++){
	 h2[i]=(TH2*)HistSlice(h3[i],"","",0,etaAry[iEta][0],etaAry[iEta][1],"xy");
	 h1[i]=(TH1*)HistSlice(h2[i],"",h2[i]->GetTitle(),0,
			       vtxAry[iVtx],vtxAry[iVtx+1],"x");
	 SetRange(h1[i]->GetXaxis(),fitPtsCut,46);
	 if(h1[i]->Integral()) h1[i]->Scale(1./h1[i]->Integral());
	 h1[i]->SetMarkerSize(0.5); SetMinMax(h1[i],0.001,.12);
	 h1[i]->SetMarkerStyle(marker[i]); h1[i]->Draw(opt[i]);	   
	 PrintMeanRms(h1[i],0.1,0.8-.2*i,0.06);
      }
    }
    sprintf(name,"fitPtsVtxZSlicesEtaBin%d",iEta);
    Print(&c1,psDir,name);
  }
  //*** mean fit pts V vtx
  
  sprintf(title,"mean fit pts (range %d-45) V vtx z (cut %d)",
	  fitPtsCut,cut);
  Divide(&c1,2,2,title,inName);

  for(int iEta=0; iEta<nEta; iEta++){
    c1.cd(iEta+1); gPad->SetGridx(); gPad->SetGridy();
    TLegend* l=new TLegend(0.1,0.1,0.3,0.2);
    for(int i=0; i<2; i++){
      h2[i]=(TH2*)HistSlice(h3[i],"","",0,etaAry[iEta][0],etaAry[iEta][1],"xy","e");
      p[i]=Profile(h2[i],"",h2[i]->GetTitle(),0,fitPtsCut,46,"y","e");
      SetMinMax(p[i],30,40);
      SetRange(p[i]->GetXaxis(),vtxAry[0],vtxAry[nVtx]);
      p[i]->SetMarkerStyle(marker[i]); p[i]->Draw(optp[i]);
      if(iEta==0){
	l->AddEntry(p[i],type[i],"p");
      }
      
    }
  }
  sprintf(title,"fitPtsMeanVz");
  Print(&c1,psDir,title);
    
}
    
