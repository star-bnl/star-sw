#include "commonmacro/common.h"
#include "common/Name.cc"
#include "commonmacro/histutil.h"

void yieldVvtxZ(const char* inName=
		"links/P01hi.minbias.2000.hist/hianalysis_1000.hist.root",
		const char* psDir="ps",
		int cut = 1,
		const char* outDir="./",
		const char* more = "embeddingfile",
		   float extraValue = 0) 
{
 
  gSystem->Load("StHiMicroAnalysis");
 
  Cut::SetCut(cut);
  Cut::ShowCuts();

  TFile* inRoot = new TFile(inName);
  TFile* embedRoot;
  if(more){
    strcpy(name,more);
  }
  else if(extraValue>0){
    // attempt to find the embed file
    char* trigger = (strstr(inName,"central")) ? "central" :"minbias";
    sprintf(name,"~/wrk/assoc/links/P01hi.%s.HighpT_piminus_%d.hist/efficiency_cut%d.hist.root",trigger,(int)extraValue,cut);
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
  float minRealPt=2,maxRealPt=3;
  float minMcPt=2,maxMcPt=8;

  float minVtxZ=-160,maxVtxZ=160;
  float etaAry[][2] = { { .0,.1}, {.2,.3}, {.4,.5}, {.6,.7},
    {-.1,0}, {-.3,-.2},{-.5,-.4},{-.7,-.6} };
  const int nEta = 8; int nx=2; int ny=4;
  int nRebin=2;
  int nMcRebin=4;
  

  TH3* h3;
  TH3* h3Real; // 10 cm bins, 
  TH3* h3Mc[2]; // 5 cm bins

  TH2* h2Real,*h2Mc[2];
  TH1* h1Real[2]; // raw and corrected
  TH1* h1Mc[2][nEta]; // raw,matched
  TH1* h1VtxZ,*h1;

  // real stuff
  h3Real=(TH3*)inRoot.Get("Minus.h3VtxZEtaPrPtPr");
  h3 = (TH3*)inRoot.Get("Plus.h3VtxZEtaPrPtPr");
  h3Real->Add(h3);
  //slice off pt
  h2Real=HistSlice(h3Real,"","",0,minRealPt,maxRealPt,"yx","e");

  // mc stuff
  h3Mc[0]=(TH3*)embedRoot.Get("h3McRawVtxZEtaPt");
  h3Mc[1]=(TH3*)embedRoot.Get("h3MatchedVtxZEtaPt");

  h2Mc[0]=HistSlice(h3Mc[0],"","",0,minMcPt,maxMcPt,"yx","e");
  h2Mc[1]=HistSlice(h3Mc[1],"","",0,minMcPt,maxMcPt,"yx","e");

  h3=(TH3*)inRoot.Get("h3VtxXYZ");
  h1VtxZ=(TH3*)h3->Project3D("ze"); h1VtxZ->Rebin(nMcRebin);

  char* type[] = {"real","mc"}; 
  int marker[] = {4,8};
  char* opt[] = { "p","psame"};
  char* opte[] = {"e","esame"};
  float markerSize = 0.4;
  

  TCanvas c1("c1","c1",400,500);
  TText* t=new TText;
  gStyle->SetOptStat(0);

  Cut::SetCut(cut);
  
  //-------------------------------------------------------------
  // see what the eta slices look like
  TH1* hSlice = new TH1D("hSlice","hSlice",20,-200,200);
  SetRange(hSlice->GetXaxis(),minVtxZ,maxVtxZ);
  hSlice->SetMinimum(0); hSlice->SetMaximum(200);

  sprintf(title,"eta range(cut %d)",cut);
  TLine line;
  Divide(&c1,nx,ny,title,inName);
  for(int iEta=0;iEta<nEta;iEta++){
    c1.cd(iEta+1);

    sprintf(title,"%.1f<eta<%.1f",etaAry[iEta][0],etaAry[iEta][1]);
    h1=(TH1*)hSlice->Clone();
    h1->SetTitle(title);
    h1->Draw(); TAxis* axis=h1->GetXaxis();    

    for(int i=axis->GetFirst();i<=axis->GetLast();i++){
      float dip1 = 3.14159/2.-2.*atan(TMath::Exp(-etaAry[iEta][0]));
      float dip2 = 3.14159/2.-2.*atan(TMath::Exp(-etaAry[iEta][1]));
      
      //cout << "dip1=" << dip1*180./3.14159 
      //	   << ",dip2="<<dip2*180./3.14159 << endl;

      cout << "eta="<< etaAry[iEta][1] << ",z=" << 192*tan(dip2) << endl;

      float xmax1 = 200*tan(dip2) + axis->GetBinLowEdge(i);
      float xmax2 = 200*tan(dip1) + axis->GetBinLowEdge(i);

      float xmin = axis->GetBinLowEdge(i);
      line.SetLineStyle(2);
      line.SetLineColor(kBlue);
      line.DrawLine(xmin,0,xmax1,200);
      line.SetLineStyle(1);
      line.SetLineColor(kRed);
      line.DrawLine(xmin,0,xmax2,200);
    }
    line.SetLineStyle(1);
    line.SetLineColor(kBlack);
    line.DrawLine(0,0,0,200);

    line.DrawLine(minVtxZ,60,maxVtxZ,60);

  }
  sprintf(name,"etaRange");
  Print(&c1,psDir,name);

  //  return;
  // look at raw and matched counts

  sprintf(title,"raw and matched yield V vtxZ (eta slices) (cut %d)",cut);
  Divide(&c1,nx,ny,title,inName);
  for(int iEta=0;iEta<nEta;iEta++){
    c1.cd(iEta+1);
    for(int i=0; i<2; i++){
      h1Mc[i][iEta]=HistSlice(h2Mc[i],"",h2Mc[i]->GetTitle(),0,
			      etaAry[iEta][0],etaAry[iEta][1],"x","e");
      h1Mc[i][iEta]->SetMarkerStyle(marker[i]);
      if(nMcRebin>1)h1Mc[i][iEta]->Rebin(nMcRebin);
      if(i==0)SetMinMax(h1Mc[i][iEta],h1Mc[i][iEta]->GetMinimum()*0.2,
			h1Mc[i][iEta]->GetMaximum()*1.2);
      SetRange(h1Mc[i][iEta]->GetXaxis(),minVtxZ,maxVtxZ);
      h1Mc[i][iEta]->SetMarkerSize(markerSize);
      h1Mc[i][iEta]->Draw(opt[i]);
    }
  }
  sprintf(name,"mcYieldVvtxZEtaSlices");
  Print(&c1,psDir,name);

  // look at efficiency*acceptance
  sprintf(title,"efficiency*acceptance V vtxZ (eta slices) (cut %d)",cut);
  Divide(&c1,nx,ny,title,inName);
  for(int iEta=0;iEta<nEta;iEta++){
    c1.cd(iEta+1); gPad->SetGridx(); gPad->SetGridy();
    h1Mc[1][iEta]->Divide(h1Mc[0][iEta]); SetMinMax(h1Mc[1][iEta],0,1);
    h1Mc[1][iEta]->SetXTitle("vtxZ");
    h1Mc[1][iEta]->Draw("e"); 
   
  }
  sprintf(name,"efficiencyVvtxZEtaSlices");
  Print(&c1,psDir,name);

  // look at raw and uncorrected real yields V vtx Z
  sprintf(title,"raw and corrected yield/Nevent V vtxZ (eta slices) (cut %d)",cut);
  Divide(&c1,nx,ny,title,inName);
  for(int iEta=0;iEta<nEta;iEta++){
    c1.cd(iEta+1); gPad->SetGridx(); gPad->SetGridy();
    h1Real[0]=HistSlice(h2Real,"",h2Real->GetTitle(),0,
			etaAry[iEta][0],etaAry[iEta][1],"x","e");
    h1Real[0]->Rebin(nRebin);
    h1Real[0]->Divide(h1VtxZ); // uncorrected
    h1Real[1]=(TH1*)h1Real[0]->Clone(); // corrected
    h1Real[1]->Divide(h1Mc[1][iEta]);
    

    for(int i=0; i<2; i++){
      h1Real[i]->SetMarkerStyle(marker[i]);
      if(i==0)SetMinMax(h1Real[i],0,.5);
      SetRange(h1Mc[i][iEta]->GetXaxis(),minVtxZ,maxVtxZ);
      h1Real[i]->SetMarkerSize(markerSize);
      h1Real[i]->SetXTitle("vtxZ");
      h1Real[i]->Draw(opte[i]);
    }
  }
  Print(&c1,psDir,"yieldVvtxZEtaSlices");
  
}




  
  

