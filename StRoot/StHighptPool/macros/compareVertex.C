#include "commonmacro/common.h"
#include "common/Name.cc"
#include "commonmacro/histutil.h"

void compareVertex(const char* inName=
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
  TFile* embedRoot=0;
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
    cout << "cannot find " << more << endl;
  }
  TH1* h1[2]={0}; //real,embed

  h1[0]=(TH1*)inRoot.Get("h1VtxZCentCut");
  h1[1]=(TH1*)embedRoot.Get("h1VtxZCentCut");
  if(!h1[0] || !h1[1]) { cout << "cannot find histo" << endl; return; }

  int nrebin=10;
  h1[0]->Rebin(nrebin); h1[1]->Rebin(nrebin);
  TH1* ha, *hb;

  char* type[] = {"real","embed"};

  TCanvas c1("c1","c1",400,500);

  float minZ=-80,maxZ=80;
  TText* t=new TText;
  gStyle->SetOptStat(0);

  TLegend l(0.15,0.65,0.25,0.85);
  l.SetFillColor(kWhite); l.SetBorderSize(0);
  //-------------------------------------------------------------
  int lowBin=h1[0]->GetXaxis()->FindBin(minZ);
  int upBin =h1[1]->GetXaxis()->FindBin(maxZ-0.0001);

  // show both separately
  sprintf(title,"vertexZ (cut %d)",cut);
  Divide(&c1,1,2,title,inName);
  for(int i=0; i<2; i++){
    c1.cd(i+1); h1[i]->SetTitle(title); h1[i]->Draw(); 
    PrintMeanRms(h1[i],0.7,0.8);
  }
  Print(&c1,psDir,"vertexZ");
  
  // scale them
  sprintf(title,"vertex z scaled (cut %d)",cut);
  Divide(&c1,1,2,title,inName);
  c1.cd(1); ha=(TH1*)h1[0]->Clone(); hb=(TH1*)h1[1]->Clone();
  ha->SetTitle("scaled");
  ha->Scale(1./ha->Integral());
  hb->Scale(1./hb->Integral());
  ha->SetMarkerStyle(4); hb->SetMarkerStyle(8);
  ha->Draw("p"); hb->Draw("psame");
  l.AddEntry(ha,"real","p"); l.AddEntry(hb,"embed","p"); l.Draw();
  
  c1.cd(2); ha=(TH1*)h1[0]->Clone(); hb=(TH1*)h1[1]->Clone();
  ha->SetTitle("weighted");
  cout << "min = " << ha->GetXaxis()->GetBinLowEdge(lowBin)
       << ",max = " << ha->GetXaxis()->GetBinUpEdge(upBin) << endl;

  double realPeak = ha->Integral(lowBin,upBin);
  double realOut = ha->Integral()-realPeak;
  double realFrac = realPeak/realOut;
  double embedPeak = hb->Integral(lowBin,upBin);
  double embedOut = hb->Integral()-hb->Integral(lowBin,upBin);
  double embedFrac = embedPeak/embedOut;
  double weight=realFrac/embedFrac;

  cout << "real frac="<<realFrac <<", embedFrac="<<embedFrac
       << ", weight = " << weight << endl;
  for(int i=lowBin;i<=upBin;i++){
    hb->SetBinContent(i,hb->GetBinContent(i)*weight);
    //    hb->SetBinError(i,hb->GetBinError(i)*weight);
  }
  ha->Scale(1./ha->Integral()); hb->Scale(1./hb->Integral());
  ha->SetMarkerStyle(4); hb->SetMarkerStyle(8);
  ha->Draw("p"); hb->Draw("psame");
  sprintf(name,"weight %.2f",weight); t->DrawTextNDC(0.15,0.75,name);

  Print(&c1,psDir,"vertexZScaled");
 
}




  
  


    
