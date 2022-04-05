#include "common/Name.h"
#include "commonmacro/histutil.h"
#include "commonmacro/common.h"

//{kFive, kTen, kTwenty,kThirty, kForty, kFifty,kSixty, 
// kSeventy, kEighty, kTotal};

void yieldVcent(const char* inName=
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
 
   inRoot = new TFile(inName);

  if(!inRoot){
    cout << "cannot find the infile" << endl;
    return;
  }
  TCanvas c1("c1","c1",400,500);
  TString sName,sTitle; TH1* h1a,*h1b; TH3* h3a, *h3b; TH2* h2a,*h2b;
  int nSlice=9;
  //---------------------------------------------------
  // zdc cent
  int zdcMin[]={0,5,10,20,30,40,50,60,70,80};
  int zdcMax[]={5,10,20,30,40,50,60,70,80,100};

  gStyle->SetOptLogy(1); gStyle->SetOptStat(0);
  sprintf(name,"raw pt yield, zdc slices (cut %d)",cut);
  Divide(&c1,3,3,name,inName);
  h2a=(TH2*)inRoot.Get("Plus.h2ZDCCentralityPtPr");
  h2b=(TH2*)inRoot.Get("Minus.h2ZDCCentralityPtPr");
  for(int i=1; i<=nSlice; i++){
    c1.cd(i);
    sprintf(name,"zdc%d%s",i,sPM[0]);
    h1a=h2a->ProjectionY(name,i,i,"e");
    sprintf(name,"zdc%d%s",i,sPM[1]);
    h1b=h2b->ProjectionY(name,i,i,"e");
    h1a->SetMarkerSize(0.5); h1b->SetMarkerSize(0.5);
    h1a->SetMarkerStyle(4); h1b->SetMarkerStyle(8);
    sprintf(title,"zdc cent %d (%d-%d %)",
	    h2a->GetXaxis()->GetBinCenter(i),zdcMin[i-1],zdcMax[i-1]);
    h1a->SetTitle(title); SetRange(h1a->GetXaxis(),1.5,6);
    //h1a->SetMinimum(0);
    h1a->Draw(); h1b->Draw("same");
    printHighPtYield(h1a,h1b);
  }
  Print(&c1,psDir,"yieldPtZdcSlices");
  // flow cent
  gStyle->SetOptLogy(1); gStyle->SetOptStat(0);
  sprintf(name,"raw pt yield, flow slices (cut %d)",cut);
  Divide(&c1,3,3,name,inName);
  h2a=(TH2*)inRoot.Get("Plus.h2CentralityPtPr");
  h2b=(TH2*)inRoot.Get("Minus.h2CentralityPtPr");
  for(int i=1; i<=nSlice; i++){
    c1.cd(i);
    sprintf(name,"flow%d%s",i,sPM[0]);
    h1a=h2a->ProjectionY(name,i,i,"e");
    sprintf(name,"flow%d%s",i,sPM[1]);
    h1b=h2b->ProjectionY(name,i,i,"e");
    h1a->SetMarkerStyle(4); h1b->SetMarkerStyle(8);
    h1a->SetMarkerSize(0.5); h1b->SetMarkerSize(0.5);
    sprintf(title,"flow cent %d",h2a->GetXaxis()->GetBinCenter(i));
    h1a->SetTitle(title); SetRange(h1a->GetXaxis(),1.5,6);
    //h1a->SetMinimum(0);
    h1a->Draw(); h1b->Draw("same");
    printHighPtYield(h1a,h1b);
  }
  Print(&c1,psDir,"yieldPtFlowSlices");
}

void printHighPtYield(TH1* ha,TH1* hb)
{
  char buf[20]; TH1* h[2]; h[0]=ha; h[1]=hb;
  int minBin[2],maxBin[2],count(0);
  float min,max;
  float ptBins[]={4,4.5,5,5.5};
  int nBin=4;
  TText* text = new TText;

  for(int iBin=0;iBin<nBin;iBin++){
    count=0;
    for(int i=0; i<2; i++){
      if(!h[i]) break;
      minBin[i] = h[i]->GetXaxis()->FindBin(ptBins[iBin]);
      maxBin[i] = h[i]->GetXaxis()->FindBin(ptBins[iBin]);
      float integral=h[i]->Integral(minBin[i],maxBin[i]);
      count += integral;
      if(i==0){
	min=h[i]->GetXaxis()->GetBinLowEdge(minBin[i]);
	max=h[i]->GetXaxis()->GetBinUpEdge(maxBin[i]);
      }
    }
    sprintf(buf,"%.1f<pt<%.1f=%d",min,max,count);    
    text->DrawTextNDC(0.55,0.8-iBin*0.1,buf);
  }
  
}
