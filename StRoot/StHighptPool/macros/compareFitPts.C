#include "commonmacro/common.h"
#include "common/Name.cc"
#include "commonmacro/histutil.h"

void compareFitPts(const char* inName=
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
  char trigger[100];
  if(strstr(inName,"central")){
    strcpy(trigger,"central");
  }
  else{
    strcpy(trigger,"minbias");
  }

  if(more){
    strcpy(name,more);
  }
  else if(extraValue>0){
    // attempt to find the embed file
    sprintf(name,"~/wrk/assoc/links/P01hi.%s.HighpT_piminus_%d.hist/assoc_cut%d.hist.root",trigger,(int)extraValue,cut);
  }
  else{
    cout << "Unknown embed file" << endl; return;
  }

  TString half = "";
  if(strstr(inName,"east")){ half="east"; }
  if(strstr(inName,"west")){ half="west"; }

  
  inRoot2 = new TFile(name);
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
  if(!inRoot2){
    cout << "cannot find " << name << endl;
  }
  TH3* h3[2]; TH2* h2[2];
  TH1* h1[2]; //real,embed

  TH3* h3a; TH2* h2a;

  //real
  h3[0]=(TH3*)inRoot.Get("Minus.h3PhiPrFitPtsPtPr");
  h3a=(TH3*)inRoot.Get("Plus.h3PhiPrFitPtsPtPr");
  if(!h3a){cout << "cannot find h3a" << endl; return; }
  h3[0]->Add(h3a);

  //--- check if the second file is real data
  bool isReal=false;
  if(!strstr(name,"HighpT")) {
    cout << "###Second file is real" << endl;
    isReal=true;
  }
  h3[1]=(TH3*)inRoot2.Get("Minus.h3PhiPrFitPtsPtPr");
  if(1){ // always do plus+minus
    h3a=(TH3*)inRoot2.Get("Plus.h3PhiPrFitPtsPtPr");
    h3[1]->Add(h3a);
  }
  char* real2="P00hm";
  //----

  char* type[] = {"P01hi","mc"}; 
  int marker[] = {4,8};
  char* opt[] = { "e","esame"};
  
  if(isReal) type[1]=real2;

  int npt=4;
  float ptary[] = {2,3,4,5,6};
  char ctitle[100],cname[100];

  float maxFitPts=0;
  float minMean=0,maxMean=0;

  if(strcmp(trigger,"central")==0){
    maxFitPts=.14; minMean=30; maxMean=40;
  }
  else{
    maxFitPts=.24; minMean=35; maxMean=45;
  }


  TCanvas c1("c1","c1",400,500);
  TText* t=new TText;
  gStyle->SetOptStat(0);

  Cut::SetCut(cut);
  int fitPtsCut=Cut::mFitPts[0];
  float ptmin=2, ptmax=4;
  float fitptsary[] = { 3,fitPtsCut};
  char* fitptstitle[] = { "Wide","Cut"};
  //-------------------------------------------------------------

  for(int if=0;if<2;if++){  
    // *** fit pts summed over phi
    for(int i=0;i<2;i++){ //real, embed
      sprintf(name,"%s",type[i]);
      h2[i]=HistSlice(h3[i],name,"",0,-165,195,"zy","e");
    }

    sprintf(title,"fit pts range %d-45 (cut %d) %s",
	    fitptsary[if],cut,half.Data());
    Divide(&c1,2,2,title,inName);
    for(int ipt=0;ipt<npt;ipt++){
      c1.cd(ipt+1);
      for(int i=0;i<2;i++){
	
	h1[i]= HistSlice(h2[i],"","",0,ptary[ipt],ptary[ipt+1],"x","e");
	SetRange(h1[i]->GetXaxis(),fitptsary[if],45.5);
	h1[i]->Scale(1./h1[i]->Integral());
	if(i==0)SetMinMax(h1[i],0,maxFitPts);
	h1[i]->SetMarkerSize(0.6);
	h1[i]->SetMarkerStyle(marker[i]); h1[i]->Draw(opt[i]);

	PrintMeanRms(h1[i],0.15,0.8-.1*i);
      }
      if(ipt==0){
	 TLegend l(0.15,0.4,0.3,0.5); l.SetFillColor(kWhite); 
	 l.SetBorderSize(0);
	 l.AddEntry(h1[0],type[0],"p"); l.AddEntry(h1[1],type[1],"p");
	 l.Draw();
      }
    }
    sprintf(title,"fitPts%sC%s",fitptstitle[if],type[1]);
    Print(&c1,psDir,title);

    //*** fit pts distribution, phi slices
    // combine pt 1.5-4
    // one plot for each charge type

    /*
    sprintf(cname,"fitPtsPhiSlices%sC%s",fitptstitle[if],type[1]);	    
    sprintf(ctitle,"fit pts range %d-45, phi slices (%.1f<ptPr<%.1f)%s (cut %d)",
	    fitptsary[if],ptmin,ptmax,half.Data(),cut);
    Divide(&c1,3,4,ctitle,inName);
      
    h2[0]=(TH2*)HistSlice(h3[0],"","",0,ptmin,ptmax,"xy");
    h2[1]=(TH2*)HistSlice(h3[1],"","",0,ptmin,ptmax,"xy");
    for(int iPhi=1; iPhi<=h2[0]->GetNbinsY(); iPhi++){
      c1.cd(iPhi);
      TAxis *axis = h2[0]->GetYaxis();
      for(int i=0;i<2;i++){
	sprintf(title,"%d<phi<%d",
		axis->GetBinLowEdge(iPhi),axis->GetBinUpEdge(iPhi));
	sprintf(name,"%s%s%s",fitptstitle[if],type[i],title);
	SetRange(h2[i]->GetYaxis(),fitptsary[if],46);
	h1[i] = h2[i]->ProjectionX(name,iPhi,iPhi);
	h1[i]->Scale(1./h1[i]->Integral());
	h1[i]->SetMarkerSize(0.5);
	SetRange(h1[i]->GetXaxis(),fitptsary[if],45.5);
	SetMinMax(h1[i],0,maxFitPts);
	h1[i]->SetTitle(title); 
	h1[i]->SetMarkerStyle(marker[i]); h1[i]->Draw(opt[i]);

      
	PrintMeanRms(h1[i],0.1,0.8-.2*i,0.08);  
      }
    }
    Print(&c1,psDir,cname);
     */
  
    // profile
    sprintf(cname,"fitPtsMeanVPhi%sC%s",fitptstitle[if],type[1]);
    sprintf(ctitle,"mean fit pts V phi (%.1f<ptPr<%.1f) %s %d-45 (cut %d)",
	    ptmin,ptmax,half.Data(),fitptsary[if],cut);
    Divide(&c1,1,1,ctitle,inName);
    c1.cd(1); gPad->SetGridx(); gPad->SetGridy();
    for(int i=0;i<2;i++){
      h2a=(TH2*)HistSlice(h3[i],"","",0,ptmin,ptmax,"xy");
      sprintf(name,"meanVphi%s%s",type[i],fitptstitle[if]);
      int low=h2a->GetXaxis()->FindBin(fitptsary[if]);
      TProfile *p=h2a->ProfileY(name,low,9999); p->SetTitle("");
      SetMinMax(p,minMean,maxMean); 

      p->SetMarkerStyle(marker[i]); p->Draw(opt[i]);

      
    }
    Print(&c1,psDir,cname);
    
  } // fit pts type
}




  
  

