#include "commonmacro/common.h"
#include "common/Name.cc"
#include "commonmacro/histutil.h"

void fitPts(const char* inName=
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

  gSystem->Load("StHiMicroEvent");
  gSystem->Load("StHiMicroAnalysis");
  Cut::SetCut(cut);
  Cut::ShowCuts();

  if(!inRoot){
    cout << "cannot find the infile" << endl;
    return;
  }
  TText* t = new TText;
  float ptAry[] = {2,3,4,5,6};
  char* ptType[] = { "PtPr","PtGl"};
  char* chargeType[]= { "Pos","Neg","PosPlusNeg"};
  TH1 *ha, *hb;
  TCanvas c1("c1","c1",400,500);
  TH3* h3[2][3]; // ptType; plus,minus,plus+minus
  TH2* h2a, *h2b; TH1* h1a, *h1b;
  float ptmin=1.5,ptmax=4.0;
  char cname[100],ctitle[100];
  int nc=2;
  int nptType=1;

  int fitPtsCut=Cut::mFitPts[0];
  float fitptsary[] = { 3,fitPtsCut};
  char* fitptstitle[] = { "Wide","Cut"};

  gStyle->SetOptStat(0);
  //-------------------------------------------------------------------
  // 
  // plus,minus,plus+minus
  
  for(int iptType=0;iptType<nptType;iptType++){
    //plus
    sprintf(name,"Minus.h3PhiPrFitPts%s",ptType[iptType]);
    cout << name << endl;
    h3[iptType][0]=(TH3*)inRoot.Get(name);
    // minus
    sprintf(name,"Plus.h3PhiPrFitPts%s",ptType[iptType]);
    h3[iptType][1]=(TH3*)inRoot.Get(name);
    // plus+minus
    h3[iptType][2]=(TH3*)h3[iptType][0]->Clone();
    sprintf(name,"PlusPlusMinus.h3PhiPrFitPts");
    h3[iptType][2]->SetName(name); h3[iptType][2]->SetTitle(name);
    h3[iptType][2]->Add(h3[iptType][1]);
  }
  //---------------------------------------------------------------
  for(int if=0;if<2; if++){
    for(int iptType=0;iptType<nptType;iptType++){

      //**** fit pts, Pos and Neg on same plot
      sprintf(cname,"fitPts%s%s",ptType[iptType],
	      fitptstitle[if]);
      sprintf(ctitle,"fit pts, %s slices  %d-45 (cut %d)",
	      ptType[iptType],fitptsary[if],cut);
      Divide(&c1,2,2,ctitle,inName);
    
      for(int j=0;j<4;j++){
	c1.cd(j+1);
	if(j==0){
	  TLegend l(0.1,0.4,0.3,0.5); l.SetBorderSize(0);
	  l.SetFillColor(0);
	}
	for(int ic=0;ic<nc;ic++){
	  h2a=(TH2*)HistSlice(h3[iptType][ic],"","",0,
			      ptAry[j],ptAry[j+1],"xy");
	  h1a=HistSlice(h2a,"",h2a->GetTitle(),0,0,0,"x");
	  SetRange(h1a->GetXaxis(),fitptsary[if],46);
	  h1a->SetMarkerSize(0.6);
	  if(ic<1){
	    SetMinMax(h1a,0,h1a->GetMaximum()*2.5);
	    h1a->SetMarkerStyle(4); 
	    h1a->Draw("p");
	    PrintMeanRms(h1a,0.1,.8);
	    
	  }
	  else if (ic==1){
	    h1a->SetMarkerStyle(8);
	    h1a->Draw("psame");
	    PrintMeanRms(h1a,0.1,.7);
	  }	
	  else if (ic==2){
	    h1a->SetMarkerStyle(12);
	    h1a->Draw("psame");
	    PrintMeanRms(h1a,0.1,.6);
	  }
  	  if(j==0) l.AddEntry(h1a,chargeType[ic],"p");
	}
	if(j==0) l.Draw();
      } // pt loop
      Print(&c1,psDir,cname);

      //*** fit pts distribution, phi slices
      // combine pt 1.5-4
      // one plot for each charge type
      
      for(int ic=0;ic<nc;ic++){
	sprintf(cname,"fitPts%s%sPhiSlices%s",
		ptType[iptType],chargeType[ic],fitptstitle[if]);
	sprintf(ctitle,"fit pts %d-45, phi slices (%.1f<%s<%.1f) %s (cut %d)",
		fitptsary[if],ptmin,ptType[iptType],ptmax,chargeType[ic],cut);
	Divide(&c1,3,4,ctitle,inName);
	
	h2a=(TH2*)HistSlice(h3[iptType][ic],"","",0,
			    ptmin,ptmax,"xy");
	for(int iPhi=1; iPhi<=h2a->GetNbinsY(); iPhi++){
	  c1.cd(iPhi);
	  TAxis *axis = h2a->GetYaxis();
	  sprintf(title,"%d<phi<%d",
		  axis->GetBinLowEdge(iPhi),axis->GetBinUpEdge(iPhi));
	  sprintf(name,"%s%s%s%s",
		  chargeType[ic],ptType[iptType],fitptstitle[if],title);
	  
	  h1a = h2a->ProjectionX(name,iPhi,iPhi);
	  h1a->SetMarkerStyle(8); h1a->SetMarkerSize(0.6);
	  h1a->SetTitle(title); SetRange(h1a->GetXaxis(),fitptsary[if],45.5);
	  h1a->Draw("p");
	  PrintMeanRms(h1a,0.1,0.6,0.08);
	}
	Print(&c1,psDir,cname);

      } // ic
      
      //** mean fit, profile vs phi
      for(int ic=0;ic<nc;ic++){
	sprintf(cname,"fitPtsMeanVPhi%s%s%s",
		ptType[iptType],chargeType[ic],fitptstitle[if]);
	sprintf(ctitle,"mean fit pts V phi %d-45 (%.1f<%s<%.1f) %s (cut %d)",
		fitptsary[if],ptmin,ptType[iptType],ptmax,chargeType[ic],cut);
	Divide(&c1,1,1,ctitle,inName);
	c1.cd(1); gPad->SetGridx(); gPad->SetGridy();
	h2a=(TH2*)HistSlice(h3[iptType][ic],"","",0,
			    ptmin,ptmax,"xy");
	int low=h2a->GetXaxis()->FindBin(fitptsary[if]);
	sprintf(name,"meanVphi%s%s%s",
		chargeType[ic],ptType[iptType],fitptstitle[if]);
	TProfile *p=h2a->ProfileY(name,low,9999); p->SetTitle("");
	SetMinMax(p,30,40);
	p->SetMarkerStyle(8);	
	p->Draw();     

	Print(&c1,psDir,cname);
	
      } // ic
      
    }
  }
}
			    
    
