#include "commonmacro/common.h"
#include "common/Name.cc"
#include "commonmacro/histutil.h"

void dca(const char* inName=
	 "links/P01hi.minbias.2000.hist/hianalysis_1000.hist.root",
	 const char* psDir="ps",
	 int cut = 1,
	 const char* outDir="./",
	 const char* more = "west",
	 float extraValue = 1)
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
  TText* t = new TText;
  float ptAry[] = {2,3,4,5,6};
  char* ptType[] = { "PtPr","PtGl"};
  char* chargeType[]= { "Pos","Neg","PosPlusNeg"};
  TH1 *ha, *hb;
  TCanvas c1("c1","c1",400,500);
  TH3* h3[2][3]; // ptType; plus,minus,plus+minus
  TH2* h2a, *h2b; TH1* h1a, *h1b;
  float dcaCut[]={0,1}; float dcaXYCut[]={-1,1};
  char cname[100],ctitle[100];
  int nc=2;
  int npt=2;
  float maxDcaPlot=.2; float maxDcaXYPlot=.1;
  int nRebin=4;
  if(nRebin>1){cout << ">>Rebinning " << nRebin << endl;}

  gStyle->SetOptStat(0);
  //-------------------------------------------------------------------
  // 
  // plus,minus,plus+minus
  
  for(int ipt=0;ipt<npt;ipt++){
    //plus
    sprintf(name,"Plus.h3DcaGlDcaXYGl%s",ptType[ipt]);
    cout << name << endl;
    h3[ipt][0]=(TH3*)inRoot.Get(name);
    // minus
    sprintf(name,"Minus.h3DcaGlDcaXYGl%s",ptType[ipt]);
    cout << name << endl;
    h3[ipt][1]=(TH3*)inRoot.Get(name);
    // plus+minus
    h3[ipt][2]=(TH3*)h3[ipt][0]->Clone();
    sprintf(name,"PlusPlusMinus.h3DcaGlDcaXYGl%s",ptType[ipt]);
    h3[ipt][2]->SetName(name); h3[ipt][2]->SetTitle(name);
    h3[ipt][2]->Add(h3[ipt][1]);
  }
  //---------------------------------------------------------------
  // plot
  for(int ipt=0;ipt<npt;ipt++){ // pt type
    cout << "***** " << ipt << "****** " << endl;
    for(int ic=0;ic<nc;ic++){ // charge type
      cout << "\t****" << ic <<"*****" << endl;

      //**** dca3d vs dca xy
      sprintf(cname,"DcaVDcaXY%s%s",chargeType[ic],ptType[ipt]);
      sprintf(ctitle,"dca3d vs dca xy, %s slices (%s) (cut %d)",
	      ptType[ipt],chargeType[ic],cut);
      Divide(&c1,2,2,ctitle,inName);
      
      for(int j=0;j<4;j++){
	c1.cd(j+1); 
	h2a=(TH2*)HistSlice(h3[ipt][ic],"","",0,
			    ptAry[j],ptAry[j+1],"xy");
	h2a->Draw("box");
      }
      Print(&c1,psDir,cname);
      
      //**** dca3d
      sprintf(cname,"Dca%s%s",chargeType[ic],ptType[ipt]);
      sprintf(ctitle,"dca 3d, %s slices, (%s) (cut %d)",
	      ptType[ipt],chargeType[ic],cut);
      Divide(&c1,2,2,ctitle,inName);

      for(int j=0;j<4;j++){
	c1.cd(j+1);  gPad->SetLogy(1); 
	h2a=(TH2*)HistSlice(h3[ipt][ic],"","",0,
			    ptAry[j],ptAry[j+1],"yx");
	h1a=HistSlice(h2a,"",h2a->GetTitle(),0,0,0,"x");
	h1b=HistSlice(h2a,"",h2a->GetTitle(),0,
		      dcaXYCut[0],dcaXYCut[1],"x");
	//	cout << h1b->GetTitle() << endl;
	
	//h1a->Scale(1./h1a->Integral()); h1b->Scale(1./h1b->Integral());
	h1b->SetLineStyle(2);
	//SetMinMax(h1a);
	if(nRebin>1){h1a->Rebin(nRebin); h1b->Rebin(nRebin); }
	h1a->Draw(); h1b->Draw("same"); 

	PrintMeanRms(h1a,0.6,0.8);
	PrintMeanRms(h1b,0.6,0.6);
	
	
      }
      Print(&c1,psDir,cname);

      //**** dca xy
      sprintf(cname,"DcaXY%s%s",chargeType[ic],ptType[ipt]);
      sprintf(ctitle,"dca xy, %s slices, (%s) (cut %d)",
	      ptType[ipt],chargeType[ic],cut);
      Divide(&c1,2,2,ctitle,inName);

      for(int j=0;j<4;j++){
	c1.cd(j+1); gPad->SetLogy(1);
	h2a=(TH2*)HistSlice(h3[ipt][ic],"","",0,
			    ptAry[j],ptAry[j+1],"xy");
	h1a=HistSlice(h2a,"",h2a->GetTitle(),0,0,0,"x");
	h1b=HistSlice(h2a,"",h2a->GetTitle(),0,
		      dcaCut[0],dcaCut[1],"x");
	
	//h1a->Scale(1./h1a->Integral()); h1b->Scale(1./h1b->Integral());
	h1b->SetLineStyle(2);
	//SetMinMax(h1a); 
	if(nRebin>1){h1a->Rebin(nRebin); h1b->Rebin(nRebin); }
	h1a->Draw(); h1b->Draw("same");	
	PrintMeanRms(h1a,0.6,0.8);
	PrintMeanRms(h1b,0.6,0.6);
      }
      Print(&c1,psDir,cname);

    }
  }
}
			    
    
