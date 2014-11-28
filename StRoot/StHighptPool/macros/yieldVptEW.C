#include "commonmacro/common.h"
#include "common/Name.cc"
#include "commonmacro/histutil.h"

void yieldVptEW(const char* inName=
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
 
  TString side;
  if(strstr(inName,"west")){ side="west"; }
  else if(strstr(inName,"east")){ side="east"; }
  else {side =more; }

  inRoot = new TFile(inName);

  if(!inRoot){
    cout << "cannot find the infile" << endl;
    return;
  }
  
  
  TText* t = new TText;

  Stat_t max1, max, max2, min,min1,min2,maxary[2],minary[2];
  Stat_t dcaCut = 1.2;

  TH1 *ha, *hb;
  TCanvas c1("c1","c1",400,500);
  float dcaXYMin=-1.5,dcaXYMax=1.5;
  float phiMin=-165,phiMax=195;
  float ptMin=1.5,ptMax=6;
  gStyle->SetPadGridX(1); gStyle->SetPadGridY(1);
  gStyle->SetOptStat(0);
  
  const Int_t nBase = 2;
  
  // these are the histograms to get from the root file
  //
  char* baseName[] = { 
    "h3PhiGlDcaXYGlPtGl","h3PhiPrDcaXYGlPtPr"
      };
  TH1* h1[nBase][2]; // pt/ plus,minus
  //-------------------------------------------------------------------
  // 
  // type a :2 pads.  east on top, west on bottom. plus, minus on same pad.
  
 
  for(int i=0;i<nBase;i++){

    TH3* h3; TH2* h2;
    for(int ic=0; ic<2; ic++){
      
      // get the 3d histogram
      setName(name,baseName[i],sPM[ic].Data());
      h3 = (TH3*) inRoot.Get(name);      
      //cout << name << endl;
	
      // project 2d 
      h2 = HistSlice(h3,"dummy2d","",0,
		  phiMin,phiMax,"yz","e");
      //      cout << h2->GetTitle() << endl;
	
      // project 1d
      h1[i][ic] =
	HistSlice(h2,"dummy1d","",0,
		  dcaXYMin,dcaXYMax,"x","e");
    }
    
    h1[i][0]->SetMarkerStyle(4); h1[i][1]->SetMarkerStyle(8);
      
    max = FindMax(h1[i][0],h1[i][1]);
    min = FindMin(h1[i][0],h1[i][1]);
    
    //
    // yields
    //
    sprintf(title,"yield v %s %s (%s) (cut %d)",
	    h3->GetZaxis()->GetTitle(),h2->GetTitle(),side.Data(),cut);
    

    Divide(&c1,1,2,title,inName);
    TLegend legend(.1,.1,.25,.2);
    
    //SetMinMax(h1[0],min*0.5,max*1.2);
    sprintf(title,"%s (%s)",h1[i][0]->GetTitle(),side.Data());
    h1[i][0]->SetTitle(title);
    c1.cd(1);h1[i][0]->Draw("e1"); h1[i][1]->Draw("e1same");
    SetRange(h1[i][0]->GetXaxis(),ptMin,ptMax);
    gPad->SetLogy();

    legend.AddEntry(h1[i][0],"plus","p");
    legend.AddEntry(h1[i][1],"minus","p");
    legend.SetFillColor(kWhite);
    legend.Draw();
    
    // sum plus and minus
    ha=(TH1*)h1[i][0]->Clone();
    ha->Add(h1[i][1]);
    //SetMinMax(ha,min,max*2);
    sprintf(title,"plus+minus %s",side.Data());
    ha->SetTitle(title);
    SetRange(ha->GetXaxis(),ptMin,ptMax);
    c1.cd(2);ha->Draw("e1");
    gPad->SetLogy();

    sprintf(name,"yieldV%s%s",h3->GetZaxis()->GetTitle(),side.Data());
    Print(&c1,psDir,name); 
  }

  //
  // minus/plus
  //
  sprintf(title,"yield neg/pos v pT %s (%s)(cut %d)",
	  h2->GetTitle(),side.Data(),cut);
  
  Divide(&c1,1,2,title,inName);
  
  for(int iBase=0; iBase<2; iBase++){
    ha = (TH1*)h1[iBase][1]->Clone();
    ha->Divide(h1[iBase][0]);
    ha->SetMinimum(0.5); ha->SetMaximum(1.5);
    sprintf(title,"neg/pos V %s",h1[iBase][0]->GetXaxis()->GetTitle()); 
    ha->SetTitle(title);
    SetRange(ha->GetXaxis(),ptMin,ptMax);
    c1.cd(iBase+1);ha->Draw("e1");
    TLine* line=new TLine; 
    line->DrawLine(ha->GetXaxis()->GetBinLowEdge(ha->GetXaxis()->GetFirst()),1,
		   ha->GetXaxis()->GetBinUpEdge(ha->GetXaxis()->GetLast()),1);
    sprintf(name,"yieldNegOverPosVpt%s",side.Data());
  }
  Print(&c1,psDir,name); 
  
  

}
