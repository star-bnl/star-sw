#include "commonmacro/common.h"
#include "common/Name.cc"
#include "commonmacro/histutil.h"

void yieldVphiEW(const char* inName=
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
  TCanvas c1("c1","c1",400,500);
  TH1* ha, *hb;
  float dcaMin=-1.5,dcaMax=1.5;

  
  gStyle->SetOptStat(0);
  
  Int_t nBase = 2;

  char* baseName[] = { 
    "h3PhiPrDcaXYGlPtPr",
      "h3PhiGlDcaXYGlPtGl"
      };
  const int nPt=4;
  float ptary[]={1.5,2,3,4,6};

  //--------------------------------------------------------------
  // type b : slices
  
  // pr pt and gl pt cut
  for(int i=0; i<nBase; i++){
    cout << border << endl;
    
    TH1D* h1[2][nPt];    
    TH3* h3; TH2* h2;
    TLegend legend(.1,.1,.25,.2);
    legend.SetFillColor(kWhite); legend.SetBorderSize(0);
      
    for(int ic=0; ic<2; ic++){      
      // get the 3d histogram
      setName(name,baseName[i],sPM[ic].Data());
      h3 = (TH3D*) inRoot.Get(name);
      
      // project 2d 
      h2 = 
	HistSlice(h3,"dummy2d","",0,dcaMin,dcaMax,"zx","e");
      
      // slices 1d
      for(int is=0; is<nPt; is++){
	h1[ic][is]=HistSlice(h2,"dummy1d","",0,
			     ptary[is],ptary[is+1],"x","e");
      }
    }// ic
    float min,max;
    // canvas names
    sprintf(title,"yield v phi %s %s slices (%s) (cut %d)",
	    h2->GetTitle(),h3->GetZaxis()->GetTitle(),side.Data(),cut);
    sTitle=title;
    sprintf(name,"yieldVphi%s%s",h3->GetZaxis()->GetTitle(),side.Data());
    sName=name;
   
    Divide(&c1,2,2,sTitle.Data(),inName);
    for(int iSlice=0; iSlice<nPt;iSlice++){
      ha=h1[0][iSlice];  //pos 
      hb=h1[1][iSlice]; //neg
      ha->SetMarkerStyle(4); 
      hb->SetMarkerStyle(8);
      
      max = FindMax(ha,hb);
      min = FindMin(ha,hb);  
      SetMinMax(ha,min*0.5,max*1.2);
      
      c1.cd(iSlice+1); ha->Draw("e"); 
      hb->Draw("esame");
	gPad->SetGridx(); gPad->SetGridy();
      
      if(iSlice==0){
	legend.AddEntry(h1[0][0],"plus","p");
	legend.AddEntry(h1[1][0],"minus","p");
	legend.Draw();
      }
    }
    Print(&c1,psDir,sName.Data());

    // plus+minus
    sName += "_sum"; sTitle += "sum";
    Divide(&c1,2,2,sTitle.Data(),inName);
    for(int iSlice=0; iSlice<nPt;iSlice++){
      TH1* ha = (TH1*)h1[0][iSlice]->Clone();
      ha->Add(h1[1][iSlice]);
      
      SetMinMax(ha,ha->GetMinimum()*0.5,ha->GetMaximum()*2.2);
      
      c1.cd(iSlice+1); ha->Draw("e"); 
      //      PrintMeanRmsYield(ha,0.2,0.8,0.04);
      gPad->SetGridx(); gPad->SetGridy();
      
    }
    Print(&c1,psDir,sName.Data());
    
    
    //	delete[][] h1;
      
    
  }
 

}
