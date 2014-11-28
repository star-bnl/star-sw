#include "commonmacro/common.h"
#include "common/Name.cc"
#include "commonmacro/histutil.h"

void rawSpectra(const char* inName=
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
  TH1* ha,*hb; TCanvas c1("c1","c1",400,500);
  gStyle->SetOptLogy(1); gStyle->SetPadGridX(1); gStyle->SetPadGridY(1);
  //---------------------------------------------
  const int nBin=2; float min=0; float max=1e6;
  for(int iBin=0;iBin<nBin;iBin++){
    sprintf(title,"raw yield bin %d (cut %d)",iBin,cut);
    Divide(&c1,1,3,title,inName);
    setName(name,"h1Raw",iBin); cout << name<< endl;
    c1.cd(1);ha=(TH1*)inRoot.Get(name); sTitle=name;
    ha->SetMarkerStyle(8);
    SetRange(ha->GetXaxis(),2,6); 
    ha->Draw();
    for(int ic=0;ic<2;ic++){
      setName(name,"h1Raw",iBin,sPM[ic].Data());
      c1.cd(ic+2); ha=(TH1*)inRoot.Get(name);
      ha->SetMarkerStyle(8);
      SetRange(ha->GetXaxis(),2,6);ha->Draw();
    }
    
    Print(&c1,psDir,sTitle.Data());
  }
}

  
