#include "commonmacro/common.h"
#include "commonmacro/histutil.h"
#include "common/Name.cc"

void compareSpectra(const char* inName=
		    "links/P01hi.minbias.2000.hist/hianalysis_1000.hist.root",
		    const char* psDir="ps",
		    int cut = 1,
		    const char* outDir="./",
		    const char* more = "west",
		    float extraValue = 0)
{
   bool doScale= (extraValue>0);
   
  cout << "--------------------------" << endl;
  cout << "in name=" << inName << endl
       << "ps dir=" << psDir << endl
       << "cut=" << cut << endl
       << "scale? " << (doScale ? "yes" : "no") << endl;
  cout << "--------------------------" << endl;

 

  TFile* inRoot[2];
  inRoot[0] = new TFile(inName);
  inRoot[1] = new TFile(more); // second
  if(!inRoot[0] || !inRoot[1]){
    cout << "cannot find the infile" << endl;
    return;
  }
  cout << "file 2=" << more << endl;

  // deduce whether file name
  TString trigger[2]; trigger[0]=inName; trigger[1]=more;
  for(int i=0;i<2;i++){
    trigger[i].Remove(0,trigger[i].Last('/'));
    //    cout << "file " << i+1 << " " << trigger[i] << endl;
  }
 
  // deduce cut of 2nd file
  TString sCut = trigger[1];
  sCut.Remove(0,sCut.First("_")+1);
  sCut.Remove(sCut.First('.'),sCut.Length());
  sCut.ReplaceAll("cut",""); 
  cout << "cut2=" << sCut << endl;
    


  TH1* h1[2]; TH1* ha[2]; TH1* hRatio;
  TGraphAsymmErrors* g[2];

  TCanvas c1("c1","c1",400,500);
  float minpt=2,maxpt=6;
  //---------------------------------------------------
  // histogram ratios
  const int nBin=2; const int nBase=2; const int nCharge=3;
  char* baseName[] = {"h1Raw","h1Corrected" };
  char* baseTitle[] = { "raw","corrected"};
  char* charge[] = { 0,"Plus","Minus"};
  int markerStyle[] = {4,8,2};
  float ratioMin=0.2,ratioMax=1.5;

  gStyle->SetOptStat(0); 
  gStyle->SetTitleBorderSize(0);
  for(int iBin=0; iBin<nBin; iBin++){ // loop over binning
    for(int iCharge=0; iCharge<nCharge; iCharge++){ // loop over charge
 
      sprintf(title,"bin %d %s : %s/%s ",
	      iBin,charge[iCharge],
	      trigger[0].Data(),trigger[1].Data());
      //Divide(&c1,1,1,title,inName);
      
      c1.Clear();c1.cd(1); gPad->SetTickx(); gPad->SetTicky();
      gPad->SetGridx(); gPad->SetGridy();

      for(int iBase=0; iBase<nBase; iBase++){ //raw,corrected,etc
	for(int if=0; if<2; if++){ // different files
	  setName(name,baseName[iBase],iBin,charge[iCharge]);
	  h1[if]=(TH1*)inRoot[if]->Get(name);
	}
	 
	hRatio=(TH1*)h1[0]->Clone();
	ha[iBase]=hRatio; // save info for Tlegend
	hRatio->Divide(h1[1]); 
	SetRange(hRatio->GetXaxis(),minpt,maxpt);
	hRatio->SetMarkerStyle(8); SetMinMax(hRatio,ratioMin,ratioMax);
	// scale it 
	hRatio->SetTitle(title); //cout << h1[0]->GetTitle() << endl;
	hRatio->SetMarkerStyle(markerStyle[iBase]);
	if(doScale)
	  hRatio->Scale(1./hRatio->GetBinContent(hRatio->GetXaxis()->GetFirst()));
	/*
	for(int k=hRatio->GetXaxis()->GetFirst();
	    k<=hRatio->GetXaxis()->GetLast();k++){
	  cout << "\t" << hRatio->GetBinCenter(k) 
	       << "\t" << hRatio->GetBinContent(k) << endl;
	}
	*/
	if(iBase==0){
	  hRatio->Draw("e1");
	  hRatio->GetXaxis()->SetTitle("pT (GeV/c)");
	  //sTitle = h1[0]->GetName(); sTitle += "Ratio";
	}
	else{
	  hRatio->Draw("e1same");
	}

      } // base
      //DrawLine(h1[0]);
      if(nBase>1){
	TLegend* l=new TLegend(.7,.7,.85,.85); l->SetBorderSize(0);
	l->SetFillColor(0);
	l->AddEntry(ha[0],"raw","p");
	l->AddEntry(ha[1],"corrected","p");
	l->Draw();
      }
      sTitle=hRatio->GetName(); 
      sTitle.Remove(sTitle.Last('.')); //c1.Update();
      sprintf(title,"_cut%dcut%s",cut,sCut.Data());
      sTitle += title;
      Print(&c1,psDir,sTitle.Data());
       
    }
  }
}
  
