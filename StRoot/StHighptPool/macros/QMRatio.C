#include "commonmacro/common.h"
#include "commonmacro/histutil.h"
#include "common/Name.cc"

void QMRatio(const char* inName=
	     "links/P01hi.minbias.2000.hist/hianalysis_1000.hist.root",
	     const char* psDir="ps",
	     int cut = 1,
	     const char* outDir="./",
	     char* more = "west",
	     float extraValue = 0)
{
   bool doScale=(extraValue>0);
   
  cout << "--------------------------" << endl;
  cout << "in name=" << inName << endl
       << "ps dir=" << psDir << endl
       << "cut=" << cut << endl
       << "scale? " << (doScale ? "yes" : "no") << endl;
  cout << "--------------------------" << endl;

  char qm[100]; strcpy(qm,"finish_central_cut1_QM.hist.root");
  more=qm;

  TFile* inRoot[2];
  inRoot[0] = new TFile(inName);
  inRoot[1] = new TFile(more); // second
  if(!inRoot[0] || !inRoot[1]){
    cout << "cannot find the infile" << endl;
    return;
  }
  cout << "file 2=" << more << endl;

  // deduce whether file name
  TString trigger[2]; trigger[0]=inName; trigger[1]="QM";
  trigger[0].Remove(0,trigger[0].Last('/'));

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
  char* oldcharge[] = {0,"plus","minus"};

  gStyle->SetOptStat(0); 
  gStyle->SetTitleBorderSize(0);
  for(int iCharge=0; iCharge<nCharge; iCharge++){	
    
    sprintf(title,"%s : %s/%s ",
	    charge[iCharge],
	    trigger[0].Data(),trigger[1].Data());
    //Divide(&c1,1,1,title,inName);
      
    c1.Clear();c1.cd(1); gPad->SetTickx(); gPad->SetTicky();
    gPad->SetGridx(); gPad->SetGridy();

    for(int iBase=0; iBase<nBase; iBase++){ //raw,corrected,etc
      setName(name,baseName[iBase],0,charge[iCharge]); // bin0
      h1[0]=(TH1*)inRoot[0]->Get(name);
      if(iBase==0){
	setName(name,baseName[iBase],1,oldcharge[iCharge]); // bin1
      }
      else{
	setName(name,baseName[iBase],1,3,oldcharge[iCharge]);
      }
      //      cout << name << endl;
      h1[1]=(TH1*)inRoot[1]->Get(name);
      if(!h1[1])return;
      
      hRatio=(TH1*)h1[0]->Clone();
      ha[iBase]=hRatio; // save info for Tlegend
      hRatio->Divide(h1[1]); 
      SetRange(hRatio->GetXaxis(),minpt,maxpt);
      hRatio->SetMarkerStyle(8); SetMinMax(hRatio,0.5,1.5);
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
      TLegend* l=new TLegend(.2,.7,.3,.80); l->SetBorderSize(0);
      l->SetFillColor(0);
      l->AddEntry(ha[0],"raw","p");
      l->AddEntry(ha[1],"corrected","p");
      l->Draw();
    }
    sTitle=hRatio->GetName(); 
    sTitle.Remove(sTitle.Last('.')); //c1.Update();
    Print(&c1,psDir,sTitle.Data());
    
  }

}
  
