#include "commonmacro/common.h"
#include "common/Name.cc"
#include "commonmacro/histutil.h"

void dirtyBack(const char* inName=
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

  gSystem->Load("StHiMicroAnalysis");
  if(!inRoot){
    cout << "cannot find the infile" << endl;
    return;
  }
  // deduce the output dir and name
  TString outName=outDir; 
  sprintf(name,"/background_cut%d.hist.root",cut);
  outName+=name;

  TFile* outRoot=new TFile(outName.Data(),"RECREATE");
  double badBkgFrac[]={.56,.70,.70,.80,.80};
  double sigBkgOverBadBkg[] = {6.5,3.9,1.4,.5,.2};
  int npt=5;
  float ptary[]={1.5,2,3,4,5,6};

  //----------------------------------------
  
  TH2* h2=(TH2*)inRoot->Get("Minus.h2SDcaGlPtPr");
  
  TH1D* hBack=new TH1D("h1BackGround","h1BackGround",
		       5,ptary);
  int lowBin,upBin;
  for(int ipt=0;ipt<npt;ipt++){
    TH1* h1=(TH2*)HistSlice(h2,"","",0,ptary[ipt],ptary[ipt+1],"x");
    lowBin=h1->FindBin(-3);
    upBin=h1->FindBin(-1.99999);
    double badAll=h1->Integral(lowBin,upBin);
    double sigBkg=badAll*badBkgFrac[ipt]*sigBkgOverBadBkg[ipt];
    lowBin=h1->FindBin(Cut::mSDcaGl[0]);
    upBin=h1->FindBin(Cut::mSDcaGl[1]-.00001);
    double sig=h1->Integral(lowBin,upBin);
    double bkgFrac = sigBkg/sig;
    cout << h1->GetTitle() " : " << bkgFrac << endl;
    hBack->SetBinContent(ipt+1,bkgFrac);
  }
  cout << "writing " << outName << endl;
  outRoot->Write();
}
