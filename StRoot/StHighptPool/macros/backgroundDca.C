#include "commonmacro/common.h"
#include "common/Name.cc"
#include "commonmacro/histutil.h"

int show=0;

void backgroundDca(const char* inName=
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

  TFile* inRoot[2]={0};
 
  // real
  inRoot[0] = new TFile(inName);

  // mc
  inRoot[1] = new TFile(more);

  if(!inRoot[0] || !inRoot[1]){
    cout << "cannot find input file" << endl; return;
  }

  gSystem->Load("StHiMicroAnalysis");
  Cut::SetCut(cut);
  Cut::ShowCuts();

  if(!inRoot){
    cout << "cannot find the infile" << endl;
    return;
  }

  // deduce the output root file directory from the input file
  TString sOutDir=inName;
  sOutDir.Remove(sOutDir.Last('/'),sOutDir.Length());
  cout << sOutDir << endl;

  TText* t = new TText;
  float ptAry[][100] = 
    {
      {1.5,2,2.5,3,4,5,6},
      {1.7, 1.8, 1.9, 2.0, 2.2, 2.45, 2.7, 3.0, 3.35, 3.8, 4.4, 5.1, 6.0}
    };
  char* ptType[] = { "PtPr","PtGl"};
  char* chargeType[]= { "","Pos","Neg"};
  char* dcaType[] = { "DcaXYGl","DcaGl", "SDcaGl"};
  char* ptRebin[] = { "", "Rebin" };
  int nDcaRebin[] = {2,2,2};
  float bkgMin[] = {-3, 2, -3 };
  float bkgMax[] = {-2, 3, -2 };
  float dcaCut = fabs(Cut::mSDcaGl[0]);
  float sigMin[] = {-dcaCut,0,-dcaCut};
  float sigMax[] = {dcaCut,dcaCut,dcaCut};

  TCanvas c1("c1","c1",400,500);
  TCanvas c2("c2","c2",400,500);

  TH2* h2[2], *h2b[2], *h2temp[2]; TH1* h1a[2], *h1b[2];
  TH2* h2Real; TH2* h2Mc, *h2Contam;
  TH1* h1Real; TH1* h1Mc, *h1Contam;

  int nDcaType=3;
  int nptType=1;
  int npt[] = {6,12};
  int nc=3;
  int nx[]={2,3}; int ny[]={3,4}; // per ptRebin

  gStyle->SetOptStat(0); gStyle->SetOptLogy(1);

  
  //--------------------------------
  for(int iPtRebin=0; iPtRebin<2; iPtRebin++){

    for(int iDcaType=0; iDcaType<nDcaType; iDcaType++){
      for(int ic=0;ic<nc;ic++){
	if(ic<2){
	  sprintf(name,"%s.h2%sPtPrRebin",sPM[ic].Data(),dcaType[iDcaType]);

	  // real
	  h2Real=(TH2*)inRoot[0]->Get(name);

	  // mc
	  h2Mc=(TH2*)inRoot[1]->Get(name);

	  sprintf(name,"%s.h2Contam%sPtPrRebin",
		  sPM[ic].Data(),dcaType[iDcaType]); 
	  // contam
	  h2Contam=(TH2*)inRoot[1]->Get(name);

	}
	else{ // sum up the charges
	  // real
	  sprintf(name,"Plus.h2%sPtPrRebin",
		  dcaType[iDcaType]);
	  h2Real=(TH2*)inRoot[0]->Get(name);
	  sprintf(name,"Minus.h2%sPtPrRebin",
		  dcaType[iDcaType],ptRebin[iPtRebin]);
	  h2temp=(TH2*)inRoot[0]->Get(name);
	  h2Real->Add(h2temp);

	  // mc
	  sprintf(name,"Plus.h2%sPtPrRebin",
		  dcaType[iDcaType]);
	  h2Mc=(TH2*)inRoot[1]->Get(name);
	  sprintf(name,"Minus.h2%sPtPrRebin",
		  dcaType[iDcaType]);
	  h2temp=(TH2*)inRoot[1]->Get(name);
	  h2Mc->Add(h2temp);

	  // contam
	  sprintf(name,"Plus.h2Contam%sPtPrRebin",
		  dcaType[iDcaType]);
	  h2Contam=(TH2*)inRoot[1]->Get(name);
	  sprintf(name,"Minus.h2Contam%sPtPrRebin",
		  dcaType[iDcaType]);
	  h2temp=(TH2*)inRoot[1]->Get(name);
	  h2Contam->Add(h2temp);
	}
	cout <<"### " << h2Real->GetName() << endl;

	// background and signal
	sprintf(title,"bkg + signal%s %s (cut %d)",
		dcaType[iDcaType],chargeType[ic],cut);
	Divide(&c1,nx[iPtRebin],ny[iPtRebin],title,inName);

	c2.Clear();
	
	
	sprintf(title,"h1Background%s%s%s",
		dcaType[iDcaType],chargeType[ic],ptRebin[iPtRebin]);

	TH1D* hb=new TH1D(title,title,npt[iPtRebin],ptAry[iPtRebin]);

	for(int ipt=0;ipt<npt[iPtRebin];ipt++){
	  c1.cd(ipt+1);
	  h1Real=HistSlice(h2Real,"","",0,
			   ptAry[iPtRebin][ipt],ptAry[iPtRebin][ipt+1],
			   "x","e");
	  h1Mc=HistSlice(h2Mc,"","",0,
			 ptAry[iPtRebin][ipt],ptAry[iPtRebin][ipt+1],"x");
	  h1Contam=HistSlice(h2Contam,"","",0,
			     ptAry[iPtRebin][ipt],ptAry[iPtRebin][ipt+1],"x");

	  
	  if(nDcaRebin[iDcaType]>1){
	    h1Real->Rebin(nDcaRebin[iDcaType]);
	    h1Mc->Rebin(nDcaRebin[iDcaType]);
	    h1Contam->Rebin(nDcaRebin[iDcaType]);
	  }
	  // check bin sizes
	  if(h1Real->GetNbinsX() != h1Mc->GetNbinsX()){
	    cout << "different bin widths? " << endl; return;
	  }

	  //	  if(ipt==0) { dump(h1Mc); dump(h1Contam); }
	  /*
	  if(iDcaType==1){
	    int bin = h1b->GetXaxis()->FindBin(.99999);
	    cout << ptAry[iPtRebin][ipt] << "-" << ptAry[iPtRebin][ipt+1]
		 << ":\t" << h1a->Integral(1,bin)
		 << "\t" << h1b->Integral(1,bin)
		 << endl;
	  }
	  */
	  if(show)cout << h1Real->GetTitle() << endl;
	  
	  // find the background
	  int lowBin = h1Contam->FindBin(bkgMin[iDcaType]);
	  int upBin  = h1Contam->FindBin(bkgMax[iDcaType]-.0000001);
	       
	  TAxis* axis=h1Contam->GetXaxis();
	  if(show)
	    cout << "bkg region : " << axis->GetBinLowEdge(lowBin) << " -- " 
		 << axis->GetBinUpEdge(upBin) << endl;

	  // background yield in bad region
	  float badBkgCount = h1Contam->Integral(lowBin,upBin);
	  if(show)cout << "\tbad bkg count=" << badBkgCount << endl;

	  if(badBkgCount<1) continue;

	  // all counts in bad region
	  float allBadCount = h1Mc->Integral(lowBin,upBin);
	  
	  if(show)cout << "\tall bad count=" << allBadCount << endl;

	  // fraction of bkg in bad region
	  float fractionBkg = badBkgCount/allBadCount;
	  
	  if(show)cout << "\tfraction bkg=" << fractionBkg << endl;

	  // find background in signal region
	  lowBin = h1Contam->FindBin(sigMin[iDcaType]);
	  upBin  = h1Contam->FindBin(sigMax[iDcaType]-.0000001);
	  
	  if(show)cout << "signal region : " 
		       << axis->GetBinLowEdge(lowBin) << " -- " 
		       << axis->GetBinUpEdge(upBin) << endl;

	  // bkg in signal region
	  
	  float signalBkgCount = h1Contam->Integral(lowBin,upBin);
	  if(show)cout << "\tsignal bkg count=" << signalBkgCount << endl;
	  
	  float signalBkgOverBkg = signalBkgCount/badBkgCount;
	  
	  if(show)cout << "\tsignal background/background=" 
		       << signalBkgOverBkg << endl;

	  // now look at real data
	  lowBin = h1Real->FindBin(bkgMin[iDcaType]);
	  upBin  = h1Real->FindBin(bkgMax[iDcaType]-.0000001);

	  axis = h1Real->GetXaxis();
	  if(show)cout << "real bkg region : " 
		       << axis->GetBinLowEdge(lowBin) << " -- " 
		       << axis->GetBinUpEdge(upBin) << endl;
	  
	  // count in bad region
	  float realBadCount =h1Real->Integral(lowBin,upBin);
	  if(show)cout << "\treal bad count=" << realBadCount << endl;

	  if(realBadCount<1) continue;

	  // estimate background in signal region
	  float realSignalBkgCount = realBadCount*fractionBkg*signalBkgOverBkg;
	  if(show)cout << "\treal signal bkg=" << realSignalBkgCount << endl;
	  

	  lowBin = h1Real->FindBin(sigMin[iDcaType]);
	  upBin  = h1Real->FindBin(sigMax[iDcaType]-.0000001);

	  float bkgPlusSignal = h1Real->Integral(lowBin,upBin);
	  if(show)cout << "real bkg+sig=" << bkgPlusSignal << endl;

	  float bkgOverData=realSignalBkgCount/bkgPlusSignal;

	  if(show)cout << "fraction of bkg under signal region real="
		       << bkgOverData << endl;

	  hb->SetBinContent(ipt+1,bkgOverData);
	  

	  h1Contam->SetFillColor(17);

	  if(h1Mc->Integral())h1Mc->Scale(1./h1Mc->Integral());
	  if(h1Contam->Integral())h1Contam->Scale(1./h1Contam->Integral());
	  if(h1Real->Integral())h1Real->Scale(1./h1Real->Integral());

	  h1Mc->Draw();
	  h1Contam->Draw("same");
	  h1Real->SetMarkerStyle(8); h1Real->SetMarkerSize(0.6); 
	  h1Real->Draw("esame");

	  c2.cd();
	  hb->Draw();
	  
	    
	}
	Print(&c2,psDir,hb->GetName());
	sprintf(name,"contam%s%s%s",
		dcaType[iDcaType],chargeType[ic],ptRebin[iPtRebin]);
	Print(&c1,psDir,name);
      } // ic
    } // iDcaType
    
  }
}
