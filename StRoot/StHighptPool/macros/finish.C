/*
  add all the corrections, backgrounds, etc, to the spectra
*/

#include <vector> 
#include <utility>
#include "finish.h" // histograms defined here
#include "common/Name.cc"
#include "commonmacro/histutil.h"

char* inrootname="~/wrk/real/links/P01hi.minbias.2000.hist/hispectra_cut2100.hist.root";
char* correctname="~/wrk/assoc/links/P01hi.central.HighpT_piminus_101.hist/momcorrection_cut2100.hist.root";
char* bkgname = "~/afs/real/background_test.hist.root";
char* outrootname="~/wrk/real/links/temp.hist/finish_test.root";
char* textdir = "TEXT";

void finish(const char* inRootName=inrootname,
	    const char* correctionName=correctname,
	    const char* backgroundName=bkgname,
	    const char* outRootName=outrootname,
	    const char* textDir=textdir,
	    int cut = 2100,
	    int iter = 0)
{

  cout << "In file     : " << inRootName << endl
       << "mom correct : " << correctionName << endl
       << "bkg         : " << backgroundName << endl
       << "out file    : " << outRootName << endl
       << "text dir    : " << textDir << endl
       << "cut         : " << cut << endl
       << "iter        : " << iter << endl
       << "offset type : " << offSetType << endl
       << "fit high    : " << fitHighPt << endl
       << "cor high    : " << corHighPt << endl;
  
  Int_t stat=0;
  int doMomCorrection = (iter>0);
  
  gSystem->Load("StHiMicroAnalysis");
  Cut::SetCut(cut); Cut::ShowCuts();

  char *trigger = "test";
  //
  // make output text files
  //
  initTextFiles(textDir,cut,iter);

  *ofVal << ">>>>>>>>>>" << endl;
  *ofVal << "Doing momentum correction? " << doMomCorrection << endl;
  cout << ">>>>>>>>>>>" << endl;
  cout << "Doing momentum correction? " << doMomCorrection << endl;

  //
  // read the momentum correction histograms
  //
  if(doMomCorrection) getCorrectionHistograms(correctionName);

 
  //
  // get the background stuff
  //
  getBackgroundHistograms(backgroundName);


  //
  // read the histograms from the root file
  //
  getHistograms(inRootName);

  //
  // add the raw yields
  //
  doBothChargeSigns();

  //
  // output root file
  //
  outRoot = new TFile(outRootName,"RECREATE");
 
  //
  // compute all the corrections (momentum and background)
  //
  doCorrections();

 

  //
  // two ways to get the offset of the pt bin.
  // 1. find the weighted mean according to a fit of the histograms
  // 2. use the histogram of the weighted mean.
  // which is better?
  //

  initFit();

  switch(offSetType){
  case 1:
    fitHistograms(); // fit to obtain the weighted mean
    doOffSetGraphs(1); // from fit
    break;
  case 2:
    doOffSetGraphs(2); // from histograms
    break;
  default:
    cout << "wrong type"; return;
  }

  //
  // fit the graphs.  these parameters will be used for mom correction
  //
  fitGraphs();

  //
  // make spectra (just devide by 1/pt)
  //
  doSpectraGraphs();

  //
  // make some plots
  //
  //draw(psDir,trigger,cut);

  
  
  if(1){
    writeHistograms(outRootName);
  }
  


}
//____________________

void initTextFiles(const char* textDir, int cut, int iter)
{
  
  sprintf(name,"%s/corValues_cut%d_iter%d.txt",textDir,cut,iter);
  cout << "correction values : " << name << endl;
  ofCor = new ofstream(name);
  if(!ofCor) { cout << "cannot create " << name << endl;}

  //  sprintf(name,"%s/ptBinValues_cut%d_iter%d.txt",textDir,cut,iter);
  //ofPtBin = new ofstream(name);

  sprintf(name,"%s/values_cut%d_iter%d.txt",textDir,cut,iter);
  cout << "values : " << name << endl;
  ofVal = new ofstream(name);

  //sprintf(name,"%s/ua1_%s_cut%d.txt",textDir,trigger,cut);
  // ofRatio = new ofstream(name);
}


//____________________

void getCorrectionHistograms(const char* correctionName)
{
  cout << "\t**getCorrectionHistograms**" << endl;

  Int_t stat = 0;;
  //
  // momentum correction
  //
  TFile* corRoot = new TFile(correctionName);

  if(!corRoot) return 1;

  for(Int_t iBin=0; iBin<nVarBin; iBin++){
    // deltra crippled
    setName(name,"h1PtDCrRatio",iBin);
    correction[iBin].crip.h1PtDCrRatio = (TH1D*) corRoot->Get(name);
    if(!correction[iBin].crip.h1PtDCrRatio) {
      cout << "no " << name << endl; exit(1);     
    }
    showHistogramValues(correction[iBin].crip.h1PtDCrRatio,
			"MOMENTUM CORRECTIONS");

    // weight used (reality check)
    setName(name,"fCorrected",iBin);
    correction[iBin].crip.fCorrected = (TF1*)corRoot->Get(name);
    if(!correction[iBin].crip.fCorrected){
      cout << "no " << name << endl; exit(1);
    }
    for(Int_t iCharge=0; iCharge<2; iCharge++){

      setName(name,"h1PtDCrRatio",iBin,sPM[iCharge].Data());
      correction[iBin].cripPM[iCharge].h1PtDCrRatio = (TH1D*)corRoot->Get(name);
      showHistogramValues(correction[iBin].cripPM[iCharge].h1PtDCrRatio,
			"MOMENTUM CORRECTIONS");
      
      if(!correction[iBin].cripPM[iCharge].h1PtDCrRatio) {
	cout << "no " << name << endl; exit(1);
      }
      setName(name,"fCorrected",iBin,sPM[iCharge].Data());
      correction[iBin].cripPM[iCharge].fCorrected = (TF1*)corRoot->Get(name);
      if(!correction[iBin].cripPM[iCharge].fCorrected){
	cout << "no " << name << endl; exit(1);
	stat++;
      }
    }

      
  } // varbin

  //
  // reality check
  //

  cout << "Momentum correction used weights ..." << endl;

  for(Int_t iBin=0; iBin<nVarBin; iBin++){
    cout << "Bin " << iBin << endl;
    cout << "\tBoth charges" 
	 << "\tpt0="
	 <<correction[iBin].crip.fCorrected->GetParameter(0)
	 << "\t\tptPow="
	 <<correction[iBin].crip.fCorrected->GetParameter(1)
	 <<endl;
    
    for(Int_t iCharge=0; iCharge<2; iCharge++){
      cout << "\t"<< sPM[iCharge].Data() 
	   << "\tpt0="
	   <<correction[iBin].cripPM[iCharge].fCorrected->GetParameter(0)
	   << "\t\tptPow="
	   <<correction[iBin].cripPM[iCharge].fCorrected->GetParameter(1)
	   <<endl;
      
    }
  }
  
}
//________________
//
// need to fix this
//

void getBackgroundHistograms(const char* backgroundName)
{
  cout << "\t**getBackgroundHistograms()**" << endl;
  
  TFile* bkgRoot = new TFile(backgroundName);
  Int_t stat = 0;

  // for now, set the +&- backgrounds to be the same

  strcpy(name,"h1BackGround");

  char* dcaType = "DcaGl";

  for(int iBin=0; iBin<nVarBin; iBin++){
    setName(name,"h1Background",iBin);
    sprintf(name,"%s%s",name,dcaType);
    correction[iBin].bkg.h1Background=(TH1D*) bkgRoot->Get(name);
    //background[iBin].h1Background = (TH1D*) bkgRoot->Get(name);
    if(!correction[iBin].bkg.h1Background){
      cout << "Cannot find " << name << " in " 
	   << backgroundName << endl; exit(1);
    }
    showHistogramValues(correction[iBin].bkg.h1Background,"");
    
    for(int ic=0; ic<2; ic++){
      setName(name,"h1Background",iBin,sPM[ic].Data());
      sprintf(name,"%s%s",name,dcaType);
      correction[iBin].bkgPM[ic].h1Background=(TH1D*) bkgRoot->Get(name);
      
      if(!correction[iBin].bkg.h1Background){
	cout << "Cannot find " << name << " in " 
	     << backgroundName << endl; exit(1);
      }
      showHistogramValues(correction[iBin].bkgPM[ic].h1Background,"");
    }
  }
  
}
//____________________

void getHistograms(const char* inRootName)
{
  cout <<"\t**getHistograms()**" << endl;


  TFile* inRoot = new TFile(inRootName);
  if(!inRoot || !inRoot->IsOpen()){
    cout << "cannot open " << inRootName << endl; exit(1);
  }

  // vertex z
  //
  h1VertexZCut = (TH1D*)inRoot->Get("h1VertexZCut");

  // number of events and the eta cuts
  //
  h1NEvent = (TH1D*) inRoot->Get("h1NEvent");
  h1EtaCut = (TH1D*) inRoot->Get("h1EtaCut");

  
  nEvent   = h1NEvent->GetBinContent(1);
  etaCut[0] = h1EtaCut->GetBinContent(1);
  etaCut[1] = h1EtaCut->GetBinContent(2);
  scale    = (1./nEvent)*(1./(etaCut[1]-etaCut[0]));

  cout << "n event : " << nEvent << endl;
  cout << "eta cut : " << etaCut[0] << ", " << etaCut[1] << endl;
  cout << "scale   : " << scale  << endl;

  *ofVal << "n event : " << nEvent << endl
	 << "eta cut : " << etaCut[0] << ", " << etaCut[1] << endl
	 << "scale   : " << scale << endl;

  for(Int_t iBin=0; iBin<nVarBin; iBin++){

    setName(name,"h1OneOverPt",iBin);
    varBin[iBin].mean.h1OneOverPt = (TH1D*)inRoot->Get(name);
    
    setName(name,"h1WeightedMean",iBin);
    varBin[iBin].mean.h1WeightedMean = (TH1D*)inRoot->Get(name);
    
    // raw
    //
    setName(name,"h1Raw",iBin);
    varBin[iBin].spec.h1Raw = (TH1D*) inRoot->Get(name);
    
    showHistogramValues(varBin[iBin].spec.h1Raw,"YIELDS");

    //
    // efficiency corrected
    //
    setName(name,"h1EffCorrected",iBin);
    varBin[iBin].spec.h1EffCorrected = (TH1D*) inRoot->Get(name);
    showHistogramValues(varBin[iBin].spec.h1EffCorrected,"YIELDS");

    //
    // corrected (background + momentum resolution)
    //
    setName(name,"h1Corrected",iBin);
    varBin[iBin].spec.h1Corrected = (TH1D*) inRoot->Get(name);
    showHistogramValues(varBin[iBin].spec.h1Corrected,"YIELDS");
    
    


    for(Int_t iCharge=0; iCharge<2; iCharge++){

      // mean stuff
      //
      setName(name,"h1OneOverPt",iBin,sPM[iCharge].Data());
      varBin[iBin].meanPM[iCharge].h1OneOverPt = (TH1D*)inRoot->Get(name);

      setName(name,"h1WeightedMean",iBin,sPM[iCharge].Data());
      varBin[iBin].meanPM[iCharge].h1WeightedMean = (TH1D*)inRoot->Get(name);

      //
      // raw
      //
      setName(name,"h1Raw",iBin,sPM[iCharge].Data());
      varBin[iBin].specPM[iCharge].h1Raw =
	(TH1D*) inRoot->Get(name);
      showHistogramValues(varBin[iBin].specPM[iCharge].h1Raw,"YIELDS");
      //
      // efficiency corrected
      //
      setName(name,"h1EffCorrected",iBin,sPM[iCharge].Data());
      varBin[iBin].specPM[iCharge].h1EffCorrected = 
	(TH1D*) inRoot->Get(name);
      showHistogramValues(varBin[iBin].specPM[iCharge].h1EffCorrected,"YIELDS");

      //
      // corrected (background + momentum resolution)
      //
      setName(name,"h1Corrected",iBin,sPM[iCharge].Data());
      varBin[iBin].specPM[iCharge].h1Corrected = 
	(TH1D*) inRoot->Get(name);
      showHistogramValues(varBin[iBin].specPM[iCharge].h1Corrected,"YIELDS");
    }    
    
    //***** ratios

    setName(name,"h1RawRatio",iBin);
    varBin[iBin].h1RawRatio = (TH1D*) inRoot->Get(name);

    setName(name,"h1CorrectedRatio",iBin);
    varBin[iBin].h1CorrectedRatio = (TH1D*) inRoot->Get(name);

  }  // varBin  
  //  cout << "done " <<endl;
}
//__________________

void doBothChargeSigns()
{
  cout << "\t**doBothChargsSigns()**" << endl;

  //
  // just scale by a half
  //

  for(Int_t iBin=0; iBin<nVarBin; iBin++){
    varBin[iBin].spec.h1Raw->Scale(0.5);
    //showHistogramValues(varBin[iBin].spec.h1Raw,"SCALED 1/2");

    varBin[iBin].spec.h1EffCorrected->Scale(0.5);
    //showHistogramValues(varBin[iBin].spec.h1EffCorrected,"SCALED 1/2");
    
    varBin[iBin].spec.h1Corrected->Scale(0.5);
    //showHistogramValues(varBin[iBin].spec.h1Corrected,"SCALED 1/2");
  }

}

//____________________

void doCorrections()
{
  cout << "\t**doCorrections()**" << endl;

  // bkg and momentum resolution

  for(Int_t iBin=0; iBin<nVarBin; iBin++){

    computeCorrection(varBin[iBin].spec.h1EffCorrected,
		      correction[iBin].crip.h1PtDCrRatio,
		      correction[iBin].bkg.h1Background,
		      varBin[iBin].spec.h1Corrected);

    showHistogramValues(varBin[iBin].spec.h1Corrected,"CORRECTED");

    for(Int_t iCharge=0; iCharge<2; iCharge++){

      computeCorrection(varBin[iBin].specPM[iCharge].h1EffCorrected,
			correction[iBin].cripPM[iCharge].h1PtDCrRatio,
			correction[iBin].bkgPM[iCharge].h1Background,
			varBin[iBin].specPM[iCharge].h1Corrected);
      showHistogramValues(varBin[iBin].specPM[iCharge].h1Corrected,
			  "CORRECTED");
    }

       
  }
  
  // scale the eff corrected separately for safety
  //
  for(Int_t iBin=0; iBin<nVarBin; iBin++){
    varBin[iBin].spec.h1Raw->Scale(scale);
    varBin[iBin].spec.h1Corrected->Scale(scale);
    varBin[iBin].spec.h1EffCorrected->Scale(scale);
    for(Int_t iCharge=0; iCharge<2; iCharge++){
      varBin[iBin].specPM[iCharge].h1Raw->Scale(scale);
      varBin[iBin].specPM[iCharge].h1Corrected->Scale(scale);
      varBin[iBin].specPM[iCharge].h1EffCorrected->Scale(scale);
    }

  }
  
  
  // compute the pt bin separately
  //
  for(Int_t iBin=0; iBin<nVarBin; iBin++){
    computePtBin(varBin[iBin].spec.h1Raw);
    computePtBin(varBin[iBin].spec.h1EffCorrected);
    computePtBin(varBin[iBin].spec.h1Corrected);

    for(Int_t iCharge=0; iCharge<2; iCharge++){
      computePtBin(varBin[iBin].specPM[iCharge].h1Raw);
      computePtBin(varBin[iBin].specPM[iCharge].h1EffCorrected);
      computePtBin(varBin[iBin].specPM[iCharge].h1Corrected);

    }    
  }  
}

//____________________

void initFit()
{
  cout << "\tinitFit()" << endl;

  
  fDist = new TF1("fDist",dist,fitLowPt,fitHighPt);
  fMean = new TF1("fMean",mean,fitLowPt,fitHighPt);

  Double_t p2 = 1e4;

  fDist->SetParameters(2.0,13.,p2);
  fMean->SetParameters(2.0,13.,p2);
  const int nParam = 3;

  Double_t param[nParam];
  fDist->GetParameters(param);

  //cout << "initial param : " ;
  //for(int i=0; i<nParam; i++){
  //  cout << "\t" << param[i] ;
  // }
  cout << endl;
}

//____________________
//
// fit the spectra.  use the weighted mean of each bin
// for the 1/pt scale

void fitHistograms()
{
  cout << "\t**fit histograms**" << endl;

  //
  // dummy canvas
  //
  TCanvas* dummy = new TCanvas("dummy","dummy",100,100,500,500);
  gPad->SetLogy();


  for(Int_t iBin=0; iBin<nVarBin; iBin++){
    varBin[iBin].spec.fRawTmp = 
      fit(varBin[iBin].spec.h1Raw);

    varBin[iBin].spec.fEffCorrectedTmp = 
      fit(varBin[iBin].spec.h1EffCorrected);
    
    varBin[iBin].spec.fCorrectedTmp = 
      fit(varBin[iBin].spec.h1Corrected);

    for(Int_t iCharge=0; iCharge<2; iCharge++){
      varBin[iBin].specPM[iCharge].fRawTmp = 
	fit(varBin[iBin].specPM[iCharge].h1Raw);

      varBin[iBin].specPM[iCharge].fEffCorrectedTmp =
	fit(varBin[iBin].specPM[iCharge].h1EffCorrected);

      varBin[iBin].specPM[iCharge].fCorrectedTmp =
	fit(varBin[iBin].specPM[iCharge].h1Corrected);
    }
  }

  delete dummy;
}

  
//____________________
//
// only used for getting the weighted mean from the fit
//

TF1* fit(TH1* h1)
{
  cout << "\tfitting " << h1->GetName() << endl;

  double param[3];
  TF1* f=0;

  // first get a reasonable initial parameters
  TH1* hClone=(TH1*)h1->Clone();
  hClone->Fit("fDist","QR");
  f = hClone->GetFunction("fDist");
  
  f->GetParameters(param);
  fDist->SetParameters(param);

  // fit for real
  hClone->Fit("fDist","QR");
  f = hClone->GetFunction("fDist");
  /*
  cout << "\t\tparams : " 
       << "p0=" << f->GetParameter(0) 
       << ",p1=" << f->GetParameter(1)
       << ",p2=" << f->GetParameter(2)
       << endl;
  */
  TString sName = h1->GetName();

  Ssiz_t last = findLast(sName,".");

  sName.Replace(last,2,"fTmp");

  f->SetName(sName.Data());
  f->SetTitle(sName.Data());

  return f;
}

//_____________________
//
// fit tgraphs to obtain fit parameters for mom correction
//

TF1* fit(TGraphAsymmErrors* g)
{
  cout << "\tfitting " << g->GetName() << endl;

  double param[3];
  TF1* f=0;

  // first get a reasonable initial parameters
  TGraphAsymmErrors* gClone=(TGraphAsymmErrors*)g->Clone();
  gClone->Fit("fDist","QR");
  f = gClone->GetFunction("fDist");
  
  f->GetParameters(param);
  fDist->SetParameters(param);

  // fit for real
  gClone->Fit("fDist","QR");
  f = gClone->GetFunction("fDist");

  cout << "\t\tparams : " 
       << "p0=" << f->GetParameter(0) 
       << ",p1=" << f->GetParameter(1)
       << ",p2=" << f->GetParameter(2)
       << endl;
  
  TString sName = g->GetName();

  Ssiz_t last = findLast(sName,".");

  sName.Replace(last,1,"f");
  f->SetName(sName.Data());
  f->SetTitle(sName.Data());

  cout << "\t\t fcn name " << f->GetName() << endl;

  return f;
}

//_____________________


Double_t getWeightedMean(TF1* f,Double_t lowEdge, Double_t upEdge)
{
  Double_t param[3];

  f->GetParameters(param);

  //
  // set the parameters for the mean function
  //
  fMean->SetParameters(param); // defined in the header file

  return fMean->Integral(lowEdge,upEdge)/f->Integral(lowEdge,upEdge);

}

//_________________
//
// make the off set tgraphs


TGraphAsymmErrors*
graphOffset(TH1D* h1,TF1* f,TH1* hMean,Int_t type)
{

  Int_t lowBin  = h1->GetXaxis()->FindBin(fitLowPt);
  Int_t highBin = h1->GetXaxis()->FindBin(fitHighPt-.0001);
  Int_t nBin = highBin-lowBin+1;

  TArrayD yErrorAry(nBin), yValueAry(nBin), xValueAry(nBin), 
    xLowErrorAry(nBin), xHighErrorAry(nBin);

  // dont fill the first and last bin.
  // histograms bin numbered from 1 to n.
  // array number from 0 to n-1.
  
  Int_t binAry(0);
  
  for(Int_t iBin=lowBin; iBin<=highBin; iBin++,binAry++){
    
    //
    // x error
    //
    Double_t lowEdge = h1->GetXaxis()->GetBinLowEdge(iBin);
    Double_t upEdge  = h1->GetXaxis()->GetBinUpEdge(iBin);
    Double_t mean = 0;

    switch(type){
    case 1:
      mean = getWeightedMean(f,lowEdge,upEdge); break;
    case 2:
      mean = h1Mean->GetBinContent(h1Mean->GetXaxis()->FindBin((lowEdge+upEdge)/2)); break;
    default:
      mean = getWeightedMean(f,lowEdge,upEdge); 
    }
    
    xLowErrorAry.AddAt(fabs(mean-lowEdge),binAry);
    xHighErrorAry.AddAt(fabs(mean-upEdge),binAry);

    xValueAry.AddAt(mean,binAry); // x                

    yErrorAry.AddAt(h1->GetBinError(iBin),binAry); // y error

    yValueAry.AddAt(h1->GetBinContent(iBin),binAry); // y
    
  }
  
  //
  // now actually make the graph
  //
    
  TGraphAsymmErrors* g = new TGraphAsymmErrors(nBin,
					       xValueAry.GetArray(),
					       yValueAry.GetArray(),
					       xLowErrorAry.GetArray(),
					       xHighErrorAry.GetArray(),
					       yErrorAry.GetArray(),
					       yErrorAry.GetArray());
  
  TString sName = h1->GetName();
  Ssiz_t last = findLast(sName,".");

  sName.Replace(last,2,"g");

  g->SetName(sName.Data());
  g->SetTitle(sName.Data());

  return g;
}
//__________________

TGraphAsymmErrors*
graphSpectra(TGraphAsymmErrors* g)
{
  
  const Int_t nBin = g->GetN();
  
  Double_t* x = g->GetX();
  Double_t* y = g->GetY();
  Double_t* eXlow  = g->GetEXlow();
  Double_t* eXhigh = g->GetEXhigh();
  Double_t* eYlow  = g->GetEYlow();

  TArrayD yErrorAry(nBin), yValueAry(nBin); 

  // divide each bin by the value of that bin.
  // also divide the errors.
  //

  for(Int_t i=0; i<nBin; i++){
    Double_t mean = x[i];

    yErrorAry.AddAt(eYlow[i]/mean,i);
    yValueAry.AddAt(y[i]/mean,i);
  }


  TGraphAsymmErrors* gSpec 
    = new TGraphAsymmErrors(nBin,x,yValueAry.GetArray(),eXlow,eXhigh,
			    yErrorAry.GetArray(),yErrorAry.GetArray());

  TString sName = g->GetName();
  Ssiz_t last = findLast(sName,".");

  sName.Replace(last,1,"gSpec");

  gSpec->SetName(sName.Data());
  gSpec->SetTitle(sName.Data());
  
  return gSpec;


}

//____________________
//
//

void doOffSetGraphs(Int_t type)
{
  
  for(Int_t iBin=0; iBin<nVarBin; iBin++){

    varBin[iBin].spec.gRaw = 
      graphOffset(varBin[iBin].spec.h1Raw,
		  varBin[iBin].spec.fRawTmp,
		  varBin[iBin].mean.h1WeightedMean,type);

    varBin[iBin].spec.gEffCorrected = 
      graphOffset(varBin[iBin].spec.h1EffCorrected,
		  varBin[iBin].spec.fEffCorrectedTmp,
		  varBin[iBin].mean.h1WeightedMean,type);

    varBin[iBin].spec.gCorrected = 
      graphOffset(varBin[iBin].spec.h1Corrected,
		  varBin[iBin].spec.fCorrectedTmp,
		  varBin[iBin].mean.h1WeightedMean,type);
    

    for(Int_t iCharge=0; iCharge<2; iCharge++){

      varBin[iBin].specPM[iCharge].gRaw = 
      graphOffset(varBin[iBin].specPM[iCharge].h1Raw,
		  varBin[iBin].specPM[iCharge].fRawTmp,
		  varBin[iBin].mean.h1WeightedMean,type);


      varBin[iBin].specPM[iCharge].gEffCorrected = 
	graphOffset(varBin[iBin].specPM[iCharge].h1EffCorrected,
		    varBin[iBin].specPM[iCharge].fEffCorrectedTmp,
		    varBin[iBin].meanPM[iCharge].h1WeightedMean,type);

      varBin[iBin].specPM[iCharge].gCorrected = 
	graphOffset(varBin[iBin].specPM[iCharge].h1Corrected,
		    varBin[iBin].specPM[iCharge].fCorrectedTmp,
		    varBin[iBin].meanPM[iCharge].h1WeightedMean,type);
      
    }

  }
}

//____________________
//
// fit the offset graphs.  
// these parameters are used for the momentum corrections
//

void fitGraphs()
{
  cout << "\t**fitGraphs()**" << endl;

  //
  // dummy canvas
  //
  TCanvas* dummy = new TCanvas("dummy","dummy",100,100,500,500);
  gPad->SetLogy();

  for(Int_t iBin=0; iBin<nVarBin; iBin++){

    varBin[iBin].spec.fEffCorrected =
      fit(varBin[iBin].spec.gEffCorrected);
    
    varBin[iBin].spec.fCorrected =
      fit(varBin[iBin].spec.gCorrected);
    
    for(Int_t iCharge=0; iCharge<2; iCharge++){

      varBin[iBin].specPM[iCharge].fEffCorrected =
	fit(varBin[iBin].specPM[iCharge].gEffCorrected);

      varBin[iBin].specPM[iCharge].fCorrected =
	fit(varBin[iBin].specPM[iCharge].gCorrected);
    }
  }

  delete dummy;

}
//____________________

void doSpectraGraphs()
{
  cout << "\t**doSpectraGraphs()**" << endl;

  for(Int_t iBin=0; iBin<nVarBin; iBin++){
    varBin[iBin].spec.gSpecCorrected = 
      graphSpectra(varBin[iBin].spec.gCorrected);

    for(Int_t iCharge=0; iCharge<2; iCharge++){

      varBin[iBin].specPM[iCharge].gSpecCorrected =
	graphSpectra(varBin[iBin].specPM[iCharge].gCorrected);
    }

  }

}
//____________________



//____________________

void draw(const char* psDir, const char* trigger, const char* cut)
{

}


//____________________
//
// set the background error at 100%
//

void computeCorrection(TH1D* hRc, TH1D* hDCrOverRc, 
		       TH1D* hBkg, TH1D* hCorrected)
{
 
  Stat_t rc, dCr, rcError, dCrError, error, corrected, bkgCorrected,
    bkgCorrectedError, center, bkg, bkgError;

  Int_t bin;
  Int_t nBin = hRc->GetNbinsX();
  Int_t maxBkgBin = hBkg->GetNbinsX();

  *ofCor << "******************************************************" << endl;
  *ofCor << "## computeCorrection ##" << endl;
  *ofCor << "## corrected=" << hCorrected->GetName() << endl
	 << "   rc=" << hRc->GetName() << endl
	 << "  bkg=" << hBkg->GetName() << endl;
  
  int firstCorBin=hRc->GetXaxis()->FindBin(corLowPt);
  *ofCor << "First correction bin is " << firstCorBin << endl;
  for(Int_t iBin=1; iBin<=nBin; iBin++){
  
    rc  = hRc->GetBinContent(iBin); // real data
    
    // dCr weighted w/flat input
    dCr = (hDCrOverRc) ? hDCrOverRc->GetBinContent(iBin) :0;
    
    // background
    center = hRc->GetXaxis()->GetBinCenter(iBin);
    bin = hBkg->GetXaxis()->FindBin(center);
    if(bin==0 || bin>maxBkgBin){
      bkg = 0;
    }
    else{
      bkg = hBkg->GetBinContent(bin);
    }

    // changed the definition a little

    bkgCorrected = rc*(1-bkg);

    rcError = hRc->GetBinError(iBin);
    
    // set the background error as the same as the bkg
    bkgError = bkg;

    bkgCorrectedError 
      = TMath::Sqrt(rcError*rcError*(1+bkg*bkg) + rc*rc*bkgError*bkgError); 
    
    // now do the dCr stuff
    //
    if(iBin>=firstCorBin){
      corrected = bkgCorrected*(1-dCr);
    }
    else{
      *ofCor << "\tskipping momentum correction for this bin" << endl;
      corrected = bkgCorrected;
    }
    // forget about the crippled error
    //
    error = bkgCorrectedError;

    hCorrected->SetBinContent(iBin,corrected);       
    hCorrected->SetBinError(iBin,error);

    float fracError = (rc>0) ? ((error/rc)*100) : 0;
    float fracRcError = (rc>0) ? ((rcError/rc)*100) : 0;
			

    *ofCor << " rc : " << hRc->GetXaxis()->GetBinLowEdge(iBin) << "-" 
	   << hRc->GetXaxis()->GetBinUpEdge(iBin) 
	   << "\trc bin content : " << rc 
	   << ", crippled : " << dCr
	   << ", background : " << bkg << endl
	   << "\tbkg corrected  : " << bkgCorrected << endl
	   << "\tcorrected      : " << corrected << endl;
    *ofCor << "\t rc error     : " << rcError 
	   << "(" << fracRcError << " %)"
	   << ", dCrError : " << dCrError 
	   << ", bkg error: " << bkgError
	   << endl
	   << "\tbkg cor error : " << bkgCorrectedError << endl
	   << "\ttotal error   : " << error << endl
	   << "\ttotal err % : " << fracError << endl;
    
  }
}
//____________________
// just divide histograms by bin center


void ptDivide(TH1* h1)
{
  Int_t nBin=h1->GetNbinsX();
  for(int iBin=1; iBin<=nBin; iBin++){
    double content= h1->GetBinContent(iBin);
    double center = h1->GetBinCenter(iBin);
    double error  = h1->GetBinError(iBin);
    if(content) h1->SetBinContent(iBin,content/center);
    if(error)h1->SetBinError(iBin,error/center);
  }

}


void computePtBin(TH1* h1)
{
  
  double binWidth;
  double binContent;
  double binError;

  Int_t nBin = h1->GetNbinsX();
  for(Int_t iBin=1; iBin<=nBin; iBin++){
    binWidth = h1->GetBinWidth(iBin);
    binContent= h1->GetBinContent(iBin);
    if(binContent>0) binContent /= binWidth;
    binError = h1->GetBinError(iBin);
    if(binError!=0) binError /= binWidth;
    
    h1->SetBinContent(iBin,binContent);
    h1->SetBinError(iBin,binError);

  }  
}
//_______________
//
// also saves the TF1's and TGraphs


void writeHistograms(const char* outRootName)
{
  cout << "writeHistograms() : " << outRootName << endl;

  //  TFile* outRoot = new TFile(outRootName,"RECREATE");

  for(Int_t iBin=0; iBin<nVarBin; iBin++){

    varBin[iBin].spec.h1Raw->Write();
    varBin[iBin].spec.h1EffCorrected->Write();
    varBin[iBin].spec.h1Corrected->Write();
    varBin[iBin].spec.gRaw->Write();
    varBin[iBin].spec.gEffCorrected->Write();
    varBin[iBin].spec.gCorrected->Write();
    varBin[iBin].spec.gSpecCorrected->Write();
    varBin[iBin].spec.fCorrected->Write();

    showHistogramValues(varBin[iBin].spec.h1Raw,"FINAL");
    showHistogramValues(varBin[iBin].spec.h1EffCorrected,"FINAL");
    showHistogramValues(varBin[iBin].spec.h1Corrected,"FINAL");
    
    showTGraphValues(varBin[iBin].spec.gRaw,"FINAL");
    showTGraphValues(varBin[iBin].spec.gEffCorrected,"FINAL");
    showTGraphValues(varBin[iBin].spec.gCorrected,"FINAL");
    showTGraphValues(varBin[iBin].spec.gSpecCorrected,"FINAL");

    for(Int_t iCharge=0; iCharge<2; iCharge++){

      // ua1 stuff
      //ua1[iBin].ratioPM[iCharge].gRatioWeight->Write();
      //ua1[iBin].ratioPM[iCharge].gRatio->Write();

      varBin[iBin].specPM[iCharge].h1Raw->Write();
      varBin[iBin].specPM[iCharge].h1EffCorrected->Write();
      varBin[iBin].specPM[iCharge].h1Corrected->Write();
      varBin[iBin].specPM[iCharge].gEffCorrected->Write();
      varBin[iBin].specPM[iCharge].gCorrected->Write();
      varBin[iBin].specPM[iCharge].gSpecCorrected->Write();
      varBin[iBin].specPM[iCharge].fCorrected->Write();
    
      showHistogramValues(varBin[iBin].specPM[iCharge].h1Raw,"FINAL");
      showHistogramValues(varBin[iBin].specPM[iCharge].h1EffCorrected,"FINAL");
      showHistogramValues(varBin[iBin].specPM[iCharge].h1Corrected,"FINAL");

      showTGraphValues(varBin[iBin].specPM[iCharge].gRaw,"FINAL");
      showTGraphValues(varBin[iBin].specPM[iCharge].gEffCorrected,"FINAL");
      showTGraphValues(varBin[iBin].specPM[iCharge].gCorrected,"FINAL");
      showTGraphValues(varBin[iBin].specPM[iCharge].gSpecCorrected,"FINAL");

    }

  }

}




//________________________

Ssiz_t findLast(TString temp, const char* a)
{
  Ssiz_t last = 0;
 
  while(1){
    Ssiz_t pos = temp.First(a);
    if(pos<0) break;
    last += ++pos;
    temp.Remove(0,pos);
  }

  return last;

}
