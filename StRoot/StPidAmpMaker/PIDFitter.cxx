///////////////////////////////////////////////////////////////////////////////
//
// $Id: PIDFitter.cxx,v 1.11 2007/07/12 19:47:31 fisyak Exp $
//
// Authors: Aihong Tang
//
///////////////////////////////////////////////////////////////////////////////
//
// Description: fit histo. produced by StPidAmpMaker.
//
///////////////////////////////////////////////////////////////////////////////

#include "PIDFitter.h"
#include "Stsstream.h"
#include "Stiostream.h"
#include <math.h>
//#include "TH1.h"
#include "TH2.h"
#include "TLine.h"
#include "TF1.h"
#include <float.h>
#include "TStyle.h"
#include "TCanvas.h"
#include "TMath.h"
#include "TFile.h"
#include "TGraphErrors.h"


#include "BetheBlochFunction.hh"
//#include "MaxllBoltz.hh"


#include <Stiostream.h>

ClassImp(PIDFitter);
Double_t sigmaNSampleFitFcn(Double_t* x, Double_t *par);

//----------------------------------------------------------------------
PIDFitter::PIDFitter(){
 Init();
}
//----------------------------------------------------------------------
void PIDFitter::Process( Char_t* sigmaOfSigmTrialInputName,
                         Char_t* sigmaOfSigmTrialOutputName, 
                         Char_t* phaseSpaceCalibInputName,
                         Char_t* phaseSpaceCalibOutputName, 
                         Char_t* gausFitInputName,
                         Char_t* gausFitOutputName,
                         Char_t* ampFitOutputName ){

  GetSigmaOfSingleTrail(sigmaOfSigmTrialInputName,sigmaOfSigmTrialOutputName);
  DoPhaseSpaceCalibration(phaseSpaceCalibInputName,phaseSpaceCalibOutputName);
  FitMultiGaus(gausFitInputName,gausFitOutputName);
  ExtrapAmp(gausFitOutputName,ampFitOutputName);

}
//----------------------------------------------------------------------
void PIDFitter::GetSigmaOfSingleTrail(Char_t* fileName4SigmaOfSingleTrail,Char_t* sigmaFileName){//use pion

       cout<<" getting sigma of single trail..."<<endl;
       TFile* fitResoFile;

       if (mWriteSigmaNSampleGraph) {
         fitResoFile = new TFile("sigmaNSample.root","UPDATE");
       }


       ofstream SigmaFileOut;
       SigmaFileOut.open(sigmaFileName);

       float pionPosition4Calib=0.4;
               
       int i = int((pionPosition4Calib-mPStart)*mNPBins/(mPEnd-mPStart) );

      TH1F* theHist=0;

      TFile histo4CalibFile(fileName4SigmaOfSingleTrail,"READ");

      TH1F* theDummyHisto = (TH1F *)histo4CalibFile.Get("h2000");
      

  	for (int j=0; j<mNEtaBins; j++) {


  TGraphErrors* sigmaNSampleGraph = new TGraphErrors();

  TString grName;
  grName.Append(fileName4SigmaOfSingleTrail);
  grName.Append("SigmaNSample");
  grName.Append((i<10) ? "0" : "");
  grName +=i;
  grName +=j;
  grName.ReplaceAll(".root","");

   sigmaNSampleGraph->SetTitle(grName.Data());
   sigmaNSampleGraph->SetName(grName.Data());


      for (int k=0; k<mNNHitsBins;k++){

	TH1F* tempHisto = new TH1F(*theDummyHisto);
	tempHisto->Reset();

	char *theName = new char[80];
        if (i<10)
	  sprintf(theName,"h0%d%d%d",i,j,k);
        else sprintf(theName,"h%d%d%d",i,j,k);
              
	theHist= (TH1F *)histo4CalibFile.Get(theName);
        delete theName;


	for (int mm=0; mm<tempHisto->GetNbinsX(); mm++)
        tempHisto->SetBinContent(mm,(theHist->GetBinContent(mm)));


        Float_t theError=0;
 
        Float_t theFitGausHalfRange = 0.03e-05*0.95;
  
        Float_t  mean=FitResoGaus(tempHisto,theFitGausHalfRange,theError,
                                     0.005e-4,0.04e-4,1,j,k,pionPosition4Calib);
        Float_t sigma=FitResoGaus(tempHisto,theFitGausHalfRange,theError,
                                     0.005e-4,0.04e-4,2,j,k,pionPosition4Calib);

    sigmaNSampleGraph->SetPoint(sigmaNSampleGraph->GetN(),(k*mNNHitsBins+(5./2.)),sigma/mean);
    sigmaNSampleGraph->SetPointError((sigmaNSampleGraph->GetN()-1),0,theError);

    if (tempHisto) delete tempHisto;

      }


     TF1* f1 = new TF1("f1",sigmaNSampleFitFcn,12.,40.,1);
     sigmaNSampleGraph->Fit("f1","R");

    cout<<"sigNSamplePion1Graph fit parameter: "<<f1->GetParameter(0);
    double theSigmaOfSingleTrail= f1->GetParameter(0);    
    double theErrorOfSigmaOfSingleTrail = f1->GetParError(0);
    if(theErrorOfSigmaOfSingleTrail) {/*warnOff*/}
    if (mWriteSigmaNSampleGraph) {
        fitResoFile->cd();
        sigmaNSampleGraph->SetMarkerColor(2);
        sigmaNSampleGraph->SetMarkerStyle(20);
        sigmaNSampleGraph->Write(grName.Data(),TObject::kOverwrite | TObject::kSingleKey);
        fitResoFile->Write();
    }


    if (f1) delete f1;
    if (sigmaNSampleGraph) delete sigmaNSampleGraph;
    mSigmaOfSingleTrail[j]=theSigmaOfSingleTrail;



       SigmaFileOut<<j<<endl;
       SigmaFileOut<<theSigmaOfSingleTrail<<endl;//" +/- "<<theErrorOfSigmaOfSingleTrail<<endl;

	} //integrated over all eta bins

    if (mWriteSigmaNSampleGraph)  fitResoFile->Close();
       SigmaFileOut.close();


  cout<<" end of getting sigma of single trail..."<<endl;

}

//----------------------------------------------------------------------
float PIDFitter::FitResoGaus(TH1F* resoHist,float fitRange,float& er,float theStart, float theEnd, int ParIndex, int j, int k, float thePPosition){
  //get the par from fitting of sigma _ Nsample.

  TFile* fitResoFile;

  if (mWriteGaus4SigmaNSampleHist) {
    fitResoFile = new TFile("Gaus4SigmaNSample.root","UPDATE"); //no name change!
  }

    Double_t tempSigmaOfSingleTrial =0.42;

   RefreshPresettings(resoHist,  tempSigmaOfSingleTrial,  k,  thePPosition);

   //setup vary range

   Double_t ECenterVary=0.3;
   Double_t EWidthVary=0.4;

   Double_t PiCenterVary=0.3;
   Double_t PiHeightVary=0.2;
   Double_t PiWidthVary=0.4;  

   Double_t KCenterVary=0.3;
   Double_t KWidthVary=0.4;

    TF1*     sumGs
    = new TF1("sumGs","gaus(0)+gaus(3)+gaus(6)", 0., 0.08e-4);

    Double_t pars_b[9] = {mPiGausHeight, mPiGausCenter, mPiSigma,
			  mEGausHeight,  mEGausCenter,  mESigma,
			  mKGausHeight,  mKGausCenter,  mKSigma  };

    sumGs->SetParameters(&pars_b[0]);

    sumGs->SetParLimits( 0, mPiGausHeight*(1.-PiHeightVary),
		 	    mPiGausHeight*(1.+PiHeightVary) );
    sumGs->SetParLimits( 1, mPiGausCenter*(1.-PiCenterVary),
			    mPiGausCenter*(1.+PiCenterVary) );
    sumGs->SetParLimits( 2, mPiSigma*(1.-PiWidthVary),
			    mPiSigma*(1.+PiWidthVary) );

    sumGs->SetParLimits( 3, 0.,mEGausHeight);
    sumGs->SetParLimits( 4, mEGausCenter*(1.-ECenterVary),
			    mEGausCenter*(1.+ECenterVary) );
    sumGs->SetParLimits( 5, mESigma*(1.-EWidthVary),
			    mESigma*(1.+EWidthVary) );

    sumGs->SetParLimits( 6, 0.,mKGausHeight);
    sumGs->SetParLimits( 7, mKGausCenter*(1.-KCenterVary),
			    mKGausCenter*(1.+KCenterVary) );
    sumGs->SetParLimits( 8, mKSigma*(1.-KWidthVary),
			    mKSigma*(1.+KWidthVary) );

    
    resoHist->Fit("sumGs","R");

    if (mWriteGaus4SigmaNSampleHist){
        fitResoFile->cd();
        resoHist->Write();
    }

    if (mWriteGaus4SigmaNSampleHist)    fitResoFile->Close();
    float thePar=sumGs->GetParameter(ParIndex);
    if (mWriteGaus4SigmaNSampleHist) {  delete fitResoFile; fitResoFile=0;}
    if (sumGs) { delete sumGs; }

  return thePar;
}

//----------------------------------------------------------------------
void PIDFitter::RefreshPresettings(TH1F* resoHist, double tempSigmaOfSingleTrial, int k, float thePPosition){

     mEGausHeight =0;      mESigma=0.;
     mPiGausHeight =0;     mPiSigma=0.;
     mKGausHeight =0;      mKSigma=0.;
     mPGausHeight =0;      mPSigma=0.;

     mEGausCenter      = electronBandCenter  ->Eval(thePPosition,0,0);
     mPiGausCenter     = pionBandCenter      ->Eval(thePPosition,0,0);
     mKGausCenter      = kaonBandCenter      ->Eval(thePPosition,0,0);
     mPGausCenter      = antiprotonBandCenter->Eval(thePPosition,0,0);

    PresetHeightAndSigma(mEGausCenter,  mEGausHeight,  mESigma,  
                         resoHist,  tempSigmaOfSingleTrial,  k);
    PresetHeightAndSigma(mPiGausCenter, mPiGausHeight, mPiSigma, 
                         resoHist,  tempSigmaOfSingleTrial,  k);
    PresetHeightAndSigma(mKGausCenter,  mKGausHeight,  mKSigma,  
                         resoHist,  tempSigmaOfSingleTrial,  k);
    PresetHeightAndSigma(mPGausCenter,  mPGausHeight,  mPSigma,  
                         resoHist,  tempSigmaOfSingleTrial,  k);

}

//----------------------------------------------------------------------
void PIDFitter::PresetHeightAndSigma(double center, double& height, double& sigma, TH1F* resoHist, double tempSigmaOfSingleTrial, int k){

       int nbins   = resoHist->GetNbinsX();
       int centBin = 
       int((center-mDedxStart)*float(nbins)/((mDedxEnd-mDedxStart)));

      height = (centBin<nbins) ? resoHist->GetBinContent(centBin) : 0.;
      sigma  = tempSigmaOfSingleTrial*center/TMath::Sqrt((k+0.5)*((double(mNNHitsEnd)-double(mNNHitsStart))/double(mNNHitsBins)));

}


//----------------------------------------------------------------------
void PIDFitter::DoPhaseSpaceCalibration(Char_t* fileName4Calibration,Char_t* phaseSpaceCalibFileName){

  cout<<" doing phase space calibration..........  "<<endl;

  double protonRef[mNEtaBins][mNNHitsBins];
  double pionRef[mNEtaBins][mNNHitsBins];

     double  fitEnd   = mDedxEnd;
     double  fitStart = mDedxStart;

  TFile histo4CalibFile(fileName4Calibration,"READ");

 TH1F* theHist=0;
 TF1* sumGs=0;

 int pionPosition4Calib   =int((0.5-mPStart)*mNPBins/(mPEnd-mPStart) );
 int protonPosition4Calib =int((0.7-mPStart)*mNPBins/(mPEnd-mPStart) );


     //get protonRef
    for (int j=0; j<mNEtaBins; j++)
      for (int k=2; k<mNNHitsBins;k++){

     int i = protonPosition4Calib;  //for protonRef

     //if dE/dx does not shift regarding to nhits, 
     //set kname=4.(just use long tracks)
     int kname = k; 
     int jname = j;

	char *theName = new char[80];
        if (i<10)
	  sprintf(theName,"h0%d%d%d",i,jname,kname);
        else sprintf(theName,"h%d%d%d",i,jname,kname);

	theHist= (TH1F *)histo4CalibFile.Get(theName);
        delete theName;

      float pPosition = 
            float(i)*((mPEnd-mPStart)/float(mNPBins)) + 
             (((mPEnd-mPStart)/mNPBins)/2.);

  //setup initial values.
  RefreshPresettings(theHist, mSigmaOfSingleTrail[j] ,  k, pPosition);

   //setup vary range
   Double_t KCenterVary=0.15;
   Double_t KWidthVary=0.03;

   Double_t PiCenterVary=0.1;
   Double_t PiHeightVary=0.1;
   Double_t PiWidthVary=0.05;  

   Double_t PCenterVary=0.1;
   Double_t PWidthVary=0.03;


     sumGs
    = new TF1("sumGs","gaus(0)+gaus(3)+gaus(6)", fitStart, fitEnd);

    Double_t pars_c[9] = { mPiGausHeight, mPiGausCenter, mPiSigma,
			   mKGausHeight,  mKGausCenter,  mKSigma,
			   mPGausHeight,  mPGausCenter,  mPSigma  };

    sumGs->SetParLimits( 0, mPiGausHeight*(1.-PiHeightVary),
			    mPiGausHeight*(1.+PiHeightVary) );
    sumGs->SetParLimits( 1, mPiGausCenter*(1.-PiCenterVary),
			    mPiGausCenter*(1.+PiCenterVary) );
    sumGs->SetParLimits( 2, mPiSigma*(1.-PiWidthVary),
			    mPiSigma*(1.+PiWidthVary) );


    sumGs->SetParLimits( 3, 0.,mKGausHeight);
    sumGs->SetParLimits( 4, mKGausCenter*(1.-KCenterVary),
			    mKGausCenter*(1.+KCenterVary) );
    sumGs->SetParLimits( 5, mKSigma*(1.-KWidthVary),
			    mKSigma*(1.+KWidthVary) );


    sumGs->SetParLimits( 6, 0.,mPGausHeight);
    sumGs->SetParLimits( 7, mPGausCenter*(1.-PCenterVary),
			    mPGausCenter*(1.+PCenterVary) );
    sumGs->SetParLimits( 8, mPSigma*(1.-PWidthVary),
			    mPSigma*(1.+PWidthVary) );

    sumGs->SetParameters(&pars_c[0]);
    
    theHist->Fit("sumGs","NR");

    protonRef[j][k]=sumGs->GetParameter(7); 

    if (sumGs) {delete sumGs;}
      }


     //get pionRef
    for (int j=0; j<mNEtaBins; j++)
      for (int k=2; k<mNNHitsBins;k++){

        int i = pionPosition4Calib;  //for pionRef

        //if dE/dx does not shift regarding to nhits, 
	//set kname=4.(just use long tracks)
        int kname = k; 
        int jname = j;

	char *theName = new char[80];
        if (i<10)
	  sprintf(theName,"h0%d%d%d",i,jname,kname);
        else sprintf(theName,"h%d%d%d",i,jname,kname);

	theHist= (TH1F *)histo4CalibFile.Get(theName);

      float pPosition = 
            float(i)*((mPEnd-mPStart)/float(mNPBins)) + 
             (((mPEnd-mPStart)/mNPBins)/2.);

  //setup initial values.
  RefreshPresettings(theHist, mSigmaOfSingleTrail[j] ,  k, pPosition);

   //setup vary range
   Double_t KCenterVary=0.15;
   Double_t KWidthVary=0.03;

   Double_t PiCenterVary=0.1;
   Double_t PiHeightVary=0.1;
   Double_t PiWidthVary=0.05;  

   Double_t PCenterVary=0.1;
   Double_t PWidthVary=0.03;

     sumGs
    = new TF1("sumGs","gaus(0)+gaus(3)+gaus(6)", fitStart, fitEnd);

    Double_t pars_c[9] = { mPiGausHeight, mPiGausCenter, mPiSigma,
			   mKGausHeight,  mKGausCenter,  mKSigma,
			   mPGausHeight,  mPGausCenter,  mPSigma  };

    sumGs->SetParLimits( 0, mPiGausHeight*(1.-PiHeightVary),
			    mPiGausHeight*(1.+PiHeightVary) );
    sumGs->SetParLimits( 1, mPiGausCenter*(1.-PiCenterVary),
			    mPiGausCenter*(1.+PiCenterVary) );
    sumGs->SetParLimits( 2, mPiSigma*(1.-PiWidthVary),
			    mPiSigma*(1.+PiWidthVary)  );


    sumGs->SetParLimits( 3, 0.,mKGausHeight );
    sumGs->SetParLimits( 4, mKGausCenter*(1.-KCenterVary),
			    mKGausCenter*(1.+KCenterVary) );
    sumGs->SetParLimits( 5, mKSigma*(1.-KWidthVary),
			    mKSigma*(1.+KWidthVary) );


    sumGs->SetParLimits( 6, 0.,mPGausHeight );
    sumGs->SetParLimits( 7, mPGausCenter*(1.-PCenterVary),
			    mPGausCenter*(1.+PCenterVary) );
    sumGs->SetParLimits( 8, mPSigma*(1.-PWidthVary),
			    mPSigma*(1.+PWidthVary) );

    sumGs->SetParameters(&pars_c[0]);
    
    theHist->Fit("sumGs","NR");

    pionRef[j][k]=sumGs->GetParameter(1); 


    if (sumGs) {delete sumGs; }
      }

    for (int j=0; j<mNEtaBins; j++)
      for (int k=2; k<mNNHitsBins;k++){
	double  theCalib=4.68971e-07;
        double  theOffSet=3.01022e-07;
        double  deltaReference=protonRef[j][k]-pionRef[j][k];
        double  mimimumIonizingPionPosition=
            float(pionPosition4Calib)*((mPEnd-mPStart)/float(mNPBins)) + 
             (((mPEnd-mPStart)/mNPBins)/2.);
        double  protonTestPosition=
            float(protonPosition4Calib)*((mPEnd-mPStart)/float(mNPBins)) + 
             (((mPEnd-mPStart)/mNPBins)/2.);

     BBScalePar[j][k]=look4MinDeltaDiff(theCalib*0.7, theCalib*1.3, 100,mimimumIonizingPionPosition ,protonTestPosition , deltaReference);
     BBOffSetPar[j][k]= theOffSet+minimumIonizingdEdx(BBScalePar[j][k],mimimumIonizingPionPosition )-pionRef[j][k];

     cout<<" j "<<j<<" k "<<k<<" BBScalePar["<<j<<"]["<<k<<"]="<<BBScalePar[j][k]<<";  BBOffSetPar["<<j<<"]["<<k<<"]="<<BBOffSetPar[j][k]<<"; "<<endl;

      }

    //now deal with k<2
    for (int j=0; j<mNEtaBins; j++)
      for (int k=0; k<2; k++) {
	BBScalePar[j][k]=BBScalePar[j][2];
        BBOffSetPar[j][k]=BBOffSetPar[j][2];
      }  //nhits<10, hard to calibrate. use nhits 10-15 instead.


       ofstream PhaseSpaceFileOut;
       PhaseSpaceFileOut.open(phaseSpaceCalibFileName);

     for (int j=0; j<mNEtaBins; j++)
       for (int k=0; k<mNNHitsBins; k++) {
	 PhaseSpaceFileOut<<j<<endl<<k<<endl;
	 PhaseSpaceFileOut<<electronBandCenter->GetParameter(0)<<endl;
	 PhaseSpaceFileOut<<electronBandCenter->GetParameter(1)<<endl;
	 PhaseSpaceFileOut<<BBOffSetPar[j][k]<<endl;
         PhaseSpaceFileOut<<BBScalePar[j][k]<<endl;
	 PhaseSpaceFileOut<<electronBandCenter->GetParameter(6)<<endl;

       }

     PhaseSpaceFileOut.close();

     cout<<"  phase space calibration finished ..........  "<<endl;

}


//--------------------------------------------------------------------------
void PIDFitter::Init(){

  mWriteGaus4SigmaNSampleHist=kFALSE;

  //create a prototype for update in FitResoGaus()
  if (mWriteGaus4SigmaNSampleHist){
    TFile* fitResoFile = new TFile("Gaus4SigmaNSample.root","RECREATE");
    fitResoFile->Write();
    fitResoFile->Close();
    delete fitResoFile;
  }

  mWriteSigmaNSampleGraph=kFALSE;

  if (mWriteSigmaNSampleGraph) {
     TFile* fitResoFile = new TFile("sigmaNSample.root","RECREATE");
     delete fitResoFile;
  }



  mSigmaOfSingleTrail = new double[mNEtaBins];

  BBOffSetPar = new double*[mNEtaBins];
  BBScalePar  = new double*[mNEtaBins];

  for (int ii=0; ii<mNEtaBins; ii++) {
    BBOffSetPar[ii] = new double[mNNHitsBins];
    BBScalePar[ii]  = new double[mNNHitsBins];
  }

  

  //allocate functions...
  electronBandCenter 
     =new TF1("electronBandCenter",BetheBlochFunction, mPStart,mPEnd, NParameters); 
  pionBandCenter 
     =new TF1("pionBandCenter",BetheBlochFunction, mPStart,mPEnd, NParameters); 
  kaonBandCenter 
     =new TF1("kaonBandCenter",BetheBlochFunction, mPStart,mPEnd, NParameters); 
  antiprotonBandCenter 
     =new TF1("antiprotonBandCenter",BetheBlochFunction, mPStart,mPEnd, NParameters); 

  pionKaonBandCenter //for drawing line between pion and kaon bands
     =new TF1("pionKaonBandCenter",BetheBlochFunction, mPStart,mPEnd, NParameters);
  kaonAntiprotonBandCenter //for drawing line between kaon and antiproton bands
     =new TF1("kaonAntiprotonBandCenter",BetheBlochFunction, mPStart,mPEnd, NParameters);





  Double_t electronPars[7]
        ={ 1.09344, 0.0199, 3.01022e-07, 1, 0.511e-3, 4.68971e-07, 0.0005 };
  Double_t pionPars[7]
        ={ 1.09344, 0.0199, 3.01022e-07, 1, 0.13957,  4.68971e-07, 0.0005 };
  Double_t kaonPars[7]
        ={ 1.09344, 0.0199, 3.01022e-07, 1, 0.49368,  4.68971e-07, 0.0005 };
  Double_t antiprotonPars[7]
        ={ 1.09344, 0.0199, 3.01022e-07, 1, 0.93827,  4.68971e-07, 0.0005 };

  Double_t pionKaonPars[7]
        ={ 1.09344, 0.0199, 3.01022e-07, 1, 0.387447*1.1,  4.68971e-07, 0.0005 };

  Double_t kaonAntiprotonPars[7]
        ={ 1.09344, 0.0199, 3.01022e-07, 1, 0.804893*1.05,  4.68971e-07, 0.0005 };

   electronBandCenter->SetParameters(&electronPars[0]);
       pionBandCenter->SetParameters(&pionPars[0]);
       kaonBandCenter->SetParameters(&kaonPars[0]);
     antiprotonBandCenter->SetParameters(&antiprotonPars[0]);
   pionKaonBandCenter->SetParameters(&pionKaonPars[0]);
 kaonAntiprotonBandCenter->SetParameters(&kaonAntiprotonPars[0]);



}


//--------------------------------------------------------------------------
double PIDFitter::delta(double calib, double pionPosition, double protonPosition){ //for phase space calibration use only.

    pionBandCenter->SetParameter(5,calib);
    antiprotonBandCenter->SetParameter(5,calib);

    return (antiprotonBandCenter->Eval(protonPosition,0,0)-
            pionBandCenter->Eval(pionPosition,0,0));
}

//--------------------------------------------------------------------------
double PIDFitter::look4MinDeltaDiff(double calibStart, double calibEnd, int calibSteps, double pionPosition, double protonPosition, double DeltaRef){

     double calibSeg=(calibEnd-calibStart)/double(calibSteps);
     double thisCalib=calibStart;
     double minDeltaDiffCalib=5000;//calib associated with minDeltaDiff.
     double minDeltaDiff=5000;

     do {

        double myDelta=delta(thisCalib,pionPosition,protonPosition);
        double diff=TMath::Abs(myDelta-DeltaRef);
        if (diff<minDeltaDiff) {
            minDeltaDiff=diff;
            minDeltaDiffCalib=thisCalib;
	}
          
        thisCalib=thisCalib+calibSeg;
     }while(thisCalib<calibEnd);

     return minDeltaDiffCalib;
}

//--------------------------------------------------------------------------
double PIDFitter::minimumIonizingdEdx(double calib, double pionPosition){
    pionBandCenter->SetParameter(5,calib);
    
    return pionBandCenter->Eval(pionPosition,0,0);
}


//--------------------------------------------------------------------------
void PIDFitter::FitMultiGaus(Char_t* fileNameOfInput, Char_t* fileNameOfOutput){
  //ATTN: DoPhaseSpaceCalibration() and GetSigmaOfSingleTrail have to be called
  //befor calling this function, otherwise it brokes.

  TFile histoFile(fileNameOfInput,"READ");

  TFile fittedHistoFile(fileNameOfOutput,"RECREATE");
  

 TH1F* theHist =0;
 TF1*  sumGs   =0;

    for (int i=0; i<mNPBins; i++)
     for (int j=0; j<mNEtaBins; j++)
      for (int k=0; k<mNNHitsBins;k++){

        cout<<" p bin # "<<i<<", eta bin # "<<j<<", ndedx bin # "<<k<<endl;

	char *theName = new char[80];
        if (i<10)
	  sprintf(theName,"h0%d%d%d",i,j,k);
        else sprintf(theName,"h%d%d%d",i,j,k);

	theHist= (TH1F *)histoFile.Get(theName);

      float pPosition = 
            float(i)*((mPEnd-mPStart)/float(mNPBins)) + 
             (((mPEnd-mPStart)/mNPBins)/2.);

      //setup initial values.

       electronBandCenter->SetParameter(5,BBScalePar[j][k]);
           pionBandCenter->SetParameter(5,BBScalePar[j][k]);
           kaonBandCenter->SetParameter(5,BBScalePar[j][k]);
     antiprotonBandCenter->SetParameter(5,BBScalePar[j][k]);
       pionKaonBandCenter->SetParameter(5,BBScalePar[j][k]);
 kaonAntiprotonBandCenter->SetParameter(5,BBScalePar[j][k]);

       electronBandCenter->SetParameter(2,BBOffSetPar[j][k]);
           pionBandCenter->SetParameter(2,BBOffSetPar[j][k]);
           kaonBandCenter->SetParameter(2,BBOffSetPar[j][k]);
     antiprotonBandCenter->SetParameter(2,BBOffSetPar[j][k]);
       pionKaonBandCenter->SetParameter(2,BBOffSetPar[j][k]);
 kaonAntiprotonBandCenter->SetParameter(2,BBOffSetPar[j][k]);

  //setup initial values.
  RefreshPresettings(theHist, mSigmaOfSingleTrail[j] ,  k, pPosition);

   //setup vary range
   Double_t KCenterVary=0.1;
   Double_t KWidthVary=0.1;

   Double_t PiCenterVary=0.1;
   Double_t PiHeightVary=0.1;
   Double_t PiWidthVary=0.1;  

   Double_t PCenterVary=0.1;
   Double_t PWidthVary=0.1;

   Double_t ECenterVary=0.;
   Double_t EWidthVary=0.1;

   //for low p range, the width for pion and e is big, allow them vary for a better fitting.

   if (pPosition<0.13) {
     PiWidthVary=0.5;
     PiCenterVary=0.2;
     EWidthVary=0.5;
     ECenterVary=0.2;
   }


 ///////////  different sum Gaussian for different p range   //////////
 //             p<0.2  g=pion+e                                      //
 //        0.2 <p<0.44 g=pion+e+kaon                                 //
 //        0.44<p<0.85 g=pion+e+kaon+antiproton                      //
 //             p>0.85 g=pion+kaon+antiproton                        //
 //////////////////////////////////////////////////////////////////////

      Double_t fitStart=0;
      Double_t fitEnd=0; 

      if (pPosition<junction1){//p<0.2  g=pion+e

	Double_t kaonBandLowEdge=pionKaonBandCenter->Eval(pPosition,0.,0.);
        fitEnd = TMath::Min(kaonBandLowEdge,mDedxEnd);

     sumGs
    = new TF1("sumGs","gaus(0)+gaus(3)", fitStart, fitEnd);

    Double_t pars_a[6] = { mPiGausHeight, mPiGausCenter, mPiSigma,
			   mEGausHeight,  mEGausCenter,  mESigma };

    sumGs->SetParameters(&pars_a[0]);

    sumGs->SetParLimits( 0, mPiGausHeight*(1.-PiHeightVary),
			    mPiGausHeight*(1.+PiHeightVary) );
    sumGs->SetParLimits( 1, mPiGausCenter*(1.-PiCenterVary),
			    mPiGausCenter*(1.+PiCenterVary) );
    sumGs->SetParLimits( 2, mPiSigma*(1.-PiWidthVary),
			    mPiSigma*(1.+PiWidthVary) );

    sumGs->SetParLimits( 3, 0., mEGausHeight );
    sumGs->SetParLimits( 4, mEGausCenter*(1.-ECenterVary),
			    mEGausCenter*(1.+ECenterVary));
    sumGs->SetParLimits( 5, mESigma*(1.-EWidthVary),
			    mESigma*(1.+EWidthVary) );

    
    if (theHist->GetEntries()>1.) theHist->Fit("sumGs","R");

    fittedHistoFile.cd();
    theHist->Write();
    if (sumGs) delete sumGs;

      }



      if (pPosition>=junction1 && pPosition<junction2){//0.2<p<0.44 g=pion+e+kaon

	Double_t antiprotonBandLowEdge=kaonAntiprotonBandCenter->Eval(pPosition,0.,0.);
        fitEnd = TMath::Min(antiprotonBandLowEdge,mDedxEnd);

     sumGs
    = new TF1("sumGs","gaus(0)+gaus(3)+gaus(6)", fitStart, fitEnd);

    Double_t pars_b[9] = { mPiGausHeight, mPiGausCenter, mPiSigma,
			   mEGausHeight,  mEGausCenter,  mESigma,
			   mKGausHeight,  mKGausCenter,  mKSigma };

    sumGs->SetParameters(&pars_b[0]);


    sumGs->SetParLimits( 0, mPiGausHeight*(1.-PiHeightVary),
			    mPiGausHeight*(1.+PiHeightVary));
    sumGs->SetParLimits( 1, mPiGausCenter*(1.-PiCenterVary),
			    mPiGausCenter*(1.+PiCenterVary) );
    sumGs->SetParLimits( 2, mPiSigma*(1.-PiWidthVary),
			    mPiSigma*(1.+PiWidthVary) );

    sumGs->SetParLimits( 3, 0., mEGausHeight );
    sumGs->SetParLimits( 4, mEGausCenter*(1.-ECenterVary),
			    mEGausCenter*(1.+ECenterVary) );
    sumGs->SetParLimits( 5, mESigma*(1.-EWidthVary),
			    mESigma*(1.+EWidthVary) );

    sumGs->SetParLimits( 6, 0., mKGausHeight );
    sumGs->SetParLimits( 7, mKGausCenter*(1.-KCenterVary),
			    mKGausCenter*(1.+KCenterVary) );
    sumGs->SetParLimits( 8, mKSigma*(1.-KWidthVary),
			    mKSigma*(1.+KWidthVary) );

    
    theHist->Fit("sumGs","R");

    fittedHistoFile.cd();
    theHist->Write();
    if (sumGs) delete sumGs;
      }



      if (pPosition>=junction2 && pPosition<junction3){//0.44<p<0.85 g=pion+e+kaon+antiproton

        fitEnd = mDedxEnd;

     sumGs
    = new TF1("sumGs","gaus(0)+gaus(3)+gaus(6)+gaus(9)", fitStart, fitEnd);

    Double_t pars_c[12] = { mPiGausHeight, mPiGausCenter, mPiSigma,
			    mEGausHeight,  mEGausCenter,  mESigma,
			    mKGausHeight,  mKGausCenter,  mKSigma,
			    mPGausHeight,  mPGausCenter,  mPSigma };

    sumGs->SetParLimits( 0, mPiGausHeight*(1.-PiHeightVary),
			    mPiGausHeight*(1.+PiHeightVary) );
    sumGs->SetParLimits( 1, mPiGausCenter*(1.-PiCenterVary),
			    mPiGausCenter*(1.+PiCenterVary) );
    sumGs->SetParLimits( 2, mPiSigma*(1.-PiWidthVary),
			    mPiSigma*(1.+PiWidthVary) );


    sumGs->SetParLimits( 3, 0., mEGausHeight );
    sumGs->SetParLimits( 4, mEGausCenter*(1.-ECenterVary),
			    mEGausCenter*(1.+ECenterVary) );
    sumGs->SetParLimits( 5, mESigma*(1.-EWidthVary),
			    mESigma*(1.+EWidthVary) );


    sumGs->SetParLimits( 6, 0., mKGausHeight );
    sumGs->SetParLimits( 7, mKGausCenter*(1.-KCenterVary),
			    mKGausCenter*(1.+KCenterVary) );
    sumGs->SetParLimits( 8, mKSigma*(1.-KWidthVary),
			    mKSigma*(1.+KWidthVary) );


    sumGs->SetParLimits( 9, 0., mPGausHeight );
    sumGs->SetParLimits(10, mPGausCenter*(1.-PCenterVary),
			    mPGausCenter*(1.+PCenterVary) );
    sumGs->SetParLimits(11, mPSigma*(1.-PWidthVary),
			    mPSigma*(1.+PWidthVary) );

    sumGs->SetParameters(&pars_c[0]);
    
    theHist->Fit("sumGs","R");

    fittedHistoFile.cd();
    theHist->Write();
    if (sumGs) delete sumGs;
      }


      if (pPosition>=junction3 && pPosition<2.){//p>0.85 g=pion+kaon+antiproton

        fitEnd = mDedxEnd;

     sumGs
    = new TF1("sumGs","gaus(0)+gaus(3)+gaus(6)", fitStart, fitEnd);

    Double_t pars_d[9] = { mPiGausHeight, mPiGausCenter, mPiSigma,
			   mKGausHeight,  mKGausCenter,  mKSigma,
			   mPGausHeight,  mPGausCenter,  mPSigma };

    sumGs->SetParLimits( 0, mPiGausHeight*(1.-PiHeightVary),
			    mPiGausHeight*(1.+PiHeightVary));
    sumGs->SetParLimits( 1, mPiGausCenter*(1.-PiCenterVary),
			    mPiGausCenter*(1.+PiCenterVary));
    sumGs->SetParLimits( 2, mPiSigma*(1.-PiWidthVary),
			    mPiSigma*(1.+PiWidthVary));



    sumGs->SetParLimits( 3, 0., mKGausHeight );
    sumGs->SetParLimits( 4, mKGausCenter*(1.-KCenterVary),
			    mKGausCenter*(1.+KCenterVary));
    sumGs->SetParLimits( 5, mKSigma*(1.-KWidthVary),
			    mKSigma*(1.+KWidthVary));


    sumGs->SetParLimits( 6, 0., mPGausHeight );
    sumGs->SetParLimits( 7, mPGausCenter*(1.-PCenterVary),
			    mPGausCenter*(1.+PCenterVary));
    sumGs->SetParLimits( 8, mPSigma*(1.-PWidthVary),
			    mPSigma*(1.+PWidthVary));

    sumGs->SetParameters(&pars_d[0]);
    
    theHist->Fit("sumGs","R");

    fittedHistoFile.cd();
    theHist->Write();
    if (sumGs) delete sumGs;
      }


      }


      fittedHistoFile.Write();
      fittedHistoFile.Close();
}

//---------------------------------------------------------------------------
void  PIDFitter::ExtrapAmp(Char_t* fileNameOfInput, Char_t* fileNameOfOutput){

       cout<<" **********extraping pion amp. begin********* "<<endl;

       Double_t pBegin      = mPStart;
       Double_t pEnd        = mPEnd;
 
  TFile* gausHistFile = new TFile(fileNameOfInput,"READ");

  TFile* ampHistFile = new TFile(fileNameOfOutput,"RECREATE");


        for (int j=0; j<mNEtaBins; j++)
           for (int k=0; k<mNNHitsBins;k++){

	char *PiAmpName = new char[80];
        sprintf(PiAmpName,"PiAmp%d%d",j,k);
	char *EAmpName = new char[80];
        sprintf(EAmpName,"EAmp%d%d",j,k);
 	char *KAmpName = new char[80];
        sprintf(KAmpName,"KAmp%d%d",j,k);
	char *PAmpName = new char[80];
        sprintf(PAmpName,"PAmp%d%d",j,k);

     TH1F* PiAmpHisto = new TH1F(PiAmpName, PiAmpName, mNPBins,mPStart,mPEnd);
     TH1F* EAmpHisto    = new TH1F(EAmpName,  EAmpName,  mNPBins,mPStart,mPEnd);
     TH1F* KAmpHisto    = new TH1F(KAmpName,  KAmpName,  mNPBins,mPStart,mPEnd);
     TH1F* PAmpHisto    = new TH1F(PAmpName,  PAmpName,  mNPBins,mPStart,mPEnd);

     PiAmpHisto->SetLineColor(4);
     EAmpHisto->SetLineColor(6);
     KAmpHisto->SetLineColor(2);
     PAmpHisto->SetLineColor(3);



     double nhitsPosition=double((k+0.5)*(float(mNNHitsEnd-mNNHitsStart)/float(mNNHitsBins)));


        for (int i=0; i<mNPBins; i++){ 

         float pPosition = 
            float(i)*((pEnd-pBegin)/float(mNPBins)) + 
             (((pEnd-pBegin)/mNPBins)/2.);

        char *theHistName = new char[80];
        if (i<10)
	  sprintf(theHistName,"h0%d%d%d",i,j,k);
        else sprintf(theHistName,"h%d%d%d",i,j,k);

      TH1F* theHist= (TH1F *)gausHistFile->Get(theHistName);


      TF1*  sumGs=theHist->GetFunction("sumGs");
 
	if (!sumGs) continue; //due to bin 0-3 might not be fitted (no entries)

      if (pPosition<junction1){//p<0.2  g=pion+e
   PiAmpHisto->SetBinContent(i+1,sumGs->GetParameter(0));
   PiAmpHisto->SetBinError(i+1, sumGs->GetParError(0));
    EAmpHisto->SetBinContent(i+1,sumGs->GetParameter(3));
    EAmpHisto->SetBinError(i+1, sumGs->GetParError(3));
      }

      if (pPosition>=junction1 && pPosition<junction2){//0.2<p<0.38 g=pion+e+kaon
   PiAmpHisto->SetBinContent(i+1,sumGs->GetParameter(0));
   PiAmpHisto->SetBinError(i+1, sumGs->GetParError(0));
    EAmpHisto->SetBinContent(i+1,sumGs->GetParameter(3));
    EAmpHisto->SetBinError(i+1, sumGs->GetParError(3));
    KAmpHisto->SetBinContent(i+1,sumGs->GetParameter(6));
    KAmpHisto->SetBinError(i+1, sumGs->GetParError(6));
      }
      if (pPosition>=junction2 && pPosition<junction3 ){//0.85>p>0.38 g=pion+e+kaon+antiproton
   PiAmpHisto->SetBinContent(i+1,sumGs->GetParameter(0));
   PiAmpHisto->SetBinError(i+1, sumGs->GetParError(0));
    EAmpHisto->SetBinContent(i+1,sumGs->GetParameter(3));
    EAmpHisto->SetBinError(i+1, sumGs->GetParError(3));
    KAmpHisto->SetBinContent(i+1,sumGs->GetParameter(6));
    KAmpHisto->SetBinError(i+1, sumGs->GetParError(6));
    PAmpHisto->SetBinContent(i+1,sumGs->GetParameter(9));
    PAmpHisto->SetBinError(i+1, sumGs->GetParError(9));
      }

    if (pPosition>=junction3 && pPosition<2.){//p>0.85 g=pion+kaon+antiproton
   PiAmpHisto->SetBinContent(i+1,sumGs->GetParameter(0));
   PiAmpHisto->SetBinError(i+1, sumGs->GetParError(0));
    KAmpHisto->SetBinContent(i+1,sumGs->GetParameter(3));
    KAmpHisto->SetBinError(i+1, sumGs->GetParError(3));
    PAmpHisto->SetBinContent(i+1,sumGs->GetParameter(6));
    PAmpHisto->SetBinError(i+1, sumGs->GetParError(6));
    }


	}

  //fit Pi Amp
  double  low        = 0.15; 
  double  high       = 0.6;
  double  ampPeakPos = 0.45;


                 //find the peak position of Pi amp.
  for (int t=0; t<4; t++){
  TF1* gs = new TF1("gs","gaus",low,high);
  PiAmpHisto->Fit("gs","NWR");
  low  = gs->GetParameter(1)-0.13;
  high = gs->GetParameter(1)+0.13;
  ampPeakPos= gs->GetParameter(1);
  delete gs;
  }

  //  if (nhitsPosition <= 15.) {//ampPeakPos found fails for low ndedx, assign a number.
  ampPeakPos=0.6; 
  //  }


  TF1* PiFcnCenter = new TF1("PiFcnCenter","pol3",0.4,0.7);

  TF1* PiFcnLeft   = new TF1("PiFcnLeft","pol3",0.1,ampPeakPos-0.05);
  PiFcnLeft->SetLineColor(4);

  TF1* PiFcnRight  = new TF1("PiFcnRight","expo",0.7,1.4);
  PiFcnRight->SetLineColor(3);

    PiAmpHisto->Fit("PiFcnCenter","WR+");
  //  PiAmpHisto->Fit("PiFcnLeft",  "WR+");

  // blow error between 0.85-1.2 GeV/c, because K pi crosssing, put less weight on fitting for that range.
  
  float crossBandBegin = 0.8;
  float brossBandEnd   = 1.2;


  TGraphErrors* PiAmpGraphRight = new TGraphErrors();
  
  for (int ampbin = 0; ampbin<PiAmpHisto->GetNbinsX(); ampbin++){
    if (PiAmpHisto->GetBinCenter(ampbin) < crossBandBegin || 
        PiAmpHisto->GetBinCenter(ampbin) > brossBandEnd){ 
    PiAmpGraphRight->SetPoint(PiAmpGraphRight->GetN(), PiAmpHisto->GetBinCenter(ampbin), PiAmpHisto->GetBinContent(ampbin));
    PiAmpGraphRight->SetPointError(PiAmpGraphRight->GetN()-1, PiAmpHisto->GetBinCenter(ampbin), PiAmpHisto->GetBinContent(ampbin));
    }
  }


  PiAmpGraphRight->Fit("PiFcnRight", "WR+");

  PiAmpHisto->GetListOfFunctions()->Add(PiAmpGraphRight->GetFunction("PiFcnRight"));


  if (PiFcnCenter) delete PiFcnCenter;
  if (PiFcnLeft)   delete PiFcnLeft;
  if (PiFcnRight)  delete PiFcnRight;

  //now fit E tail 
  float EAmpFitBegin = EAmpHisto->GetBinCenter(EAmpHisto->GetMaximumBin())+0.03;//was +0.04
  

  if (nhitsPosition <= 15. && nhitsPosition >10) EAmpFitBegin +=0.05; //peak shift.
  if (nhitsPosition <= 10. && nhitsPosition >5) EAmpFitBegin +=0.09; //peak shift.


  TF1* EFcnLeft = new TF1("EFcnLeft","expo",EAmpFitBegin,EAmpFitBegin+0.22);
       EFcnLeft->SetLineColor(2);
  TF1* EFcnRight = new TF1("EFcnRight","expo",0.35,0.8);
       EFcnRight->SetLineColor(4);

  //  EAmpHisto->Fit("EFcnLeft","RW+");

   crossBandBegin = 0.45;
   brossBandEnd   = 0.6;

  TGraphErrors* EAmpGraphRight = new TGraphErrors();
  
  for (int ampbin = 0; ampbin<EAmpHisto->GetNbinsX(); ampbin++){
    if (EAmpHisto->GetBinCenter(ampbin) < crossBandBegin || 
        EAmpHisto->GetBinCenter(ampbin) > brossBandEnd){ 
    EAmpGraphRight->SetPoint(EAmpGraphRight->GetN(), EAmpHisto->GetBinCenter(ampbin), EAmpHisto->GetBinContent(ampbin));
    EAmpGraphRight->SetPointError(EAmpGraphRight->GetN()-1, EAmpHisto->GetBinCenter(ampbin), EAmpHisto->GetBinContent(ampbin));
    }
  }


  EAmpGraphRight->Fit("EFcnRight", "WR+");

  EAmpHisto->GetListOfFunctions()->Add(EAmpGraphRight->GetFunction("EFcnRight"));





  if (EFcnLeft)  delete EFcnLeft;
  if (EFcnRight)  delete EFcnRight;


  //now fit K 
  low        = 0.4; 
  high       = 0.9;
  ampPeakPos = 0.68;

          //find the peak position of K amp.

  /*
  for (int t=0; t<4; t++){
  TF1* gs = new TF1("gs","gaus",low,high);
  KAmpHisto->Fit("gs","NWR");
  low  = gs->GetParameter(1)-0.1;
  high = gs->GetParameter(1)+0.1;
  ampPeakPos= gs->GetParameter(1);
  delete gs;
  }
  */


  TF1* KFcnCenter = new TF1("KFcnCenter","gaus",low,high);

  TF1* KFcnLeft   = new TF1("KFcnLeft","pol3", (nhitsPosition <= 25.) ? 0.28 : 0.35, 0.65);
  KFcnLeft->SetLineColor(4);

  TF1* KFcnRight  = new TF1("KFcnRight","expo",0.8,2.);
  KFcnRight->SetLineColor(3);


   crossBandBegin = 0.48;
   brossBandEnd   = 0.58;


  TGraphErrors* KAmpGraphLeft = new TGraphErrors();
  
  for (int ampbin = 0; ampbin<KAmpHisto->GetNbinsX(); ampbin++){
    if (KAmpHisto->GetBinCenter(ampbin) < crossBandBegin || 
        KAmpHisto->GetBinCenter(ampbin) > brossBandEnd){ 
    KAmpGraphLeft->SetPoint(KAmpGraphLeft->GetN(), KAmpHisto->GetBinCenter(ampbin), KAmpHisto->GetBinContent(ampbin));
    KAmpGraphLeft->SetPointError(KAmpGraphLeft->GetN()-1, KAmpHisto->GetBinCenter(ampbin), KAmpHisto->GetBinContent(ampbin));
    }
  }

  KAmpGraphLeft->Fit("KFcnLeft", "WR+");
  KAmpHisto->GetListOfFunctions()->Add(KAmpGraphLeft->GetFunction("KFcnLeft"));
   crossBandBegin = 0.9;
   brossBandEnd   = 1.5;


  TGraphErrors* KAmpGraphRight = new TGraphErrors();
  
  for (int ampbin = 0; ampbin<KAmpHisto->GetNbinsX(); ampbin++){
    if (KAmpHisto->GetBinCenter(ampbin) < crossBandBegin || 
        KAmpHisto->GetBinCenter(ampbin) > brossBandEnd){ 
    KAmpGraphRight->SetPoint(KAmpGraphRight->GetN(), KAmpHisto->GetBinCenter(ampbin), KAmpHisto->GetBinContent(ampbin));
    KAmpGraphRight->SetPointError(KAmpGraphRight->GetN()-1, KAmpHisto->GetBinCenter(ampbin), KAmpHisto->GetBinContent(ampbin));
    }
  }


  KAmpGraphRight->Fit("KFcnRight", "WR+");

  KAmpHisto->GetListOfFunctions()->Add(KAmpGraphRight->GetFunction("KFcnRight"));

  KAmpHisto->Fit("KFcnCenter","WR+");

  //for the part overlay with FcnLeft, use FcnLeft
  if ( KFcnCenter->GetXmin()<KFcnLeft->GetXmax() && KFcnLeft->GetXmax() < KFcnCenter->GetXmax() ) 
  KAmpHisto->GetFunction("KFcnCenter")->SetRange(KFcnLeft->GetXmax()-0.05, KFcnCenter->GetXmax());

  //  KAmpHisto->GetListOfFunctions()->Add(KAmpHisto->GetFunction("KFcnCenter"));

  if (KFcnCenter) delete KFcnCenter;
  if (KFcnLeft)   delete KFcnLeft;
  if (KFcnRight)  delete KFcnRight;


  //now fit P

  TF1* PFcnLeft = new TF1("PFcnLeft","pol4",0.6,1.4);
  TF1* PFcnRight = new TF1("PFcnRight","expo",1.4,2.);

  crossBandBegin = 1.5;
  brossBandEnd   = 1.8;


  TGraphErrors* PAmpGraphRight = new TGraphErrors();

  for (int ampbin = 0; ampbin<PAmpHisto->GetNbinsX(); ampbin++){
    if (PAmpHisto->GetBinCenter(ampbin) < crossBandBegin ||
        PAmpHisto->GetBinCenter(ampbin) > brossBandEnd){
      PAmpGraphRight->SetPoint(PAmpGraphRight->GetN(), PAmpHisto->GetBinCenter(ampbin), PAmpHisto->GetBinContent(ampbin));
      PAmpGraphRight->SetPointError(PAmpGraphRight->GetN()-1, PAmpHisto->GetBinCenter(ampbin), PAmpHisto->GetBinContent(ampbin));
    }
  }


  PAmpGraphRight->Fit("PFcnRight", "WR+");

  PAmpHisto->GetListOfFunctions()->Add(PAmpGraphRight->GetFunction("PFcnRight"));


  PAmpHisto->Fit("PFcnLeft","RW+");

  if (PFcnRight) delete PFcnRight;
  if (PFcnLeft) delete PFcnLeft;

  //write hito.
  
  ampHistFile->cd();
  PiAmpHisto->Write(PiAmpHisto->GetName(),TObject::kOverwrite | TObject::kSingleKey);
   EAmpHisto->Write(EAmpHisto->GetName(),TObject::kOverwrite | TObject::kSingleKey);
   KAmpHisto->Write(KAmpHisto->GetName(),TObject::kOverwrite | TObject::kSingleKey);
   PAmpHisto->Write(PAmpHisto->GetName(),TObject::kOverwrite | TObject::kSingleKey);


	   }

ampHistFile->Write();
ampHistFile->Close();
gausHistFile->Close();
       cout<<" **********extraping pion amp. end********* "<<endl;
}


Double_t sigmaNSampleFitFcn(Double_t* x, Double_t *par){

  if (x[0])  return par[0]/::sqrt(x[0]);
  else return 0.;
}

///////////////////////////////////////////////////////////////////////////////
//
// $Log: PIDFitter.cxx,v $
// Revision 1.11  2007/07/12 19:47:31  fisyak
// Add includes for ROOT 5.16
//
// Revision 1.10  2003/09/07 03:49:04  perev
// gcc 3.2 + WarnOff
//
// Revision 1.9  2003/09/02 17:58:46  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.8  2003/08/04 18:46:26  perev
// warnOff
//
// Revision 1.7  2003/07/25 15:04:17  aihong
// change fit range etc.
//
// Revision 1.4  2002/12/17 17:05:48  aihong
// write out sigmaNsample graph
//
// Revision 1.3  2002/03/27 14:19:21  aihong
// use two functions to fit e+/- amp. instead of one
//
// Revision 1.2  2002/02/25 18:31:24  jeromel
// More SetFormat() stripped.
//
// Revision 1.1  2002/02/14 21:25:55  aihong
// re-install the new version
//

//
/////////////////////////////////////////////////////////////////////////////////
