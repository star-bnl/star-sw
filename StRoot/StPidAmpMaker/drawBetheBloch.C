#include "StRoot/StEventUtilities/BetheBlochFunction.hh"

void drawBetheBloch(){


    gSystem->Load("St_base");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StEvent");
    gSystem->Load("StEventUtilities");

TFile f("nhitsBin_0_10_20_45_ptBin_0_Inf_dcaBin_0_2_50000_BGBands.root",
     "READ","nhitsBin_0_10_20_45_ptBin_0_Inf_dcaBin_0_2_50000_BGBands.root");
     
  StPidAmpNetOut* electronNetOut=(StPidAmpNetOut *)f.Get("e-"); 
  StPidAmpNetOut* pionMinusNetOut=(StPidAmpNetOut *)f.Get("pi-"); 
  StPidAmpNetOut* kaonMinusNetOut=(StPidAmpNetOut *)f.Get("kaon-"); 
  StPidAmpNetOut* antiProtonNetOut=(StPidAmpNetOut *)f.Get("anti_proton"); 

  int NParameters=electronNetOut->GetNBandPars(); 

  TF1* electronBandCenter 
     =new TF1("electronBandCenter",BetheBlochFunction, 0.02,5, NParameters); 
  TF1* pionMinusBandCenter 
     =new TF1("pionMinusBandCenter",BetheBlochFunction, 0.02,5, NParameters); 
  TF1* kaonMinusBandCenter 
     =new TF1("kaonMinusBandCenter",BetheBlochFunction, 0.02,5, NParameters); 
  TF1* antiProtonBandCenter 
     =new TF1("antiProtonBandCenter",BetheBlochFunction, 0.02,5, NParameters); 


  electronBandCenter->SetMinimum(0.05e-05);
  pionMinusBandCenter->SetMinimum(0.05e-05);
  kaonMinusBandCenter->SetMinimum(0.05e-05);
  antiProtonBandCenter->SetMinimum(0.05e-05);

  for ( int i=0; i<NParameters;i++){
  electronBandCenter->
       SetParameter(i,(electronNetOut->GetBandParArray())->At(i));
  pionMinusBandCenter->
       SetParameter(i,(pionMinusNetOut->GetBandParArray())->At(i));
  kaonMinusBandCenter->
       SetParameter(i,(kaonMinusNetOut->GetBandParArray())->At(i));
  antiProtonBandCenter->
       SetParameter(i,(antiProtonNetOut->GetBandParArray())->At(i));
  }


  TCanvas* myCanvas=new TCanvas("bandCenter");

  myCanvas->cd();

   electronBandCenter->Draw("L");
   pionMinusBandCenter->Draw("SAME");
   kaonMinusBandCenter->Draw("SAME");
   antiProtonBandCenter->Draw("SAME");




}
