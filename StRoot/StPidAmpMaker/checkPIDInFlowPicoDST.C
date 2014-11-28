#include "/afs/rhic.bnl.gov/star/replicas/DEV/StRoot/StEventUtilities/BetheBlochFunction.hh"

void checkPIDInFlowPicoDST(){


  TChain* chain = new TChain("FlowTree");

  chain->Add("/star/data10/GC/aihong/P01glPIDFlowPicoDST/st_physics_2269001_raw_0001.event.root.flowpicoevent.root");

  chain->Add("/star/data10/GC/aihong/P01glPIDFlowPicoDST/st_physics_2269001_raw_0003.event.root.flowpicoevent.root");
  chain->Add("/star/data10/GC/aihong/P01glPIDFlowPicoDST/st_physics_2269001_raw_0005.event.root.flowpicoevent.root");



  chain->Add("/star/data10/GC/aihong/P01glPIDFlowPicoDST/st_physics_2269001_raw_0004.event.root.flowpicoevent.root");
  chain->Add("/star/data10/GC/aihong/P01glPIDFlowPicoDST/st_physics_2269001_raw_0008.event.root.flowpicoevent.root");
  chain->Add("/star/data10/GC/aihong/P01glPIDFlowPicoDST/st_physics_2269001_raw_0015.event.root.flowpicoevent.root");



  chain->Add("/star/data10/GC/aihong/P01glPIDFlowPicoDST/st_physics_2269001_raw_0009.event.root.flowpicoevent.root");
  chain->Add("/star/data10/GC/aihong/P01glPIDFlowPicoDST/st_physics_2269001_raw_0012.event.root.flowpicoevent.root");
  chain->Add("/star/data10/GC/aihong/P01glPIDFlowPicoDST/st_physics_2269001_raw_0016.event.root.flowpicoevent.root");
  //  chain->Add("/star/data10/GC/aihong/P01glPIDFlowPicoDST/st_physics_2269001_raw_0011.event.root.flowpicoevent.root");
  chain->Add("/star/data10/GC/aihong/P01glPIDFlowPicoDST/st_physics_2269001_raw_0014.event.root.flowpicoevent.root");
  chain->Add("/star/data10/GC/aihong/P01glPIDFlowPicoDST/st_physics_2269001_raw_0013.event.root.flowpicoevent.root");
  chain->Add("/star/data10/GC/aihong/P01glPIDFlowPicoDST/st_physics_2269001_raw_0006.event.root.flowpicoevent.root");
  chain->Add("/star/data10/GC/aihong/P01glPIDFlowPicoDST/st_physics_2269001_raw_0002.event.root.flowpicoevent.root");




  float dedxBegin =0.;
  float dedxEnd   =1.0e-5;
  float pBegin    =0.;
  float pEnd      =2.;

    TH2D* PionHist = 
      new TH2D("PionHist","PionHist", 100, pBegin,pEnd, 100,dedxBegin,dedxEnd);
    PionHist->SetMarkerColor(6);

    TH2D* KaonHist = 
      new TH2D(*PionHist);
    KaonHist->SetName("KaonHist"); KaonHist->SetTitle("KaonHist");
    KaonHist->SetMarkerColor(2);   


    TH2D* ProtonHist = 
      new TH2D(*PionHist);
    ProtonHist->SetName("ProtonHist"); ProtonHist->SetTitle("ProtonHist");
    ProtonHist->SetMarkerColor(3);

    TH2D* ElectronHist = 
      new TH2D(*PionHist);
    ElectronHist->SetName("ElectronHist"); ElectronHist->SetTitle("ElectronHist");
    ElectronHist->SetMarkerColor(4);


    TH2D* LowProbHist = 
      new TH2D(*PionHist);
    LowProbHist->SetName("LowProbHist"); LowProbHist->SetTitle("LowProbHist");
    LowProbHist->SetMarkerColor(1);


    TH2D* ExtrapHist = 
      new TH2D(*PionHist);
    ExtrapHist->SetName("ExtrapHist"); ExtrapHist->SetTitle("ExtrapHist");
    ExtrapHist->SetMarkerColor(7);


    chain->Draw("mDedx: (mPt/sqrt(1-(tanh(mEta)*tanh(mEta)))) >>PionHist","mMostLikelihoodProb>0.25 && (mMostLikelihoodPID == 8 || mMostLikelihoodPID == 9)  && mNdedxPts>15 && mCharge<5 && mDca >3 ");

    chain->Draw("mDedx: (mPt/sqrt(1-(tanh(mEta)*tanh(mEta)))) >>KaonHist","mMostLikelihoodProb>0.25 && (mMostLikelihoodPID == 11 || mMostLikelihoodPID == 12) && mNdedxPts>15 && mCharge<5 && mDca >3");

    chain->Draw("mDedx: (mPt/sqrt(1-(tanh(mEta)*tanh(mEta)))) >>ProtonHist","mMostLikelihoodProb>0.25 && (mMostLikelihoodPID == 14 || mMostLikelihoodPID == 15) && mNdedxPts>15 && mCharge<5 && mDca >3");

    chain->Draw("mDedx: (mPt/sqrt(1-(tanh(mEta)*tanh(mEta)))) >>ElectronHist","mMostLikelihoodProb>0.25 && (mMostLikelihoodPID == 2 || mMostLikelihoodPID == 3) && mNdedxPts>15 && mCharge<5 && mDca >3");

    chain->Draw("mDedx: (mPt/sqrt(1-(tanh(mEta)*tanh(mEta)))) >>LowProbHist","mMostLikelihoodProb<0.25  && mNdedxPts>15 && mCharge<5 && mDca >3");
    chain->Draw("mDedx: (mPt/sqrt(1-(tanh(mEta)*tanh(mEta)))) >>ExtrapHist","mExtrapTag==1 && mCharge<5 && mDca >3");


    PionHist->Draw();
    KaonHist->Draw("SAME");
    ProtonHist->Draw("SAME");
    ElectronHist->Draw("SAME");
    LowProbHist->Draw("SAME");
    //  ExtrapHist->Draw("SAME");






    double mPStart=0.05;
    double mPEnd=2.;
    int    NParameters=7;

TF1*  EBandCenter 
     =new TF1("EBandCenter",BetheBlochFunction, mPStart,mPEnd, NParameters); 
TF1*  PiBandCenter 
     =new TF1("PiBandCenter",BetheBlochFunction, mPStart,mPEnd, NParameters); 
TF1*  KBandCenter 
     =new TF1("KBandCenter",BetheBlochFunction, mPStart,mPEnd, NParameters); 
TF1*  PBandCenter 
     =new TF1("PBandCenter",BetheBlochFunction, mPStart,mPEnd, NParameters); 


 double offsetPar =  3.01022e-07;
   double scalePar = 4.68971e-07;



   double turnOver = 0.0199;
   double firstPar = 1.09344;

  Double_t electronPars[7]
        ={ firstPar, turnOver, offsetPar, 1, 0.511e-3, scalePar, 0.0005 };
  Double_t pionPars[7]
        ={ firstPar, turnOver, offsetPar, 1, 0.13957,  scalePar, 0.0005 };
  Double_t kaonPars[7]
        ={ firstPar, turnOver, offsetPar, 1, 0.49368,  scalePar, 0.0005 };
  Double_t antiprotonPars[7]
        ={ firstPar, turnOver, offsetPar, 1, 0.93827,  scalePar, 0.0005 };

        EBandCenter->SetParameters(&electronPars[0]);
       PiBandCenter->SetParameters(&pionPars[0]);
        KBandCenter->SetParameters(&kaonPars[0]);
        PBandCenter->SetParameters(&antiprotonPars[0]);


	EBandCenter->Draw("SAME");
       PiBandCenter->Draw("SAME");
        KBandCenter->Draw("SAME");
        PBandCenter->Draw("SAME");


}
   
