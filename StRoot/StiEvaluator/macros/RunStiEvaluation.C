//
//Evaluation Macro
//
//A. Rose
//
//Compares momentum components for Pi- (GEANTID=9
//

#include <iostream>
#include "TROOT.h"
#include "TSystem.h"
#include "TStyle.h"
#include "TTree.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TCanvas.h"
#include "TPostScript.h"

void SetResiduals()
{
}


void RunStiEvaluation(
		      //const char* evalFName="/star/data22/ITTF/evaluation/Miller/Evaluation.root",
		      //const char* histFName="/star/data22/ITTF/evaluation/Miller/EvalHists.root",
		      //const char* postFName="/star/data22/ITTF/evaluation/Miller/EvalHists.ps",
		      const char* evalFName="Evaluation.root",
		      const char* histFName="EvalHists.root",
		      const char* postFName="EvalHists.ps",
		      Int_t debug = 1000)
{
  //File stuff - input/output files open and setup
  //test for existence of file
   //get Tree Entries
   //Reset ROOT and connect tree file
   TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject(evalFName);
   if (!f) {
      f = new TFile(evalFName);
   }
   TTree *TestTree = (TTree*)gDirectory->Get("TestTree");
   cout << "Opening file: " << evalFName << endl;

   // open output file for histogram objects
   TFile histFile(histFName, "RECREATE");

   //create a multi-page portraitpostscript file for the histograms
   gStyle->SetPaperSize(TStyle::kUSLetter);
   TPostScript *ps = new TPostScript(postFName, 111);
   ps->Range(20, 26);  //set x,y of printed page (us letter)

   cout << "Create histograms" << endl;
   //1. Px
   TH1* hstpx= new TH1F("hstpx","StiTrack Px Distribution",100,-2,2);
   //2. Py
   TH1* hstpy= new TH1F("hstpy","StiTrack Py Distribution",100,-2,2);
   //3. Pz
   TH1* hstpz= new TH1F("hstpz","StiTrack Pz Distribution",100,-2,2);
   //3. Pt
   TH1* hstpt = new TH1F("hstpt","StiTrack Pt Distribution",100,0,2);
   //Momentum Residual Hists
   TH1* h2pz = new TH2F("h2pz","StiTrack vs. Global Pz",256,-1,1,256,-1,1);
   TH1* h2pt = new TH2F("h2pt","StiTrack vs. Global Pt",256,0,2,256,0,2);
   TH1* h2mpt = new TH2F("h2mpt","StiTrack vs. Monte Carlo Pt",256,0,2,256,0,2);
   TH1* hpzGvsSt =new TH1F("hpzGvsSt","Fractional Difference, ITTF and Global Pt",256,-.2,.2);
   TH1* hptMvsSt =new TH1F("hptMvsSt","Fractional Difference, ITTF and Monte Carlo Pt",256,-1,1);
   
   
  cout << "Plotting" <<endl;

  //////////////// page 1 //////////////////////////////////////////////

  ps->NewPage();
  TCanvas* canvas1=new TCanvas("canvas1","ITTF Momentum", 425,550);
  canvas1->Divide(2,2);

  canvas1->cd(1);
  TestTree->Draw("stiTrackPz>>hstpz","mcNTimesFound>0","goff",debug); 
  hstpz->SetYTitle("Counts");
  hstpz->SetXTitle("Pz (GeV/c)");
  hstpz->Draw();

  canvas1->cd(2);
  TestTree->Draw("stiTrackPx>>hstpx","mcNTimesFound>0","goff",debug); 
  hstpx->SetYTitle("Counts");
  hstpx->SetXTitle("Px (GeV/c)");
  hstpx->Draw();

  canvas1->cd(3);
  TestTree->Draw("stiTrackPy>>hstpy","mcNTimesFound>0","goff",debug); 
  hstpy->SetYTitle("Counts");
  hstpy->SetXTitle("Py (GeV/c)");
  hstpy->Draw();

  canvas1->cd(4);
  TestTree->Draw("stiTrackPt>>hstpt","mcNTimesFound>0","goff",debug); 
  hstpt->SetYTitle("Counts");
  hstpt->SetXTitle("Pt (GeV/c)");
  hstpt->Draw();

  canvas1->Update();

  //////////////// page 2 //////////////////////////////////////////////

  ps->NewPage();
  TCanvas* canvas2 = new TCanvas("canvas2","Momentum Comparison", 425, 550);
  canvas2->Divide(2,2);

  canvas2->cd(1);
  TestTree->Draw("stiTrackPz:globalTrackPz >>h2pz","mcNTimesFound>0","goff",debug);
  h2pz->SetYTitle("ITTF Pz (GeV/c)");
  h2pz->SetXTitle("Global Pz (GeV/c)");
  h2pz->Draw();

  canvas2->cd(2);
  TestTree->Draw("(stiTrackPz-globalTrackPz)/globalTrackPz >>hpzGvsSt","mcNTimesFound>0","goff",debug);
  hpzGvsSt->SetYTitle("Counts");
  hpzGvsSt->SetXTitle("(ITTF-Global)/Global Pz (%)");
  hpzGvsSt->Draw();

  canvas2->cd(3);
  TestTree->Draw("stiTrackPt:mcTrackPt >>h2mpt","mcNTimesFound>0","goff",debug);
  h2mpt->SetYTitle("ITTF Pt (GeV/c)");
  h2mpt->SetXTitle("Global Pt (GeV/c)");
  h2mpt->Draw();

  canvas2->cd(4);
  TestTree->Draw("(stiTrackPt-mcTrackPt)/mcTrackPt >>hptMvsSt","mcNTimesFound>0","goff",debug);
  hptMvsSt->SetYTitle("Counts");
  hptMvsSt->SetXTitle("(ITTF-MC)/MC Pt (%)");
  hptMvsSt->Draw();

  canvas2->Update();

  //////////////// page X //////////////////////////////////////////////

  //canvas3 = new TCanvas("canvas3","Momentum Error Vertex Dependence", 120,70,820,720);
  //canvas3->Divide(2,2);
  //canvas3->cd(1);

  //////////////// page 3 //////////////////////////////////////////////

  ps->NewPage();
  TCanvas* canvas4 = new TCanvas("canvas4","Track Reconstruction Charateristics",425,550);

  //3. Pz
  TH1* hstc2= new TH1F("hstc2","StiTrack Chi2 Distribution",100,-1,1);
  //3. Pt
  TH1* hgc2 = new TH1F("hgc2","Global Track Chi2 Distribution",100,0,10);
  canvas4->Divide(1,1); // kludge for TPostScript

  canvas4->cd(1);
  TestTree->Draw("globalTrackChi2 >> hgc2", "mcNTimesFound>0", "goff",debug);
  TestTree->Draw("stiTrackChi2 >>hstc2", "mcNTimesFound>0", "goff",debug);
  hgc2->Draw();
  hstc2->Draw("SAME");

  canvas4->Update();

  //////////////// page 4 //////////////////////////////////////////////

  ps->NewPage();
  TCanvas* canvas5 = new TCanvas("canvas5", "Track Residuals", 425,550);
  canvas5->Divide(2,2);

  canvas5->cd(1);
  TH1* hresx=new TH1F("hresx","StiTrack Residual in X",256,-.2,.2);
  TestTree->Draw("nodeLocalX-hitLocalX >> hresx","mcNTimesFound>0 && abs(nodeLocalX-hitLocalX)<.2 && nodeHasHit==1","goff",debug/100);
  hresx->Draw();

  canvas5->cd(2);
  TH1* hresy=new TH1F("hresy","StiTrack Residual in Y",256,-.2,.2);
  TestTree->Draw("nodeLocalY-hitLocalY >> hresy","mcNTimesFound>0 && abs(nodeLocalY-hitLocalY)<.2 && nodeHasHit==1","goff",debug/100);
  hresy->Draw();

  canvas5->cd(3);
  TH1* hresz=new TH1F("hresz","StiTrack Residual in Z",256,-.2,.2);
  TestTree->Draw("nodeLocalZ-hitLocalZ >> hresz","mcNTimesFound>0 && abs(nodeLocalZ-hitLocalZ)<.2 && nodeHasHit==1","goff",debug/100);
  hresz->Draw();

  canvas5->cd(4);
  TH1* hresr=new TH1F("hresr","StiTrack Residual",256,-.2,.2);
  TestTree->Draw("sqrt((nodeLocalX-hitLocalX)*(nodeLocalX-hitLocalX))>> hresr","mcNTimesFound>0 && nodeHasHit==1","goff",debug/100);
  hresr->Draw();

  canvas5->Update();

  //////////////// page 5 //////////////////////////////////////////////

  ps->NewPage();
  TCanvas* canvas6 = new TCanvas("canvas6","Hit and Node Characteristics", 425,550);
  canvas6->Divide(1,1); // kludge for TPostScript

  canvas6->cd(1);
  TH2* missedHitRZ= new TH2F("missedHitRZ","Position of Nodes without Associated Hit", 256, -100,100,256,0,200);
  TestTree->Draw("sqrt(nodeLocalX*nodeLocalX+nodeLocalY*nodeLocalY):nodeLocalZ >> missedHitRZ","mcNTimesFound>0 && nodeHasHit==0","goff",debug/100);
  missedHitRZ->SetMarkerColor(4);
  missedHitRZ->SetMarkerStyle(20);
  missedHitRZ->SetMarkerSize(.5);
  missedHitRZ->Draw("p");
  TH2* gotHitRZ = new TH2F("gotHitRZ","Position of Nodes Associated with a Hit", 256, -100,100,256,0,200);
  TestTree->Draw("sqrt(nodeLocalX*nodeLocalX+nodeLocalY*nodeLocalY):nodeLocalZ >> gotHitRZ","mcNTimesFound>0 && nodeHasHit==1",
		 "goff",debug/100);
  gotHitRZ->SetMarkerColor(3);
  gotHitRZ->SetMarkerStyle(19);
  gotHitRZ->SetMarkerSize(.5);
  gotHitRZ->Draw("Same,p");

  canvas6->Update();

  ps->Close();

  // convert ps to pdf
  char szBuf[128];
  sprintf(szBuf, "ps2pdf %s", postFName);
  gSystem->Exec(szBuf);

  // write out histogram objects
  histFile.Write();
  histFile.Close();

  //missedHitXY = new TH2F("missedHitXY","Missed Node Position", 256, -100,100,256,-100,100);
  //TestTree->Draw("nodeLocalX:nodeLocalY >> missedHitXY","nodeHasHit==0","goff",debug);
  //canvas6->cd(2);
  //missedHitXY->Draw();
  //f->Close();
}








