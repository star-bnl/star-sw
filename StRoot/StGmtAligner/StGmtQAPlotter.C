#define StGmtQAPlotter_cxx
#include "StGmtQAPlotter.h"
#include <TStyle.h>
#include <TMath.h>
#include <TPad.h>
#include <TDirectory.h>
#include <TF1.h>
#include <TLegend.h>
#include <vector>

double *FitPtDependence(TH1* h);

//_________________
void StGmtQAPlotter::Loop() {
  //   In a ROOT session, you can do:
  //      Root > .L StGmtQAPlotter.C
  //      Root > StGmtQAPlotter t
  //      Root > t.GetEntry(12); // Fill t data members with entry number 12
  //      Root > t.Show();       // Show values of entry 12
  //      Root > t.Show(16);     // Read and show values of entry 16
  //      Root > t.Loop();       // Loop on all entries
  //

  //     This is the loop skeleton where:
  //    jentry is the global entry number in the chain
  //    ientry is the entry number in the current Tree
  //  Note that the argument to GetEntry must be:
  //    jentry for TChain::GetEntry
  //    ientry for TTree::GetEntry and TBranch::GetEntry
  //
  //       To read only selected branches, Insert statements like:
  // METHOD1:
  //    fChain->SetBranchStatus("*",0);  // disable all branches
  //    fChain->SetBranchStatus("branchname",1);  // activate branchname
  // METHOD2: replace line
  //    fChain->GetEntry(jentry);       //read all branches
  //by  b_branchname->GetEntry(ientry); //read only this branch
  if (fChain == 0) return;

  Long64_t nentries = fChain->GetEntriesFast();
  cout << "Total events number: " << nentries << endl;

  SetDefaults();

  Float_t mUdiff, mVdiff, mDvAdcDOverDuAdcD, mInversePt;
  Float_t mUdiffOverTuP, mVdiffOverTvP;
  
  cout << "Processing events...";
  for (Long64_t jentry=0; jentry<nentries; jentry++) {
    Long64_t ientry = LoadTree(jentry);
    if (ientry < 0) break;
    // if (Cut(ientry) < 0) continue;

    b_EventT_fEvtHdr_fRun->GetEntry(ientry);
    b_EventT_fEvtHdr_fEvtNum->GetEntry(ientry);
    b_EventT_fEvtHdr_fDate->GetEntry(ientry);
    b_EventT_fNhit->GetEntry(ientry);
    b_fHits_pT->GetEntry(ientry);
    b_fHits_barrel->GetEntry(ientry);
    b_fHits_sLength->GetEntry(ientry);

    hFNhits->Fill(fNhit);

    /*//Degbug
    cout << "Run number: " << fEvtHdr_fRun
	 << " Event number: " << fEvtHdr_fEvtNum
	 << " Date: " << fEvtHdr_fDate << endl;
    */

    //Measured values
    b_fHits_uD->GetEntry(ientry);
    b_fHits_vD->GetEntry(ientry);
    b_fHits_duD->GetEntry(ientry);
    b_fHits_dvD->GetEntry(ientry);
    b_fHits_uAdcD->GetEntry(ientry);
    b_fHits_vAdcD->GetEntry(ientry);
    b_fHits_duAdcD->GetEntry(ientry);
    b_fHits_dvAdcD->GetEntry(ientry);
#ifdef PRIMARY
    b_fHits_isPrimary->GetEntry(ientry);
    b_fHits_isCrossingMembrain->GetEntry(ientry);
#endif

    //Predicted values
    b_fHits_uP->GetEntry(ientry);
    b_fHits_vP->GetEntry(ientry);
    b_fHits_tuP->GetEntry(ientry);
    b_fHits_tvP->GetEntry(ientry);

    //Loop through the hits
    for(Int_t iHit=0; iHit<fNhit; iHit++) {

      //Variables
      mUdiff = fHits_uD[iHit] - fHits_uP[iHit];
      mVdiff = fHits_vD[iHit] - fHits_vP[iHit];
      mDvAdcDOverDuAdcD = fHits_dvAdcD[iHit]/fHits_duAdcD[iHit];

      //Before cuts
      hUAdcDVsVAdcD[8]->Fill(TMath::Log(fHits_vAdcD[iHit]),
			     TMath::Log(fHits_uAdcD[iHit]));
      hDuAdcDVsDvAdcD[8]->Fill(fHits_dvAdcD[iHit],
			       fHits_duAdcD[iHit]);
      hDvAdcDOverDuAdcD[8]->Fill(mDvAdcDOverDuAdcD);

      if(fHits_barrel[iHit]<0 || fHits_barrel[iHit]>7) {
	cout << "Wrong GMT module number #"<<fHits_barrel[iHit] << endl;
	break;
      }
      
      hUAdcDVsVAdcD[fHits_barrel[iHit]]->Fill(TMath::Log(fHits_vAdcD[iHit]),
					      TMath::Log(fHits_uAdcD[iHit]));
      hDuAdcDVsDvAdcD[fHits_barrel[iHit]]->Fill(fHits_dvAdcD[iHit],
						fHits_duAdcD[iHit]);
      hDvAdcDOverDuAdcD[fHits_barrel[iHit]]->Fill(mDvAdcDOverDuAdcD);

      //Main cuts
      if(mDvAdcDOverDuAdcD < 0.5) continue;
      if(mDvAdcDOverDuAdcD > 2.) continue;
      if(TMath::Abs(mUdiff) > 1.) continue;
      if(TMath::Abs(mVdiff) > 1.) continue;
      if(TMath::Abs(fHits_pT[iHit]) < 0.5) continue;
      if(fHits_sLength[iHit] > 40.) continue;
#ifdef PRIMARY
      //Use only primary
      //if(fHits_isPrimary[iHit] == 0) continue;
      //if(fHits_isCrossingMembrain[iHit] == 0) continue;
#endif

      mInversePt = 1./fHits_pT[iHit];
      mUdiffOverTuP = mUdiff/fHits_tuP[iHit];
      mVdiffOverTvP = mVdiff/fHits_tvP[iHit];

      //After cuts
      hUdiffVsBarrel->Fill(fHits_barrel[iHit],mUdiff);
      hVdiffVsBarrel->Fill(fHits_barrel[iHit],mVdiff);
      if(fHits_pT[iHit]>0) {
	hUdiffVsBarrelPosCharge->Fill(fHits_barrel[iHit],mUdiff);
	hVdiffVsBarrelPosCharge->Fill(fHits_barrel[iHit],mVdiff);
      }
      else {
	hUdiffVsBarrelNegCharge->Fill(fHits_barrel[iHit],mUdiff);
	hVdiffVsBarrelNegCharge->Fill(fHits_barrel[iHit],mVdiff);
      }

      //Integrated over all modules
      hUd[8]->Fill(fHits_uD[iHit]);
      hUp[8]->Fill(fHits_uP[iHit]);
      hVd[8]->Fill(fHits_vD[iHit]);
      hVp[8]->Fill(fHits_vP[iHit]);
      hUdVsUp[8]->Fill(fHits_uP[iHit],fHits_uD[iHit]);
      hVdVsVp[8]->Fill(fHits_vP[iHit],fHits_vD[iHit]);
      hUdVsVd[8]->Fill(fHits_vD[iHit],fHits_uD[iHit]);
      hUdiffVsInvPt[8]->Fill(mInversePt,mUdiff);
      hVdiffVsInvPt[8]->Fill(mInversePt,mVdiff);
      //Derivatives
      hTuP[8]->Fill(fHits_tuP[iHit]);
      hTvP[8]->Fill(fHits_tvP[iHit]);
      hUdiffVsTuP[8]->Fill(fHits_tuP[iHit],mUdiff);
      hVdiffVsTvP[8]->Fill(fHits_tvP[iHit],mVdiff);
      hUdiffVsVp[8]->Fill(fHits_vP[iHit],mUdiff);
      hVdiffVsUp[8]->Fill(-fHits_uP[iHit],mVdiff);
      hUdiffOverTuPVsVp[8]->Fill(fHits_vP[iHit],mUdiffOverTuP);
      hVdiffOverTvPVsVp[8]->Fill(fHits_vP[iHit],mVdiffOverTvP);
      hUdiffOverTuPVsUp[8]->Fill(-fHits_uP[iHit],mUdiffOverTuP);
      hVdiffOverTvPVsUp[8]->Fill(-fHits_uP[iHit],mVdiffOverTvP);

      //For each GMT module
      hUd[fHits_barrel[iHit]]->Fill(fHits_uD[iHit]);
      hUp[fHits_barrel[iHit]]->Fill(fHits_uP[iHit]);
      hVd[fHits_barrel[iHit]]->Fill(fHits_vD[iHit]);
      hVp[fHits_barrel[iHit]]->Fill(fHits_vP[iHit]);
      hUdVsUp[fHits_barrel[iHit]]->Fill(fHits_uP[iHit],fHits_uD[iHit]);
      hVdVsVp[fHits_barrel[iHit]]->Fill(fHits_vP[iHit],fHits_vD[iHit]);
      hUdVsVd[fHits_barrel[iHit]]->Fill(fHits_vD[iHit],fHits_uD[iHit]);
      hUdiffVsInvPt[fHits_barrel[iHit]]->Fill(mInversePt,mUdiff);
      hVdiffVsInvPt[fHits_barrel[iHit]]->Fill(mInversePt,mVdiff);
      //Derivatives
      hTuP[fHits_barrel[iHit]]->Fill(fHits_tuP[iHit]);
      hTvP[fHits_barrel[iHit]]->Fill(fHits_tvP[iHit]);
      hUdiffVsTuP[fHits_barrel[iHit]]->Fill(fHits_tuP[iHit],mUdiff);
      hVdiffVsTvP[fHits_barrel[iHit]]->Fill(fHits_tvP[iHit],mVdiff);
      hUdiffVsVp[fHits_barrel[iHit]]->Fill(fHits_vP[iHit],mUdiff);
      hVdiffVsUp[fHits_barrel[iHit]]->Fill(-fHits_uP[iHit],mVdiff);
      hUdiffOverTuPVsVp[fHits_barrel[iHit]]->Fill(fHits_vP[iHit],mUdiffOverTuP);
      hVdiffOverTvPVsVp[fHits_barrel[iHit]]->Fill(fHits_vP[iHit],mVdiffOverTvP);
      hUdiffOverTuPVsUp[fHits_barrel[iHit]]->Fill(-fHits_uP[iHit],mUdiffOverTuP);
      hVdiffOverTvPVsUp[fHits_barrel[iHit]]->Fill(-fHits_uP[iHit],mVdiffOverTvP);

      dutuP[fHits_barrel[iHit]]->Fill(fHits_tuP[iHit],mUdiff);
      dvtvP[fHits_barrel[iHit]]->Fill(fHits_tvP[iHit],mVdiff);
      duvP[fHits_barrel[iHit]]->Fill(fHits_vP[iHit],mUdiff);
      dvuP[fHits_barrel[iHit]]->Fill(-fHits_uP[iHit],mVdiff);
      duOvertuPvP[fHits_barrel[iHit]]->Fill(fHits_vP[iHit],mUdiffOverTuP);
      dvOvertvPvP[fHits_barrel[iHit]]->Fill(fHits_vP[iHit],mVdiffOverTvP);
      duOvertuPuP[fHits_barrel[iHit]]->Fill(-fHits_uP[iHit],mUdiffOverTuP);
      dvOvertvPuP[fHits_barrel[iHit]]->Fill(-fHits_uP[iHit],mVdiffOverTvP);
    } //for(Int_t iHit=0; iHit<fNhit; iHit++)
  } //for (Long64_t jentry=0; jentry<nentries;jentry++)
  cout << "\t[DONE]" << endl;

  //Slicing main histograms
  MakeSlices();
  //Set styles to slices
  SetHistosStyle();
  //Create and fill canvases with histograms
  CreateCanvases();
  FillCanvases();
  //Saving to file
  if(mWrite2File) {
    Write2File();
  }

  //Print results to the screen
  PrintPtFitParameters();
}

//_________________
void StGmtQAPlotter::CreateCanvases() {

  cout << "Creating canvases...";

  //Variables
  Int_t mCanvSizeX = 1200;
  Int_t mCanvSizeY = 800;
  Double_t mPadSpaceX = 0.001;
  Double_t mPadSpaceY = 0.001;

  //Canvases

  //Before cuts
  cUAdcDVsVAdcDCanvas = new TCanvas("hUAdcDVsVAdcDCanvas","hUAdcDVsVAdcDCanvas",
				    mCanvSizeX,mCanvSizeY);
  cUAdcDVsVAdcDCanvas->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cDuAdcDVsDvAdcDCanvas = new TCanvas("hDuAdcDVsDvAdcDCanvas","hDuAdcDVsDvAdcDCanvas",
				      mCanvSizeX,mCanvSizeY);
  cDuAdcDVsDvAdcDCanvas->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cDvAdcDOverDuAdcDCanvas = new TCanvas("hDvAdcDOverDuAdcDCanvas","hDvAdcDOverDuAdcDCanvas",
					mCanvSizeX,mCanvSizeY);
  cDvAdcDOverDuAdcDCanvas->Divide(4,2,mPadSpaceX,mPadSpaceY);

  //After cuts
  cUdCanvas = new TCanvas("cUdCanvas","cUdCanvas",mCanvSizeX,mCanvSizeY);
  cUdCanvas->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cUpCanvas = new TCanvas("cUpCanvas","cUpCanvas",mCanvSizeX,mCanvSizeY);
  cUpCanvas->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cVdCanvas = new TCanvas("cVdCanvas","cVdCanvas",mCanvSizeX,mCanvSizeY);
  cVdCanvas->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cVpCanvas = new TCanvas("cVpCanvas","cVpCanvas",mCanvSizeX,mCanvSizeY);
  cVpCanvas->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cUdVsUpCanvas = new TCanvas("cUdVsUpCanvas","cUdVsUpCanvas",mCanvSizeX,mCanvSizeY);
  cUdVsUpCanvas->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cVdVsVpCanvas = new TCanvas("cVdVsVpCanvas","cVdVsVpCanvas",mCanvSizeX,mCanvSizeY);
  cVdVsVpCanvas->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cUdVsVdCanvas = new TCanvas("cUdVsVdCanvas","cUdVsVdCanvas",mCanvSizeX,mCanvSizeY);
  cUdVsVdCanvas->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cUdiffVsInversePtCanvas = new TCanvas("cUdiffVsInversePtCanvas","cUdiffVsInversePtCanvas",
					mCanvSizeX,mCanvSizeY);
  cUdiffVsInversePtCanvas->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cVdiffVsInversePtCanvas = new TCanvas("cVdiffVsInversePtCanvas","cVdiffVsInversePtCanvas",
					mCanvSizeX,mCanvSizeY);
  cVdiffVsInversePtCanvas->Divide(4,2,mPadSpaceX,mPadSpaceY);

  cFitsUdiffVsInvPt = new TCanvas("cFitsUdiffVsInvPt","cFitsUdiffVsInvPt",
				  mCanvSizeX,mCanvSizeY);
  cFitsUdiffVsInvPt->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cFitsVdiffVsInvPt = new TCanvas("cFitsVdiffVsInvPt","cFitsVdiffVsInvPt",
				  mCanvSizeX,mCanvSizeY);
  cFitsVdiffVsInvPt->Divide(4,2,mPadSpaceX,mPadSpaceY);

  cUandVdiffVsBarrel = new TCanvas("cUandVdiffVsBarrel","cUandVdiffVsBarrel",
				   mCanvSizeX,mCanvSizeY);
  cUandVdiffVsBarrel->Divide(3,2);

  //Derivatives
  cUdiffVsTuP = new TCanvas("cUdiffVsTuP","cUdiffVsTuP",mCanvSizeX,mCanvSizeY);
  cUdiffVsTuP->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cVdiffVsTvP = new TCanvas("cVdiffVsTvP","cVdiffVsTvP",mCanvSizeX,mCanvSizeY);
  cVdiffVsTvP->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cUdiffVsVp = new TCanvas("cUdiffVsVp","cUdiffVsVp",mCanvSizeX,mCanvSizeY);
  cUdiffVsVp->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cVdiffVsUp = new TCanvas("cVdiffVsUp","cVdiffVsUp",mCanvSizeX,mCanvSizeY);
  cVdiffVsUp->Divide(4,2,mPadSpaceX,mPadSpaceY);

  cUdiffOverTuPVsVp = new TCanvas("cUdiffOverTuPVsVp","cUdiffOverTuPVsVp",
				  mCanvSizeX,mCanvSizeY);
  cUdiffOverTuPVsVp->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cVdiffOverTvPVsVp = new TCanvas("cVdiffOverTvPVsVp","cVdiffOverTvPVsVp",
				  mCanvSizeX,mCanvSizeY);
  cVdiffOverTvPVsVp->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cUdiffOverTuPVsUp = new TCanvas("cUdiffOverTuPVsUp","cUdiffOverTuPVsUp",
				  mCanvSizeX,mCanvSizeY);
  cUdiffOverTuPVsUp->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cVdiffOverTvPVsUp = new TCanvas("cVdiffOverTvPVsUp","cVdiffOverTvPVsUp",
				  mCanvSizeX,mCanvSizeY);
  cVdiffOverTvPVsUp->Divide(4,2,mPadSpaceX,mPadSpaceY);

  cUdiffVsTuPSlices = new TCanvas("cUdiffVsTuPSlices","cUdiffVsTuPSlices",mCanvSizeX,mCanvSizeY);
  cUdiffVsTuPSlices->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cVdiffVsTvPSlices = new TCanvas("cVdiffVsTvPSlices","cVdiffVsTvPSlices",mCanvSizeX,mCanvSizeY);
  cVdiffVsTvPSlices->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cUdiffVsVpSlices = new TCanvas("cUdiffVsVpSlices","cUdiffVsVpSlices",mCanvSizeX,mCanvSizeY);
  cUdiffVsVpSlices->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cVdiffVsUpSlices = new TCanvas("cVdiffVsUpSlices","cVdiffVsUpSlices",mCanvSizeX,mCanvSizeY);
  cVdiffVsUpSlices->Divide(4,2,mPadSpaceX,mPadSpaceY);

  cUdiffOverTuPVsVpSlices = new TCanvas("cUdiffOverTuPVsVpSlices","cUdiffOverTuPVsVpSlices",
					mCanvSizeX,mCanvSizeY);
  cUdiffOverTuPVsVpSlices->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cVdiffOverTvPVsVpSlices = new TCanvas("cVdiffOverTvPVsVpSlices","cVdiffOverTvPVsVpSlices",
					mCanvSizeX,mCanvSizeY);
  cVdiffOverTvPVsVpSlices->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cUdiffOverTuPVsUpSlices = new TCanvas("cUdiffOverTuPVsUpSlices","cUdiffOverTuPVsUpSlices",
					mCanvSizeX,mCanvSizeY);
  cUdiffOverTuPVsUpSlices->Divide(4,2,mPadSpaceX,mPadSpaceY);
  cVdiffOverTvPVsUpSlices = new TCanvas("cVdiffOverTvPVsUpSlices","cVdiffOverTvPVsUpSlices",
					mCanvSizeX,mCanvSizeY);
  cVdiffOverTvPVsUpSlices->Divide(4,2,mPadSpaceX,mPadSpaceY);

  cout << "\t[DONE]" << endl;
}

//_________________
void StGmtQAPlotter::FillCanvases() {

  cout << "Filling canvases...";

  //For each GMT module
  Int_t iPad = 0;
  double *mArrPointer;
  for(Int_t iModule=0; iModule<8; iModule++) {

    iPad = iModule+1;

    //Before cuts
    cUAdcDVsVAdcDCanvas->cd(iPad);
    hUAdcDVsVAdcD[iModule]->Draw("colz");
    gPad->SetLogz(1);
    gPad->SetGrid();
    cDuAdcDVsDvAdcDCanvas->cd(iPad);
    hDuAdcDVsDvAdcD[iModule]->Draw("colz");
    gPad->SetLogz(1);
    gPad->SetGrid();
    cDvAdcDOverDuAdcDCanvas->cd(iPad);
    hDvAdcDOverDuAdcD[iModule]->Draw("colz");
    gPad->SetLogz(1);
    gPad->SetGrid();

    //After cuts
    cUdCanvas->cd(iPad);
    hUd[iModule]->Draw();
    cUpCanvas->cd(iPad);
    gPad->SetGrid();
    hUp[iModule]->Draw();
    cVdCanvas->cd(iPad);
    gPad->SetGrid();
    hVd[iModule]->Draw();
    gPad->SetGrid();
    cVpCanvas->cd(iPad);
    gPad->SetGrid();
    hVp[iModule]->Draw();
    gPad->SetGrid();

    cUdVsUpCanvas->cd(iPad);
    hUdVsUp[iModule]->Draw("colz");
    gPad->SetGrid();
    gPad->SetLogz(1);
    cVdVsVpCanvas->cd(iPad);
    hVdVsVp[iModule]->Draw("colz");
    gPad->SetLogz(1);
    gPad->SetGrid();
    cUdVsVdCanvas->cd(iPad);
    hUdVsVd[iModule]->Draw("colz");
    gPad->SetLogz(1);
    gPad->SetGrid();
    //pT dependence
    cUdiffVsInversePtCanvas->cd(iPad);
    hUdiffVsInvPt[iModule]->Draw("colz");
    gPad->SetGrid();
    gPad->SetLogz(1);
    cVdiffVsInversePtCanvas->cd(iPad);
    hVdiffVsInvPt[iModule]->Draw("colz");
    gPad->SetLogz(1);
    gPad->SetGrid();

    cFitsUdiffVsInvPt->cd(iPad);
    hUdiffVsInvPtSl[iModule]->Draw();
    if(iModule!=6) {
      mArrPointer = FitPtDependence(hUdiffVsInvPtSl[iModule]);
      mUConstArr[iModule] = mArrPointer[0];
      mUConstErrArr[iModule] = mArrPointer[1];
      mUSlopeArr[iModule] = mArrPointer[2];
      mUSlopeErrArr[iModule] = mArrPointer[3];
    } //if(iModule!=6)
    cFitsVdiffVsInvPt->cd(iPad);
    hVdiffVsInvPtSl[iModule]->Draw();
    if(iModule!=6) {
      mArrPointer = FitPtDependence(hVdiffVsInvPtSl[iModule]);
      mVConstArr[iModule] = mArrPointer[0];
      mVConstErrArr[iModule] = mArrPointer[1];
      mVSlopeArr[iModule] = mArrPointer[2];
      mVSlopeErrArr[iModule] = mArrPointer[3];
    } //if(iModule!=6)

    //Derivatives
    cUdiffVsTuP->cd(iPad);
    hUdiffVsTuP[iModule]->Draw("colz");
    gPad->SetLogz(1);
    gPad->SetGrid();
    cVdiffVsTvP->cd(iPad);
    hVdiffVsTvP[iModule]->Draw("colz");
    gPad->SetLogz(1);
    gPad->SetGrid();
    cUdiffVsVp->cd(iPad);
    hUdiffVsVp[iModule]->Draw("colz");
    gPad->SetLogz(1);
    gPad->SetGrid();
    cVdiffVsUp->cd(iPad);
    hVdiffVsUp[iModule]->Draw("colz");
    gPad->SetLogz(1);
    gPad->SetGrid();
    cUdiffOverTuPVsVp->cd(iPad);
    hUdiffOverTuPVsVp[iModule]->Draw("colz");
    gPad->SetLogz(1);
    gPad->SetGrid();
    cVdiffOverTvPVsVp->cd(iPad);
    hVdiffOverTvPVsVp[iModule]->Draw("colz");
    gPad->SetLogz(1);
    gPad->SetGrid();
    cUdiffOverTuPVsUp->cd(iPad);
    hUdiffOverTuPVsUp[iModule]->Draw("colz");
    gPad->SetLogz(1);
    gPad->SetGrid();
    cVdiffOverTvPVsUp->cd(iPad);
    hVdiffOverTvPVsUp[iModule]->Draw("colz");
    gPad->SetLogz(1);
    gPad->SetGrid();

    cUdiffVsTuPSlices->cd(iPad);
    hUdiffVsTuPSlices[iModule]->Draw();
    gPad->SetGrid();
    cVdiffVsTvPSlices->cd(iPad);
    hVdiffVsTvPSlices[iModule]->Draw();
    gPad->SetGrid();
    cUdiffVsVpSlices->cd(iPad);
    hUdiffVsVpSlices[iModule]->Draw();
    gPad->SetGrid();
    cVdiffVsUpSlices->cd(iPad);
    hVdiffVsUpSlices[iModule]->Draw();
    gPad->SetGrid();
    cUdiffOverTuPVsVpSlices->cd(iPad);
    hUdiffOverTuPVsVpSlices[iModule]->Draw();
    gPad->SetGrid();
    cVdiffOverTvPVsVpSlices->cd(iPad);
    hVdiffOverTvPVsVpSlices[iModule]->Draw();
    gPad->SetGrid();
    cUdiffOverTuPVsUpSlices->cd(iPad);
    hUdiffOverTuPVsUpSlices[iModule]->Draw();
    gPad->SetGrid();
    cVdiffOverTvPVsUpSlices->cd(iPad);
    hVdiffOverTvPVsUpSlices[iModule]->Draw();
    gPad->SetGrid();
    
  } //for(Int_t iModule=0; iModule<8; iModule++)

  //Main canvases
  cUandVdiffVsBarrel->cd(1);
  hUdiffVsBarrel->Draw("colz");
  gPad->SetGrid();
  gPad->SetLogz(1);
  cUandVdiffVsBarrel->cd(2);
  hUdiffVsBarrelSlices[0]->Draw();
  hUdiffVsBarrelSlicesPosCharge[0]->Draw("same");
  hUdiffVsBarrelSlicesNegCharge[0]->Draw("same");
  gPad->SetGrid();
  cUandVdiffVsBarrel->cd(3);
  hUdiffVsBarrelSlices[1]->Draw();
  hUdiffVsBarrelSlicesPosCharge[1]->Draw("same");
  hUdiffVsBarrelSlicesNegCharge[1]->Draw("same");
  gPad->SetGrid();
  cUandVdiffVsBarrel->cd(4);
  hVdiffVsBarrel->Draw("colz");
  gPad->SetLogz(1);
  gPad->SetGrid();
  cUandVdiffVsBarrel->cd(5);
  hVdiffVsBarrelSlices[0]->Draw();
  hVdiffVsBarrelSlicesPosCharge[0]->Draw("same");
  hVdiffVsBarrelSlicesNegCharge[0]->Draw("same");
  gPad->SetGrid();
  cUandVdiffVsBarrel->cd(6);
  hVdiffVsBarrelSlices[1]->Draw();
  hVdiffVsBarrelSlicesPosCharge[1]->Draw("same");
  hVdiffVsBarrelSlicesNegCharge[1]->Draw("same");
  gPad->SetGrid();

  cout << "\t[DONE]" << endl;
}

//_________________
void StGmtQAPlotter::WriteCanvases2File() {

  //Before cuts
  cUAdcDVsVAdcDCanvas->Write();
  cDuAdcDVsDvAdcDCanvas->Write();
  cDvAdcDOverDuAdcDCanvas->Write();

  //After cuts
  cUdCanvas->Write();
  cUpCanvas->Write();
  cVdCanvas->Write();
  cVpCanvas->Write();
  cUdVsUpCanvas->Write();
  cVdVsVpCanvas->Write();
  cUdVsVdCanvas->Write();
  cUdiffVsInversePtCanvas->Write();
  cFitsUdiffVsInvPt->Write();
  cVdiffVsInversePtCanvas->Write();
  cFitsVdiffVsInvPt->Write();
  cUandVdiffVsBarrel->Write();
}

//_________________
void StGmtQAPlotter::Set1DHistoStyle(TH1* histo, Int_t mMode, Int_t mCharge) {

  //Define variables
  Int_t mLineColor;
  Int_t mLineWidth = 2;
  Int_t mMarkerStyle = 8;
  Int_t mMarkerColor = 1;
  Double_t mMarkerSize = 1.2;
  Double_t mYaxisMin, mYaxisMax;
  if(mMode==0) {
    mYaxisMin = -0.75;
    mYaxisMax = 0.75;
  }
  else if(mMode==1) {
    mYaxisMin = 0.0;
    mYaxisMax = 0.5;
  }
  else {
    mYaxisMin = -1;
    mYaxisMax = 1;    
  }

  //0-all, 1-positive, 2-negative
  if(mCharge==0) {
    mLineColor = 1;
    mMarkerStyle = 8;
    mMarkerColor = 1;
    mMarkerSize = 1.2;
  }
  else if(mCharge==1) {
    mLineColor = 2;
    mMarkerStyle = 21;
    mMarkerColor = 2;
    mMarkerSize = 1.2;    
  }
  else if(mCharge==2) {
    mLineColor = 4;
    mMarkerStyle = 34;
    mMarkerColor = 4;
    mMarkerSize = 1.2;     
  }
  else {
    cout << "StGmtQAPlotter::Set1DHistoStyle -- Impossible particle charge:" 
	 << mCharge << endl;
    mLineColor = 1;
    mMarkerStyle = 8;
    mMarkerColor = 1;
    mMarkerSize = 1.2;
  }

  //Set parameters
  histo->SetLineColor(mLineColor);
  histo->SetLineWidth(mLineWidth);
  histo->SetMarkerStyle(mMarkerStyle);
  histo->SetMarkerColor(mMarkerColor);
  histo->SetMarkerSize(mMarkerSize);

  histo->GetYaxis()->SetRangeUser(mYaxisMin,mYaxisMax);
}

//_________________
void StGmtQAPlotter::SetHistosStyle() {

  cout << "Setting histograms styles...";

  //All charges
  Set1DHistoStyle(hUdiffVsBarrelSlices[0],0,0);
  Set1DHistoStyle(hUdiffVsBarrelSlices[1],1,0);
  Set1DHistoStyle(hVdiffVsBarrelSlices[0],0,0);
  Set1DHistoStyle(hVdiffVsBarrelSlices[1],1,0);

  //Positive charge
  Set1DHistoStyle(hUdiffVsBarrelSlicesPosCharge[0],0,1);
  Set1DHistoStyle(hUdiffVsBarrelSlicesPosCharge[1],1,1);
  Set1DHistoStyle(hVdiffVsBarrelSlicesPosCharge[0],0,1);
  Set1DHistoStyle(hVdiffVsBarrelSlicesPosCharge[1],1,1);

  //Negative charge
  Set1DHistoStyle(hUdiffVsBarrelSlicesNegCharge[0],0,2);
  Set1DHistoStyle(hUdiffVsBarrelSlicesNegCharge[1],1,2);
  Set1DHistoStyle(hVdiffVsBarrelSlicesNegCharge[0],0,2);
  Set1DHistoStyle(hVdiffVsBarrelSlicesNegCharge[1],1,2);

  for(Int_t iModule=0; iModule<8; iModule++) {
    Set1DHistoStyle(hUdiffVsInvPtSl[iModule],0,0);
    Set1DHistoStyle(hVdiffVsInvPtSl[iModule],0,0);

    //Derivatives
    Set1DHistoStyle(hUdiffVsTuPSlices[iModule],0,0);
    Set1DHistoStyle(hVdiffVsTvPSlices[iModule],0,0);
    Set1DHistoStyle(hUdiffVsVpSlices[iModule],0,0);
    Set1DHistoStyle(hVdiffVsUpSlices[iModule],0,0);
    Set1DHistoStyle(hUdiffOverTuPVsVpSlices[iModule],0,0);
    Set1DHistoStyle(hVdiffOverTvPVsVpSlices[iModule],0,0);
    Set1DHistoStyle(hUdiffOverTuPVsUpSlices[iModule],0,0);
    Set1DHistoStyle(hVdiffOverTvPVsUpSlices[iModule],0,0);
  }
  

  cout << "\t[DONE]" << endl;
}

//_________________
void StGmtQAPlotter::MakeSlices() {

  cout << "Fit slices main histograms...";
  //Main histograms
  hUdiffVsBarrel->FitSlicesY();
  //hUdiffVsBarrelSlices[0] = (TH1D*)gDirectory->Get("hUdiffVsBarrel_1");
  hUdiffVsBarrelSlices[0] = (TH1D*)gDirectory->Get(Form("hUdiffVsBarrel_%i",1));
  hUdiffVsBarrelSlices[1] = (TH1D*)gDirectory->Get(Form("hUdiffVsBarrel_%i",2));
  hUdiffVsBarrelPosCharge->FitSlicesY();
  hUdiffVsBarrelSlicesPosCharge[0] = (TH1D*)gDirectory->Get(Form("hUdiffVsBarrelPosCharge_%i",1));
  hUdiffVsBarrelSlicesPosCharge[1] = (TH1D*)gDirectory->Get(Form("hUdiffVsBarrelPosCharge_%i",2));
  hUdiffVsBarrelNegCharge->FitSlicesY();
  hUdiffVsBarrelSlicesNegCharge[0] = (TH1D*)gDirectory->Get(Form("hUdiffVsBarrelNegCharge_%i",1));
  hUdiffVsBarrelSlicesNegCharge[1] = (TH1D*)gDirectory->Get(Form("hUdiffVsBarrelNegCharge_%i",2));
  
  hVdiffVsBarrel->FitSlicesY();
  hVdiffVsBarrelSlices[0] = (TH1D*)gDirectory->Get(Form("hVdiffVsBarrel_%i",1));
  hVdiffVsBarrelSlices[1] = (TH1D*)gDirectory->Get(Form("hVdiffVsBarrel_%i",2));
  hVdiffVsBarrelPosCharge->FitSlicesY();
  hVdiffVsBarrelSlicesPosCharge[0] = (TH1D*)gDirectory->Get(Form("hVdiffVsBarrelPosCharge_%i",1));
  hVdiffVsBarrelSlicesPosCharge[1] = (TH1D*)gDirectory->Get(Form("hVdiffVsBarrelPosCharge_%i",2));
  hVdiffVsBarrelNegCharge->FitSlicesY();
  hVdiffVsBarrelSlicesNegCharge[0] = (TH1D*)gDirectory->Get(Form("hVdiffVsBarrelNegCharge_%i",1));
  hVdiffVsBarrelSlicesNegCharge[1] = (TH1D*)gDirectory->Get(Form("hVdiffVsBarrelNegCharge_%i",2));


  for(int iModule=0; iModule<8; iModule++) {

    //Momentum dependence
    hUdiffVsInvPt[iModule]->FitSlicesY();
    hUdiffVsInvPtSl[iModule] = (TH1D*)gDirectory->Get(Form("hUdiffVsInvPt_%i_1",iModule));
    hUdiffVsInvPtSl[iModule]->SetNameTitle(Form("hUdiffVsInvPtSl_%i",iModule),
					   Form("hUdiffVsInvPtSl_%i",iModule));
    hVdiffVsInvPt[iModule]->FitSlicesY();
    hVdiffVsInvPtSl[iModule] = (TH1D*)gDirectory->Get(Form("hVdiffVsInvPt_%i_%i",iModule,1));
    hVdiffVsInvPtSl[iModule]->SetNameTitle(Form("hVdiffVsInvPtSl_%i",iModule),
					   Form("hVdiffVsInvPtSl_%i",iModule));
    //Derivatives
    hUdiffVsTuP[iModule]->FitSlicesY();
    hUdiffVsTuPSlices[iModule] = (TH1D*)gDirectory->Get(Form("hUdiffVsTuP_%i_1",iModule));
    hUdiffVsTuPSlices[iModule]->SetNameTitle(Form("hUdiffVsTuPSlices_%i",iModule),
					     Form("hUdiffVsTuPSlices_%i",iModule));
    hVdiffVsTvP[iModule]->FitSlicesY();
    hVdiffVsTvPSlices[iModule] = (TH1D*)gDirectory->Get(Form("hVdiffVsTvP_%i_1",iModule));
    hVdiffVsTvPSlices[iModule]->SetNameTitle(Form("hVdiffVsTvPSlices_%i",iModule),
					     Form("hVdiffVsTvPSlices_%i",iModule));
    hUdiffVsVp[iModule]->FitSlicesY();
    hUdiffVsVpSlices[iModule] = (TH1D*)gDirectory->Get(Form("hUdiffVsVp_%i_1",iModule));
    hUdiffVsVpSlices[iModule]->SetNameTitle(Form("hUdiffVsVpSlices_%i",iModule),
					    Form("hUdiffVsVpSlices_%i",iModule));
    hVdiffVsUp[iModule]->FitSlicesY();
    hVdiffVsUpSlices[iModule] = (TH1D*)gDirectory->Get(Form("hVdiffVsUp_%i_1",iModule));
    hVdiffVsUpSlices[iModule]->SetNameTitle(Form("hVdiffVsUpSlices_%i",iModule),
					    Form("hVdiffVsUpSlices_%i",iModule));
    hUdiffOverTuPVsVp[iModule]->FitSlicesY();
    hUdiffOverTuPVsVpSlices[iModule] = (TH1D*)gDirectory->Get(Form("hUdiffOverTuPVsVp_%i_1",iModule));
    hUdiffOverTuPVsVpSlices[iModule]->SetNameTitle(Form("hUdiffOverTuPVsVpSlices_%i",iModule),
						   Form("hUdiffOverTuPVsVpSlices_%i",iModule));
    hVdiffOverTvPVsVp[iModule]->FitSlicesY();
    hVdiffOverTvPVsVpSlices[iModule] = (TH1D*)gDirectory->Get(Form("hVdiffOverTvPVsVp_%i_1",iModule));
    hVdiffOverTvPVsVpSlices[iModule]->SetNameTitle(Form("hVdiffOverTvPVsVpSlices_%i",iModule),
						   Form("hVdiffOverTvPVsVpSlices_%i",iModule));
    hUdiffOverTuPVsUp[iModule]->FitSlicesY();
    hUdiffOverTuPVsUpSlices[iModule] = (TH1D*)gDirectory->Get(Form("hUdiffOverTuPVsUp_%i_1",iModule));
    hUdiffOverTuPVsUpSlices[iModule]->SetNameTitle(Form("hUdiffOverTuPVsUpSlices_%i",iModule),
						   Form("hUdiffOverTuPVsUpSlices_%i",iModule));
    hVdiffOverTvPVsUp[iModule]->FitSlicesY();
    hVdiffOverTvPVsUpSlices[iModule] = (TH1D*)gDirectory->Get(Form("hVdiffOverTvPVsUp_%i_1",iModule));
    hVdiffOverTvPVsUpSlices[iModule]->SetNameTitle(Form("hVdiffOverTvPVsUpSlices_%i",iModule),
						   Form("hVdiffOverTvPVsUpSlices_%i",iModule));
  }

  cout << "\t[DONE]" << endl;
}

//_________________
double* FitPtDependence(TH1* histo) {

  Double_t mPosConst, mNegConst, mPosConstErr, mNegConstErr;
  Double_t mPosSlope, mNegSlope, mPosSlopeErr, mNegSlopeErr;

  double *mRetPointer;
  double mRetArr[4];
  mRetPointer = mRetArr;

  TF1* fNegFunc = new TF1("fNegFunc","[0]+[1]*x",-1.9,-0.6);
  TF1* fPosFunc = new TF1("fPosFunc","[0]+[1]*x",0.6,1.9);
  
  fNegFunc->SetParameters(0.,0.);
  fNegFunc->SetLineColor(kBlue);
  fPosFunc->SetParameters(0,0);
  fPosFunc->SetLineColor(kRed);

  histo->Sumw2();
  histo->GetYaxis()->SetRangeUser(-1.,1.);
  histo->Draw();
  histo->Fit("fNegFunc","ER");
  histo->Fit("fPosFunc","ER+");

  mNegConst = fNegFunc->GetParameter(0);
  mNegConstErr = fNegFunc->GetParError(0);
  mNegSlope = fNegFunc->GetParameter(1);
  mNegSlopeErr = fNegFunc->GetParError(1);

  mPosConst = fPosFunc->GetParameter(0);
  mPosConstErr = fPosFunc->GetParError(0);
  mPosSlope = fPosFunc->GetParameter(1);
  mPosSlopeErr = fPosFunc->GetParError(1);

  mRetArr[0] = (mPosConst + mNegConst)/2;
  mRetArr[1] = (mPosConstErr + mNegConstErr)/2;
  mRetArr[2] = (mPosSlope + mNegSlope)/2;
  mRetArr[3] = (mPosSlopeErr + mNegSlopeErr)/2;


  TLegend* lLegend = new TLegend(0.5,0.5,0.95,0.95);
  lLegend->SetFillColor(kWhite);
  lLegend->SetHeader("Fit parameters:");
  lLegend->AddEntry((TObject*)0,Form("Negative charge:"),"");
  lLegend->AddEntry((TObject*)0,Form("Constant: %4.2f \\pm %4.2f",
				     fNegFunc->GetParameter(0),fNegFunc->GetParError(0)), "");
  lLegend->AddEntry((TObject*)0,Form("Slope: %4.2f \\pm %4.2f",
				     fNegFunc->GetParameter(1),fNegFunc->GetParError(1)), "");
  lLegend->AddEntry((TObject*)0,Form("Positive charge:"),"");
  lLegend->AddEntry((TObject*)0,Form("Constant: %4.2f \\pm %4.2f",
				     fPosFunc->GetParameter(0),fPosFunc->GetParError(0)), "");
  lLegend->AddEntry((TObject*)0,Form("Slope: %4.2f \\pm %4.2f",
				     fPosFunc->GetParameter(1),fPosFunc->GetParError(1)), "");
  lLegend->Draw();

  return mRetPointer;
}

//_________________
void StGmtQAPlotter::SetDefaults() {

  gStyle->SetOptStat(0);
  for(Int_t iIter=0; iIter<8; iIter++) {
    
    mUConstArr[iIter] = 0;
    mUConstErrArr[iIter] = 0;
    mUSlopeArr[iIter] = 0;
    mUSlopeErrArr[iIter] = 0;

    mVConstArr[iIter] = 0;
    mVConstErrArr[iIter] = 0;
    mVSlopeArr[iIter] = 0;
    mVSlopeErrArr[iIter] = 0;
  }
}

//_________________
void StGmtQAPlotter::PrintPtFitParameters() {

  cout << "Measured pT fit parameters:" << endl;
  cout << "---------------------------------------------------------------" << endl;
  cout << "Constant U: " ;
  for(Int_t iModule=0; iModule<8; iModule++) {
    cout << Form("%5.4f +/- %5.4f", mUConstArr[iModule], mUConstErrArr[iModule]) << " | ";
  }
  cout << endl;
  cout << "Constant V: " ;
  for(Int_t iModule=0; iModule<8; iModule++) {
    cout << Form("%5.4f +/- %5.4f", mVConstArr[iModule], mVConstErrArr[iModule]) << " | ";
  }
  cout << endl;
  cout << "---------------------------------------------------------------" << endl;
  cout << "Slope U:    " ;
  for(Int_t iModule=0; iModule<8; iModule++) {
    cout << Form("%5.4f +/- %5.4f", mUSlopeArr[iModule], mUSlopeErrArr[iModule]) << " | ";
  }
  cout << endl;
  cout << "Slope V:    " ;
  for(Int_t iModule=0; iModule<8; iModule++) {
    cout << Form("%5.4f +/- %5.4f", mVSlopeArr[iModule], mVSlopeErrArr[iModule]) << " | ";
  }
  cout << endl << endl;
}
