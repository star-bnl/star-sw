TCanvas *c1 = 0;
void TpcSectorOffSet(TChain *laser=0) {// offset between sector 16 (tpx) and all other East sector
  //  root.exe *tags.root 'Chain.C("laser")' 'TpcSectorOffSet.C(chain)'
  if (! laser) {
    cout << "Can't find laser  TTree" << endl;
    return;
  }
  if (! c1) c1 = new TCanvas();
  c1->Clear();
  laser->Draw("fHits.xyz.mX3/fEvtHdr.fDriVel*fEvtHdr.fClock*1e6>>M12(200,-25,25)","fHits.sector>12&&fHits.sector!=16");
  laser->Draw("fHits.xyz.mX3/fEvtHdr.fDriVel*fEvtHdr.fClock*1e6>>M16(200,-25,25)","fHits.sector>12&&fHits.sector==16");
  laser->Draw("fTracks.dU.mX3/fEvtHdr.fDriVel*fEvtHdr.fClock*1e6>>S12(800,-20,20)",
	      "fTracks.mSector>12&&fTracks.mSector!=16&&fTracks.Laser.Bundle&&fTracks.mNumberOfFitPointsTpc>40");
  laser->Draw("fTracks.dU.mX3/fEvtHdr.fDriVel*fEvtHdr.fClock*1e6>>S16(800,-20,20)",
	      "fTracks.mSector>12&&fTracks.mSector==16&&fTracks.Laser.Bundle&&fTracks.mNumberOfFitPointsTpc>40");
  TH1F *M12 = (TH1F *) gDirectory->Get("M12");
  M12->Fit("gaus");
  TF1 *gaus = (TF1 *) M12->GetListOfFunctions()->FindObject("gaus");
  if (! gaus) return;
  Double_t x12 = gaus->GetParameter(1);
  Double_t dx12 = gaus->GetParError(1);
  TH1F *M16 = (TH1F *) gDirectory->Get("M16");
  M16->Fit("gaus");
  gaus = (TF1 *) M16->GetListOfFunctions()->FindObject("gaus");
  Double_t x16 = gaus->GetParameter(1);
  Double_t dx16 = gaus->GetParError(1);
  Double_t shift = x16 - x12;
  Double_t dshift = TMath::Sqrt(dx12*dx12 + dx16*dx16);
  cout << "Membrane Shift = " << shift << " +/- " << dshift << endl;
  TH1F *S12 = (TH1F *) gDirectory->Get("S12");
  S12->Fit("gaus");
  TF1 *gaus = (TF1 *) S12->GetListOfFunctions()->FindObject("gaus");
  if (! gaus) return;
  Double_t xL12 = gaus->GetParameter(1);
  Double_t dxL12 = gaus->GetParError(1);
  TH1F *S16 = (TH1F *) gDirectory->Get("S16");
  S16->Fit("gaus");
  gaus = (TF1 *) S16->GetListOfFunctions()->FindObject("gaus");
  Double_t xL16 = gaus->GetParameter(1);
  Double_t dxL16 = gaus->GetParError(1);
  Double_t shiftL = - (xL16 - xL12);
  Double_t dshiftL = TMath::Sqrt(dxL12*dxL12 + dxL16*dxL16);
  cout << "ShiftL = " << shiftL << " +/- " << dshiftL << endl;
  c1->Clear();
  c1->Divide(1,2);
  c1->cd(1);
  M12->Draw();
  M16->Draw("sames");
  c1->cd(2);
  S12->Draw();
  S16->Draw("sames");
  if (dshift > 0 && dshiftL > 0) {
    Double_t w = 1./(dshift*dshift) + 1./(dshiftL*dshiftL);
    Double_t Shift = (shift/(dshift*dshift) + shiftL/(dshiftL*dshiftL))/w;
    Double_t dShift = 1./TMath::Sqrt(w);
    cout << "Averaged Shift = " << Shift << " +/- " << dShift << endl;
  }
}
