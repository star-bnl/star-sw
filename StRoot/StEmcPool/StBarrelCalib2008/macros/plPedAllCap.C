plPedAllCap() {

  fd=new TFile("iter3/pedBprsR9067013allCap.hist.root"); assert(fd->IsOpen());

  gStyle->SetPalette(1,0);

  c=new TCanvas();
 pedBPRScap->Draw("colz");

  c=new TCanvas();
  c->Divide(1,2);
  c->cd(1); peakPedBPRScap->Draw("colz");
  c->cd(2); statBPRSallCap->Draw();

  c=new TCanvas();
  sigPedBPRScap->Draw("colz");

  c=new TCanvas(); gStyle->SetOptStat(1001110);
  c->Divide(2,2);
  c->cd(1);pedSoftId7->Draw(); pedSoftId7->SetAxisRange(150,190);  gPad->SetLogy();
 c->cd(2);pedSoftId607->Draw();  pedSoftId607->SetAxisRange(150,190); gPad->SetLogy();
 c->cd(3);pedSoftId2407->Draw();  pedSoftId2407->SetAxisRange(150,190); gPad->SetLogy();
 c->cd(4);pedSoftId3007->Draw();  pedSoftId3007->SetAxisRange(150,190); gPad->SetLogy();


 printf("MANY pages has piled up!!!\n");
}
