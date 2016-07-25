void dump() {// dump information from Bichsel NTuples
  Int_t nBinx = 101;
  Double_t bglow = -1.025, bgup = 4.025;
  TProfile *bich = 
    new TProfile("bich","The most probable value of log(dE/dx) versus log10(beta*gamma) at dx = 2 cm",
		 nBinx,bglow,bgup);
  dEdxS->Draw("zm:log10(bg)>>bich","x==2","prof");
  TProfile *bichS = 
    new TProfile("bichS","The average value of z = log(dE/dx) versus log10(beta*gamma) at dx = 2 cm",
		 nBinx,bglow,bgup);
  dEdxS->Draw("mean_z:log10(bg)>>bichS","x==2","prof");
  bichS->SetMarkerStyle(20);
  bichS->SetMarkerColor(2);
  bich->Draw();
  bichS->Draw("same");
  TProfile *bichD = 
    new TProfile("bichD","The zm - mean_z of z = log(dE/dx) versus log10(beta*gamma) at dx = 2 cm",
		 nBinx,bglow,bgup);
  //  dEdxS->Draw("zm-mean_z:log10(bg)>>bichD","x==2","prof");
  TProfile *b = 0;
  for (Int_t l=0; l<2; l++) {
    b = bich;
    if (l == 1) b = bichS;
    cout << "//" << b->GetTitle() << endl;
    cout << "//In range of log10(beta*gamma) = [" 
	 <<  b->GetBinCenter(1) << "," 
	 <<  b->GetBinCenter(nBinx) << "]" << endl;
    cout << "Double_t si[" << nBinx << "] = {" << endl;
    for (Int_t i=1; i<=nBinx; i++) {
      cout << "\t" << b->GetBinContent(i);
      if (i != nBinx) cout << ",";
      if (i%5 == 0) cout << endl;
    }
    cout << endl;
    cout << "};" << endl;
  }
}
