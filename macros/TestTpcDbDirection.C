TCanvas *c1 = 0;
void TestTpcDbDirection() {
  StTpcCoordinateTransform transform(gStTpcDb);
  StGlobalDirection glob;
  StGlobalCoordinate gC;
  Double_t beta = 0;
  Int_t numSectors = 24;
  TH2D *histW = new TH2D("sectorsW","sectors",100,-120,120,100,-120,120);
  TH2D *histE = new TH2D("sectorsW","sectors",100,-120,120,100,-120,120);
  for (Int_t sector = 1; sector <= 24; sector++) {
    beta = (sector>12) ? (numSectors-sector)*2.*180/((numSectors)/2.)
      : sector*2.*180/((numSectors)/2.);
    StTpcLocalSectorDirection  locD(10,100,  1,sector,1);
    StTpcLocalSectorCoordinate locC( 8, 80,20,sector,1);
    transform(locD,glob); cout << sector << "\t" << beta << "\t" << locD << "\t" <<  glob << endl;
    transform(glob,locD,sector,1); cout << " back to " << locD << endl;
#if 1
    transform(locC,gC); cout << sector << "\t" << locC << "\t" <<  gC << endl;
    transform(gC,locC,sector,1); cout << " back to " << locC << endl;
#endif
    if (sector <= 12) {
      histW->Fill(glob.position().x(),glob.position().y(),sector);
      histW->Fill(gC.position().x(),gC.position().y(),sector);
    }  else {
      histE->Fill(glob.position().x(),glob.position().y(),sector);
      histE->Fill(gC.position().x(),gC.position().y(),sector);
    } 
  }
  TCanvas *c1 = new TCanvas();
  c1->Divide(2,1);
  c1->cd(1);
  histW->Draw("text");
  c1->cd(2);
  histE->Draw("text");
}
