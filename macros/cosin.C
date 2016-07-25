void cosin() {
  Int_t numSectors = 24;
  for (Int_t sector = 1; sector <= numSectors; sector++) {
    Double_t beta; 
    if (sector>12)  beta = (numSectors-sector)*2.*TMath::Pi()/(numSectors/2);
    else            beta =  sector*2.*TMath::Pi()/(numSectors/2);
    Double_t mCosForSector = TMath::Cos(beta); // careful, sector is the sector number, not index
    Double_t mSinForSector = TMath::Sin(beta); // careful, sector is the sector number, not index
    //    cout << "Sector #\t" << sector << "\tcos\t" << mCosForSector << "\tsin\t" << mSinForSector << endl;
    printf("Sector #\t %2i \tsin %10f\tcos %10f \n",sector, mSinForSector, mCosForSector);
  }
}
