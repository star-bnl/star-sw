void PrintTpcRaw(StTpcRawData *data) {
  if (! data) return;
  for (UInt_t sector = 1; sector <= 24; sector++) {
    StTpcDigitalSector *sectorD = data->GetSector(sector);
    if (! sectorD) continue;
    cout << "sector " << sector << endl;
    Int_t nrows = sectorD->numberOfRows();
    for (Int_t row = 1; row <= nrows; row++) {
      cout << "sector/row " << sector << "/" << row << endl;
      Int_t npads = sectorD->numberOfPadsInRow(row);
      for (Int_t pad = 1; pad <= npads; pad++) {
	cout << "sector/row/pad = " << sector << "/" << row << "/" << pad << endl;
	Int_t ntb = sectorD->numberOfTimeBins(row,pad);
	if (! ntb) continue;
	cout << "sector/row/pad = " << sector << "/" << row << "/" << pad << " = " << ntb << " time sequences" << endl;
      }
    }
  }
}
