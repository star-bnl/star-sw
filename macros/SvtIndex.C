void SvtIndex() {
  StSvtGeometry *geomF = new StSvtGeometry("FULL");
  const Int_t BL[3] = {8, 12, 16};
  const Int_t BW[3] = {4,  6,  7};
  Int_t H = 1;
  for (Int_t B = 1; B <= 3; B++) 
    for (Int_t L = 1; L <= BL[B-1]; L++) 
      for (Int_t W = 1; W <= BW[B-1]; W++) {
	//	for (Int_t H = 1; H <= 2; H++) 
	Int_t layer = 2*B - 1 + L%2;
	Int_t Id = 1000*layer + 100*W + L;
	cout << Form("{\"L%02iB%iW%02i\",%5i,%5i},",L,B,W,Id,geomF->getWaferIndex(B,L,W)) << endl;
	//	cout << "B/L/W/H\t" << B << "/" << L << "/" << W << "/" << H 
	//	     << "\tindex " << geomF->getWaferIndex(B,L,W) << endl;
      }
}
