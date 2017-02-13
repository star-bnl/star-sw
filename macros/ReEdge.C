TH2D *ReEdge(TH2D *mu) { // Edge => pad
  Int_t Npads[45] = {  88,    96,   104,   112,   118,   126,   134,   142,   150,   158,
		      166,   174,   182,
		       98,   100,   102,   104,   106,   106,   108,   110,   112,   112,
                      114,   116,   118,   120,   122,   122,   124,   126,   128,   128,
                      130,   132,   134,   136,   138,   138,   140,   142,   144,   144,    
                      144,   144};

  if (! mu) return 0;
  TH2D *muP = new TH2D(*mu);
  muP->SetName("muP");
  muP->Reset();  
  TAxis *xa = mu->GetXaxis();
  TAxis *ya = mu->GetYaxis();
  TAxis *xan = muP->GetXaxis();
  xan->Set
  for (Int_t iy = ya->GetFirst(); iy <= ya->GetLast(); iy++) {
    Double_t edge = ya->GetBinCenter(iy);
    // CdEdx[NdEdx].edge = pad - (1 + gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(row));
    for (Int_t ix = xa->GetFirst(); ix <= xa->GetLast(); ix++) {
      Double_t x = xa->GetBinCenter(ix);
      Int_t sector = (x-1)/45. + 1;
      Int_t row    =  x  - 45*(sector-1);
      Double_t pad  = edge;
      if (edge < 0) pad += 1 + Npads[row-1];
      Int_t iyn = ya->FindBin(pad);
      Double_t error = mu->GetBinError(ix,iy);
      Double_t value = mu->GetBinContent(ix,iy);
      //      cout << "ix = " << ix << "\tiy = " << iy << "\tv = " << value << " +/- " << error << "\tiyn = " << iyn << endl;
      muP->SetBinContent(ix,iyn,value);
      muP->SetBinError(ix,iyn,error);
    }
  }
}
