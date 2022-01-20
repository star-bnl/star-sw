void CheckPads(Int_t sector = 0) {
  Int_t nPads[72] = { 
    52, 54, 56, 58, 60, 62, 62, 64, 66, 68,
    70, 72, 74, 74, 76, 78, 80, 82, 84, 86,
    86, 88, 90, 92, 94, 96, 98, 98,100,102,
   104,106,108,110,110,112,114,116,118,120,
    98,100,102,104,106,106,108,110,112,112,
   114,116,118,120,122,122,124,126,128,128,
   130,132,134,136,138,138,140,142,144,144,
   144,144};
  TH3F *AlivePads = (TH3F *) gDirectory->Get("AlivePads");
  if (! AlivePads) {cout << "AlivePads is missing" << endl; return;}
  TProfile3D *ActivePads = (TProfile3D *) gDirectory->Get("ActivePads");
  if (! ActivePads) {cout << "ActivePads is missing" << endl; return;}
  Int_t nx = AlivePads->GetXaxis()->GetNbins();
  Int_t ny = AlivePads->GetYaxis()->GetNbins();
  Int_t nz = AlivePads->GetZaxis()->GetNbins();
  cout << gDirectory->GetName() << endl;
  TCanvas *c1 = 0;
  if (sector > 0) {
    c1 = new TCanvas("c1","c1",800,1200);
    c1->Divide(1,2);
  }
  //  for (Int_t s = 1; s <= nx; s++) {
  Int_t s1 = 1, s2 = nx;
  if (sector > 0) {s1 = s2 = sector;}
  for (Int_t s = s1; s <= s2; s++) {
    for (Int_t r = 1; r <= ny; r++) {
      TString  Dead(Form("/*Dead: */ {%2i,%3i,",s,r));
      TString  Alive(Form("/*Alive:*/ {%2i,%3i,",s,r));
      Int_t p1d = 0, p2d = -1;
      Int_t p1a = 0, p2a = -1;
      Int_t edge1 = 1;
      if (r > 40) edge1 = 2;
      Int_t edge2 = 2;
      for (Int_t p = edge1 + 1; p < nPads[r-1] - edge2; p++) {
	Double_t g = AlivePads->GetBinContent(s,r,p);
	Double_t a = ActivePads->GetBinContent(s,r,p);
	if (g >  0.0 && a >  0.0) continue;
	Double_t g0 = AlivePads->GetBinContent(s,r,p-1);
	Double_t g1 = AlivePads->GetBinContent(s,r,p+1);
	if ((g <= 0.0 || g0 <= 0.0 || g1 <= 0.0) && a <= 0.0) continue;
	if (a <= 0.0 || g <= 0.0) {
#if 0
#if 1
	  cout << "s " << s << " r " << r << " p " << p << " a = " << a << " g = " << g << " g0 " << g0 << " g1 " << g1 << endl; 
#else
	  cout << "s " << s << " r " << r << " p " << p << " a = " << a << " g = " << g << endl;
#endif
#endif
	}
	if (g >  0.0){// Dead
	  if (p1d == 0) {
	    p1d = p;
	    p2d = p;
	  } else {
	    if (p == p2d + 1) p2d = p;
	    else {
	      cout << Dead.Data() << Form("%3i,%3i}",p1d, p2d) << endl;
	      p1d = p2d = p;
	    }
	  }
	} else {// alive
	  if (p1a == 0) {
	    p1a = p;
	    p2a = p;
	  } else {
	    if (p == p2a + 1) p2a = p;
	    else {
	      cout << Alive.Data() << Form("%3i,%3i}",p1a, p2a) << endl;
	      p1a = p2a = p;
	    }
	  }
	}
      }
      if (p1d <= p2d) cout << Dead.Data() << Form("%3i,%3i}",p1d, p2d) << endl;
      if (p1a <= p2a) cout << Alive.Data() << Form("%3i,%3i}",p1a, p2a) << endl;
    }
    if (c1) {
      c1->cd(1); ActivePads->GetXaxis()->SetRange(s,s); ActivePads->Project3D(Form("yz_%i",s))->Draw("colz");
      c1->cd(2); AlivePads->GetXaxis()->SetRange(s,s); AlivePads->Project3D(Form("yz_%i",s))->Draw("colz");
      c1->Update();
    }
  }
}
