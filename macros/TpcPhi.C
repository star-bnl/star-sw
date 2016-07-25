const Int_t Nrows = 45;
const Double_t Radii[Nrows] = {
  60.0,    64.8,   69.6,    74.4,   79.2, // inner
  84.0,    88.8,   93.6,    98.8,  104.0,
 109.2,   114.4,  119.6, 
                            127.195,129.195, // Outer
  131.195, 133.195, 135.195,137.195,139.195, 
  141.195, 143.195, 145.195,147.195,149.195, 
  151.195, 153.195, 155.195,157.195,159.195, 
  161.195, 163.195, 165.195,167.195,169.195, 
  171.195, 173.195, 175.195,177.195,179.195, 
  181.195, 183.195, 185.195,187.195,189.195 };
const Int_t Npads[Nrows] = {
   88,  96, 104, 112, 118,  // inner
  126, 134, 142, 150, 158, 
  166, 174, 182,
  98, 100, // Outer
  102, 104, 106, 106, 108, // 15-20 
  110, 112, 112, 114, 116, // 21-25 
  118, 120, 122, 122, 124, // 26-30
  126, 128, 128, 130, 132, // 31-35
  134, 136, 138, 138, 140, // 36-40
  142, 144, 144, 144, 144}; // 41-45
Double_t  pitchI  = 0.335;//        ! tpc padrow pitch width
Double_t  pitchO  = 0.670;//         ! outer tpc padrow pitch width

//________________________________________________________________________________
TGraph *TpcPhi(Int_t Edge = 0) {
  // from tpcegeo.g
  Double_t  x[Nrows];
  Double_t  y[Nrows];
  Double_t xx[Nrows];
  Double_t Phi[Nrows];
  for (Int_t i = 1; i <= Nrows; i++) {
    x[i-1] = Radii[i-1];
    xx[i-1] = i;
    Double_t pitch = pitchI;
    if (i >  13) pitch = pitchO;
    Double_t pp = Npads[i-1];
    y[i-1] = pitch*(0.5*pp - Edge);
    Phi[i-1] = 180./TMath::Pi()*TMath::ATan2(y[i-1],x[i-1]);
    //    cout << "i = " << i << "\tx = " << x[i-1] << "\ty = " << y[i-1] << "\tPhi = " << Phi << endl;
    printf("i = %2i x = %10.3f y = %10.3f Phi = %10.3f\n",i,x[i-1],y[i-1],Phi[i-1]); 
  }
  TGraph *gr = new TGraph(Nrows,xx,Phi);
  gr->SetMarkerStyle(20);
  gr->SetMarkerColor(Edge+2);
  return gr;
}
//________________________________________________________________________________
Double_t EdgeDist(Double_t Phi, Double_t row) {
  Int_t irow = row;
  if (irow <  1) irow =  1;
  if (irow > 45) irow = 45;
  Double_t pitch = pitchI;
  if (irow >  13) pitch = pitchO;
  
  Double_t pp = Npads[irow-1];
  Double_t y = 0.5*pp;
  Double_t PhiMax = 180./TMath::Pi()*TMath::ATan2(y,Radii[irow-1]/pitch);
  //  if (Phi < 0) dist = - dist;
  return Phi/PhiMax;
}
//________________________________________________________________________________
Double_t Loop() {
  TTreeIter iter;
  iter.AddFile(gDirectory->GetName());
  const Float_t  &i               = iter("i");
  const Float_t  &j               = iter("j");
  const Float_t  &x               = iter("x"); // Phi
  const Float_t  &y               = iter("y"); // row
  const Float_t  &mean            = iter("mean");
  const Float_t  &rms             = iter("rms");
  const Float_t  &peak            = iter("peak");
  const Float_t  &mu              = iter("mu");
  const Float_t  &sigma           = iter("sigma");    
  TProfile *Inner = new TProfile("Inner","mu versus edge for Inner",210,-1.05,1.05);
  Inner->SetMarkerStyle(20);
  TProfile *Outer = new TProfile("Outer","mu versus edge for Outer",210,-1.05,1.05);
  Outer->SetMarkerStyle(20);
  Outer->SetMarkerColor(2);
  while (iter.Next()) {
    if (i < 1 || j < 1) continue;
    if (TMath::Abs(mu) > 0.45) continue;
    Double_t edge = EdgeDist(x,y);
    cout << "Phi " << x << "\trow " << y << "\t edge " << edge << "\tmu " << mu << endl;
    if (y < 13) Inner->Fill(edge,mu);
    else        Outer->Fill(edge,mu);
  }
}
