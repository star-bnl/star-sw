void muon(int n=1) {
  particleGun(n,"mu+,mu-",9.9,10.1,-0.5,-0.25);
  gROOT->ProcessLine("chain->Clear();");
};
