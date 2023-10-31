void pi0(int n=1, double ptmn=1.0, double ptmx=10.0, double etamn=-1.0, double etamx=2.0 ) {
  particleGun(n,"pi0",ptmn,ptmx,etamn,etamx);
gROOT->ProcessLine("chain->Clear();");
};
