/*
  root.exe 11p5GeV.A+C.root
 */

void PVxyz() {
  TString fName("PVxyz");
  TDirectory *dir = gDirectory;
  fName += gSystem->BaseName(dir->GetName());
  TFile *fOut = new TFile(fName,"recreate");
  TH1 *x = (TH1 *) dir->Get("/Particles/KFParticlesFinder/PrimaryVertexQA/x");
  x->Write();
  TH1 *y = (TH1 *) dir->Get("/Particles/KFParticlesFinder/PrimaryVertexQA/y");
  y->Write();
  TH1 *z = (TH1 *) dir->Get("/Particles/KFParticlesFinder/PrimaryVertexQA/z");
  z->Write();
  TH1 *e = (TH1 *) dir->Get("/Tracks/hPVError");
  e->Write();
}
