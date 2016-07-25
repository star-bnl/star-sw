class PAI;
PAI *xpai = 0;
void pai() {
  gSystem->Load("PAI");
  xpai = PAI::Instance();
   TFile *file = new TFile("PAI200K.root","recreate");
   xpai->xGenerate();
   file->Write();
   delete file;
}
