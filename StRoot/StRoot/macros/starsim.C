// macro to instantiate the Geant3 from within
// STAR  C++  framework and get the starsim prompt
// To use it do
//  root4star starsim.C
class St_geant_Maker;
void starsim() {
  gROOT->Macro("Load.C");
  gSystem->Load("libSt_g2t");
  gSystem->Load("libStarMagField.so");
  gSystem->Load("St_geant_Maker");
  (new St_geant_Maker())->Init();
  printf ("\n\n . . . . Switching to the starsim interactive session:\n");
  TGiant3::Geant3()->GetKuipPrompt();
  printf ("\n----------\nRestore the ROOT interactive session\n");
}
