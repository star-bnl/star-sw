// macro to instantiate the Geant3 from within
// STAR  C++  framework and get the starsim prompt
// To use it do
//  root4star starsim.C
void starsim() {
  gROOT->LoadMacro("bfc.C");
  bfc(0,"gstar,StEvent,nodefault",0,0,0);
  printf ("\n\n . . . . Switching to the starsim interactive session:\n");
  TGiant3::Geant3()->GetKuipPrompt();
  printf ("\n----------\nRestore the ROOT interactive session\n");
}
