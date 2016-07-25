void GenericHitCollection() {
  gROOT->LoadMacro("bfc.C");
  bfc(0,"event,y2005d,GenericHit,evout,tree,debug,mysql,nodefault",0,"GenericContainer.root");
}
