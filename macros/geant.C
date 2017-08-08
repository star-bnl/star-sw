void geant() {
  gROOT->LoadMacro("bfc.C");
  bfc(0,"VMC,VMCAlignment,mysql,sdt20160301,pxlDb,sstDb,istDb,RunG.1,sim_T,nodefault,quiet",0);
  //  bfc(0,"VMC,VMCAlignment,mysql,y2016a,pxlDb,sstDb,istDb,RunG.1,sim_T,nodefault,quiet",0);
}
