plR() {
 gROOT->Reset();
  gSystem->Load("St_base");
  gROOT->LoadMacro("/afs/rhic/star/spin/balewski/root_macros/JBRoot.C");
 char* ifile="st_physics_5109025_raw_1030001.hist.root";
 
 // Connect the input file and get the 2-d histogram in memory
  TList *top = Johf(ifile);

  TIter Hi(Jcd("eeRawHist",top));

    TPad *pd1=Jcan(300,400,"file=XXX", .99);
    pd1->Divide(1,2);
   Jpl("snB",1, Hi,pd1,0,"h");
   Jpl("snT",2, Hi,pd1,0,"h");
}

