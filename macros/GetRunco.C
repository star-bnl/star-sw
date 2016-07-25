void  GetRunco(const char *MainFile=
	       "/star/data47/reco/productionMinBias/ReversedFullField/P04ik.ittf-W46/2004/008/st_physics_5008019_raw_2010010.event.root") {
  gROOT->LoadMacro("bfc.C");
  bfc(0,"in,nodefault",MainFile,0,0);
  StIOMaker *IOMk = (StIOMaker *) chain->Maker("inputStream");
  IOMk->SetBranch("runcoBranch",0,"r");
  chain->Make();
  TObjectSet *k =  (TObjectSet *)  StMaker::GetChain()->Find("bfc/.make/inputStream/.make/inputStream_Root/.data/bfcTree/runcoBranch");
  k->ls(2);
}

