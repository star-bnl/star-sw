// $Id: evg.C,v 1.2 1999/01/08 21:39:53 fisyak Exp $
// $Log: evg.C,v $
// Revision 1.2  1999/01/08 21:39:53  fisyak
// Add Gene Van Buren bfc description
//
TBrowser *b = 0;
class StChain;
StChain  *chain=0;
void Load(){
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("xdf2root");
    gSystem->Load("St_xdfin_Maker");
    gSystem->Load("St_Tables");
    gSystem->Load("libEG");
    gSystem->Load("St_evg_Maker");
}
evg(const Int_t   Nevents=1,
    const Char_t *fileinp = "/disk1/star/auau200/central/vni/evg/vni_aft.xdf",
    const Char_t *fileout =0,
    const Char_t *FileOut = "vni_aft.root")
{
  // Dynamically link some shared libs
  if (gClassTable->GetID("StChain") < 0) Load();
  St_XDFFile   *xdf_in   = 0;
  if (fileinp)  xdf_in   = new St_XDFFile(fileinp,"r");
  St_XDFFile  *xdf_out   = 0;
  if (fileout) xdf_out   = new St_XDFFile(fileout,"wb");
  TFile       *root_out  = 0; 
  if (FileOut) root_out  =  new TFile(FileOut,"RECREATE");
  //TFile      *root_tree= new TFile("auau_central_hijing.tree.root","RECREATE");
  // Create the main chain object
  if (chain) delete chain;
  chain = new StChain("evg","The STAR ROOT event generator interface");
  //  Create the makers to be called by the current chain
  if (xdf_in) {
    St_xdfin_Maker *xdfin = new St_xdfin_Maker("xdfin");
    xdfin->Set_Init_Done(kTRUE);
    chain->SetInputXDFile(xdf_in);
  }
  St_evg_Maker      *evgen = new St_evg_Maker("evgen","evgen");
  evgen->Save();
  // Create HTML docs of all Maker's involved
  //  chain->MakeDoc();
  chain->PrintInfo();
  // Init the chain and all its makers
  int iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  //  chain->MakeTree("StChainTree","Title");
  // Prepare TCanvas to show some histograms created by makers
  gBenchmark->Start("evg");
  Int_t i=0;
  chain->MakeTree("evg","Proba 1");
  chain->SetBranches();
  for (Int_t i =1; i <= Nevents; i++){
    if (chain->Make(i)) break;
    St_DataSet *particle = chain->DataSet("evgen");
      if (xdf_out){
	gBenchmark->Start("xdf out");
	xdf_out->NextEventPut(particle); // xdf output
	gBenchmark->Stop("xdf out");
      }
      if (root_out){
	gBenchmark->Start("root i/o");
	root_out->cd();
	chain->FillClone();
	//	dst->SetWrite();// root output
	gBenchmark->Stop("root i/o");
      }
    //    root_tree->cd();
    //    printf ("Fill Tree\n");
    //    chain->FillTree();
    //  histCanvas->Modified();
    //  histCanvas->Update();
    if (i != Nevents) chain->Clear();
    printf ("===========================================\n");
    printf ("=========================================== Done with Event no. %d\n",i);
    printf ("===========================================\n");
  }
  if (Nevents > 1) {
    chain->Finish();
    delete xdf_in;
    if (xdf_out){
      delete xdf_out;;
      gBenchmark->Print("xdf out");
    }
    if (root_out){
      root_out->Write();
      root_out->Close();
      delete root_out;
      gBenchmark->Print("root i/o");
    }
    gBenchmark->Print("evg");
  }
  else {if (!b)   b = new TBrowser;}
}

