// $Id: Bfc.C,v 1.2 1999/02/11 15:44:27 wenaus Exp $
// $Log: Bfc.C,v $
// Revision 1.2  1999/02/11 15:44:27  wenaus
// macro to read DSTs into StEvent and analyze
//

TBrowser *b = 0;
class StChain;
StChain  *chain=0;


void Load(){
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("xdf2root");
    gSystem->Load("St_Tables");
    gSystem->Load("St_io_Maker");
    gSystem->Load("St_xdfin_Maker");
    gSystem->Load("libmsg");
    gSystem->Load("libtls");
    gSystem->Load("St_params_Maker");
    gSystem->Load("StMagF");
    //    gSystem->Load("St_db_Maker");
    gSystem->Load("St_calib_Maker");
    gSystem->Load("libEG");
    gSystem->Load("St_evg_Maker");
    //    gSystem->Load("geometry");
   //    gSystem->Load("St_geant_Maker");
    gSystem->Load("St_TLA_Maker");
    gSystem->Load("St_tpc");
    gSystem->Load("St_tss_Maker");
    gSystem->Load("St_tcl_Maker");
    gSystem->Load("St_tpt_Maker");
    gSystem->Load("St_ftpc");
    gSystem->Load("St_fss_Maker");
    gSystem->Load("St_fcl_Maker");
    gSystem->Load("St_fpt_Maker");
    gSystem->Load("St_emc");
    gSystem->Load("St_ems_Maker");
    gSystem->Load("St_emc_Maker");
    gSystem->Load("St_ctf");
    gSystem->Load("St_ctf_Maker");
    gSystem->Load("St_svt");
    gSystem->Load("St_srs_Maker");
    gSystem->Load("St_stk_Maker");
    gSystem->Load("St_strange");
    gSystem->Load("St_global");
    gSystem->Load("St_dst_Maker");
    gSystem->Load("St_run_summary_Maker");
}


Bfc(const Int_t   Nevents=1,
    const Char_t *FileInput = "/afs/rhic/star/data/samples/hijet-g2t.root")
{
  // Dynamically link some shared libs
  if (gClassTable->GetID("StChain") < 0) Load();

  // Create the main chain object
  if (!chain) chain = new StChain("bfc");
  St_io_Maker *in    = new St_io_Maker("Input","all");
  //  Create the makers to be called by the current chain
  St_params_Maker  *params = new St_params_Maker("params","params");
  //  St_TLA_Maker       *geom = new St_TLA_Maker("geom","run/geant/Run");
  St_TLA_Maker      *geant = new St_TLA_Maker("geant","event/geant/Event");
  // St_geant_Maker    *geant = new St_geant_Maker("geant","event/geant/Event");
  //  geant->LoadGeometry("detp geometry field_only");
  St_calib_Maker    *calib = new St_calib_Maker("calib","calib"); 
  St_evg_Maker      *evgen = new St_evg_Maker("evgen","event/evgen");
  //  St_fss_Maker   *ftpc_raw = new St_fss_Maker("ftpc_raw","event/raw_data/ftpc");
  //  St_tss_Maker    *tpc_raw = new St_tss_Maker("tpc_raw","event/raw_data/tpc");
  // Set parameters
  //  tpc_raw->adcxyzon();
  //  St_ems_Maker          *emc_raw = new St_ems_Maker("emc_raw","event/raw_data/emc");
  //  St_emc_Maker         *emc_hits = new St_emc_Maker("emc_hits","event/data/emc/hits");
  St_tcl_Maker         *tpc_hits = new St_tcl_Maker("tpc_hits","event/data/tpc/hits");
  St_srs_Maker         *svt_hits = new St_srs_Maker("svt_hits","event/data/svt/hits");
  St_fcl_Maker         *fcl_hits = new St_fcl_Maker("ftpc_hits","event/data/ftpc/hits");
  St_ctf_Maker         *ctf      = new St_ctf_Maker("ctf","event/data/ctf");
  St_tpt_Maker       *tpc_tracks = new St_tpt_Maker("tpc_tracks","event/data/tpc/tracks");
  St_stk_Maker       *stk_tracks = new St_stk_Maker("svt_tracks","event/data/svt/tracks");
  St_fpt_Maker      *ftpc_tracks = new St_fpt_Maker("ftpc_tracks","event/data/ftpc/tracks");
  St_glb_Maker           *global = new St_glb_Maker("global","event/data/global");
  global->Save();
  //  St_dst_Maker        *dst_Maker = new St_dst_Maker("dst","dst");
  //  St_dst_Maker        *dst_Maker = new St_dst_Maker("dst","event/data/global/dst");
  //  dst_Maker->Save();
  St_io_Maker    *out = new St_io_Maker("Output","all");
  //  St_run_summary_Maker  *summary = new St_run_summary_Maker("run_summary","run/dst");
  TFile * root_file = 0;
  if (FileInput)  {
    root_file = new TFile(FileInput,"UPDATE");
    root_file->ls();
    TTree *tree = (TTree *) root_file.Get("Output");
    chain->SetTree(tree);
    TObjArray *list = tree->GetListOfBranches();
    if (list) {
      TIter next(list);
      TBranch *nextb = 0;
      while (nextb = (TBranch *) next()) cout << "Branch: "<< nextb->GetName() << endl;
    // chain =  (StChain *) root_file.Get("bfc");
    // prepare StChain
    }
  }
  out->Add("global");
  out->Add("dst");
  TObjArray *list = chain->GetTree()->GetListOfBranches();
  if (list) {
    TIter next(list);
    TBranch *nextb = 0;
    while (nextb = (TBranch *)next()) cout << "Read Branch: "<< nextb->GetName() << endl;    
  }
  
  // Create HTML docs of all Maker's involved
  //  chain->MakeDoc();


  chain->PrintInfo();
  // Init the chain and all its makers
  int iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");


  //  chain->MakeTree("StChainTree","Title");
  //  chain->SetBranches();
  // Prepare TCanvas to show some histograms created by makers

  gBenchmark->Start("bfc");
  Int_t i=0;
  for (Int_t i =1; i <= Nevents; i++){
    //    root_file->cd();
    //    chain->GetEvent(i-1);
    if (chain->Make(i)) break;
    chain->Maker("geant")->DataSet()->ls("*");

#if 0
    if (root_file){
      gBenchmark->Start("root i/o");
      root_file->cd();
      cout << "============================ bytes written =" << chain->FillTree() << endl;
       chain->Tree()->ls();
      gBenchmark->Stop("root i/o");
    }
#endif

    if (i != Nevents) chain->Clear();
    printf ("===========================================\n");
    printf ("=========================================== Done with Event no. %d\n",i);
    printf ("===========================================\n");
  }

  if (Nevents > 1) {
    chain->Finish();
    if (root_file){
      //      chain->Tree()->ls();
      //      chain->Tree()->Write();
      root_file->Write();
      root_file->Close();   
      delete root_file;
      gBenchmark->Print("root i/o");
    }
    gBenchmark->Print("bfc");
  }
  else {if (!b)   b = new TBrowser;}
}
