// $Id: bfz.C,v 1.1 1999/02/19 16:31:01 fisyak Exp $
//#define gtrack
#define trs
#define emc
#define ctf
TBrowser *b = 0;
class StChain;
StChain  *chain=0;
class St_geant_Maker;
St_geant_Maker *geant;

void Load(){
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("xdf2root");
    gSystem->Load("St_Tables");
    gSystem->Load("StSclRoot");
    gSystem->Load("libmsg");
    gSystem->Load("libtls");
    gSystem->Load("St_params_Maker");
    gSystem->Load("St_calib_Maker");
    gSystem->Load("geometry");
    gSystem->Load("St_g2r");
    gSystem->Load("St_geant_Maker");
    gSystem->Load("St_TLA_Maker");
    gSystem->Load("St_tpc");
#ifndef trs
    gSystem->Load("St_tss_Maker");
#else
    gSystem->Load("StTrsMaker");
    gSystem->Load("St_tpcdaq_Maker");
#endif
    gSystem->Load("St_tcl_Maker");
    gSystem->Load("St_tpt_Maker");
    gSystem->Load("St_ftpc");
    gSystem->Load("St_fss_Maker");
    gSystem->Load("St_fcl_Maker");
    gSystem->Load("St_fpt_Maker");
#ifdef emc
    gSystem->Load("St_emc");
    gSystem->Load("St_ems_Maker");
    gSystem->Load("St_emc_Maker");
#endif
#ifdef ctf
    gSystem->Load("St_ctf");
    gSystem->Load("St_ctf_Maker");
    gSystem->Load("St_mwc");
    gSystem->Load("St_mwc_Maker");
    gSystem->Load("St_trg");
    gSystem->Load("St_trg_Maker");
    gSystem->Load("St_l3");
    gSystem->Load("St_l3t_Maker");
#endif
    gSystem->Load("StRchMaker");

    gSystem->Load("St_svt");
    gSystem->Load("St_srs_Maker");
    gSystem->Load("St_stk_Maker");
    gSystem->Load("St_strange");
    gSystem->Load("St_global");
    gSystem->Load("St_dst_Maker");
    gSystem->Load("St_run_summary_Maker");
    gSystem->Load("St_QA_Maker");
    gSystem->Load("St_io_Maker");
}

bfz(const Int_t Nevents=1,const Char_t *fzfile ="/disk1/star/test/muons.fz")
{                              
  // Dynamically link some shared libs
  if (gClassTable->GetID("StChain") < 0) Load();
  TString FileOut = gSystem->BaseName(fzfile);
  FileOut.ReplaceAll(".fz",".root");
  TFile       *root_out  =  new TFile(FileOut.Data(),"RECREATE");
  // Create the main chain object
  if (chain) delete chain;
  chain = new StChain("bfc");
  //  Create the makers to be called by the current chain
  St_params_Maker  *params = new St_params_Maker("params","params");
  St_TLA_Maker       *geom = new St_TLA_Maker("geom","run/geant/Run");
                      geant = new St_geant_Maker("geant","event/geant/Event");
  geant->SetNwGEANT(40 000 000);
  //  geant->SetNwPAW(1000000);
#ifdef gtrack
  geant->SetIwtype(1);
  geant->Do("debug on;");
#else
  TString cmd("gfile p ");
  cmd += fzfile;
  geant->Do(cmd.Data());
#endif
#ifdef  gtrack
  //  geant->LoadGeometry("detp geometry field_only");
  geant->LoadGeometry("detp geometry complete");
  geant->Do("subevent 0;");
  geant->Do("gkine 10 6 1. 1. -1. 1. 0 6.28  -1. 1.;");
  geant->Do("mode g2tm prin 1;");
  //  geant->Do("next;");
  //  geant->Do("dcut cave z 1 10 10 0.03 0.03;");
  geant->Do("debug on;");
  geant->Do("swit 2 3;");
#endif
  //  geant->Do("mode tpce prin 1 digi 2");   // make tpc_hit in local coordinates
  St_calib_Maker    *calib = new St_calib_Maker("calib","calib"); 
  //  St_evg_Maker      *evgen = new St_evg_Maker("evgen","event/evgen");
  St_fss_Maker   *ftpc_raw = new St_fss_Maker("ftpc_raw","event/raw_data/ftpc");
#ifndef trs
  St_tss_Maker    *tpc_raw = new St_tss_Maker("tpc_raw","event/raw_data/tpc");
  // Set parameters
  //  tpc_raw->adcxyzon();
#else
  StTrsMaker          *trs = new StTrsMaker;
  St_tpcdaq_Maker *tpc_raw = new St_tpcdaq_Maker;
#endif
#ifdef emc
  St_ems_Maker          *emc_raw = new St_ems_Maker("emc_raw","event/raw_data/emc");
  St_emc_Maker         *emc_hits = new St_emc_Maker("emc_hits","event/data/emc/hits");
#endif
#if 1
  St_tcl_Maker         *tpc_hits = new St_tcl_Maker("tpc_hits","event/data/tpc/hits");
  St_srs_Maker         *svt_hits = new St_srs_Maker("svt_hits","event/data/svt/hits");
  St_fcl_Maker         *fcl_hits = new St_fcl_Maker("ftpc_hits","event/data/ftpc/hits");
#ifdef ctf
  St_ctf_Maker         *ctf      = new St_ctf_Maker("ctf","event/data/ctf");
  St_mwc_Maker         *mwc      = new St_mwc_Maker("mwc","event/data/mwc");
  St_trg_Maker         *trg      = new St_trg_Maker("trg","event/data/trg");
#endif
  StRchMaker           *rch      = new StRchMaker("rch","event/raw_data/rch");
  St_tpt_Maker       *tpc_tracks = new St_tpt_Maker("tpc_tracks","event/data/tpc/tracks");
#ifdef ctf
  St_l3t_Maker       *l3Tracks   = new St_l3t_Maker("l3Tracks","event/data/l3/tracks");
#endif
  St_stk_Maker       *stk_tracks = new St_stk_Maker("svt_tracks","event/data/svt/tracks");
  St_fpt_Maker      *ftpc_tracks = new St_fpt_Maker("ftpc_tracks","event/data/ftpc/tracks");
  St_glb_Maker           *global = new St_glb_Maker("global","event/data/global");
  St_dst_Maker        *dst_Maker = new St_dst_Maker("dst","dst");
  //  dst_Maker->Save();
  St_run_summary_Maker  *summary = new St_run_summary_Maker("run_summary","run/dst");
  St_QA_Maker        *qa         = new St_QA_Maker;  
  St_io_Maker *out  = new St_io_Maker("Output","all");
  // Create HTML docs of all Maker's involved
  //   chain->MakeDoc();


  chain->PrintInfo();
  // Init the chain and all its makers
  //  chain->SetDebug(1);
  int iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");

  //  chain->MakeTree("StChainTree","Title");
  //  chain->SetBranches();
  // Prepare TCanvas to show some histograms created by makers
  out->Add(global->GetName(),"global_branch.root");
  if (root_out) {chain->Write();}

  gBenchmark->Start("bfc");
  Int_t i=0;
  for (Int_t i =1; i <= Nevents; i++){
    if (chain->Make(i)) break;

#if 0
    St_DataSet *dst = chain->DataSet("dst");
    if (dst) {
      if (root_out){
	gBenchmark->Start("root i/o");
	root_out->cd();
	chain->FillClone();
	//	dst->SetWrite();// root output
	gBenchmark->Stop("root i/o");
      }
#endif
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
    if (root_out){
      root_out->Write();
      root_out->Close();   
      delete root_out;
      gBenchmark->Print("root i/o");
    }
    gBenchmark->Print("bfc");
  }
  else {if (!b)   b = new TBrowser;}
}
