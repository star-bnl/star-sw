// $Id: Bfc.C,v 1.1 1999/01/21 00:53:18 fisyak Exp $
// $Log: Bfc.C,v $
// Revision 1.1  1999/01/21 00:53:18  fisyak
// Cleanup
//
// Revision 1.34  1999/01/11 18:29:33  fisyak
// More HTML correction from Gene
//
// Revision 1.33  1999/01/08 21:39:52  fisyak
// Add Gene Van Buren bfc description
//
/*------------------------------------- Begin_Html <font color="#800000" size="-1">
The letters <i>bfc</i> stand for "big full chain," which for STAR
means running all raw data analyses. This macro starts off with a
header listing version numbers, all commented by the "//" characters
at the beginning of the line so that, as in normal C/C++, they are
not interpretted by Root.<br>

The comments in this macro file follow the outline of the steps
for writing a macro described in <a href="/STAR/html/comp_l/train/root/RootMacros.html">Understanding Root Macros</a>.<br>

The code in this macro begins with the definitions of some pointers we
will use later. The macro then defines two functions: Load and bfc.
The bfc function definition allows some input parameters to be specified,
and then determines whether to call Load.
</font> End_Html -------------------------------------*/

TBrowser *b = 0;
class StChain;
StChain  *chain=0;

/*------------------------------------- Begin_Html <font color="#800000" size="-1">
<b>STEP 1:</b> Here in the macro are a series of system load calls. In
this example, they enumerated in a function which may or may not get
called depending on whether the libraries have already been loaded.
This is where Root picks up what it needs to know to run our programs.
These are calls which load the code libraries containing the classes we
will use. A few loads are of particular interest here. <a
href="http://www.rhic.bnl.gov/cgi-bin/star-cgi/cvsweb.pl/StRoot/base/"><i>St_base</i></a> is the
library which contains all the STAR base classes. It will be necessary
to load this library for any STAR analysis. <a
href="http://www.rhic.bnl.gov/cgi-bin/star-cgi/cvsweb.pl/StRoot/StChain/"><i>StChain</i></a> contains the
classes necessary for running a chain and makers. <a
href="http://www.rhic.bnl.gov/cgi-bin/star-cgi/cvsweb.pl/StRoot/xdf2root/"><i>xdf2root</i></a> is
needed to read in xdf files.<br>

<i>St_Tables</i> stores the class definitions of all the tables
in which data, parameters, calibration, run/event header information,
etc. are stored. This can be the cause of some confusion. Let me be
specific here: all tables are defined as derived classes of a base class
called End_Html St_Table. Begin_Html While the definitions of the derived classes are in
the <i>St_Tables</i> library, the definition of the base class
End_Html St_Table Begin_Html itself is in the <a href="http://www.rhic.bnl.gov/cgi-bin/star-cgi/cvsweb.pl/StRoot/base/"><i>St_base</i></a> library.<br>

One can see many individual libraries loaded for each analysis module
among the load calls as well.
</font> End_Html -------------------------------------*/

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
    gSystem->Load("tpc");
    gSystem->Load("St_tpc");
    gSystem->Load("St_tss_Maker");
    gSystem->Load("St_tcl_Maker");
    gSystem->Load("St_tpt_Maker");
    gSystem->Load("ftpc");
    gSystem->Load("St_ftpc");
    gSystem->Load("St_fss_Maker");
    gSystem->Load("St_fcl_Maker");
    gSystem->Load("St_fpt_Maker");
    gSystem->Load("emc");
    gSystem->Load("St_emc");
    gSystem->Load("St_ems_Maker");
    gSystem->Load("St_emc_Maker");
    gSystem->Load("ctf");
    gSystem->Load("St_ctf");
    gSystem->Load("St_ctf_Maker");
    gSystem->Load("svt");
    gSystem->Load("St_svt");
    gSystem->Load("St_srs_Maker");
    gSystem->Load("St_stk_Maker");
    gSystem->Load("strange");
    gSystem->Load("St_strange");
    gSystem->Load("global");
    gSystem->Load("St_global");
    gSystem->Load("St_dst_Maker");
    gSystem->Load("St_run_summary_Maker");
}

/*------------------------------------- Begin_Html <font color="#800000" size="-1">
<b>STEP 2:</b> Next, we must specify input files we want to process.
The actual file names are specified in the bfc function call. Then,
any input/output files are given a reference pointer and a class.
In particular, xdf files need the End_Html St_XDFFile Begin_Html class loaded in the
<a href="http://www.rhic.bnl.gov/cgi-bin/star-cgi/cvsweb.pl/StRoot/xdf2root/"><i>xdf2root</i></a> library mentioned above, while Root files need
simply the End_Html TFile Begin_Html base class.
</font> End_Html -------------------------------------*/

Bfc(const Int_t   Nevents=1,
    const Char_t *FileInput = "/afs/rhic/star/data/samples/hijet-g2t.root")
{
  // Dynamically link some shared libs
  if (gClassTable->GetID("StChain") < 0) Load();
/*------------------------------------- Begin_Html <font color="#800000" size="-1">
<b>STEP 3:</b> You may instantly recognize this next bit as the construction of a
<i>chain</i>.  We first check to see if an old chain exists and delete it if it
does. The new chain is given the name "bfc".<br>

One could easily substitute the End_Html StChainSpy Begin_Html class for the
End_Html StChain Begin_Html class. End_Html StChainSpy Begin_Html allows all running chains to
be monitered across Root sessions and across the network.
</font> End_Html -------------------------------------*/

/*------------------------------------- Begin_Html <font color="#800000" size="-1">
<b>STEP 4:</b> Next it's time to construct our <i>makers</i>. Each maker
puts its name on a list to which the chain has access. This allows the
chain to call upon the makers when it needs them. Each maker constructor
requires a name label, and a directory path for where its data set will reside
in the overall data structure. Knowing the latter would probably require
some basic knowledge about the analysis module of interest. You can generally
get by with copying it from a macro like this one. Note that xdf input
files require a maker to read in the data file.
</font> End_Html -------------------------------------*/

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
  St_dst_Maker        *dst_Maker = new St_dst_Maker("dst","dst");
  //  St_dst_Maker        *dst_Maker = new St_dst_Maker("dst","event/data/global/dst");
  dst_Maker->Save();
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

/*------------------------------------- Begin_Html <font color="#800000" size="-1">
<b>STEP 5:</b> The PrintInfo() member function of chain just prints version
numbers and any other log-worthy information about the makers in the chain.
The Init() member function goes down the list of makers and calls any
initiallization routines each maker may have. If any returns a non-zero
value (failure), then Init() returns a non-zero value as well.
</font> End_Html -------------------------------------*/

  chain->PrintInfo();
  // Init the chain and all its makers
  int iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");

/*------------------------------------- Begin_Html <font color="#800000" size="-1">
In this macro, the xdfin maker for the xdf file will read in the
first record of the file during Init, which contains run header
information including paramaters. These parameters are
subsequently written to the output files here.<br>

You will see many lines in macro files which call End_HTML gBenchmark->Start() Begin_HTML
and End_Html gBenchmark->Stop(). Begin_Html These allow processing to be timed, and their use is
clear enough not to warrant further discussion here.
</font> End_Html -------------------------------------*/

  //  chain->MakeTree("StChainTree","Title");
  //  chain->SetBranches();
  // Prepare TCanvas to show some histograms created by makers

/*------------------------------------- Begin_Html <font color="#800000" size="-1">
<b>STEP 6a:</b> Now it's time to loop over the events. A simple <i>for</i>
loop suffices. The key ingredient in the event loop is the call to
chain->Make(). Again, chain goes through the list of makers, calling their
individual Make() member functions and returning a zero if all goes OK.
It is in these makers' Make() routines that analysis code for each event
is processed. Any new tables these routines create and fill are placed in
their maker's branch of the overall data tree structure.
</font> End_Html -------------------------------------*/

  gBenchmark->Start("bfc");
  Int_t i=0;
  for (Int_t i =1; i <= Nevents; i++){
    //    root_file->cd();
    //    chain->GetEvent(i-1);
    if (chain->Make(i)) break;
    chain->Maker("geant")->DataSet()->ls("*");

/*------------------------------------- Begin_Html <font color="#800000" size="-1">
<b>STEP 6b:</b> The data is now available for any further processing, such
as histogramming or writing to an output file. Below, the entire "dst"
branch of the data structure is output to disk:
</font> End_Html -------------------------------------*/
#if 0
    if (root_file){
      gBenchmark->Start("root i/o");
      root_file->cd();
      cout << "============================ bytes written =" << chain->FillTree() << endl;
       chain->Tree()->ls();
      gBenchmark->Stop("root i/o");
    }
#endif
/*------------------------------------- Begin_Html <font color="#800000" size="-1">
<b>STEP 6c:</b> The data for each event must be cleared from memory before the
next event is read in, and that is performed with the call to chain->Clear(). 
An <i>if</i> statement here prevents the chain from clearing on the final
event in this particular example, permitting the data from the last event to
persist in memory after the macro finishes.
</font> End_Html -------------------------------------*/

    if (i != Nevents) chain->Clear();
    printf ("===========================================\n");
    printf ("=========================================== Done with Event no. %d\n",i);
    printf ("===========================================\n");
  }

/*------------------------------------- Begin_Html <font color="#800000" size="-1">
<b>STEP 7:</b> After the event loop, the macro then makes a call to
chain->Finish() to perform post-event loop routines in the makers.
You should note that the file pointers are all deleted, essentially
closing the files. In this macro, chain->Finish() is called only if
more than one event is analyzed. Otherwise, a browser is called up
to provide simplified access to the data structures, a useful tool
when debugging/testing your code on a single event.
</font> End_Html -------------------------------------*/

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
