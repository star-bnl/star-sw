// $Id: strange.C,v 1.3 1999/01/21 00:53:24 fisyak Exp $
// $Log: strange.C,v $
// Revision 1.3  1999/01/21 00:53:24  fisyak
// Cleanup
//
// Revision 1.2  1999/01/19 23:05:14  genevb
// MakeDoc() removed
//
// Revision 1.1  1998/12/29 10:24:40 genevb
// strangeness dst analysis
//

{

/*------------------------------------- Begin_Html <font color="#800000" size="-1">
The strange.C macro is intended for use by the strangeness group
in analyzing a STAR dst to produce microdst tables.<br>

The comments in this macro file follow the outline of the steps
for writing a macro described in <a href="/STAR/html/comp_l/train/root/RootMacros.html">Understanding Root Macros</a>.
You may want to first examine <a href="/STAR/html/comp_l/root/html/examples/bfc.C.html">bfc.C</a> when learning
about macros.<br>

<b>STEP 1:</b> Here in the macro are a series of system load calls.
Only the relevant libraries need to be loaded. Often, for a given
maker, one will need to load libraries for the definitions of the
maker's tables (St_Tables), the associated PAM (strange), the Root
wrapper for the PAM (St_strange), and the maker (St_smdst_Maker).
</font> End_Html -------------------------------------*/

  gSystem->Load("St_base.so");
  gSystem->Load("StChain.so");
  gSystem->Load("xdf2root.so");
  gSystem->Load("St_Tables.so");
  gSystem->Load("St_xdfin_Maker.so");
  gSystem->Load("St_TLA_Maker.so");
  gSystem->Load("St_ana_Maker.so");
  gSystem->Load("libtls.so");
  gSystem->Load("strange.sl");
  gSystem->Load("St_strange.so");
  gSystem->Load("St_smdst_Maker.so");

/*------------------------------------- Begin_Html <font color="#800000" size="-1">
<b>STEP 2:</b> Next, we  specify the input file we want to process.
In this case, a simple XDF file.
</font> End_Html -------------------------------------*/

  const Char_t *fileinp =
  "/disk1/star/genevb/year1a_90evts_dst.xdf";
//  "/disk1/star/genevb/year2a_46evts_dst.xdf";
  const Int_t Nevents=90;
  St_XDFFile   *xdf_in   = 0;
  if (fileinp)  xdf_in   = new St_XDFFile(fileinp,"r");

// Root output
//  TFile      *root_tree= new TFile("smdst.tree.root","RECREATE");
//  Create the main chain object

/*------------------------------------- Begin_Html <font color="#800000" size="-1">
<b>STEP 3:</b> You may instantly recognize this next bit as the construction of a
<i>chain</i>.  In this macro, the chain is actually of the class End_Html StChainSpy Begin_Html
and is named "dstChain". This macro also differs from <a href="/STAR/html/comp_l/root/html/examples/bfc.C.html">bfc.C</a> in that <b>chain</b> 
is the name of the structure itself, not a pointer to it.
</font> End_Html -------------------------------------*/

  StChainSpy chain("dstChain");

/*------------------------------------- Begin_Html <font color="#800000" size="-1">
<b>STEP 4:</b> Now it's time to construct our <i>makers</i>. Only a few
makers are required for this analysis. A call to the DoHistograms member
function of the smdst maker causes its diagnostic histograms to be
turned on.
</font> End_Html -------------------------------------*/

  if (xdf_in) {
    St_xdfin_Maker xdfin("xdfin");
    chain.SetInputXDFile(xdf_in);
  }
  St_ana_Maker        dst("dst","event/data/global/dst");
  St_smdst_Maker      smdst("smdst","event/data/strange/smdst");
  smdst.MakeDoc();
  smdst.DoHistograms(5);  // Turn on smdst histograms and update every 5 events

/*------------------------------------- Begin_Html <font color="#800000" size="-1">
<b>STEP 5:</b> Next, we place calls to the PrintInfo() member
function of chain, followed by the Init() member function to
initialize all histograms and read in the run header of the file.
</font> End_Html -------------------------------------*/

  chain.PrintInfo();

// Init the main chain and all its makers
  int iInit = chain.Init();
  if (iInit) chain.Fatal(iInit,"on init");

/*------------------------------------- Begin_Html <font color="#800000" size="-1">
<b>STEP 6a:</b> Here we prepare for our event loop. We could skip some
events first if we want. The NextEventGet() member function of
End_Html St_XDFFile Begin_Html allows an event to be read in outside of the chain.
</font> End_Html -------------------------------------*/

// Skip events?
//  St_DataSet *set =chain->XDFFile()->NextEventGet();  
//  delete set;

  gBenchmark->Start("dst");
  Int_t i=1;
  for (Int_t i =1; i <= Nevents; i++){
    if (chain.Make(i)) break;

/*------------------------------------- Begin_Html <font color="#800000" size="-1">
<b>STEP 6b:</b>We could at this point output or histogram the data. But the
smdst maker already provides some histograms.<br>

<b>STEP 6c:</b> The data for each event must be cleared from memory before the
next event is read in, and that is performed with the call to chain.Clear(). 
An <i>if</i> statement here prevents the chain from clearing on the final
event in this particular example, permitting the data from the last event to
persist in memory after the macro finishes.
</font> End_Html -------------------------------------*/
    
    if (i != Nevents) chain.Clear();
    printf ("===========================================\n");
    printf ("=========================================== Done with Event no. %d\n",i);
    printf ("===========================================\n");
  }

/*------------------------------------- Begin_Html <font color="#800000" size="-1">
<b>STEP 7:</b> Lastly, this macro makes a call to chain.Finish().
The input file is essentially closed by deleting its reference
pointer. In this macro, chain.Finish() is called only if
more than one event is analyzed. Otherwise, a browser is called up
to provide simplified access to the data structures, a useful tool
when debugging/testing your code on a single event.
</font> End_Html -------------------------------------*/

  if (Nevents > 1) {
    chain.Finish();
    delete xdf_in;
    gBenchmark->Print("dst");
  }
  else TBrowser b;
}
