// $Id: ebye.C,v 1.8 1999/05/21 15:33:58 kathy Exp $
// $Log: ebye.C,v $
// Revision 1.8  1999/05/21 15:33:58  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
// Revision 1.7  1999/02/16 22:35:04  dhammika
// cleaned up a little bit
//
// Revision 1.6  1999/01/27 00:18:15  dhammika
// EbyE PKG works for more than one event in ROOT
//
// Revision 1.51 1999/01/26 18:14:06  dhammika
// Minor updates
// Revision 1.5  1999/01/05 14:11:08  dhammika
// Updated to be in synch with stardev and the latest SCA V2.0 
//
// Revision 1.4  1998/08/26 12:15:15  fisyak
// Remove asu & dsl libraries
//
// Revision 1.3  1998/08/20 12:33:32  fisyak
// Splitted base libraries
//
// Revision 1.2  1998/08/07 19:27:05  dhammika
// event by event chain in root
//
// Revision 1.1  1998/08/05 14:33:40  fisyak
// Add ebye
//=======================================================================
// owner: Jeff Reid 
// what it does: 
//=======================================================================
//
#ifdef   DEBUG
#undef   DEBUG 
#endif
#define  DEBUG  0

Int_t iret;
TBrowser *b = 0;
class StChain;
StChain  *chain=0;
void Load(){
  printf (" Begin loading shared libraries  \n");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("xdf2root");
  gSystem->Load("St_Tables");
  gSystem->Load("St_xdfin_Maker");
  gSystem->Load("St_TLA_Maker");
  gSystem->Load("libmsg");
  gSystem->Load("libtls");
  gSystem->Load("St_params_Maker");
  //gSystem->Load("St_calib_Maker");
  //gSystem->Load("St_dst_Maker");
  gSystem->Load("global");
  gSystem->Load("St_global");
  gSystem->Load("St_run_summary_Maker");
  //gSystem->Load("St_ana_Maker");
  gSystem->Load("ebye");
  gSystem->Load("St_ebye");
  gSystem->Load("St_ebye_Maker");
  printf (" Done loading shared libraries  \n"); 
}

ebye(const Int_t   SetmakePrior       = 0,
     const Int_t   SetmakeEnsembleAve = 0,
     const Int_t   SetdoAnalysis      = 1,
     const Int_t   Nevents            = 10,
     const Int_t   Nskip              = 94,
     const Char_t *fileinp            = 
     "/star/scr2b/dhammika/psc362_02_160evts_h_dst.xdf",
     //const Char_t *fileprior          = "sca_prior_dir.xdf",
     const Char_t *fileprior          = 0,
     //const Char_t *fileensembleave    = "sca_ensemble_dir.xdf",
     const Char_t *fileensembleave    = 0,
     const Char_t *fileout            = "sca_out.xdf",
     //const Char_t *FileOut            = "sca_out.root")
     const Char_t *FileOut            = 0)
{
  // Dynamically link some shared libs
  if (gClassTable->GetID("StChain") < 0) Load();
  St_XDFFile                  *xdf_in  = 0;
  if (fileinp)                 xdf_in  = new St_XDFFile(fileinp,"r");
  St_XDFFile                 *xdf_out  = 0;
  if (fileout)                xdf_out  = new St_XDFFile(fileout,"w");
  St_XDFFile                *xdf_prior = 0;
  if (fileprior)             xdf_prior = new St_XDFFile(fileprior, "w");
  St_XDFFile          *xdf_ensembleave = 0;
  if (fileensembleave) xdf_ensembleave = new St_XDFFile(fileensembleave, "w");
  TFile                     *root_out  = 0; 
  if (FileOut)               root_out  = new TFile(FileOut,"RECREATE");
  // Create the main chain object
  if (chain) delete chain;
  chain = new StChain("ebye");
  //chain->SetDebug(1);
  St_params_Maker       *params = new St_params_Maker("params","params");
  //St_calib_Maker        *calib  = new St_calib_Maker("calib","calib");
  // Set input file
  if (xdf_in) {
    St_xdfin_Maker    *my_xdfin = new St_xdfin_Maker("xdfin");
    chain->SetInputXDFile(xdf_in);
  }
  //  Create the makers to be called by the current chain
  St_TLA_Maker             *dst = new St_TLA_Maker("dst","event/data/global/dst");
  St_run_summary_Maker *summary = new St_run_summary_Maker("run_summary","run/dst");
  St_ebye_Maker        *my_ebye = new St_ebye_Maker("ebye","event/data/ebye/sca");
  // Create HTML docs of all Maker's involved
  //chain->MakeDoc();
  if (DEBUG) printf("====>ebye.C::Begin chain.PrintInfo\n");
  chain->PrintInfo();
  if (DEBUG) printf("====>ebye.C::End chain.PrintInfo\n");
  // Init the chain and all its makers
  if (DEBUG) printf("====>ebye.C::Begin chain.Init\n");
  int iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  if (DEBUG) printf("====>ebye.C::End chain.Init\n");
  // Set SCA specific flags 
  if (SetmakePrior){
    iret = my_ebye->SetmakePrior(kTRUE);
    if (iret){
      printf("===>ebye.C::<<< ERROR >>> Problem with  my_ebye->SetmakePrior *****\n");
      return;
    }
    else
      printf("====>ebye.C::***** my_ebye->SetmakePrior successful *****\n");
  }
  if (SetmakeEnsembleAve){
    iret = my_ebye->SetmakeEnsembleAve(kTRUE);
    if (iret){
      printf("====>ebye.C::<<< ERROR >>> Problem with  my_ebye->SetmakeEnsembleAve *****\n");
      return;
    }
    else
      printf("====>ebye.C::***** my_ebye->SetmakeEnsembleAve successful *****\n");
  }
  if (SetdoAnalysis){
    iret = my_ebye->SetdoAnalysis(kTRUE);
    if (iret){
      printf("====>ebye.C::<<< ERROR >>> Problem with  my_ebye->SetdoAnalysis *****\n");
      return;
    }
    else
      printf("====>ebye.C::***** my_ebye->SetdoAnalysis successful *****\n");
  }
  
  // Skip events?
  for ( Int_t i=0; i<Nskip;i++){
    St_DataSet *set = chain->XDFFile()->NextEventGet();  
    delete set;
  }
  
  gBenchmark->Start("ebye");
  printf("====>ebye.C::Beging Benchmark(ebye) \n"); 
  Int_t ngood=0;
  Int_t i=1;
  for (Int_t i =1; i <= Nevents; i++){
    iret = chain->Make(i);
    if (iret == 2) {
      printf ("====>ebye.C:: <<< ERROR >>> <Chain::Make()>: failed in Event no. %d \n \t  chain->Make(i) = %d \n",i, iret);
      break;
    }
    if (!iret) ngood++;
    //if (i != Nevents) chain->Clear();
    // Write out SCA output
    if (!iret && SetdoAnalysis ){
      if (xdf_out){
	gBenchmark->Start("xdf_out");
	St_DataSet *ebye_out = chain->DataSet("ebye");
	xdf_out->NextEventPut(ebye_out); // xdf output
	gBenchmark->Stop("xdf_out");
      }
      printf ("====>ebye.C:: ========================================= Done with Event no. %d\n\n",i);
    }
    if (i != Nevents) chain->Clear();
  }
  // Set the total #of good events processed.
  my_ebye->SetnEvents(ngood);
      printf ("====>ebye.C:: ================================================== Total good events processed. %d\n\n",ngood);  
  // Write out prior
  if (SetmakePrior && xdf_prior){
    my_ebye->PutPrior();
    St_DataSet *prior = chain->DataSet("calib/ebye/sca_prior_dir");
    xdf_out->NextEventPut(prior); // xdf output
    delete xdf_prior;
  }
  // Write out ensemble average
  if (SetmakeEnsembleAve && xdf_ensembleave){
    my_ebye->PutEnsembleAve();
    St_DataSet *ensembleave = chain->DataSet("calib/ebye/sca_ensemble_dir");
    xdf_out->NextEventPut(ensembleave); // xdf output
    delete xdf_ensembleave;
  }
  if (Nevents > 1) {
    chain->Finish();
    delete xdf_in;
    if (xdf_out){
      delete xdf_out;
      gBenchmark->Print("xdf_out");
    }
    gBenchmark->Print("ebye");
  }
  else {if (!b)   b = new TBrowser;}
}

