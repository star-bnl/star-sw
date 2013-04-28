//////////////////////////////////////////////////////////////////////////
//
// Macro for running chain with different inputs
//
// Owner:  Yuri Fisyak
//
// $Id: bfcMixer_v5.C,v 1.3 2013/04/28 14:41:31 fisyak Exp $
// $Log: bfcMixer_v5.C,v $
// Revision 1.3  2013/04/28 14:41:31  fisyak
// Clean up after retirement of St_tpcdaq_Maker, StRTSClient and StMixer
//
// Revision 1.2  2013/04/24 15:27:28  fisyak
// Retire tpcdaq, StMixer, bug #2580
//
// Revision 1.1  2011/01/18 05:49:18  hmasui
// bfcMixer for different inputs
//
//////////////////////////////////////////////////////////////////////////

//TBrowser *b = 0;
class StChain;
class StBFChain;
StChain  *chain=0;
class StMaker;
StMaker    *treeMk=0;
StBFChain *chain1, *chain2, *chain3;
class StEvent;
StEvent *Event;
class St_geant_Maker;
class StIOMaker;
class StEventDisplayMaker; StEventDisplayMaker *dsMk = 0;
//class StFtpcMixerMaker;
class StEvtHddr;
//_____________________________________________________________________
void Load(){
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("StBFChain");
  //Extra things to load for the acceptance filter
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StAnalysisUtilities");
#if 0
  gSystem->Load("StV0AccMaker.so");
#endif
  if (chain) delete chain;
}
//_____________________________________________________________________
void bfcMixer_v5(const Int_t Nevents=10,
		 const Char_t *file1="/star/institutions/uky/rfatemi/AuAu62/st_physics_5086073_raw_3020008.daq",
		 const Char_t *file2="/star/institutions/uky/gorbunov/evgen.1.nt"
#if 0
	     const Char_t *file2="/star/institutions/uky/rfatemi/AuAu62/simulation_62/starlightrpii_1.fz",
	     const Char_t *file3="/home/starofl/embedding/GSTAR/st_physics_2270008_raw_0030.vertices.dat",
             const Float_t zvertex_low=-175.0,
             const Float_t zvertex_high=175.0,
	     const Char_t *mode="strange",
	     const Char_t *acc_mode="off"
#endif
		 )
{
  // Dynamically link some shared libs
  if (gClassTable->GetID("StBFChain") < 0) Load();

  // Create the main chain object
  chain = new StChain("Embedding");

  StMaker *saveMk = 0;

  // Create chain1 object
  chain1 = new StBFChain("One");
  saveMk = chain1->cd();
  //  chain1->SetFlags("in NoDefault");
  //  chain1->SetFlags("in alltrigger NoDefault");
  //  chain1->SetFlags("in Physics DbV20020226 NoDefault");
  chain1->SetFlags("in Physics DbV20050515 NoDefault");
  chain1->Set_IO_Files(file1);
  chain1->Load();
  chain1->Instantiate();

  saveMk->cd();
  
  // Create chain2 object
  chain2 = new StBFChain("Two");
  saveMk = chain2->cd();
  chain2->SetFlags("ntin DbV20050515 gen_T geomT sim_T ftpcT tpc trs fss -tcl -tpt -PreVtx -tpc_daq");   // 
  chain2->Set_IO_Files(file2);
  chain2->Load();
  chain2->Instantiate();
  St_geant_Maker *geantMk = chain2->GetMaker("geant");
  if (geantMk) geantMk->SetMode(1);   // Mixer mode - do not modify EvtHddr
  
  if (chain2->GetOption("TRS")){
    StTrsMaker *trsMk = (StTrsMaker *) chain2->GetMaker("Trs");
    trsMk->setNormalFactor(1.22);
  }
#if 0
  // Add the acceptance filter maker before TRS  
  if (!strcmp(mode,"strange")){
    if (!strcmp(acc_mode,"on")){
      
      Char_t *extraMaker = "StV0AccMaker";
      if (gClassTable->GetID(extraMaker) < 0) gSystem->Load(extraMaker);
      StMaker *extraMk = (StMaker *)chain1->GetMaker(extraMaker);
      if(extraMk) delete extraMk;
      extraMk = chain->New(extraMaker,"before");
      if (extraMk) {
	Char_t *before = "Trs";
	StMaker *trsmk = chain1->GetMaker(before);
	if (trsmk) chain1->AddBefore(before,extraMk);
	StV0AccCuts *cuts = ((StV0AccMaker *)extraMk)->GetCutsPtr();
	cuts->SetFilter();
	cuts->SetV0MinDecayLen(0.);
	cuts->SetV0DaughterMinImpact(0);
	cuts->SetV0DaughterMinHit(10.);
	cuts->SetXiV0MaxImpact(5);
	cuts->SetXiMinDecayLen(2.);
	cuts->SetXiV0PiMinImpact(0.);
	cuts->SetXiDaughterMinHit(10.);
	cuts->SetKinkMinDecayRad(128.);
	cuts->SetKinkMaxDecayRad(184.);
      }
    }
  }
  // end additional maker code
#endif
  saveMk->cd();
  gSystem->Load("StFtpcMixerMaker");
  StFtpcMixerMaker  *ftpcmixer = new StFtpcMixerMaker("FtpcMixer","daq","fss");
  ftpcmixer->SetInput("Input1","StDAQReader");
  ftpcmixer->SetInput("Input2","Event");

  // Create chain3 object
  chain3 = new StBFChain("Three");
  saveMk = chain3->cd();

  // use Simu NoDefault NoInput onlraw -onlcl and standard chain options
  // then take apart e.g. P2004/B2004 and remove corrections as well as 
  // in, physics, analyis and Event QA from Cdst, tags, SCEbyE
  // also don't use hitfilt
  //  took out svtdEdx, emcDY2 too
  // tested with SL06g
  chain3->SetFlags("STpcMixer imu NoDefault NoInput onlraw -onlcl DbV20050515 ry2005d tpc_daq tpc ftpc event cdst Kalman Tree evout useCDV Xi2 V02 Kink2 CMuDst analysis MiniMcMk");
  // P2005 DbV20050515 useCDV SCEbyE OGridLeak tofDat EST svtdEdx xiSvt pmdRaw Xi2 V02 Kink2 -dstout CMuDst OShortR OSpaceZ2 hitfilt
  //  chain3->SetFlags("NoDefault NoInput onlraw -onlcl P2005 DbV20050515 useCDV tofDat EST svtdEdx xiSvt pmdRaw Xi2 V02 Kink2 -dstout CMuDst");
  //  OShortR OSpaceZ2 hitfilt Stilibs
  //  chain3->SetFlags("NoDefault NoInput onlraw -onlcl DbV20050515 ry2005b Idst,IAna,l0,tpcI,fcf,ftpc,Tree,SvtCL,svtDb,logger,ITTF,Sti,SsdIt,SvtIt,genvtx,geant,evout,tags,bbcSim,tofsim,emcY2,EEfs,GeantOut,big,fzin,MiniMcMk"); 

  //  StRTSClientFCF *fcfMk = (StRTSClientFCF *) chain3->GetMaker("");
  //  fcfMk->SetMode("0x1");


  TString OutputFileName(gSystem->BaseName(file1));
  OutputFileName.ReplaceAll("*","");
  OutputFileName.ReplaceAll(".daq","");
  OutputFileName.Append(".root");
  chain3->Set_IO_Files(0,OutputFileName.Data());
  chain3->Load();
  chain3->Instantiate();
  St_geant_Maker *geantMk = (St_geant_Maker *) chain->GetMaker("geant");
  geantMk->SetActive(kTRUE);
  //  StMaker *ftpccluMk = chain3->GetMaker("ftpc_hits");
  //  ftpccluMk->SetInput("ftpc_raw","FtpcMixer");

  saveMk->cd();
  {
    TDatime t;
    printf ("QAInfo:Run is started at Date/Time%i/%i\n",t.GetDate(),t.GetTime());
  }
  printf ("QAInfo:Run on %s in %s\n",
        gSystem->HostName(),
        gSystem->WorkingDirectory());
  printf ("QAInfo: with %s\n", chain->GetCVS());

  // Init the chain and all its makers
  if (Nevents >= 0) {
    Int_t iInit = chain->Init();
  }
  //geantMk->Do("GSPREAD 0.1 0.1 30.");

geantMk->Do("VSIG 0. 26.");

geantMk->Do("VXYZ -0.13 0.08 0.0");

  // chain->SetDEBUG();
  treeMk = chain->GetMaker("tree");
  TBenchmark evnt;
  Int_t iMake = 0, i = 1, iBad = 0;

  StIOMaker *inpMk = (StIOMaker *)chain1->GetMaker("inputStream");
#if 0
  //comment this out cause I don't have the vertex files
  //FILE *fp = fopen(file3,"r");
  Float_t x = -0.13;
  Float_t y = 0.08;
  Float_t z = 0.0;
  Int_t ncols, eventnumber, mult, skip=0, oldskip = 0, skiptest=0;
  printf("zvertex_low = %f zvertex_high = %f\n",zvertex_low, zvertex_high);

  // vtxMk = (StVertexMaker*) chain3->GetMaker("vertex");
#endif  

  chain->SetAttr(".Privilege",0,"*"                );     //All  makers are NOT priviliged
  chain->SetAttr(".Privilege",1,"StIOInterFace::*" );     //All IO makers are priviliged
  chain->SetAttr(".Privilege",1,"St_geant_Maker::*");     //It is also IO maker


 inpMk->SetActive(kFALSE);
  while(iMake == kStOk) {
     inpMk->Clear();
     chain->Clear();
     int iretDaq = inpMk->Make();
     //if (iretDaq!= kStEOF )  {
 if ( (iretDaq!=kStOk) && (iretDaq!= kStEOF) )  {
        inpMk->Close();
        inpMk->Open();
        iretDaq = inpMk->Make();
     }
     if (iretDaq) break;
     iMake = chain->Make();
  }

 // chain->EventLoop(Nevents);
  gMessMgr->QAInfo() << "Run completed " << endm;
  gSystem->Exec("date");
  fflush(stdout);
}

