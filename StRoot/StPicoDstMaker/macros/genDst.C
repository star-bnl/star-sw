/////////////////////////////////////////////////////////////////////////////
// $Id: genDst.C,v 1.2 2019/08/06 21:44:02 gnigmat Exp $
// Author: G. Van Buren (BNL)
//
// Description:
// Process a MuDst for...
// ...creating a PicoDst
// ...re-running vertex-finding to re-create MuDsts
//
// Options are space-separated or comma-separated,
// and case-insensitive. They can be attributed
// whose values are provided after a ':'.
//
// Example options for creating PicoDsts:
// picoDst
// mtdMatch
// y2017a
//
// Example lists of options:
// "picoDst"
// "picoDst,mtdMatch,y2014a"
//
// Example options for vertex-finding:
// beamline, beamline1D, beamline3D (otherwise no beamline)
// useBTOFmatchOnly
// VFstore:100
//
// Example lists of options:
// "VFPPVnoCTB,beamline1D,VFstore:100"
// "VFPPVnoCTB,beamline3D"
//
/////////////////////////////////////////////////////////////////////////////

const char* defaultOptions = "picoDst,PicoVtxMode:PicoVtxVpdOrDefault,TpcVpdVzDiffCut:6,PicoCovMtxMode:PicoCovMtxWrite";
char *defaultInFile = "/star/rcf/test/dev/daq_sl302.ittf/Fri/year_2011/AuAu200_production/st_physics_12130084_raw_5020002.MuDst.root";
char *defaultOutFile = "oTestGenPicoDst.root";

//_________________
void genDst(unsigned int Last,
            const char* options,
            char* infile,
            char* outfile=0);

//_________________
void genDst(unsigned int First,
            unsigned int Last,
            const char* options,
            char* infile,
            char* outfile=0);

//_________________
void loadLibs() {
  gROOT->Macro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcUtil");
}

//_________________
void loadLibsVF() {
  gSystem->Load("libMinuit");
  gSystem->Load("Sti");
  gSystem->Load("StBTofUtil");
  gSystem->Load("StGenericVertexMaker");
}

//_________________
void loadLibsPico() {

  // EMC libs
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StPreEclMaker");
  gSystem->Load("StEpcMaker");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StTriggerUtilities");

  // FMS
  gSystem->Load("libStFmsUtil");
  gSystem->Load("libStFmsDbMaker");

  // PicoDstMaker and PicoEvent
  gSystem->Load("libStPicoEvent");
  gSystem->Load("libStPicoDstMaker");
}

//_________________
void loadLibsAgML(const char* tag=0) {
  // load support libraries.  util will fail to load for agml 1.0, but you can ignore the error
  gSystem->Load("libStarAgmlUtil");
  gSystem->Load("libStarAgmlLib");

  // load geometry modules and master steering codes...
  gSystem->Load("libGeometry");
  gSystem->Load("libStarGeometry");

  // Let agml know we want the ROOT geometry
  AgModule::SetStacker( new StarTGeoStacker );

  // now pass the geometry "tag" and build.  If the class StarGeometry exists, we have
  // AgML 2.0 and can run using the new steering.  Otherwise, old steering codes...
  if (tag) {
    if ( TClass::GetClass("StarGeometry") ) {      StarGeometry::Construct( tag ); }
    else                                    {      ( new Geometry() )->ConstructGeometry(tag); }
  } else {
    gMessMgr->Warning() << "No geometry tag passed! (e.g. y2017a)" << endm;
  }
}

//_________________
void loadLibsMtd() {
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StarMagField");
  gSystem->Load("StMagF");
  gSystem->Load("StMtdUtil");
  gSystem->Load("StMtdMatchMaker");
  gSystem->Load("StMtdCalibMaker");
}

//_________________
void genDst(unsigned int First,
            unsigned int Last,
            const char* options,
            char* infile,
            char* outfile) {
  loadLibs();

  StChain fullChain("genDst");

  StMuDstMaker muDstMaker(0, 0, "", infile, "st:MuDst.root", 1e9); // set up maker in read mode
  //                      0, 0                        this means read mode
  //                           dir                    read all files in this directory
  //                               file               bla.lis real all file in this list, if (file!="") dir is ignored
  //                                    filter        apply filter to filenames, multiple filters are separated by ':'
  //                                          10      maximum number of file to read


  TChain& muDstChain = *muDstMaker.chain();
  unsigned int nEntries      = muDstChain.GetEntries();
  unsigned int LastToRead = Last > 0 ? min(Last, nEntries) : nEntries;
  gMessMgr->Info() << nEntries << " events in chain, " << LastToRead-First+1 << " will be read." << endm;

  St_db_Maker* st_db_maker = new St_db_Maker("db", "StarDb", "MySQL:StarDb", "$STAR/StarDb");

  // Initialize some values and pointers
  StMaker* processMaker = 0;
  TFile* outFile = 0;
  TTree* muDstTreeOut = 0;
  TObject* obj = 0;
  int tk = 0;

  // Basic decisions based on options
  TString Options = options;
  Options.ToLower();
  TString optDelim = " ,";
  TObjArray* optionTokens = Options.Tokenize(optDelim);
  optionTokens->SetOwner(kTRUE);

  if (obj = optionTokens->FindObject("picodst")) {

    optionTokens->Remove(obj);
    optionTokens->Compress();

    loadLibsPico();

    // Specify active branches but first disable all branches
    muDstMaker.SetStatus("*", 0);
    muDstMaker.SetStatus("MuEvent", 1);
    muDstMaker.SetStatus("PrimaryVertices", 1);
    muDstMaker.SetStatus("PrimaryTracks", 1);
    muDstMaker.SetStatus("GlobalTracks", 1);
    muDstMaker.SetStatus("CovGlobTrack", 1);
    muDstMaker.SetStatus("BTof*", 1);
    muDstMaker.SetStatus("Emc*", 1);
    muDstMaker.SetStatus("MTD*", 1);
    muDstMaker.SetStatus("ETof*", 1);
    muDstMaker.SetStatus("Epd*", 1);
    muDstMaker.SetStatus("Fms*", 1);

    // EMCs
    StEEmcDbMaker* eemcDb = new StEEmcDbMaker;
    StEmcADCtoEMaker* adc2e = new StEmcADCtoEMaker();
    adc2e->saveAllStEvent(true);
    StPreEclMaker* pre_ecl = new StPreEclMaker();
    StEpcMaker* epc = new StEpcMaker();

    // FMS
    StFmsDbMaker* fmsdb = new StFmsDbMaker("fmsDb");

#ifdef 1
    // Trigger simulator
    StTriggerSimuMaker* trigSimu = new StTriggerSimuMaker;
    trigSimu->setMC(false);
    trigSimu->useBemc();
    trigSimu->useEemc();
    trigSimu->useOfflineDB();
    trigSimu->bemc->setConfig(StBemcTriggerSimu::kOffline);
#endif

    if (obj = optionTokens->FindObject("mtdmatch")) {

      optionTokens->Remove(obj);
      optionTokens->Compress();

      const char* tag = 0;
      for (tk=0; tk < optionTokens->GetEntries(); tk++) {
        TString& tok = ((TObjString*) (optionTokens->At(tk)))->String();
        if (tok.BeginsWith("y20")){
          tag = tok.Data();
          optionTokens->RemoveAt(tk);
          optionTokens->Compress();
          break;
        }  
      }
      loadLibsAgML(tag);
      loadLibsMtd();

      StMagFMaker* magfMk = new StMagFMaker;
      StMtdMatchMaker* mtdMatchMaker = new StMtdMatchMaker();
      StMtdCalibMaker* mtdCalibMaker = new StMtdCalibMaker("mtdcalib");

    }

    processMaker = (StMaker*) (new StPicoDstMaker(StPicoDstMaker::IoWrite, infile, "picoDst"));

  } else if (Options.Contains("vf")) {

    loadLibsVF();

    // Specify inactive branches but first enable all branches
    muDstMaker.SetStatus("*",1);
    muDstMaker.SetStatus("PrimaryTracks",0);
    muDstMaker.SetStatus("PrimaryVertices",0);

    // Create new branch
    TClonesArray* verticesRefitted = new TClonesArray("StMuPrimaryVertex", 1000);

    // Specify output
    if (outfile) {
      outFile = new TFile(outfile, "RECREATE");
    } else {
      // Use the same filename for output as was given by input
      TString fileStr = infile;
      Ssiz_t dir = fileStr.Last('/');
      if (dir<0) {
        gMessMgr->Error() << "No specification for output when input is in local directory!" << endm;
        return;
      }
      fileStr.Remove(0,dir+1);
      outFile = new TFile(fileStr.Data(), "RECREATE");
    }
    muDstTreeOut = muDstChain.CloneTree(0);
    muDstTreeOut->Branch("PrimaryVertices", &verticesRefitted, 65536, 99);

    processMaker = (StMaker*) (new StGenericVertexMaker());
    processMaker->ToWhiteConst("vtxArray",verticesRefitted);
    processMaker->SetAttr("useMuDst",1);

  } else {

    gMessMgr->Info() << "No processing specified - just reading a MuDst?" << endm;
    // User code may be inserted here

  }

  // Set additional options as maker attributes
  if (processMaker) {
    for (tk=0; tk < optionTokens->GetEntries(); tk++) {
      TString& tok = ((TObjString*) (optionTokens->At(tk)))->String();
      Ssiz_t delim = tok.First(':');
      if (delim < 0) {
        processMaker->SetAttr(tok.Data(),1);
      } else {
        TString key(tok(0,delim));
        TString& val = tok.Remove(0,delim+1);
        if (val.IsDigit()) { processMaker->SetAttr(key.Data(),val.Atoi()); }
        else if (val.IsFloat()) { processMaker->SetAttr(key.Data(),val.Atof()); }
        else { processMaker->SetAttr(key.Data(),val.Data()); }
      }
    }
    processMaker->PrintAttr();
  }

  {
    TDatime t;
    gMessMgr->QAInfo() << Form("Run is started at Date/Time %i/%i",t.GetDate(),t.GetTime()) << endm;
  }
  gMessMgr->QAInfo() << Form("Run on %s in %s",gSystem->HostName(),gSystem->WorkingDirectory()) << endm;
  gMessMgr->QAInfo() << Form("with %s", fullChain.GetCVS()) << endm;


  // Main loop over events
  int iInit = fullChain.Init();
  if (iInit >= kStEOF) {fullChain.FatalErr(iInit,"on init"); return;}
  if (Last == 0) return;
  int eventCount = 0;
  // Skip, if any
  if (First > 1) fullChain.Skip(First - 1);
  for (unsigned int iEvent = First; iEvent <= LastToRead; iEvent++)
  {
     int iMake = fullChain.Make();
     if (iMake) {fullChain.FatalErr(iMake,"on make"); return;}

     if (muDstTreeOut) muDstTreeOut->Fill();

     int iClear = fullChain.Clear();
     if (iClear) {fullChain.FatalErr(iClear,"on clear"); return;}
     eventCount++;
  }
  fullChain.Finish();
  gMessMgr->QAInfo() << "NumberOfEvents= " << eventCount << endm;
  gMessMgr->QAInfo() << "Run completed " << endm;

  if (outFile) {
    outFile->Write();
    outFile->Close();
    delete outFile;
  }

  delete st_db_maker;
  delete processMaker;
  delete optionTokens;
}

//_________________
void genDst(unsigned int Last,
            const char* options,
            char* infile,
            char* outfile) {
  genDst(1,Last,options,infile,outfile);
}

/////////////////////////////////////////////////////////////////////////////
//
// $Log: genDst.C,v $
// Revision 1.2  2019/08/06 21:44:02  gnigmat
// Update of the files for MuDst->picoDst conversion:
// - The FMS information has been added to genDst.C
// - Active the most important branches via SetStatus("BranchName*",1) method by default in genDst.C
// - producePicoDst.xml is modified according to the recent STAR scheduler updates
//
// Revision 1.1  2018/09/17 05:02:32  gnigmat
// Macroses for manual picoDst generation have been added.
//
// Revision 1.2  2017/12/15 18:36:53  genevb
// Remove explicit function of StPicoDstMaker...params should be passed by attribute
//
// Revision 1.1  2017/12/05 16:47:58  genevb
// Introduce genDst.C for creating new Dsts from MuDsts
//
//
/////////////////////////////////////////////////////////////////////////////
