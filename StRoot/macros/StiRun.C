///\file Run.C)e
///\author The ITTF Group
///\date   Changing all the times...
///Macros used to run the ITTF tracker.
///<p>
///See documentation of the Run function for an explanation of the 
///parameters.

//const char * path= "/star/data13/reco/ppMinBias/FullField/P02gf/2002/019/",
//const char * path ="/star/data22/ITTF/EvalData/Event/ppMinBias/",
//const char * file="st_physics_3019045_raw_0031.event.root",
//const char * path= "/star/data06/ITTF/EvalData/MCFiles/auau200",
//const char * path= "/data/r20b/ittf/auau200/hijing/b0_20/standard/year2001/",


void help();
void loadLibrairies();

///Prints a short help message on the command line.
void help()
{
  cout << "Usage: Run.C(firstEvtIndex,nevents,\"-\",\"some_directory/some_dst_file.xdf\")" << endl;
  cout << "       Run.C(firstEvtIndex,nevents,\"-\",\"some_directory/some_dst_file.root\")" << endl;
  cout << "       Run.C(firstEvtIndex,nevents,\"some_directory\",\"*.dst.root\")" << endl;	
}

///Loads all the shared librairies required to execute the tracker. 
///\param doProfile boolean flag indicating whether code profiling should be performed while running the tracker.
void loadLibrairies(bool doProfile)
{	
  char * list[]={"St_base","StChain","StUtilities", "St_Tables", "StarClassLibrary",
		 "libsim_Tables","libglobal_Tables","geometry","St_g2t","St_geant_Maker","libGui",
		 "StIOMaker","StTreeMaker", "St_db_Maker","StDbLib","StDbBroker",
		 "StSvtDbMaker","StDbUtilities", "StTpcDb","StEvent","StEventMaker","StEmcUtil", 
		 "StMcEvent","StMcEventMaker","StAssociationMaker","StDaqLib","StDAQMaker",
		 "StDetectorDbMaker", "StSvtClassLibrary","StSvtDaqMaker",
		 "StSvtSimulationMaker","StSvtCalibMaker", "StSvtSeqAdjMaker",
		 "StSvtClusterMaker","Sti","StiGui", "StiEvaluator",
		 "StiPixel",
		 "StiMaker",
		 "StFtpcClusterMaker",
		 "StiTpc","StiSvt","StiEmc","StiFtpc","StMiniMcEvent","StMiniMcMaker","last"};

  int i=0;  
  cout <<"Run.C::loadLibrairies() - INFO - Started"<<endl;
  while (list[i]!="last")
    {
      cout << "          Loading module:"<<list[i];
      gSystem->Load(list[i++]);
      cout << "\t\t- Done." << endl;
    }
  if(doProfile)
    {
      cout <<"/Jprof";
      gSystem->Setenv("JPROF_FLAGS", "JP_START JP_PERIOD=0.001");

    }
  cout <<"Run.C::loadLibrairies() - INFO - Done"<<endl;
}

void RunMany(int firstEvent = 0,
	     int nEvents    = 100,
	     const char * filePrefix = "rcf",
	     const char * path= "/star/data06/ITTF/EvalData/MCFiles/auau200",
	     const char * file="rcf0183_12_300evts.geant.root",
	     const char * outfile ="",
	     bool useGui=false,
	     bool useMcAsRec=false,
	     bool doPlots=true,
	     bool doSimulation=true,
	     bool doAssociation=true,
	     bool doMiniMcEvent=false,
	     bool doDst=false,
	     bool doStEventOutput=true,
	     bool doStEventInput=true,
	     bool useTpc=true,
	     bool activeTpc=true,
	     bool useSvt=true,
	     bool activeSvt=true,
	     bool useEmc=false,
	     bool activeEmc=false,
	     bool useFtpc=false,
	     bool activeFtpc=false,
	     bool usePixel=false,
	     bool activePixel=false,
	     bool useResidualCalculator=false,
	     bool doProfile=false	 )
{
  Run(firstEvent,
      nEvents,
      filePrefix,
      path,
      file,
      outfile,
      useGui, 
      useMcAsRec,
      doPlots,
      doSimulation,
      doAssociation,
      doMiniMcEvent,
      doDst,
      doStEventOutput,
      doStEventInput,
      useTpc,activeTpc,
      useSvt,activeSvt,
      useEmc,activeEmc,
      useFtpc,activeFtpc,
      usePixel,activePixel,
      useResidualCalculator,
      doProfile);
}

void RunResiduals(int firstEvent = 0, 
		  int nEvents    = 100,
		  const char * filePrefix = "rcf",
		  const char * path= "/data/r23b/star/hijingAuau/200GeV/b0_20/standard/2001/Unknown/",
		  const char * file="rcf0183_02_300evts.geant.root",
		  const char * outfile="",
		  bool useGui=false,
		  bool useMcAsRec=false,
		  bool doPlots=true,
		  bool doSimulation=true,
		  bool doAssociation=true,
		  bool doMiniMcEvent=true,
		  bool doDst=false,
		  bool doStEventOutput=false,
		  bool doStEventInput=true,
		  bool useTpc=true,
		  bool activeTpc=true,
		  bool useSvt=true,
		  bool activeSvt=false,
		  bool useEmc=false,
		  bool activeEmc=false,
		  bool useFtpc=false,
		  bool activeFtpc=false,
		  bool usePixel=false,
		  bool activePixel=false,
		  bool useResidualCalculator=true,
		  bool doProfile=false	 )
{
  Run(firstEvent,
      nEvents,
      filePrefix,
      path,
      file,
      useGui, 
      useMcAsRec,
      doPlots,
      doSimulation,
      doAssociation,
      doMiniMcEvent,
      doDst,
      doStEventOutput,
      doStEventInput,
      useTpc,activeTpc,
      useSvt,activeSvt,
      useEmc,activeEmc,
      useFtpc,activeFtpc,
      usePixel,activePixel,
      useResidualCalculator,
      doProfile);
}

void RunGui(int firstEvent = 0,
	    int nEvents    = 1,
	    const char * filePrefix = "st_physics_",
	    const char* path= "/star/data06/ITTF/EvalData/Event/ppMinBias/",
	    const char* file ="st_physics_3019045_raw_0031.event.root",
	    const char* outfile="",
	    //const char * path= "/data/r23b/star/hijingAuau/200GeV/b0_20/standard/2001/Unknown/",
	    //const char * file="rcf0183_02_300evts.geant.root",
	    bool useGui=true,
	    bool useMcAsRec=false,
	    bool doPlots=true,
	    bool doSimulation=false,
	    bool doAssociation=false,
	    bool doMiniMcEvent=false,
	    bool doDst=false,
	    bool doStEventOutput=false,
	    bool doStEventInput=true,
	    bool useTpc=true,
	    bool activeTpc=true,
	    bool useSvt=true,
	    bool activeSvt=true,
	    bool useEmc=false,
	    bool activeEmc=false,
	    bool useFtpc=false,
	    bool activeFtpc=false,
	    bool usePixel=false,
	    bool activePixel=false,
	    bool useResidualCalculator=false,
	    bool doProfile=false
	    )
{
    Run(firstEvent,
	nEvents,
	filePrefix,
	path,
	file,
	outfile,
	useGui, 
	useMcAsRec,
	doPlots,
	doSimulation,
	doAssociation,
	doMiniMcEvent,
	doDst,
	doStEventOutput,
	doStEventInput,
	useTpc,activeTpc,
	useSvt,activeSvt,
	useEmc,activeEmc,
	useFtpc,activeFtpc,
	usePixel,activePixel,
	useResidualCalculator,
	doProfile);
} 

// ---------------------------------------------------------------------
// ISOLATED 2 methods for production
void StiRun(int nEvents,
	 int IuseSvt,
	 int IactiveSvt,
	 const char *ifile){    
  Run(0,nEvents,IuseSvt,IactiveSvt,ifile);
}


void StiRun(int firstEvent,
	 int nEvents,
	 int IuseSvt,
	 int IactiveSvt,
	 const char *ifile)
{
  TString *tmpfile = new TString(ifile);

  bool useSvt    = (IuseSvt==1);
  bool activeSvt = (IactiveSvt==1);
  bool Fdata     = (tmpfile->Index("st_physics") != -1);
  bool Fevent    = (tmpfile->Index("event.root")!=-1);

  if (Fevent)  cout << "... event.root used as input " << endl;
  else         cout << "... dst or geant used as input (??) " << endl;

  if( Fdata ){
    cout << "Run.C =====> Real data file " << endl;
    Run(firstEvent,nEvents,"","-",ifile,"",
	false,false,false,false,false,false,false,true,Fevent,
	true,true,useSvt,activeSvt);

  } else {
    cout << "Run.C =====> Simulation is being processed" << endl;
    Run(firstEvent,nEvents,"","-",ifile,"",
	false,false,false,true,true,true,false,true,Fevent,
	true,true,useSvt,activeSvt);

  }
}
// ---------------------------------------------------------------------



void Run(int firstEvent,
	 int nEvents,
	 const char * filePrefix,
	 const char * path,
	 const char * file,
	 const char * outfile="",
	     bool useGui=false,
	     bool useMcAsRec=false,
	     bool doPlots=true,
	 bool doSimulation=false,       // <--
	     bool doAssociation=false,  // <--
	     bool doMiniMcEvent=false,  // <--
	     bool doDst=false,          // <--
	     bool doStEventOutput=true,
	     bool doStEventInput=true,
	     bool useTpc=true,
	     bool activeTpc=true,
	     bool useSvt=true,
	     bool activeSvt=true,
	     bool useEmc=false,
	     bool activeEmc=false,
	     bool useFtpc=false,
	     bool activeFtpc=false,
	     bool usePixel=false,
	     bool activePixel=false,
	     bool useResidualCalculator=false,
	     bool doProfile=false)
{
  const char *fileList[]={0,0};
  if (strncmp(path,"GC",2)==0) 
    fileList=0;
  else if (path[0]=='-') 
    fileList[0]=file;
  else if (!file[0]) 
    fileList[0]=path;
  else 
    fileList[0] = gSystem->ConcatFileName(path,file);

  Run(firstEvent,
      nEvents,
      filePrefix,
      fileList,
      outfile,
      useGui, 
      useMcAsRec,
      doPlots,
      doSimulation,
      doAssociation,
      doMiniMcEvent,
      doDst,
      doStEventOutput,
      doStEventInput,
      useTpc,activeTpc,
      useSvt,activeSvt,
      useEmc,activeEmc,
      useFtpc,activeFtpc,
      usePixel,activePixel,
      useResidualCalculator,
      doProfile);
}

///Run the tracker with the given set of parameter values.
///\param firstEvent index of the first event to process. Must be greater or equal to zero.
///\param nEvents number of events to process. Must be greater than one.
///\param filePrefix prefix used to save output files.
///\param fileList list of data files to process.
///\param useGui boolean flag indicating whether the tracker should be used with the graphical user interface.
///\param useMcAsRec boolean flag indicating whether monte carlo hits should be used as reconstructed hits
///\param doPlots  boolean flag indicating whether StiMaker specific histograms should be created.
///\param doSimulation boolean flag indicating whether simulation MC loader should be used.
///\param doAssociation boolean flag indicating whether the association maker should be called.
///\param doMiniMcEvent boolean flag indicating whether MiniMcEvent output should be created.
///\param doDst boolean flag indicating whether DST  output should be created.
///\param doStEventOutput boolean flag indicating whether StEvent  output should be created.
///\param doStEventInput boolean flag indicating whether input proceeds from StEvent.
///\param useTpc boolean flag indicating whether the TPC geometry will be built and used for ELOSS/MCS calculations.
///\param activeTpc boolean flag indicating whether the TPC hits will be used in track reconstruction.
///\param useSvt boolean flag indicating whether the SVT geometry will be built and used for ELOSS/MCS calculations.
///\param activeSvt boolean flag indicating whether the SVT hits will be used in track reconstruction.
///\param useEmc boolean flag indicating whether the EMC geometry will be built and used for ELOSS/MCS calculations.
///\param activeEmc boolean flag indicating whether the EMC hits will be used in track reconstruction.
///\param useFtpc boolean flag indicating whether the FTPC geometry will be built and used for ELOSS/MCS calculations.
///\param activeFtpc boolean flag indicating whether the FTPC hits will be used in track reconstruction.
///\param useResidualCalculator boolean flag indicating whether to perform residual calculations
///\param doProfile boolean flag indicating whether to perform code profiling 
void Run(Int_t firstEvent,
	 Int_t nEvents, 
	 const char *  filePrefix,
	 const char ** fileList,
	 const char * outfile,
	 bool useGui,
	 bool useMcAsRec,
	 bool doPlots,
	 bool doSimulation,
	 bool doAssociation,
	 bool doMiniMcEvent,
	 bool doDst,
	 bool doStEventOutput,
	 bool doStEventInput,
	 bool useTpc,
	 bool activeTpc,
	 bool useSvt,
	 bool activeSvt,
	 bool useEmc,
	 bool activeEmc,
	 bool useFtpc,
	 bool activeFtpc,
	 bool usePixel,
	 bool activePixel,
	 bool useResidualCalculator,
	 bool doProfile)
{
  loadLibrairies(doProfile);
  MiniChain * miniChain = new MiniChain();
  StiMakerParameters* pars = miniChain->getParameters();
  pars->useGui          = useGui;
  pars->useMcAsRec      = useMcAsRec;
  pars->doSimulation    = doSimulation;
  pars->doAssociation   = doAssociation;
  pars->doMiniMcEvent   = doMiniMcEvent;
  pars->doDst           = doDst;
  pars->doStEventOutput = doStEventOutput;
  pars->doStEventInput  = doStEventInput;  
  pars->doPlots         = doPlots; 
  pars->useTpc          = useTpc;
  pars->activeTpc       = activeTpc;
  pars->useSvt          = useSvt;
  pars->activeSvt       = activeSvt;
  pars->useEmc          = useEmc;
  pars->activeEmc       = activeEmc;
  pars->useFtpc         = useFtpc;  
  pars->activeFtpc      = activeFtpc;  
  pars->usePixel        = usePixel;
  pars->activePixel     = activePixel;
  pars->useResidualCalculator = useResidualCalculator;


  if(outfile=="") outfile = fileList[0];
  miniChain->run(firstEvent,nEvents,filePrefix,fileList,outfile);
}
