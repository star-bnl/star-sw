
void help();
void loadLibrairies();

void help()
{
  cout << "Usage: Run.C(firstEvtIndex,nevents,\"-\",\"some_directory/some_dst_file.xdf\")" << endl;
  cout << "       Run.C(firstEvtIndex,nevents,\"-\",\"some_directory/some_dst_file.root\")" << endl;
  cout << "       Run.C(firstEvtIndex,nevents,\"some_directory\",\"*.dst.root\")" << endl;	
}

void loadLibrairies(bool doProfile)
{	
	//  char * list[]={"St_base","StChain","StUtilities","StBFChain", "St_Tables","libgen_Tables",
	// "libsim_Tables","libglobal_Tables","geometry","St_g2t","St_geant_Maker",
  char * list[]={"St_base","StChain","StUtilities", "St_Tables", "StarClassLibrary",
		 "libsim_Tables","libglobal_Tables","geometry","St_g2t","St_geant_Maker","libGui",
		 "StIOMaker","StTreeMaker", "St_db_Maker","StDbLib","StDbBroker",
		 "StSvtDbMaker","StDbUtilities", "StTpcDb","StEvent","StEventMaker","StEmcUtil", 
		 "StMcEvent","StMcEventMaker","StAssociationMaker","StDaqLib","StDAQMaker",
		 "StDetectorDbMaker", "StSvtClassLibrary","StSvtDaqMaker",
		 "StSvtSimulationMaker","StSvtCalibMaker", "StSvtSeqAdjMaker",
		 "StSvtClusterMaker","Sti","StiGui", "StiEvaluator","StiMaker",
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

void RunMany(int first=0, int count=100)
{
  Run(first, count,
      "rcf", 
      "/data/r23b/star/hijingAuau/200GeV/b0_20/standard/2001/Unknown/",
      "rcf0183_02_300evts.geant.root");
}

void RunResiduals(int firstEvent = 0, 
		  int nEvents    = 100,
		  const char * filePrefix = "rcf",
		  const char * path= "/data/r23b/star/hijingAuau/200GeV/b0_20/standard/2001/Unknown/",
		  const char * file="rcf0183_02_300evts.geant.root",
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
		  bool useSvt=false,
		  bool useEmc=false,
		  bool useFtpc=false,
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
      useTpc,
      useSvt,
      useEmc,
      useFtpc,
      useResidualCalculator,
      doProfile);
}
 
void RunGui()
{
   Run(0,1, 
      "rcf", 
      "/data/r23b/star/hijingAuau/200GeV/b0_20/standard/2001/Unknown/",
      "rcf0183_02_300evts.geant.root",
       true);
 
}
void Run(int firstEvent,
	 int nEvents,
	 const char * filePrefix = "rcf",
	 //const char * path= "/star/data13/reco/ppMinBias/FullField/P02gf/2002/019/",
	 //const char * path ="/star/data22/ITTF/EvalData/Event/ppMinBias/",
  	 //const char * file="st_physics_3019045_raw_0031.event.root",
	 //const char * path= "/star/data06/ITTF/EvalData/MCFiles/auau200",
	 //const char * path= "/data/r20b/ittf/auau200/hijing/b0_20/standard/year2001/",
	 const char * path= "/data/r23b/star/hijingAuau/200GeV/b0_20/standard/2001/Unknown/",
	 const char * file="rcf0183_02_300evts.geant.root",
	 bool useGui=false,
	 bool useMcAsRec=false,
	 bool doPlots=true,
	 bool doSimulation=true,
	 bool doAssociation=true,
	 bool doMiniMcEvent=true,
	 bool doDst=false,
	 bool doStEventOutput=true,
	 bool doStEventInput=true,
	 bool useTpc=true,
	 bool useSvt=false,
	 bool useEmc=false,
	 bool useFtpc=false,
	 bool useResidualCalculator=false,
	 bool doProfile=false	 )
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
      useGui, 
      useMcAsRec,
      doPlots,
      doSimulation,
      doAssociation,
      doMiniMcEvent,
      doDst,
      doStEventOutput,
      doStEventInput,
      useTpc,
      useSvt,
      useEmc,
      useFtpc,
      useResidualCalculator,
      doProfile);
}

void Run(Int_t firstEvent,
	 Int_t nEvents, 
	 const char *  filePrefix,
	 const char ** fileList,
	 bool useGui,
	 bool useMcAsRec,
	 bool doPlots,
	 bool doSimulation,
	 bool doAssociation,
	 bool doMiniMcEvent,
	 bool doDst,
	 bool doStEventOutput,
	 bool doStEventInput,
	 bool useTpc=true,
	 bool useSvt=false,
	 bool useEmc=false,
	 bool useFtpc=false,
	 bool useResidualCalculator=false,
	 bool doProfile=false)
{
  loadLibrairies(doProfile);
  MiniChain * miniChain = new MiniChain();
  
  StiMakerParameters* pars = miniChain->getParameters();
  pars->useGui=useGui;
  pars->useMcAsRec=useMcAsRec;
  pars->doSimulation=doSimulation;
  pars->doAssociation=doAssociation;
  pars->doMiniMcEvent=doMiniMcEvent;
  pars->doDst=doDst;
  pars->doStEventOutput=doStEventOutput;
  pars->doStEventInput=doStEventInput;  
  pars->doPlots = doPlots; 
  pars->useTpc=useTpc;
  pars->useSvt=useSvt;
  pars->useEmc=useEmc;
  pars->useFtpc=useFtpc;  
  pars->useResidualCalculator=useResidualCalculator;
  miniChain->run(firstEvent,nEvents,filePrefix,fileList);
}
