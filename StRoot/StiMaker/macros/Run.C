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
		 "libsim_Tables","libglobal_Tables","geometry","St_g2t","St_geant_Maker",
		 "StIOMaker","StTreeMaker", "St_db_Maker","StDbLib","StDbBroker",
		 "StSvtDbMaker","StDbUtilities", "StTpcDb","StEvent","StEventMaker","StEmcUtil", 
		 "StMcEvent","StMcEventMaker","StAssociationMaker","StDaqLib","StDAQMaker",
		 "StDetectorDbMaker", "StSvtClassLibrary","StSvtDaqMaker",
		 "StSvtSimulationMaker","StSvtCalibMaker", "StSvtSeqAdjMaker",
		 "StSvtClusterMaker","Sti","StiGui", "StiEvaluator","libGui","StiMaker",
		 "StiTpc","StiSvt","StiEmc","StMiniMcEvent","StMiniMcMaker","last"};

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

void Run(int firstEvent=0,
	 int nEvents=5,
	 //const char * filePrefix = "st_physics_",
	 const char * filePrefix = "rcf",
	 //const char * path= "/star/data13/reco/ppMinBias/FullField/P02gf/2002/019/",
	 //const char * path ="/star/data22/ITTF/EvalData/Event/ppMinBias/",
  	 //const char * file="st_physics_3019045_raw_0031.event.root",
	 //const char * path= "/star/data22/ITTF/EvalData/MCFiles/auau200",
	 const char * path= "/a1/ittf/auau200/hijing/b0_20/standard/year2001/",
	 //const char * file="rcf0183_12_300evts.event.root",
	 const char * file="rcf0183_05_300evts.geant.root",
  
//  	 bool useGui=false,
//  	 bool doSimulation=false,
//  	 bool doAssociation=false,
//  	 bool doMiniMcEvent=false,
//  	 bool doDst=false,
//  	 bool doStEventOutput=true,
//  	 bool doStEventInput=true,
//  	 bool doProfile=false

	 //Example of switch setup for minimctree output
	 bool useGui=false,
	 bool doSimulation=true,
	 bool doAssociation=true,
	 bool doMiniMcEvent=true,
	 bool doDst=false,
	 bool doStEventOutput=true,
	 bool doStEventInput=true,
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
      doSimulation,
      doAssociation,
      doMiniMcEvent,
      doDst=false,
      doStEventOutput,
      doStEventInput,
      doProfile);
}

void Run(Int_t firstEvent,
	 Int_t nEvents, 
	 const char *  filePrefix,
	 const char ** fileList,
	 bool useGui=true,
	 bool doSimulation=true,
	 bool doAssociation=true,
	 bool doMiniMcEvent=true,
	 bool doDst=false,
	 bool doStEventOutput=false,
	 bool doStEventInput=false,
	 bool doProfile=false )
{
  loadLibrairies(doProfile);
  MiniChain * miniChain = new MiniChain();
  MiniChainParameters * pars = miniChain->getParameters();
  pars->useGui=useGui;
  pars->doSimulation=doSimulation;
  pars->doAssociation=doAssociation;
  pars->doMiniMcEvent=doMiniMcEvent;
  pars->doDst=doDst;
  pars->doStEventOutput=doStEventOutput;
  pars->doStEventInput=doStEventInput;  
  miniChain->run(firstEvent,nEvents,filePrefix,fileList);
}



// keep this here for convenience for a short while... CP

	/*
  StiRootIOBroker * stiIoBroker = miniChain->getIOBroker();
  stiIoBroker->setTPHFMinPadrow(1);
  stiIoBroker->setTPHFMaxPadrow(45);
  stiIoBroker->setETSFLowerBound(5);
  stiIoBroker->setETSFMaxHits(6);
  stiIoBroker->setDoTrackFit(false);
  //Set Kalman Track Finder (KTF) run-time values:
  //stiIoBroker->setKTFMcsCalculated(true);
  //stiIoBroker->setKTFElossCalculated(true);
  stiIoBroker->setKTFMcsCalculated(false);
  stiIoBroker->setKTFElossCalculated(false);
  stiIoBroker->setKTFMaxChi2ForSelection(20);
  stiIoBroker->setKTFBField(.5); //Tesla
  stiIoBroker->setKTFMassHypothesis(.1395); //GeV
  stiIoBroker->setKTFMinContiguousHitCount(2);
  stiIoBroker->setKTFMaxNullCount(20);
  stiIoBroker->setKTFMaxContiguousNullCount(15);
  stiIoBroker->setKTFMinSearchRadius(.5); //cm
  stiIoBroker->setKTFMaxSearchRadius(4.); //cm
  stiIoBroker->setKTFSearchWindowScale(5.); //cm
  //Set Local Track Seed Finder (LTSF) run-time values
  //stiIoBroker->setLTSFZWindow(5.);
  //stiIoBroker->setLTSFYWindow(2.);
  stiIoBroker->setLTSFZWindow(10.);
  stiIoBroker->setLTSFYWindow(4.);
  stiIoBroker->setLTSFSeedLength(2);
  stiIoBroker->setLTSFDoHelixFit(true);
  stiIoBroker->setLTSFExtrapYWindow(1.);
  stiIoBroker->setLTSFExtrapZWindow(2.);
  stiIoBroker->setLTSFExtrapMaxSkipped(2);
  stiIoBroker->setLTSFExtrapMinLength(4);
  stiIoBroker->setLTSFExtrapMaxLength(5);
  stiIoBroker->setLTSFUseVertex(true);
  stiIoBroker->setLTMDeltaR(1.); //10% in r
  //TPC Sectors
  for (unsigned int sector=1; sector<=12; ++sector) 
    stiIoBroker->addLTSFSector(sector);
  //Add padrows;
  for (unsigned int padrow=6; padrow<=45; padrow+=1)
    stiIoBroker->addLTSFPadrow(padrow);
  stiIoBroker->setSimulated(doSimulation);
	*/
