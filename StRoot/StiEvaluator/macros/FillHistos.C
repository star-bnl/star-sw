void FillHistos(
	 const char *MainFile="",
	 const char *outName="Output.root",
	 const char *outDir="")
{
  char * list[]={"St_base","StChain","StUtilities", "St_Tables", "StarClassLibrary",
		 "libsim_Tables","libglobal_Tables","geometry","St_g2t","St_geant_Maker","libGui",
		 "StIOMaker","StTreeMaker", "St_db_Maker","StDbLib","StDbBroker",
		 "StSvtDbMaker","StDbUtilities", "StTpcDb","StEvent","StEventMaker",
		 "StMcEvent","StMcEventMaker","StMiniMcEvent","StAssociationMaker",
		 "Sti", "StiEvaluator",
		 "last"};

  int i=0;  
  cout <<"Run.C::loadLibrairies() - INFO - Started"<<endl;
  while (list[i]!="last")
    {
      cout << "          Loading module:"<<list[i];
      gSystem->Load(list[i++]);
      cout << "\t\t- Done." << endl;
    } 
  TChain* chain = new TChain("StMiniMcTree");
  chain->Add(MainFile);

  cout << "Events in file " << chain->GetEntries() << endl;
  
  TString filename = outName;
  filename.Prepend(outDir);
  
  StiEvaluator effeval;
  effeval.setChain(chain);
  effeval.setFitPtsLimit(10);      // min value of accepted fit points
  effeval.setDcaLimit(3.);         // dca limit
  effeval.setEtaRange(-1.0,1.0);   // eta range
  effeval.setPullType(0);          // pulls: 0 = ITTF, 1 = TPT
                                   // this option is needed because of different definitions
                                   // of conv. matrix for ITTF and TPT 
  effeval.setFileName(filename.Data());
  cout << "Output s: " << filename        << endl;
  cout << "Output c: " << filename.Data() << endl;
  
  TStopwatch timer;
  timer.Start();
  
  effeval->initialize();
  effeval->makehistograms();
  effeval->writehistograms();
  
  timer.Stop();
  cout << "Real time = " << timer.RealTime() << " , Cpu Time = " << timer.CpuTime() << endl;
  
  return;
}
