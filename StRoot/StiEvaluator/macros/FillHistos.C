void FillHistos(
	 const char *MainFile="",
	 const char *outName="Output.root",
	 const char *outDir="")
{
  gSystem->Load("StMiniMcEvent");    
  gSystem->Load("StiEvaluator");
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
