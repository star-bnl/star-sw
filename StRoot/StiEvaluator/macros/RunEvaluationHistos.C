//
// $Id $
//
// Step 1, make the raw correction histos out of the
// minimc.root files.
//
// $Log $
//

void RunEvaluationHistos(const char *MainFile= "/star/data22/ITTF/EvalData/MCNtuple/EvalItTestrcf0183_27_300evts.minimc.root",
			 const char *outDir = "StiEvalOutputRawHistos/")
{
    //  currently only works for 1 file at a time, #$%@!
    gSystem->Load("StMiniMcEvent");    
    gSystem->Load("StiEvaluator");
    TChain* chain = new TChain("StMiniMcTree");
    chain->Add(MainFile);
    cout << "Events in file " << chain->GetEntries() << endl;

    TString filename = MainFile;
    if (filename.Contains("minimc")) {
	int preIndex = filename.Index("minimc",0);
	filename.Remove(preIndex,7); //remove minimc
// 	int fileBeginIndex = filename.Index("EvalItTest",0);
	int fileBeginIndex = filename.Last('/');
	filename.Remove(0,fileBeginIndex+1);
    }
    filename.Prepend("rawHistos");
    filename.Prepend(outDir);
    cout << "Output : " << filename << endl;

    
    StiEvaluator effeval;
    effeval.setGeantId(8); // not needed now
    effeval.setChain(chain);
    effeval.setFitPtsLimit(24); //cut is >=
    effeval.setDcaLimit(1.);
    effeval.setFileName(filename.Data());

    TStopwatch timer;
    timer.Start();

    effeval->initialize();
    effeval->makehistograms();
    effeval->writehistograms();
    
    timer.Stop();
    cout << "Real time = " << timer.RealTime() << " , Cpu Time = " << timer.CpuTime() << endl;

    return;
}
