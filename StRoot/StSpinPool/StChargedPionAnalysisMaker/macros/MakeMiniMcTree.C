// $Id: MakeMiniMcTree.C,v 1.2 2008/12/29 16:12:49 kocolosk Exp $

/*****************************************************************************
 * @author Adam Kocoloski
 *
 * macro to regenerate MiniMc tree, since it gets removed from disk sometimes
 *****************************************************************************/

void MakeMiniMcTree(Int_t nevents=3,
                    const char* MainFile="hijing_b0_3/rcf0147_01*geant.root",
                    const char* outDir = "./",
                    TString filePrefix="rcf")
{
    gROOT->Macro("LoadLogger.C");
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("StTpcDb");
    gSystem->Load("StEvent");
    gSystem->Load("StEventMaker"); 
    gSystem->Load("StEmcUtil"); 
    gSystem->Load("StEEmcUtil");
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    gSystem->Load("StAssociationMaker");
    gSystem->Load("StMcAnalysisMaker");
    gSystem->Load("StMiniMcEvent");
    gSystem->Load("StMiniMcMaker");
    
    StChain chain; 
    
    StIOMaker ioMk("IO","r",MainFile,"bfcTree");
    ioMk.SetIOMode("r");
    ioMk.SetBranch("*",0,"0");
    ioMk.SetBranch("geantBranch",0,"r");
    ioMk.SetBranch("eventBranch",0,"r");
    
    StMcEventMaker mcEventMk;
    
    StAssociationMaker assocMk;
    assocMk.useInTracker();
    
    TString filename   = MainFile;
    int fileBeginIndex = filename.Index(filePrefix,0);
    filename.Remove(0,fileBeginIndex);
    
    StMiniMcMaker miniMcMk;
    miniMcMk.setOutDir(outDir);
    miniMcMk.setPtCut(1.5);
    miniMcMk.setFileName(filename);
    miniMcMk.setFilePrefix(filePrefix);
    
    StMcParameterDB* parameterDB = StMcParameterDB::instance();  
    parameterDB->setXCutTpc(.5); // 5 mm
    parameterDB->setYCutTpc(.5); // 5 mm
    parameterDB->setZCutTpc(.5); // 5 mm
    parameterDB->setReqCommonHitsTpc(3);
    
    TStopwatch total;
    TStopwatch timer;
    TMemStat memory;
    
    chain.Init();
    
    int i=0;
    while(i<nevents && chain.Make()==kStOk) {
        if(i % 500 == 0) {
            cout << "done with event " << i 
                 << "\tcpu: " << timer.CpuTime() 
                 << "\treal: " << timer.RealTime() 
                 << "\tratio: " << timer.CpuTime()/timer.RealTime() << endl;
            timer.Start();
            memory.PrintMem(NULL);
        }
        ++i;
        chain.Clear();
    }
    
    chain.Finish();
    cout << "my macro processed " << i << " events"
         << "\tcpu: " << total.CpuTime()
         << "\treal: " << total.RealTime()
         << "\tratio: " << total.CpuTime()/total.RealTime() << endl;
}

/*****************************************************************************
 * $Log: MakeMiniMcTree.C,v $
 * Revision 1.2  2008/12/29 16:12:49  kocolosk
 * added $Id$/$Log$ as needed
 *
 * Revision 1.1  2008/07/18 16:57:41  kocolosk
 * macro to regenerate MiniMc tree, since it gets removed from disk sometimes
 *
 *****************************************************************************/
