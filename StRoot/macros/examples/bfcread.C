//  bfcread.C
//  bfcread.C
//
// 4/28/99 K.T. ---> you must copy this to the directory where the
//                   file actually is, or it won't work
//
//======================================================================
class StChain;
class St_DataSet;
St_DataSet *Event;
StChain *chain;
TBrowser *brow=0;

void bfcread(Int_t nevents=1, const char *MainFile="/disk1/star/test/SL99d/tfs_Linux/Fri/set0020_01_50evts.Event.root")
{
//
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StTreeMaker");
    gSystem->Load("St_QA_Maker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StRootEvent");
//	TOP maker
    chain = new StChain("bfc");
    chain->SetDebug();
   
//		Input Tree
  StTreeMaker *treeMk = new StTreeMaker("tree",MainFile);
  treeMk->SetIOMode("r");
  treeMk->SetDebug();

   chain->SetInput("Event","bfcTree/EventBranch/Event");
   St_QA_Maker  *qa  = new St_QA_Maker;
  
  chain->Init();
 

  for (int iev=0;iev<nevents; iev++)
  {
    chain->Clear();
    int iret = chain->Make();
    if (iret) break;
    Event = chain->GetDataSet("Event");
    if (Event) {
          Event->ls(9);
          brow = new TBrowser("Event",Event);    
    }
  } 
}
