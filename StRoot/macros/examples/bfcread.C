class StChain;
class St_DataSet;
St_DataSet *dst;
StChain *cnain;

void bfcread(Int_t nevents=1, const char *MainFile="gtrack.dst.root")
{
//
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StTreeMaker");

//	TOP maker
    chain = new StChain("bfc");
    chain->SetDebug();
   
//		Input Tree
  StTreeMaker *treeMk = new StTreeMaker("tree",MainFile);
  treeMk->SetIOMode("r");
  treeMk->SetDebug();

  chain->SetInput("dst","bfc/dst");

  chain->Init();
     
  for (int iev=0;iev<nevents; iev++)
  {
    chain->Clear();
    int iret = chain->Make();
    if (iret) break;
    dst = chain->GetDataSet("dst");
    if (dst) dst->ls(9);    
  } 
}
