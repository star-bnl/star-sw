//  
//  
{
#pragma includepath "/afs/rhic/star/packages/dev/StRoot"
#pragma includepath "/afs/rhic/star/packages/dev/lib"
   gSystem->Load("St_base");
   gSystem->Load("StChain");
   gSystem->Load("xdf2root");
   gSystem->Load("St_Tables");
   gSystem->Load("St_xdfin_Maker");
   gSystem->Load("St_db_Maker");


  Char_t *filename = "/afs/rhic/star/data/samples/hijet-g2t.xdf";
  St_XDFFile *xdf_in   = new St_XDFFile(filename,"r");
  StChain chain("StChain");
  St_params_Maker params("params","run/params"); 
  chain.SetInputXDFile(xdf_in);
  chain.PrintInfo();
// Init the main chain and all its makers
  chain.Init();
  gBenchmark->Start("params");
  for (Int_t i=0;i<1;i++){
    //    params.SetValidTime(19980101,0);
    chain.Make(i);
    St_DataSetIter local(chain.DataSet());
    local.Cd(chain.GetName());
    St_DataSet *evnt = local("event");
  }
  //  chain.Finish();
  gBenchmark->Stop("params");
  gBenchmark->Print("params");
  TBrowser b;
}
