// example macro to use  EEmcTTMMaker
// Author: Piotr A. Zolnierczuk

class StChain;
StChain      *chain     =0;


void RunTTM(StChain* chain, 
	    char* inpDir   ,
	    char* inpFile  ,
	    char* outFile  ,
	    Int_t nFiles   ,
	    Int_t nEvents  ,
	    Int_t timeStamp);

 
void 
loadSharedLibraries()
{
  // Dynamically link needed shared libs
  gSystem->Load("libTable");
  gSystem->Load("libPhysics");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");        // new addition 22jul99
  gSystem->Load("StTreeMaker");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StTriggerDataMaker"); // new starting from April 2003
  gSystem->Load("StBichsel");
  gSystem->Load("StEvent");
  gSystem->Load("StEventUtilities");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StTofUtil");
  gSystem->Load("StPmdUtil");
  gSystem->Load("StPreEclMaker");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StMuDSTMaker");  

  // load more libraries :)
  gSystem->Load("libmysqlclient");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");

  // load even more libraries (EEMC stuff) 
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEEmcPoolTTM");

  cout << " loading of shared libraries done" << endl;

}

void ttm(
 char* inpDir    = "",            // MuDST directory
 char* inpFile   = "ttm.lis",     // MuDST file(s)
 char* outFile   = "ttm.root",
 Int_t nFiles    = 50,            // # of MuDST file(s)
 Int_t nEvents   = -1,
 Int_t timeStamp = 20040331)
{ 
  //gErrorIgnoreLevel=1999;
  std::cerr << "<xml version=\"1.0\" >" << std::endl;
  
  loadSharedLibraries();

  // create the chain    
  chain = new StChain("StChain"); 

  EEmcTTMMaker::Run(chain,inpDir,inpFile,outFile,nFiles,nEvents,timeStamp);
  
  std::cerr << "</xml>" << endl;
}
