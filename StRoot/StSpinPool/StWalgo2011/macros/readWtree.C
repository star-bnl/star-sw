class StChain;
class St2011WMaker;

StChain      *chain  = 0;
St2011WMaker *wTreeMk = 0;
TString jetTreeDir = "/star/institutions/iucf/stevens4/wAnalysis/jetTreeSL10j12.22.10/";

void readWtree(	const Char_t *fileList="R10103042.lis" )
{

  // load shared libraries
  gROOT -> LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  assert( !gSystem->Load("StEEmcUtil"));
  assert( !gSystem->Load("StWalgo2011"));

  //clean up and keep jets working
  assert( !gSystem->Load("StDetectorDbMaker")); // for St_tpcGasC
  
  //gSystem->Load("StJetSkimEvent");
  gSystem->Load("StJets");
    
  // create analysis chain
  chain  = new StChain("chain");
  TObjArray* HList=new TObjArray;

  //initiate W maker
  wTreeMk = new St2011WMaker();
  wTreeMk->setJetTreeBranch("ConeJets12_100","ConeJets12_100_noEEMC"); //select jet tree braches used
  wTreeMk->setHList(HList);

  //chain W and jet trees in W maker
  chainFiles(fileList);
  
  chain->ls(3);
  chain->Init();

  Int_t nevents = wTreeMk->getNumberOfEvents();

  Int_t stat  = 0;  Int_t event = 0;
  while ( !stat ) 
    {
      if ( nevents>=0 )
	if ( event>=nevents ) break;
      chain -> Clear();
      stat = chain->Make();
      event++;
    }

  TString outFile=fileList;
  outFile.ReplaceAll(".lis",".root");
  TFile *outF=new TFile(outFile,"RECREATE");
  if(outF->IsOpen()){
    HList->Write();
    cout<<endl<<" Histo saved -->"<<outFile<<endl;
  }
  else
    cout<<endl<<" Couldn't open file "<<outFile<<endl;

  delete outF;
    

}

// ----------------------------------------------------------------------------
void chainFiles(const Char_t *fileList)
{

  cout << "chaining files from list: " << fileList << endl;

  ifstream f(fileList);
  char str[200];
  while (f >> str) {
    TString name=str;
    if ( name.Contains("root") ) 
      wTreeMk->chainFile(name);
    
    //chain jet files
    TString jetName=jetTreeDir;
    jetName+=name.ReplaceAll("trees/","jets_");
    jetName.ReplaceAll(".wana.tree","");
    wTreeMk->chainJetFile(jetName);
  }

}

