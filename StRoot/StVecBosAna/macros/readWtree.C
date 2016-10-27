class StChain;
class St2011WMaker;

StChain      *chain  = 0;
St2011WMaker *wTreeMk = 0;
TString jetTreeDir = "";
bool isZ=false;

void readWtree(	const Char_t *fileList="./R10081007.lis", int etaBin=7, int maxEvents=1e8, bool spinSort=true)
{

  if(etaBin==7) isZ=true;

  // load shared libraries
  gROOT -> LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  assert( !gSystem->Load("StDetectorDbMaker")); 
  assert( !gSystem->Load("StTpcDb"));
  assert( !gSystem->Load("StDbUtilities")); 
  assert( !gSystem->Load("StDbBroker"));
  assert( !gSystem->Load("St_db_Maker"));
  assert( !gSystem->Load("StEEmcUtil"));
  assert( !gSystem->Load("StWalgo2011"));
  assert( !gSystem->Load("StSpinDbMaker"));
  assert( !gSystem->Load("StJets"));
    
  // create analysis chain
  chain = new StChain("chain");
  TObjArray* HListZ=new TObjArray;
  TObjArray* HListEta=new TObjArray;;
  
  //define eta ranges
  float etaLow[7] = {-0.9, -0.4, 0.0, 0.4, 0.9, 1.15, -1.};
  float etaHigh[7] = {-0.4, 0.0, 0.4, 0.9, 1.15, 1.35, 1.};

  //initiate W maker
  wTreeMk = new St2011WMaker(Form("Eta%d",etaBin));
  wTreeMk->setJetTreeBranch("ConeJets12_100","ConeJets12_100_noEEMC"); //select jet tree braches used
  //set cuts for barrle and endcap algos
  wTreeMk->setWbosonCuts(25., 0.88, 14., etaLow[etaBin-1], etaHigh[etaBin-1]); //highET, nearTotEtFrac, ptBalance, etaLow, etaHigh
  wTreeMk->setE_WbosonCuts(25., 0.85, 14., etaLow[etaBin-1], etaHigh[etaBin-1]); //highET, nearTotEtFrac, ptBalance, etaLow, etaHigh
  wTreeMk->setHList(HListEta);
  wTreeMk->setMaxDisplayEve(1); // only first N events will get displayed 

  //......... spin sorting
  if(spinSort){
    enum {mxSM=1}; // only one for now
    St2011pubSpinMaker *spinMkA[mxSM];
    for(int kk=0;kk<mxSM;kk++) {
      char ttx[100]; sprintf(ttx,"%cEta%dspin",'A',etaBin);
      printf("add spinMaker %s %d \n",ttx,kk);
      spinMkA[kk]=new St2011pubSpinMaker(ttx,Form("Eta%d",etaBin)); 
      spinMkA[kk]->attachWalgoMaker(wTreeMk);
      spinMkA[kk]->setHList(HListEta);
      //assign eta bin same as W maker
      spinMkA[kk]->setEta(etaLow[etaBin-1],etaHigh[etaBin-1]); 
      
      //possibly add other copies for systematic checks later
      //if(kk==1) spinMkA[kk]->setEta(-1.,0.);
      //if(kk==2) spinMkA[kk]->setEta(0,1.);
      //if(kk==3) spinMkA[kk]->setQPT(-1);// disable Q/PT cut
      //if(kk==4) spinMkA[kk]->setNoEEMC(); 
    }  
  }

  // Z maker
  if (isZ){
    ZMk=new St2011ZMaker("Z"); 
    ZMk->attachWalgoMaker(wTreeMk);
    ZMk->setHList(HListZ); 
    ZMk->setNearEtFrac(0.88);
    ZMk->setClusterMinEt(15);
    ZMk->setPhi12Min(3.1416/2.);
    ZMk->setMinZMass(73.); // Zmass -20%
    ZMk->setMaxZMass(114.);// Zmass +20%
  }

  //chain W and jet trees in W maker
  chainFiles(fileList);
  
  chain->ls(3);
  chain->Init();

  Int_t nevents = wTreeMk->getNumberOfEvents();
  cout<<nevents<<" events in chain"<<endl;
  if(nevents<1) {cout<<"No events in chain, check your list file"<<endl;  return; }

  Int_t stat  = 0;  Int_t event = 0;
  while ( !stat ) 
    {
      if ( nevents>=0 )
	if ( event>=nevents || event > maxEvents) break;
      chain -> Clear();
      stat = chain->Make();
      event++;
    }

  char *file1=fileList;
  printf("file1=%s=%s=\n",file1);
  TString outFile="";  outFile+=file1;
  outFile.ReplaceAll(".lis",Form("_Eta%d.wana.hist.root",etaBin));
  outFile.ReplaceAll("lists/treeReader/","");
  TFile *outF=new TFile(outFile,"RECREATE");
  if(outF->IsOpen()){
    TDirectory *Eta = outF->mkdir(Form("Eta%d",etaBin));
    Eta->cd();
    HListEta->Write();
    if(isZ){
      TDirectory *Z = outF->mkdir("Z");
      Z->cd();
      HListZ->Write();
    }
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
    jetName+=name.ReplaceAll("wtree/","jets/jets_");
    jetName.ReplaceAll(".Wtree","");
    wTreeMk->chainJetFile(jetName);
  }

}

