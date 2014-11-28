class StChain;
class StEEmcPi0Reader;
class StEEmcMixEvent;

StChain         *chain  = 0;
StEEmcPi0Reader *reader = 0;
StEEmcMixEvent  *realEvent = 0;

Long64_t nevents = 0;

TH1F *hMass = 0;

void readPi0()
{

  hMass = new TH1F("hMass","Inv mass",120,0.,1.2);

  // load shared libraries
  LoadLibs();

  // create analysis chain
  chain  = new StChain("chain");

  // add the pi0 reader and add all root files in specified directory
  reader = new StEEmcPi0Reader();
  chainFiles("/auto/pdsfdv34/starspin/jwebb/2006/tests/");
  nevents = reader->getNumberOfEvents();

  chain->ls(3);
  chain->Init();

  Int_t stat  = 0;
  Int_t event = 0;
  while ( !stat ) 
    {

      chain -> Clear();
      stat = chain->Make();

      realEvent = reader->event();

      Int_t nPairs = reader -> event() -> nPairs;
      for ( Int_t i=0;i<nPairs;i++ )
	{
	  Float_t mass = reader->event()->mMass[i];
	  hMass->Fill(mass);



	  //          if ( !(event%100) )
            std::cout << "[" << event << "/" << nevents << "]"
                      << " npair=" <<  realEvent->nPairs
                      << " mass="  <<  realEvent->mMass[i]
                      << " pt="    <<  realEvent->mPT[i]
                      << " bx="    <<  realEvent->bxStar
                      << " zgg="   <<  realEvent->mZgg[i]
                      << " spin4=" <<  realEvent->mSpin4
                      << " dsmvtx=" << realEvent->mBbcTrigger.onlineTimeDifference()
                      << std::endl;
	  



	}
      event++;



    }
  

}
// ----------------------------------------------------------------------------
void chainFiles(const Char_t *path)
{

  TSystemDirectory dir("dir",path);
  TIter next( dir.GetListOfFiles() );
  TObject *file = 0;
  while ( file = (TObject*)next() )
    {
      TString name=file->GetName();
      if ( name.Contains("root") ) reader->chainFile(name);

    }


}

// ----------------------------------------------------------------------------
void LoadLibs()
{
  //-- Load muDst shared libraries --
  gROOT -> LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEEmcSimulatorMaker");

  gSystem->Load("StEEmcA2EMaker");
  gSystem->Load("StEEmcClusterMaker");
  gSystem->Load("StEEmcPointMaker");
  gSystem->Load("StEEmcPi0Mixer");
  gSystem->Load("StSpinDbMaker");

}
