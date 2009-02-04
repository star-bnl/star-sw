//This macro reads into the EEMC pi0 trees with an input directory, and produce an output root file. Users can design their output accordingly.
//Author: Weihong He

class StChain;
class StEEmcIUPi0Reader;
class StEEmcIUMixEvent;

StChain         *chain  = 0;
StEEmcIUPi0Reader *reader = 0;
StEEmcIUMixEvent  *realEvent = 0;

Long64_t nevents = 0;



void readPi02(Int_t nevents=-1,
	     const Char_t *mydir="/star/institutions/iucf/hew/7863/7136033/",
	     const Char_t *ofile="/star/u/hew/pionCVS/7136033.root")
{

  const Double_t ptbins[]={0.0,2.5,4.5,5.5,6.5,8.5,10.5,14.5,20.0};
  Int_t nptbins=sizeof(ptbins)/sizeof(Double_t) - 1;

  TFile *file=new TFile(ofile,"RECREATE");
  hEtavsMass = new TH2F("hEtavsMass","Detector #eta of #pi^{0} candidates vs Mass;Mass; #eta",120,0.,1.2,180,1.,2.5);
  hPhivsMass = new TH2F("hPhivsMass","#Phi of #pi^{0} candidates vs Mass; Mass, #Phi",120,0.,1.2,360,-180.,180.);
  hPhi= new TH1F("hPhi","#Phi",30,0.,30.);
  hPhiEta1= new TH1F("hPhiEta1","#Phi with #eta at [1.089,1.268)",30,0.,30.);
  hPhiEta2= new TH1F("hPhiEta2","#Phi with #eta at [1.268,1.476)",30,0.,30.);
  hPhiEta3= new TH1F("hPhiEta3","#Phi with #eta at [1.476,1.718)",30,0.,30.);
  hPhiEta4= new TH1F("hPhiEta4","#Phi with #eta at [1.718,2.000)",30,0.,30.);
  hZggPt= new TH2F("hZggPt","Pi0 Zgg vs Pt;pT[Gev/c];Zgg",50,0.,25.,50,0.,1.);
  // load shared libraries
  LoadLibs();

  // create analysis chain
  chain  = new StChain("chain");

  // add the pi0 reader and add all root files in specified directory
  reader = new StEEmcIUPi0Reader();
  chainFiles(mydir);

  nevents = reader->getNumberOfEvents();
    
  chain->ls(3);
  chain->Init();

  Int_t stat  = 0;
  Int_t event = 0;
  while ( !stat ) 
    {

      if ( nevents>=0 )
	if ( event>=nevents ) break;

      chain -> Clear();
      stat = chain->Make();

      realEvent = reader->event();

      Int_t nPairs = reader -> event() -> nPairs;
      for ( Int_t i=0;i<nPairs;i++ )
	{

	  Float_t mass = reader->event()->mMass[i];
	  Float_t pt   = reader->event()->mPT[i];
	  Float_t zgg  = reader->event()->mZgg[i];
	  Float_t meta = reader->event()->mEEmcEta[i];
	  Float_t mphi = reader->event()->mPhi[i];

	  hEtavsMass->Fill(mass,meta);
	  hPhivsMass->Fill(mass,mphi*180./3.14159265);
	  Int_t mod_phi=int(mphi*180./3.14159265+180.)%30;
	  hPhi->Fill(mod_phi);
	  if(meta>=1.089 && meta<1.268) hPhiEta1->Fill(mod_phi);
	  if(meta>=1.268 && meta<1.476) hPhiEta2->Fill(mod_phi);
	  if(meta>=1.476 && meta<1.718) hPhiEta3->Fill(mod_phi);
	  if(meta>=1.718 && meta<2.) hPhiEta4->Fill(mod_phi);


	  Float_t diff = realEvent->mEsmdu[i] - realEvent->mEsmdv[i];
	  Float_t sum  = realEvent->mEsmdu[i] + realEvent->mEsmdv[i];
	  Float_t zuv = TMath::Abs(diff)/sum;


	}
      event++;



    }

  file->cd();
  hEtavsMass->Write();
  hPhivsMass->Write();
  hPhi->Write();
  hPhiEta1->Write();
  hPhiEta2->Write();
  hPhiEta3->Write();
  hPhiEta4->Write();
  hZggPt->Write();
  delete file;
  

}
// ----------------------------------------------------------------------------
void chainFiles(const Char_t *path)
{

  std::cout << "chaining files " << path << std::endl;

  TSystemDirectory *dir = new TSystemDirectory("dir",path);
  
  TIter next( dir->GetListOfFiles() );
  TObject *file = 0;
  while ( file = (TObject*)next() )
    {
      TString name=file->GetName();

      if ( name.Contains("root") ) {
	reader->chainFile(name);

      }

    }

  delete dir;


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
  gSystem->Load("StEEmcIUPi0");
  gSystem->Load("StSpinDbMaker");

}
