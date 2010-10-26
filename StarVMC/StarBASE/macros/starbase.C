//------------------------------------------------------------------------------------
//--
//-- Default configuration
//--
const Float_t eta_min = -6.0;  // min pseudorapidity
const Float_t eta_max = +6.0;  // max pseudorapidity
const Float_t deta    = 0.02;  // size of etabin
//--
//const Float_t phi_min =-1.00; // min phi (in degrees)
//const Float_t phi_max =+1.00; // max phi (in degrees)
const Float_t phi_min =-15.000; // min phi (in degrees)
const Float_t phi_max =+15.000; // max phi (in degrees)
const Float_t dphi    = 0.120; // size of phibin (in degrees)
//--
const Float_t nsample = 4.0;   // number of tracks / bin
//--
//------------------------------------------------------------------------------------

int loaded=0;

//                                                      === class declarations ===
class StMCStepping;

//                                                      ===   stepping class   ===
StMCStepping *steps = 0;

void starbase( const char *gy="y2006c", 
	       Float_t etaMin=eta_min, 
	       Float_t etaMax=eta_max,
	       Float_t phiMin=phi_min,
	       Float_t phiMax=phi_max, 
	       const Char_t *path="./" )
{

  // Silence ROOT
//  gErrorIgnoreLevel = 9999;

  // Load shared libraries
  if (!loaded) { Load(); loaded = 1; }

  // Create the chain
  StChain *chain =  new StChain;
  TString tsy(gy);
  gSystem->ExpandPathName(tsy);

  // Create the geometry
  gROOT->LoadMacro   ( TString(path)+tsy+".h" );
  gROOT->ProcessLine ( tsy+"()" );
  // gROOT -> ProcessLine( TString( ".x "+tsy+".h" ) );

  std::cout << "Done creating geometry" << std::endl;


  // Create the baseline maker
  StarBASE *mk = new StarBASE("StarBASE",tsy.Data(), 1);

  // Setup fiducial volume
  mk->SetAttr("eta_min", etaMin);
  mk->SetAttr("eta_max", etaMax);
  mk->SetAttr("deta",    deta   );
  mk->SetAttr("phi_min", phiMin);
  mk->SetAttr("phi_max", phiMax);
  mk->SetAttr("dphi",    dphi);
  mk->SetAttr("sample",  nsample);

  // Initialize the chain
  chain->Init();
  steps = mk->steps();

  // Create histograms for all volumes beneath the "HALL" volume.
  steps->bookVolume("HALL");

  // Print out tracking media for all volumes for debug poirposes
  /*
  TIter next( gGeoManager->GetListOfMedia() );
  TGeoMedium *m;
  while ( (m=(TGeoMedium*)next() ) )
    {
      m->SavePrimitive(std::cout);
    }
  */

  // Run the code now
  chain->EventLoop(1);  /// NOTE THE FUGLY HACK TO 
  chain->Finish();

  // Write histograms to disk
  TFile *file = new TFile(Form("%s/%s.root",path,gy),"recreate");
  file->cd();
  mk->GetHistList()->Print();
      mk->GetHistList()->Write();

  // Write the geometry to disk
  gGeoManager->Write();
   
}

void Load()
{
  gSystem->Load("libVMC.so");
  //  gSystem->Load("libQtRoot.so");
  //  gSystem->Load("libQtRootGui.so");
  gSystem->Load("St_base.so");
  gSystem->Load("St_Tables.so");
  gSystem->Load("StUtilities.so");
  gSystem->Load("StChain.so");
  gSystem->Load("StarVMCApplication.so");
  gSystem->Load("libStarMiniCern.so");
  gSystem->Load("libgeant3.so");
  gSystem->Load("StarBASE.so");
}
