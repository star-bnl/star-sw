// macro to instantiate the Geant3 from within
// STAR  C++  framework and get the starsim prompt
// To use it do
//  root4star starsim.C

/// Helper function to define PDG ids for heavy ions
/// @param z Charge of the heavy ion
/// @param a Atomic number of the heavy ion
/// @param l Number of lambdas in a hypernucleus
Int_t hid( Int_t z, Int_t a, Int_t l=0 )
{
  //         10LZZZAAAI
  return (   1000000000
       +     10000000*l
       +        10000*z
       +           10*a );
}

class St_geant_Maker;
St_geant_Maker *geant_maker = 0;

class StarGenEvent;
StarGenEvent   *event       = 0;

class StarPrimaryMaker;
StarPrimaryMaker *_primary = 0;

class StarKinematics;
StarKinematics *kinematics = 0;

//TF1*  ptDist = 0;
//TF1* etaDist = 0;

TFile* _tagfile = 0; // file containing the tags
TTree* _tags    = 0; // tree containing the tags
double _vxyz[3] = { 0, 0, 0 }; // event vertex
double _sxyz[3] = { 0, 0, 0 }; // additional smearing
int    _runnumber = 0;
int    _evtnumber = 0;
unsigned int    _seed      = 0;
int    _unprimaries = 0;
double _eventtime = 0;
double _prodtime  = 0;
double _magfield  = -5.005;

int     _npart = 10;  // floor number of tracks per event
float   _fpart = 0.05;  // fraction of track multiplicity to embed
int     _pid[50]={1,2,3,8,9,11,12,14,15,10017,10007,45,50045,49,50049,37,61053,61054,62053,62054}; //geant3 particle ID
TString _pnm[50]={"gamma","positron","electron","pi+","pi-","K+","K-","proton","antiproton","eta","pi0","deuteron","antideuteron","he3","antihe3","D0","HyperT_2body","HyperT_bar_2body","HyperT_3body","HyperT_bar_3body"}; // particle names
TString _part  = "pi+"; // particle to simulate, default pi+
TString _part_save;
float   _ptmn  = 0.100; // min pT to simulate [GeV]
float   _ptmx  = 7.000; // max pT to simulate [GeV]
float   _etamn = -2.0;  // min eta to simulate
float   _etamx = +2.0;  // max eta to simulate
bool    _rapiditymode = false; //default is flat in pseudorapidity (eta)
//float   _mnvtx = -5.0;  // min z vertex
//float   _mxvtx = +5.0;  // max z vertex

TString _geometry = "y2014x";
TString DBV;     // If unset, will fill from tag file  "DbV20161018";
TString SDT = "sdt20140610";

const int maxEvents = 1000;


TF1 *ptDist  = 0;
TF1 *etaDist = 0;

//______________________________________________________________________________________
void setRngSeed()
{
  
  TString sid = gSystem->Getenv("JOBID");  sid.Resize(8);  
  TString six = gSystem->Getenv("JOBINDEX");

  unsigned long long id = gROOT->ProcessLine(Form( "0x%s", sid.Data() ) );
  unsigned int id1 = (0xffff&id);
  unsigned int id2 = gROOT->ProcessLine( six.Data() );
  
  StarRandom::seed( _seed = id1 + id2 );
  StarRandom::capture();

}

//______________________________________________________________________________________

void geometry( TString tag, Bool_t agml=true )
{
  TString cmd = "DETP GEOM "; cmd += tag; 
  if ( !geant_maker ) geant_maker = (St_geant_Maker *)chain->GetMaker("geant");
  geant_maker -> LoadGeometry(cmd);
  //  if ( agml ) command("gexec $STAR_LIB/libxgeometry.so");
}

//______________________________________________________________________________________

void command( TString cmd )
{
  if ( !geant_maker ) geant_maker = (St_geant_Maker *)chain->GetMaker("geant");
  geant_maker -> Do( cmd );
}

//______________________________________________________________________________________

void trig( int n=1 )
{

  for ( int i=0; i<n; i++ ) {

    if ( i+1 > maxEvents ) break; 

    // Clear the chain from the previous event
    chain->Clear();

    // Read the ttree
    _tags->GetEntry(i);
    
    cout << "_unprimaries = " << _unprimaries << endl;

    _primary->SetVertex( _vxyz[0], _vxyz[1], _vxyz[2] );
    _primary->SetSigma(0,0,0);

    //
    // Generate 5% of the uncorrected number of primaries recorded in the
    // tags file.  If less than minimum specified, generate the minimum
    //
    int npart ;
    if ( _fpart < 1.0 ) {
	 npart = int(_unprimaries * _fpart) ;
	 if ( npart < _npart ) npart = _npart;
    }
    else npart = int(_fpart);

    if(i==0) _part_save=_part;
    if(_part_save == "antideuteron" || _part_save == "he3" || _part_save == "antihe3"){
	 if(i==0)
	    _part = "deuteron"; //prime the first event with deuteron for nucleus embedding
	 else 
	    _part = _part_save;
    }

    // Print the run and vertex info
    cout << Form("run=%i event=%i seed=%i part=%s npart=%i %i vxyz=%f %f %f",
		 _runnumber,
		 _evtnumber,
		 _seed,
		 _part.Data(),
		 npart, 
		 _unprimaries,
		 _vxyz[0],_vxyz[1],_vxyz[2]) << endl;

    command( Form("RUNG %i %i", _runnumber, _evtnumber-1 ) );     
    chain->SetDateTime( int(_eventtime), int( 100000*(_eventtime-int(_eventtime)) ) ); 
    

    // Standard throw things flat in pt and eta
    if ( 0==ptDist )    kinematics->Kine( npart, _part, _ptmn, _ptmx, _etamn, _etamx );

    // Sample spectrum
    if (    ptDist )    kinematics->Dist( npart, _part, ptDist, etaDist );

#if 0
    //
    // Make sure BField can be updated on event by event
    //
    if (StarMagField::Instance() && StarMagField::Instance()->IsLocked()) {
      float scale = StarMagField::Instance()->GetFactor();
      delete StarMagField::Instance();
      new StarMagField ( StarMagField::kMapped, scale);
      cout << "New mag field.  Don't lock, let it be updated" << endl;
    }
#endif


    // Generate the event
    chain->Make();

    // // Print the event
    _primary->event()->Print();
    // command("gprint kine");
  }
}
//______________________________________________________________________________________


void Kinematics()
{
  
  //  gSystem->Load( "libStarGeneratorPoolPythia6_4_23.so" );
  gSystem->Load( "libKinematics.so");
  kinematics = new StarKinematics();

  //default is flat in eta, switch on flat in 'y'
  if (_rapiditymode){
     kinematics->SetAttr( "rapidity", 1 );
  }

  _primary->AddGenerator(kinematics);
}
//______________________________________________________________________________________

void runEmbeddingSimulation2014( 
     const char* tagfile="/star/data100/GRID/daq/2014/st_physics_adc_15161060_raw_5000043.tags.root", 
     const char* fzdfile,
     unsigned int rngSeed=1234,      
     int nevents=-1)
{ 

  //________________________________________________________
  //
  // Open tagfile from where we will obtain the event vertex
  //
  _tagfile = TFile::Open(tagfile);
  _tags    = (TTree*) _tagfile -> Get("MoreTags");
  //
  _tags->SetBranchAddress( "RunId",       &_runnumber );
  _tags->SetBranchAddress( "EvtId",     &_evtnumber );
  _tags->SetBranchAddress( "VX", &_vxyz[0] );
  _tags->SetBranchAddress( "VY", &_vxyz[1] );
  _tags->SetBranchAddress( "VZ", &_vxyz[2] );
  _tags->SetBranchAddress( "nRefMult", &_unprimaries );

  _tags->SetBranchAddress( "EvtTime",       &_eventtime );
  _tags->SetBranchAddress( "ProdTime",        &_prodtime );
  _tags->SetBranchAddress( "magField",        &_magfield );


  _tags->GetEntry(0); // read in first event

  SDT = Form("sdt%i",int(_eventtime));
  if ( DBV == "" )  DBV = Form("dbv%i",int(_prodtime)); 

  // Determine maximum number of events to process
  if ( nevents < 0 ) nevents = _tags->GetEntries();
  cout << endl;
  cout << "####################################################################" << endl;
  cout << "## Processing nevents = " << nevents << endl;
  cout << "## geometry : " << _geometry.Data() << endl;
  cout << "## sdt timestamp: " << SDT.Data() << endl;
  cout << "## dbv timestamp: " << DBV.Data() << endl;
  cout << "####################################################################" << endl;
  cout << endl;

  //
  //________________________________________________________
  //
  
  //
  //________________________________________________________
  //
  // Setup the big full chain
  //
  //________________________________________________________
  //
  gROOT->ProcessLine(".L bfc.C");
  {
    TString simple = _geometry; simple += " ";
    simple += SDT; simple += " ";
    simple += DBV; simple += " ";
    simple += " geant gstar usexgeom agml misalign newtpcalignment bigbig ";

    bfc(0, simple );
  }

  //
  //________________________________________________________
  //
  // Load in supporting libraries
  //________________________________________________________
  //
  gSystem->Load( "libVMC.so");

  gSystem->Load( "StarGeneratorUtil.so" );
  gSystem->Load( "StarGeneratorEvent.so" );
  gSystem->Load( "StarGeneratorBase.so" );
  gSystem->Load( "StarGeneratorDecay.so" );
  gSystem->Load( "libPythia8_1_62.so");
  gSystem->Load( "libMathMore.so"   );  

  // force gstar load/call... 
  gSystem->Load( "gstar.so" );
  command("call gstar");

  //________________________________________________________
  //
  // Setup RNG seed and map all ROOT TRandom here
  //________________________________________________________
  // 
  //setRngSeed();
  StarRandom::seed( _seed = rngSeed ); // but will reset based on run number and event number
  StarRandom::capture();
  
  //
  // Create the primary event generator and insert it
  // before the geant maker
  //
  //  StarPrimaryMaker *
  _primary = new StarPrimaryMaker();
  {
    _primary -> SetFileName( "kinematics.starsim.root");
    chain -> AddBefore( "geant", _primary );
  }


  Kinematics();

#if 0 
  // Only for nucleus listed below
  // Load STAR Particle DataBase and add the hypertriton definitions.  Map to the
  // decay modes as defined in gstar_part.g 
  StarParticleData &pdb = StarParticleData::instance();
  pdb.AddParticle("HyperT_2body",     new TParticlePDG( "HyperpT_2body",     "HyperTriton     --> He3    pi-", 2.99131, false, 0.0, +3.0, "hypernucleus", +hid(1,3,1), 0, 61053 ));
  pdb.AddParticle("HyperT_bar_2body", new TParticlePDG( "HyperT_bar_2body",  "AntiHyperTriton --> He3bar pi+", 2.99131, false, 0.0, -3.0, "hypernucleus", -hid(1,3,1), 0, 61054 ));
  pdb.AddParticle("HyperT_3body",     new TParticlePDG( "HyperT_3body",      "HyperTriton     --> d p pi-",    2.99131, false, 0.0, +3.0, "hypernucleus", +hid(1,3,1), 0, 62053 ));
  pdb.AddParticle("HyperT_bar_3body", new TParticlePDG( "HyperT_bar_3body",  "AntiHyperTriton --> dbar pbar pi+", 2.99131, false, 0.0, -3.0, "hypernucleus", -hid(1,3,1), 0, 62054 ));
  // Hypertriton will be phase-space decayed by geant 
  pdb.AddParticle("deuteron", new TParticlePDG( "deuteron",  "Deuteron", 1.876, true, 0.0, 3.0, "hion", hid(1,2,0), 0, 45 ));
  pdb.AddParticle("antideuteron", new TParticlePDG( "antideuteron",  "anti Deuteron", 1.876, true, 0.0, -3.0, "hion", -hid(1,2,0), 0, 50045 ));
  pdb.AddParticle("he3", new TParticlePDG( "he3",  "Helium-3", 2.809, true, 0.0, 6.0, "hion", hid(2,3,0), 0, 49 ));
  pdb.AddParticle("antihe3", new TParticlePDG( "antihe3",  "anti Helium-3", 2.809, true, 0.0, -6.0, "hion", -hid(2,3,0), 0, 50049 ));

#endif

#if 1 
  //
  // Setup decay manager
  //
  StarDecayManager   *decayMgr = AgUDecay::Manager();
  StarPythia8Decayer *decayPy8 = new StarPythia8Decayer();
  decayMgr->AddDecayer(    0, decayPy8 ); // Handle any decay requested 
  decayPy8->SetDebug(1);
  //  decayPy8->Set("WeakSingleBoson:all = on");

  // Particle data
  StarParticleData& data = StarParticleData::instance();

  if(_part == "D0"){
     //  One can add a particle to G3 using...
     data.AddParticleToG3( "D0", 0.186483E+01, 0.41010E-12, 0., 3, 421, 37, 0, 0 );
     TParticlePDG* D0     = data.GetParticle("D0");    
     D0->Print();
     //
     // Set D0 decay to K+ pi- mode (or cc).
     //
     decayPy8->Set("421:onMode = off");
     decayPy8->Set("421:onIfMatch = 211 321");
  }
  if(_part == "eta"){
     data.AddParticleToG3( "eta",5.47853e-01, 0.50244E-18, 0., 3, 221, 17, 0, 0 );
     TParticlePDG* eta    = data.GetParticle("eta");    
     eta->Print();
     //
     // Set eta Dalitz decay to gamma e- e+ mode.
     //
     decayPy8->Set("221:onMode = off");
     decayPy8->Set("221:onIfMatch = 22 11 -11");
  }
  if(_part == "pi0"){
     data.AddParticleToG3( "pi0",1.34977e-01, 0.85200E-16, 0., 3, 111, 7, 0, 0 );
     TParticlePDG* pi0    = data.GetParticle("pi0");    
     pi0->Print();
     //
     // Set pi0 Dalitz decay to gamma e- e+ mode.
     //
     decayPy8->Set("111:onMode = off");
     decayPy8->Set("111:onIfMatch = 22 11 -11");
  }
  if(_part == "deuteron" || _part == "antideuteron"){
     data.AddParticleToG3( "deuteron", 0.1876E+01, 0.10000E+16, 1., 8, 1000010020, 45, 0, 0 );
     TParticlePDG* deuteron     = data.GetParticle("deuteron");    
     deuteron->Print();
  }
  if(_part == "antideuteron"){
     data.AddParticleToG3( "antideuteron", 0.1876E+01, 0.10000E+16, -1., 8, -1000010020, 50045, 0, 0 );
     TParticlePDG* antideuteron     = data.GetParticle("antideuteron");    
     antideuteron->Print();
  }

#endif

  //
  // Initialize primary event generator and all sub makers
  //
  _primary -> Init();

  //
  // Setup geometry and set starsim to use agusread for input
  //
  geometry(Form( " field=%f %s", _magfield, _geometry.Data()));
  command("gkine -4 0");
  
  /*
  TString outputFile = gSystem->BaseName( _tagfile->GetName() );
  outputFile.ReplaceAll(".tags.root",".fzd");

  command(Form( "gfile o %s", outputFile.Data()));
  */
  command(Form( "gfile o %s", fzdfile));
 
  //
  // Trigger on nevents
  //
  trig( nevents );

  command("call agexit");  // Make sure that STARSIM exits properly

}

//______________________________________________________________________________________

void Validate()
{
  StarGenEvent*    event = _primary->event();
  StarGenParticle* part  = (*event)[1];
  part->Print();
  double Etotal = part->GetEnergy();

  // loop over MC particles
  TTable* gtable = (TTable* ) chain->GetDataSet("g2t_track");
  assert(gtable);
  int     ntable = gtable->GetNRows();

  //  gtable->Print(0,ntable);
  return;

  double Etest = 0;
  for ( int itable=1; itable<ntable; itable++ )
    {
      g2t_track_st* track = (g2t_track_st*)gtable->At(itable);
      if ( 0 == track->eg_label ) {       Etest += track->e; }
    }

  cout << "Egener = " << Etotal << endl;
  cout << "Egeant = " << Etest  << endl;
  cout << "Violation = " << 100*(Etest / Etotal - 1) << "%" << endl;
  
}
//______________________________________________________________________________________

void runEmbeddingSimulation2014( 
			     const int ne, 
			     const unsigned int seed,
			     const char* fzdfile, 
			     const char* tagfile,
			     const float mult, 
			     const int   pid,
			     const float ptmn,
			     const float ptmx,
			     const float etamn,
			     const float etamx,
			     const char* dbv = 0)
{
  _fpart = mult;
  _ptmn  = ptmn;
  _ptmx  = ptmx;
  _etamn = etamn;
  _etamx = etamx;
  if ( dbv ) DBV = dbv;

  int indx = 0;
  for (indx=0;indx<50;indx++) {
     if ( _pid[indx] == pid ) {
	  _part = _pnm[indx];
	  break;
     }
  }
  if ( indx == 50 ) cout<<"WRONG GeantID: "<<pid<<" !!!"<<endl;

  //TFile* mHistFile = new TFile("Input/D0_weight_pt.root");
  //If Particle name has D0, use distributions
  if ( _part.Contains("D0") )
  {
     ptDist = new TF1("ptDist","(1/[0])/(1+(x/[0])^2)^[1]",_ptmn,_ptmx);
     ptDist->SetParameter(0, 3.0);
     ptDist->SetParameter(1, 5.0);
     //ptDist  = (TF1 *) mHistFile->Get("D0_weight_pt");
     etaDist = new TF1("etaDist","1.0",_etamn,_etamx);

     ptDist->Print();
     etaDist->Print();

  }
  runEmbeddingSimulation2014( tagfile, fzdfile, seed, ne );

  //mHistFile->Close();
}
//______________________________________________________________________________________

