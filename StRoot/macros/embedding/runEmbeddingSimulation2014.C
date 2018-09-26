// macro to instantiate the Geant3 from within
// STAR  C++  framework and get the starsim prompt
// To use it do
//  root4star starsim.C
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

int     _npart = 10;  // floor number of tracks per event
float   _fpart = 0.05;  // fraction of track multiplicity to embed
int     _pid[50]={1,2,3,8,9,11,12,14,15,10017,10007}; //geant3 particle ID
TString _pnm[50]={"gamma","positron","electron","pi+","pi-","K+","K-","proton","antiproton","eta","pi0"}; // particle names
TString _part  = "pi+"; // particle to simulate, default pi+
float   _ptmn  = 0.100; // min pT to simulate [GeV]
float   _ptmx  = 7.000; // max pT to simulate [GeV]
float   _etamn = -2.0;  // min eta to simulate
float   _etamx = +2.0;  // max eta to simulate
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

  //
  // Setup decay manager
  //
  StarDecayManager   *decayMgr = AgUDecay::Manager();
  StarPythia8Decayer *decayPy8 = new StarPythia8Decayer();
  decayMgr->AddDecayer(    0, decayPy8 ); // Handle any decay requested 
  decayPy8->SetDebug(1);
  //  decayPy8->Set("WeakSingleBoson:all = on");




  TString name;
  double mass, lifetime, charge;
  int tracktype, pdgcode, g3code;

#if 1
  // Particle data
  StarParticleData& data = StarParticleData::instance();
  //  One can add a particle to G3 using...
  data.AddParticleToG3( "D0", 0.186483E+01, 0.41010E-12, 0., 3, 421, 37, 0, 0 );
  //data.AddParticleToG3( "eta",5.47853e-01, 0.50244E-18, 0., 3, 221, 17, 0, 0 );
  //data.AddParticleToG3( "pi0",1.34977e-01, 0.85200E-16, 0., 3, 111, 7, 0, 0 );

  TParticlePDG* D0     = data.GetParticle("D0");    
  D0->Print();
  //TParticlePDG* eta    = data.GetParticle("eta");    
  //eta->Print();
  //TParticlePDG* pi0    = data.GetParticle("pi0");    
  //pi0->Print();

  //
  // Set D0 decay to K+ pi- mode (or cc).
  //
  decayPy8->Set("421:onMode = off");
  decayPy8->Set("421:onIfMatch = 211 321");

  //
  // Set eta Dalitz decay to gamma e- e+ mode.
  //
  //decayPy8->Set("221:onMode = off");
  //decayPy8->Set("221:onIfMatch = 22 11 -11");

  //
  // Set pi0 Dalitz decay to gamma e- e+ mode.
  //
  //decayPy8->Set("111:onMode = off");
  //decayPy8->Set("111:onIfMatch = 22 11 -11");
#endif


  //
  // Initialize primary event generator and all sub makers
  //
  _primary -> Init();

  //
  // Setup geometry and set starsim to use agusread for input
  //
  geometry(Form( " field=-5.005 %s", _geometry.Data()));
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
  
  // // Particle is a D0 use distributions
  if ( _part.Contains("D0") )
    {
      ptDist = new TF1("ptDist","(1/[0])/(1+(x/[0])^2)^[1]",_ptmn,_ptmx);
      ptDist->SetParameter(0, 3.0);
      ptDist->SetParameter(1, 5.0);
      etaDist = new TF1("etaDist","1.0",_etamn,_etamx);

      ptDist->Print();
      etaDist->Print();

    }
  runEmbeddingSimulation2014( tagfile, fzdfile, seed, ne );
}
//______________________________________________________________________________________

