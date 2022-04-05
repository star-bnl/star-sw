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

class StarHijing;
StarHijing *hijing = 0; 

class StarKinematics;
StarKinematics* kine = 0; 

//____________________________________________________________
void geometry( TString tag, bool agml=true )
{
  TString cmd = "DETP GEOM "; cmd += tag;
  if ( !geant_maker ) geant_maker = (St_geant_Maker *)chain->GetMaker("geant");
  geant_maker -> LoadGeometry(cmd);
  
  //  if ( agml ) command("gexec $STAR_LIB/libxgeometry.so");
}
//____________________________________________________________
void command( TString cmd )
{
  if ( !geant_maker ) geant_maker = (St_geant_Maker *)chain->GetMaker("geant");
  geant_maker -> Do( cmd );
}
//____________________________________________________________
bool accept( TString table ) {
  if ( table.Contains("_hit") ) return true;
  if ( table.Contains("g2t_track") ) return true;
  if ( table.Contains("g2t_vertex") ) return true; 
}
//____________________________________________________________
void trig( int n=0 )
{
  int stat = 0;

  HiMain1_t& himain1 = hijing->himain1(); 

  // Histograms will be created in TObjArray for every found hit table
  TObjArray hists;

  for ( int i=0; i<n+1; i++ ) {
    chain->Clear();


    stat = chain->Make();
    if ( stat ) break; // EOF or...?

    int np = himain1.nwounded_yell;
    int nt = himain1.nwounded_blue;
    int npart = np+nt; 

    // Embed muons in proportion to number of participants 
    kine->Kine( npart/10, "mu-,mu+", 1.0, 20.0, -0.625, 0.625 );

    TDataSet*     gData  = chain->GetDataSet("geant");   if (0==gData) continue;
    TDataSetIter  gIter(gData);

    TTable* table = (TTable*)gIter.Next();
    while ( table ) {
      TString name = table->GetName();
      // Accumulate number of  hits, vertexs and tracks
      if ( accept(name) ) {
	TH1F* h = hists.FindObject(Form("num_%s",name.Data()));
	if ( 0==h ) {
	  h = new TH1F(Form("num_%s",name.Data()),Form("Size of table [%s] / n_participants ",name.Data()),100,0.,100.);
	  h->SetBit(TH1::kCanRebin);
	  hists.Add(h);
	}	
	// Fill number of hits
	h->Fill( table->GetNRows() / npart );
      }
      table = (TTable*)gIter.Next();
    }

  }

  // Iterate over all histograms and dump average and RMS
  const TObjArray* p = &hists;
  TObjArrayIter Iter( p );

  TH1F* nhits = (TH1F*)Iter();
  while ( nhits ) {

    std::cout << "STARSIM NIGHTLY QA: " << nhits->GetTitle() << " nhits = " << nhits->GetMean() << " rms = " << nhits->GetRMS() << std::endl;

    nhits= (TH1F*)Iter();

  }

  


}
//____________________________________________________________
void Kinematics()
{
  
  //  gSystem->Load( "libStarGeneratorPoolPythia6_4_23.so" );
  gSystem->Load( "libKinematics.so");
  kine = new StarKinematics();
    
  _primary->AddGenerator(kine);
}
void Hijing()
{
  hijing = new StarHijing(); 
  hijing->SetTitle("Hijing 1.383");

  // Setup collision frame, energy and beam species
  hijing->SetFrame("CMS",200.0);
  hijing->SetBlue("Au");
  hijing->SetYell("Au");  

  // Fixed impact parameter of 20
  //  hijing->SetImpact(19.9999, 20.0001);       // Impact parameter min/max (fm)    0.   30.
  hijing->SetImpact(10.0, 11.0);       // Impact parameter min/max (fm)    0.   30.

  _primary -> AddGenerator(hijing);

  // Keep it in the central region
  _primary -> SetCuts( 1.0E-6 , -1., -4.25, +4.25 );
  
}
//____________________________________________________________
//void starsim( int nevents=10,int rngSeed=1234, const char* tag="y2018" )
void starsim_nightly_test( const char* tag="y2012a", int nevents=5 )
{ 

  gROOT->ProcessLine(".L bfc.C");
  {
    TString simple = Form("%s geant gstar usexgeom agml ",tag);
    bfc(0, simple );
  }

  gSystem->Load( "libVMC.so");

  gSystem->Load( "StarGeneratorUtil.so" );
  gSystem->Load( "StarGeneratorEvent.so" );
  gSystem->Load( "StarGeneratorBase.so" );
  gSystem->Load( "libMathMore.so"   );  
  gSystem->Load( "libHijing1_383.so");
  gSystem->Load( "xgeometry.so"     );

  // Setup RNG seed and map all ROOT TRandom here
  StarRandom::seed( 12345 ); // fixed seed for nightly tests
  StarRandom::capture();
  
  //
  // Create the primary event generator and insert it
  // before the geant maker
  //
  _primary = new StarPrimaryMaker();
  {
    _primary -> SetFileName( "hijing.starsim.root");
    chain -> AddBefore( "geant", _primary );
  }


  //
  // Setup an event generator
  //
  Kinematics(); 
  Hijing();

  //
  // Initialize primary event generator and all sub makers
  //
  _primary -> Init();

  // Configure HIJING simulation
  HiParnt_t &hiparnt = hijing->hiparnt();
  {
    hiparnt.ihpr2(4) = 0;     // Jet quenching (1=yes/0=no)       0
    hiparnt.ihpr2(3) = 0;     // Hard scattering (1=yes/0=no)
    //hiparnt.hipr1(10) = -2.5;  //    pT jet (negative indicates lower limit)
    hiparnt.ihpr2(8)  = 10;   // Max number of jets / nucleon
    hiparnt.ihpr2(11) = 1;    // Set baryon production
    hiparnt.ihpr2(12) = 1;    // Turn on/off decay of particles [1=recommended]
    hiparnt.ihpr2(18) = 0;    // 1=B quark production.  0=C quark production.
    hiparnt.hipr1(7) = 5.35;  // Set B production ???? Not really used... Really ????

  // For more configuration options, see the HIJING manual
  // http://ntc0.lbl.gov/~xnwang/hijing/doc.html
  }
  //
  // Setup geometry and set starsim to use agusread for input
  //
  //geometry("y2012");
  command("gkine -4 0");
  //  command("gfile o hijing.starsim.fzd");


  
  //
  // Trigger on nevents
  //
  trig( nevents );

  //  command("gprint kine");

  //  command("call agexit");  // Make sure that STARSIM exits properly

}
//____________________________________________________________

