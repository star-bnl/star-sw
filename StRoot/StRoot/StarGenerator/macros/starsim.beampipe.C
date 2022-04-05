/*
 * Example macro for generating events distriuted along a simplified (i.e. all Al)
 * beam pipe.  We sample the vertex distribution w/in the macro and set it event-by-
 * event.
 *
 * TODO:  Provide a class which enables users to pass in arbitrary compiled functors,
 *        TF1, histograms, etc... to simulate vertex distributions...
 *
 */

class St_geant_Maker;
St_geant_Maker *geant_maker = 0;

class StarGenEvent;
StarGenEvent   *event       = 0;

class StarPrimaryMaker;
StarPrimaryMaker *_primary = 0;

class StarHijing;
StarHijing *hijing = 0;

TString  material = "Al"; // Section of beam pipe

const Double_t inch = 2.54;

Double_t bp_oradius = 1.125;        // 1cm radius beam pipe
Double_t bp_iradius = 0.975;        // 0.25 cm thickness beam pipe
Double_t bp_startz = -55.21 * inch; //
Double_t bp_endz   = +55.21 * inch; //

TH2F *hXY = 0; // XY vertex
TH1F *hZ  = 0; //  Z vertex
TH1F *hPT = 0; // PT distribution
TH1F *hPz = 0; // Pz distribution

/*
            PIPI,         <!--Mother volume of the middle section Placed in IDSM-->  
            PIHI,         <!--Hole inside the beam pipe of middle section-->   
            PALS,         <!--East aluminium part-->   
            PBES,         <!--Berillium  part-->       
            PALI,         <!--West aluminium part-->   
            SSCF,         <!-- Stainless Steel conflat flanges (ID 2 cm -->  
            SSCG,         <!-- Stainless Steel conflat flanges (ID 3 inches -->    

PALS section
     <Shape     type = "Pcon"
	       nz   = "6" phi1="0" dphi="360"
	       zi   = "{-55.46*inch, -55.21*inch, 
                        -55.21*inch, -43.71*inch, 
                        -43.71*inch, -15.75*inch}" 
                 
	       rmn  = "{0.7875*inch, 0.7875*inch,
                        0.7875*inch, 0.7875*inch, 
                        0.7875*inch, 0.7875*inch}"
	       rmx  = "{0.7875*inch+0.5875*inch, 0.7875*inch+0.5875*inch, 
                        0.7875*inch+0.065*inch, 0.7875*inch+0.065*inch, 
                        0.7875*inch+0.055*inch, 0.7875*inch+0.055*inch}" />
*/

/*
PBES
     <Shape type="TUBE" rmin="0.7875*inch" rmax="0.7875*inch + 0.030*inch" dz="47.25*INCH/2" />
   zoffset = + 7.875 inches
*/




/*    

PALI
     <Shape     type = "Pcon"
	       nz   = "6" phi1="0" dphi="360"
	       zi   = "{ 31.5 *inch,  43.72*inch, 
                         43.72*inch,  55.21*inch, 
                         55.21*inch,  55.46*inch }"

	       rmn  = "{0.7875*inch, 0.7875*inch,  
                        0.7875*inch, 0.7875*inch, 
                        0.7875*inch, 0.7875*inch}"
	       rmx  = "{0.7875*inch+0.055*inch, 0.7875*inch+0.055*inch, 
                        0.7875*inch+0.065*inch, 0.7875*inch+0.065*inch, 
                        0.7875*inch+0.5875*inch, 0.7875*inch+0.5875*inch}" />


 */

// ----------------------------------------------------------------------------
void geometry( TString tag, Bool_t agml=true )
{
  TString cmd = "DETP GEOM "; cmd += tag;
  if ( !geant_maker ) geant_maker = (St_geant_Maker *)chain->GetMaker("geant");
  geant_maker -> LoadGeometry(cmd);
  //  if ( agml ) command("gexec $STAR_LIB/libxgeometry.so");
}
// ----------------------------------------------------------------------------
void command( TString cmd )
{
  if ( !geant_maker ) geant_maker = (St_geant_Maker *)chain->GetMaker("geant");
  geant_maker -> Do( cmd );
}
// ----------------------------------------------------------------------------

void vertex()
{

  bp_iradius = 0.7875;

  // Sample z 
 REDO:
  Double_t z = StarRandom::Instance().flat(bp_startz,  bp_endz );

  // Carve out Be section of the beam pipe


  // Inner radius is pretty much constant
  bp_iradius = 0.7875 * inch;

  if ( TMath::Abs(z) < 55.21 * inch ) /* Al */  bp_oradius = bp_iradius + 0.065 * inch;
  if ( TMath::Abs(z) < 43.71 * inch ) /* Al */  bp_oradius = bp_iradius + 0.055 * inch;
  if (z>-15.75 *inch && z<31.5*inch ) /* Be */  bp_oradius = bp_iradius + 0.030 * inch;



  // Sample radius (hack)
  Double_t r = StarRandom::Instance().flat( bp_iradius, bp_oradius);

  // Sample circle
  Double_t phi = StarRandom::Instance().flat( 0., TMath::TwoPi() );
  TVector2 circle;
  circle.SetMagPhi( r, phi );
  circle.Print();
  Double_t x = circle.X();
  Double_t y = circle.Y();

  _primary->SetSigma ( 0, 0, 0, 0);
  _primary->SetVertex( x, y, z );
  

};
// ----------------------------------------------------------------------------
void trig( Int_t n=0 )
{
  for ( Int_t i=0; i<n+1; i++ ) {
    chain->Clear();
    // Set vertex
    vertex();
    chain->Make();

    DoHist();

  }
}
// ----------------------------------------------------------------------------
void DoHist()
{
  TDataSet      *geant = chain->DataSet("geant"); // Get geant dataset
  TDataSetIter   Iter(geant);
  St_g2t_track  *trackTable  = Iter.Find("g2t_track");
  St_g2t_vertex *vertexTable = Iter.Find("g2t_vertex");
  if (!trackTable) return;
  if (!vertexTable) return; // but should probably puke here...
  Int_t nt = trackTable->GetNRows();
  Int_t nv = vertexTable->GetNRows();

  g2t_track_st  *track = trackTable->GetTable();
  g2t_vertex_st *vertex = vertexTable->GetTable();

  for ( Int_t i=0;i<nt; i++, track++ )
    {
      Float_t px = track->p[0];
      Float_t py = track->p[1];
      Float_t pz = track->p[2];
      hPz -> Fill( pz );
      hPT -> Fill( TMath::Sqrt( px*px + py*py ) );
    }

  nv=1; /* only first */  for ( Int_t i=0;i<nv; i++, vertex++ )
    {
      Float_t x = vertex->ge_x[0];
      Float_t y = vertex->ge_x[1];
      Float_t z = vertex->ge_x[2];
      hXY -> Fill( x, y );
      hZ  -> Fill( z );
    }

}
// ----------------------------------------------------------------------------
void Hijing()
{
  hijing = new StarHijing("hijing");
  hijing->SetTitle("Hijing 1.383");

  Bool_t blue = true;
  Bool_t yell = !blue;

}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void starsim( const Char_t *basename="rcf14000", 
	      Int_t setnum=1000,
	      Int_t nevents=10)
{ 

  TString name = basename;
  Float_t energy = 0.0;
  if ( setnum<3000 ) energy = -7.25;
  if ( setnum<2000 ) energy = +7.25;

  name += Form("_%i_%ievts",setnum,nevents);
  

  Int_t rngSeed = setnum;

  hXY = new TH2F("hXY","Y_{vertex} vs X_{vertex}",101,-2.525,2.525,100,-2.5,2.5);
  hZ  = new TH1F("hZ", "Z_{vertex}",101,-101.,+101.);
  hPT = new TH1F("hPT","p_{T} [GeV]",100,0.,10.);
  hPz = new TH1F("hPz","p_{z} [GeV]",101,-50.5,50.5);

  gROOT->ProcessLine(".L bfc.C");
  {
    TString simple = "y2014 geant gstar usexgeom agml ";
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
  StarRandom::seed( rngSeed );
  StarRandom::capture();
  
  //
  // Create the primary event generator and insert it
  // before the geant maker
  //
  _primary = new StarPrimaryMaker();
  {
    _primary -> SetFileName( Form("%s.genevents.root",name.Data()) );
    chain -> AddBefore( "geant", _primary );
  }


  //
  // Setup an event generator
  //
  Hijing();

  // Setup collision frame, energy and beam species
  hijing->SetFrame("FIXT", energy); // 7.25 GeV incident beam on beam pipe
  hijing->SetBlue("Au");  // The BEAM
  hijing->SetYell("Al");  // The TARGET

  // For more configuration options, see the HIJING manual
  // http://ntc0.lbl.gov/~xnwang/hijing/doc.html

  _primary -> AddGenerator(hijing);
  _primary -> SetCuts( 1.0E-6 , -1., -2.5, +2.5 );




  //
  // Initialize primary event generator and all sub makers
  //
  _primary -> Init();

  hijing->SetImpact(0.0, 30.0);       // Impact parameter min/max (fm)    0.   30.
  hijing->hiparnt().ihpr2(4)  = 0;    // Jet quenching (1=yes/0=no)       0
  hijing->hiparnt().ihpr2(3)  = 0;    // Hard scattering (1=yes/0=no)
  hijing->hiparnt().hipr1(10) = 2.0;  //    pT jet
  hijing->hiparnt().ihpr2(8)  = 10;   // Max number of jets / nucleon
  hijing->hiparnt().ihpr2(11) = 1;    // Set baryon production
  hijing->hiparnt().ihpr2(12) = 1;    // Turn on/off decay of particles [1=recommended]
  hijing->hiparnt().ihpr2(18) = 1;    // Turn on/off B production
  hijing->hiparnt().hipr1(7)  = 5.35; // Set B production ???? Not really used... Really ????
  hijing->hiparnt().ihpr2(10) = 0;    // show error msgs (1=show, 0=no)

  //
  // Setup geometry and set starsim to use agusread for input
  //
  command("gkine -4 0");
  //  command("gfile o hijing.starsim.fzd");
  command( Form("gfile o %s.fzd",name.Data()) );
  
  //
  // Trigger on nevents
  //
  trig( nevents );
  command("gprint kine");

  //  command("call agexit");  // Make sure that STARSIM exits properly

}
// ----------------------------------------------------------------------------

