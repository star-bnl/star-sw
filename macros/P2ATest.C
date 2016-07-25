// Author V.Fine 06/12/2001 BNL mailto:fine@bnl.gov
// This run Pythia using ROOT TPYthia6 interface
// Thanks Michael Bussmann <Michael.Bussmann@physik.uni-muenchen.de> for Pythia living example
#ifndef __CINT__
# include <stdlib.h>
# include <ostream.h>
# include <TROOT.h>
# include <TRint.h>
# include <TApplication.h>
# include <TFile.h>
# include <TMCParticle.h>
# include <TObjArray.h>
# include <TPythia6.h>
# include <TTree.h>

#endif

 // From $include $ROOTSYS/include/TPythia6Calls.h
  struct Pyjets_t {
    int    N;
    int    NPAD;
    int    K[5][4000];
    double P[5][4000];
    double V[5][4000];
  };

  struct pythia_particle {
        Int_t status;        // status of particle          ( LUJETS K[1] )
        Int_t pdg_id;        // flavour code                ( LUJETS K[2] )
        Int_t parent;        // parrent's id                ( LUJETS K[3] )
        Int_t firstChild;    // id of first child           ( LUJETS K[4] )
        Int_t lastChild;     // id of last  child           ( LUJETS K[5] )
        Float_t momentum[4]; // X.Y,Z,energy momenta [GeV/c]( LUJETS P[1]=P[4] )
        Float_t mass;        // Mass      [Gev/c^2]         ( LUJETS P[5] )
        Float_t vertex[4];   // X,Y,Z vertex  [mm]; time of procuction [mm/c]( LUJETS V[1]-V[4] )
        Float_t lifetime;    // proper lifetime [mm/c]      ( LUJETS V[5] )
  };

TDataSet *fillathenaEvent(TPythia6 &pythia);
int P2Test(int nEvent=5, int compress=1);
//____________________________________________________________________________
int P2Test(int nEvent, int compress)
{

  LoadPythia();
  TStopwatch fulltime;
  Int_t i;
  Int_t NEvents;
  Int_t startdecay;
  Int_t enddecay;
    
  NEvents     = nEvent;
  
  TFileIter MCFile("pythia.root","RECRETE","pythia.root", compress);
  TPythia6 Pythia;
      
  //pythia switches

  Pythia.SetMSEL(0);               //set subprocesses by hand
  Pythia.SetMSTP(48, 1);           //t->W+b
  Pythia.SetMSTJ(11, 3);           //select fragmentation function "Bowler"
  Pythia.SetPARJ(50+4, -0.07);     //peterson parameter for charm
  Pythia.SetPARJ(50+5, -0.006);    //peterson parameter for bottom
  Pythia.SetPARJ(50+5, -0.000001); //peterson parameter for top
  Pythia.SetPMAS(6, 1, 174.3);     //top mass
  Pythia.SetPMAS(25, 1, 120.0);    //higgs mass
  Pythia.SetMSTP(61, 1);           //ISR "on"
  Pythia.SetMSTP(71, 1);           //FSR "on"
  Pythia.SetMSTP(81, 1);           //multiple interactions "on"
  Pythia.SetMSTP(111, 1);          //fragmentation "on"
  Pythia.SetMSTP(82, 3);           //multiple interaction "vary impact param"
  Pythia.SetPARP(82, 2.41);        //cut-off pt for multiple interaction
  Pythia.SetMRPY(1, 88158204);     //random seed number
  
  Pythia.SetMSUB(26, 1);           //set subprocess WH
  Pythia.SetMDCY(24, 1, 1);        //W decaying
  Pythia.SetMDCY(25, 1, 1);        //H decaying
  
  //close all W decay channels

  startdecay = Pythia.GetMDCY(24, 2);
  enddecay   = Pythia.GetMDCY(24, 2) + Pythia.GetMDCY(24, 3);  
  for ( i=startdecay; i<enddecay; i++ ) Pythia.SetMDME(i, 1, 0);

  //close all H decay channels

  startdecay = Pythia.GetMDCY(25, 2);
  enddecay   = Pythia.GetMDCY(25, 2) + Pythia.GetMDCY(25, 3);  
  for ( i=startdecay; i<enddecay; i++ ) Pythia.SetMDME(i, 1, 0);

  //open wanted W decay channels
  
  Pythia.SetMDME(206, 1, 1);       //W->e+nu
  Pythia.SetMDME(207, 1, 1);       //W->mu+nu
  Pythia.SetMDME(208, 1, 1);       //W->tau+nu

  //open wanted W decay channels
  
  Pythia.SetMDME(214, 1, 1);       //H->b+bbar
  
  //initialize Pythia for D0

  Pythia.Initialize("CMS", "p", "pbar", 2000.0);

// --
// Initilaise athena-compliant ROOT I/O
// --

  //generate events

  TStopwatch ioTime;
  ioTime.Stop();
  for ( i=0; i<NEvents; i++ ) {

    if ( i%10 == 0 ) cout << "Event No.: " << i << endl;

    Pythia.GenerateEvent();
// --
// -- Conversion Pythia event to Athena/Root event
// --

// Create "MCEvent" object
    ioTime.Start(kFALSE);
    TDataSet *mcEvent = fillathenaEvent(Pythia);

// Write the "whole" event into ROOT file
    int runNumber = 777;
    int eventNumber = i;
    MCFile.NextEventPut(mcEvent,runNumber,eventNumber);

// delete this "event"
    delete mcEvent;    
    ioTime.Stop();
  }
 printf(" Full time: "); fulltime.Print();
 printf(" I/O time:  "); ioTime.Print();
 printf("\nUsage: root LoadPythia.C 'P2Test.C(nEvent)'\n");
 printf(  "-----  where nEvent - the total number of the events to generate\n");
}

//____________________________________________________________________________
TDataSet *fillathenaEvent(TPythia6 &pythia) 
{
 //Create a TPythia6* object Pythia and do some settings for Pythia

  Pyjets_t* pythiaParticle = pythia.GetPyjets();
  Int_t nParticle = pythia.GetN();
  TGenericTable *particles = new TGenericTable("pythia_particle",nParticle);
  for (int i=0;i<nParticle;i++)
  {
    pythia_particle particle;
    memset(&particle,0,sizeof(particle));
    particle.momentum[0] = pythiaParticle->P[0][i]; // px
    particle.momentum[1] = pythiaParticle->P[1][i]; // px
    particle.momentum[2] = pythiaParticle->P[2][i]; // px
    particle.momentum[3] = pythiaParticle->P[3][i]; // energy
    particle.mass        = pythiaParticle->P[4][i]; // mass
     
    particle.status      = pythiaParticle->K[0][i]; // kS
    particle.pdg_id      = pythiaParticle->K[1][i]; // kF
    particle.parent      = pythiaParticle->K[2][i]; // parent
    particle.firstChild  = pythiaParticle->K[3][i]; // firstchild
    particle.lastChild   = pythiaParticle->K[4][i]; // lastchild

    particle.vertex[0]   = pythiaParticle->V[0][i]; // vx
    particle.vertex[1]   = pythiaParticle->V[1][i]; // vy
    particle.vertex[2]   = pythiaParticle->V[2][i]; // vz
    particle.vertex[3]   = pythiaParticle->V[3][i]; // time

    particle.lifetime    = pythiaParticle->V[4][i]; // lifetime
    particles->AddAt(&particle);
  }
  TDataSet *mcEvent = new TDataSet("MCEvent");
  mcEvent->Add(particles);
  return mcEvent;  // caller has to delete this object;
}
