/**!
 * Example macro for running an event generator in standalone mode.
 *
 * Usage:
 *
 * ln -s starsim.reader.C reader.C
 *
 * root4star
 * .L reader.C
 * int nevents=100;
 * reader( nevents )
 *
 * or
 *
 * root4star -q -b reader.C 
 *
 */

class St_geant_Maker;
St_geant_Maker *geant_maker = 0;

class StarGenEvent;
StarGenEvent   *event       = 0;

class StarPrimaryMaker;
StarPrimaryMaker *_primary = 0;

class StarGenEventReader;
StarGenEventReader *eventreader = 0;


// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void reader( Int_t nevents=1, UInt_t rngSeed = 12345 )
{ 

  gROOT->ProcessLine(".L bfc.C");
  {
    TString simple = "tables nodefault";
    bfc(0, simple );
  }

  gSystem->Load( "libVMC.so");
  gSystem->Load( "St_g2t.so" );
  gSystem->Load( "St_geant_Maker.so" );
 
  gSystem->Load( "StarGeneratorUtil.so" );
  gSystem->Load( "StarGeneratorEvent.so" );
  gSystem->Load( "StarGeneratorBase.so" );

  gSystem->Load( "libMathMore.so"   );  
  gSystem->Load( "libStarGenEventReader.so" );


  eventreader = new StarGenEventReader();
  eventreader -> SetInputFile("pythia_jet_vz0_run1.genevent.root","genevents","primaryEvent");


  chain->Clear();
  chain->Make();

  StarGenEvent* event = eventreader->Event();


  std::cout << "GENERATOR ID  = " << event->GetGeneratorId() << std::endl;
  std::cout << "PROCESS ID    = " << event->GetProcessId() << std::endl;
  std::cout << "RUN NUMBER    = " << event->GetRunNumber() << std::endl;
  std::cout << "EVENT NUMBER  = " << event->GetEventNumber() << std::endl;
  std::cout << "NUM PARTICLES = " << event->GetNumberOfParticles() << std::endl;

  StarGenParticle* particle;
  TIter& Next = event->IterAll();
  while( (particle = (StarGenParticle*)Next() ) ) {
    particle->Print();
  }

  


}
// ----------------------------------------------------------------------------

