#include <AliHLTTPCCAGBTracker.h>
#include <AliHLTTPCCATracker.h>

#include <fstream>
#include <iostream>
#include <iomanip>
#include <cstdio>
#include <cstring>

#ifndef HLTCA_STANDALONE
#include <AliHLTTPCCAPerformance.h>
#endif

#ifdef DRAW3
#define MAIN_DRAW
#endif //DRAW3
#ifdef DRAW2
  #define MAIN_DRAW
#endif //DRAW
#ifdef DRAW
  #define MAIN_DRAW
#endif //DRAW

static int kEvents;
static bool SAVE = false;
extern bool SINGLE_THREADED;
extern bool DRAW_EVERY_LINK;

#ifdef DUMP_LINKS
void dumpLinksFile( std::fstream &file, int sliceNumber )
{
  if ( !SAVE ) {
    return;
  }
  char name[255];
  sprintf( name, "Links/%02i.%02i.dat", kEvents, sliceNumber );
  file.open( name, std::ios::out );
}
#endif

#ifdef DUMP_TC_OUTPUT
void dumpTrackletconstructionFile( std::fstream &file, int sliceNumber )
{
  if ( !SAVE ) {
    return;
  }
  char name[255];
  sprintf( name, "TrackletConstruction/%02i.%02i.dat", kEvents, sliceNumber );
  file.open( name, std::ios::out );
}
#endif // DUMP_TRACKLETCONSTRUCTION

static void readSettings( const char *filename, AliHLTTPCCAGBTracker *tracker )
{
  std::fstream geo;
  geo.open( filename, std::ios::in );
  if ( !geo.is_open() ) {
    std::exit( 1 );
  }
  CALLGRIND_TOGGLE_COLLECT
  tracker->ReadSettings(geo);
  CALLGRIND_TOGGLE_COLLECT
  geo.close();
}

static bool readEvent( const char *filename, AliHLTTPCCAGBTracker *tracker )
{
  FILE *file = std::fopen( filename, "rb" );
  if ( !file ) {
    return false;
  }
  CALLGRIND_TOGGLE_COLLECT
  tracker->ReadEvent( file );
  CALLGRIND_TOGGLE_COLLECT
  fclose( file );
  return true;
}

static bool file_exists( const char *filename )
{
    FILE *f = 0;
    if ( ( f = std::fopen( filename, "r" ) ) != NULL ) {
        std::fclose( f );
        return true;
    }
    return false;
}

static void usage(const char *execName)
{
  std::cout << "Usage: " << execName << " [OPTION] [event number]\n"
     "Reconstruct slice tracks and merge them from cluster data read from the Events directory.\n\n"
     "  -h, --help print this message\n"
#ifdef MAIN_DRAW
     "  -links     let it draw every vector of links the NeighboursFinder found.\n"
#else
     "  -single    force tracker to run on only one core. Per default all available cores will be used\n"
#endif
     "  -save      dump result of the tracker/merger into a file for later analysis\n"
#ifndef HLTCA_STANDALONE
     "  -perf      do a performance analysis against Monte-Carlo information right after reconstruction\n\n"
#endif
     "You may specify a specific event to reconstruct if there is more than one available in the Events directory"
     << std::endl;
}

int main(int argc, char **argv)
{
  bool fullTiming = false;
#ifndef HLTCA_STANDALONE
  AliHLTTPCCAPerformance *perf = 0;
#endif
  int eventNumber = -1;
  int repetitions = 1;
  for( int i=1; i < argc; i++ ){
    if ( !std::strcmp( argv[i], "-h" ) || !std::strcmp( argv[i], "--help" ) || !std::strcmp( argv[i], "-help" ) ) {
      usage(argv[0]);
      return 0;
#ifdef DRAW
    } else if ( !std::strcmp( argv[i], "-links" ) ) {
      DRAW_EVERY_LINK = true;
#else
    } else if ( !std::strcmp( argv[i], "-single" ) ) {
      SINGLE_THREADED = true;
#endif
    } else if ( !std::strcmp( argv[i], "-save" ) ) {
      SAVE = true;
#ifndef HLTCA_STANDALONE
    } else if ( !std::strcmp( argv[i], "-perf" ) ) {
      perf = &AliHLTTPCCAPerformance::Instance();
#endif
    } else if ( !std::strcmp( argv[i], "-time" ) ) {
      fullTiming = true;
    } else if ( !std::strcmp( argv[i], "-repeat" ) && ++i < argc ) {
      repetitions = atoi( argv[i] );
    } else {
      eventNumber = atoi( argv[i] );
      std::cout << "Only process event " << eventNumber << std::endl;
    }
  }
#ifdef MAIN_DRAW
  SINGLE_THREADED = true;
  perf = &AliHLTTPCCAPerformance::Instance();
#endif


  AliHLTTPCCAGBTracker *tracker = 0;
  const AliHLTTPCCAGBTracker *trackerConst = 0;

  tracker = new AliHLTTPCCAGBTracker;

  readSettings( "Events/settings.dat", tracker );
  trackerConst = tracker;

  kEvents = 0;
  if ( eventNumber >= 0 ) {
    kEvents = eventNumber;
  }
  while ( eventNumber == -1 || eventNumber == kEvents ) {
    char name[255];
    sprintf( name,"Events/%i.bin", kEvents );

    std::cout << "Loading Event " << kEvents << "..." << std::endl;
    if ( !readEvent( name, tracker ) ) {
      std::cout << "Data for event: " << kEvents << " has NOT been read succesfull" << std::endl;
      break;
    }
    std::cout << "Event " << kEvents << " CPU reconstruction..." << std::endl;

#ifndef HLTCA_STANDALONE
    if ( perf ) {
      char nameTr[255], nameP[255];
      sprintf( nameTr, "Events/%i.mc.bin", kEvents );
      sprintf( nameP, "Events/%i.mcpoints.bin", kEvents );
      std::cout << "Loading Monte-Carlo Data for Event " << kEvents << "..." << std::endl;
      if ( !perf->SetNewEvent( tracker, nameTr, nameP ) ) std::cout << "MCdata for event: " << kEvents << " has NOT been read succesfull" << std::endl;
    }
#endif
    
    tracker->FindTracks();

#ifndef HLTCA_STANDALONE
    if ( perf) {
      perf->InitSubPerformances();
      perf->ExecPerformance();
    }
#endif

    const bool ifAvarageTime = 1;
    if (!ifAvarageTime){
        
      std::cout << "Reconstruction Time"
        << " Real = " << std::setw( 10 ) << trackerConst->SliceTrackerTime() * 1.e3 << " ms,"
        << " CPU = " << std::setw( 10 ) << trackerConst->SliceTrackerCpuTime() * 1.e3 << " ms,"
        << " parallelization speedup: " << trackerConst->SliceTrackerCpuTime() / trackerConst->SliceTrackerTime()
        << std::endl;
      if ( fullTiming ) {
        std::cout
          << " |  sum slice trackers: " << std::setw( 10 ) << trackerConst->StatTime( 0 ) * 1000. << " ms\n"
          << " |    NeighboursFinder: " << std::setw( 10 ) << trackerConst->StatTime( 1 ) * 1000. << " ms, " << std::setw( 12 ) << trackerConst->StatTime( 5 ) << " cycles\n"
          << " |     StartHitsFinder: " << std::setw( 10 ) << trackerConst->StatTime( 4 ) * 1000. << " ms\n"
          << " | TrackletConstructor: " << std::setw( 10 ) << trackerConst->StatTime( 2 ) * 1000. << " ms, " << std::setw( 12 ) << trackerConst->StatTime( 7 ) << " cycles\n"
          << " |    TrackletSelector: " << std::setw( 10 ) << trackerConst->StatTime( 3 ) * 1000. << " ms, " << std::setw( 12 ) << trackerConst->StatTime( 8 ) << " cycles\n"
          << " |         WriteOutput: " << std::setw( 10 ) << trackerConst->StatTime( 6 ) * 1000. << " ms\n"
          << " |               merge: " << std::setw( 10 ) << trackerConst->StatTime( 9 ) * 1000. << " ms" << std::endl;
      } 

    }
    else{
      const int NTimers = 10;
      static int statIEvent = 0;
      static double statTime[NTimers];
      static double statTime_SliceTrackerTime = 0;
      static double statTime_SliceTrackerCpuTime = 0;

      if (!statIEvent){
        for (int i = 0; i < NTimers; i++){
          statTime[i] = 0;
        }
      }

      statIEvent++;
      for (int i = 0; i < NTimers; i++){
        statTime[i] += trackerConst->StatTime( i );
      }
      statTime_SliceTrackerTime += trackerConst->SliceTrackerTime();
      statTime_SliceTrackerCpuTime += trackerConst->SliceTrackerCpuTime();
      
        
      std::cout << "Reconstruction Time"
          << " Real = " << std::setw( 10 ) << 1./statIEvent*statTime_SliceTrackerTime * 1.e3 << " ms,"
          << " CPU = " << std::setw( 10 ) << 1./statIEvent*statTime_SliceTrackerCpuTime * 1.e3 << " ms,"
          << " parallelization speedup: " << statTime_SliceTrackerCpuTime / statTime_SliceTrackerTime
          << std::endl;
      if ( fullTiming ) {
        std::cout
            << " |  sum slice trackers: " << std::setw( 10 ) << 1./statIEvent*statTime[ 0 ] * 1000. << " ms\n"
            << " |    NeighboursFinder: " << std::setw( 10 ) << 1./statIEvent*statTime[ 1 ] * 1000. << " ms, " << std::setw( 12 ) << 1./statIEvent*statTime[ 5 ] << " cycles\n"
            << " |     StartHitsFinder: " << std::setw( 10 ) << 1./statIEvent*statTime[ 4 ] * 1000. << " ms\n"
            << " | TrackletConstructor: " << std::setw( 10 ) << 1./statIEvent*statTime[ 2 ] * 1000. << " ms, " << std::setw( 12 ) << 1./statIEvent*statTime[ 7 ] << " cycles\n"
            << " |    TrackletSelector: " << std::setw( 10 ) << 1./statIEvent*statTime[ 3 ] * 1000. << " ms, " << std::setw( 12 ) << 1./statIEvent*statTime[ 8 ] << " cycles\n"
            << " |         WriteOutput: " << std::setw( 10 ) << 1./statIEvent*statTime[ 6 ] * 1000. << " ms\n"
            << " |               merge: " << std::setw( 10 ) << 1./statIEvent*statTime[ 9 ] * 1000. << " ms" << std::endl;
      } 
      
    }

    sprintf( name, "%i.dat", kEvents );
    if ( SAVE ) {
      trackerConst->StoreToFile( name );
    }
    kEvents++;
  }
  delete tracker;
  return 0;
}
