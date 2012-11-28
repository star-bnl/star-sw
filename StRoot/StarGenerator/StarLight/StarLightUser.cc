#include <iostream>
#include <fstream>
#include <string>

#include "inputParameters.h"
#include "starlight.h"
#include "upcevent.h"

#include <math.h>
#include <assert.h>
#include <stdlib.h>

inputParameters *_parameters = 0;
starlight       *_starlight  = 0;

extern "C" { 

  void starlightinterface_();

  int   ku_npar();
  char *ku_path();
  char *ku_gete();

  void pushtrack_( int *pdg, int *charge, float *mass, float *px, float *py, float *pz, float *e );
  void pushTrack( int pdg, int charge, float mass, float px, float py, float pz, float e )
  {
    pushtrack_( &pdg, &charge, &mass, &px, &py, &pz, &e );
  }
  void pushtrackreset_();  
#define pushTrackReset pushtrackreset_
  void inithepevtd_();
#define initHepevtd inithepevtd_  

  ///////////////////////////////////////////////////////////////////////////////////////
  //
  // On library load, establish the KUIP interface
  //
  void starlight_(){
    starlightinterface_();
  };
  
  ///////////////////////////////////////////////////////////////////////////////////////
  //
  // Handle KUIP commands
  //

  static std::ofstream par_out;
  void starlightuser_()
  {

    static bool first = true;
    if ( first )
      {
	par_out.open("starlight.in");
	first = false;
      }

//  int         npar = ku_npar(); // number of kuip parameters
    std::string path = ku_path(); // Command path
    
    //
    // StarLight configuration commands are of the form KEYWORD = VALUE.  We
    // setup the StarLightInterface.cdf such that the starlight command
    // corresponds to the expected keyword. 
    //
    size_t      found   = path.find_last_of("/");
    std::string command = path.substr(found+1);
    // Add the expected equal sign
    command += " = ";
    // And finish the command
    command += ku_gete();
    command += "\n";
    par_out << command.c_str();

  }

#if 0 /* agusread is implemented in the StarGenerator framework */
  void agusread_()
  {
    static bool first = true;
    
    //
    // Initialization on the first event
    //
    if ( first ) {

      par_out << "RND_SEED      = 1234 # Dummy (using starsim RNG)" << std::endl;   // dummy
      par_out << "OUTPUT_FORMAT = 1" << std::endl; // ???
      par_out.close(); // close output file      
      first = false;

      // Read in input parameters from the temporary input file
      _parameters = new inputParameters();
      _parameters->init( "./starlight.in" );
      // And create a new instance of the starlight object 
      _starlight = new starlight();
      _starlight -> setInputParameters( _parameters );
      _starlight -> init();
      // And some useful info into the event generator
      initHepevtd();

    }


    // Generate one event
    upcEvent event = _starlight->produceEvent();

    // Rset hepevt
    pushTrackReset();

    const std::vector<starlightParticle> &particles = *(event.getParticles());
    for ( int i=0;i<particles.size();i++ )
      {
	float px  = particles[i].GetPx();
	float py  = particles[i].GetPy();
	float pz  = particles[i].GetPz();
	float e   = particles[i].GetE();
	float m   = particles[i].M(); 
	int   q   = particles[i].getCharge();
	int   pdg = particles[i].getPdgCode();

	//	std::cout << "pdg id = " << pdg << " charge=" << q << std::endl;
	//
	// StarLight does not follow the PDG naming convention.  They drop the sign of
	// antiparticles.  
	//
	assert( pdg > 0 );// If this fails, starlight group has changed something
	pdg *= q / abs(q);
	
	pushTrack( pdg, q, m, px, py, pz, e );

      }


    
  }
#endif

}

 
