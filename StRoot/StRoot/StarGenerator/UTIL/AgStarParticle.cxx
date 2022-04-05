#include "AgStarParticle.h"
#include <stdio.h>
#include <cstring>
//____________________________________________________________________________________
#define address_of_agcpart F77_NAME( address_of_agcpart, ADDRESS_OF_AGCPART )
#define address_of_agcloca F77_NAME( address_of_agcloca, ADDRESS_OF_AGCLOCA )
#define axparticle         F77_NAME( axparticle, AXPARTICLE )
struct Agcpart_t 
{

  int   code;      // geant3 code
  int   type;    // g3 particle type
  float mass;      // particle mass
  float charge;    // particle charge
  float lifetime;  // particle lifetime
  float bratio[6]; // branching ratio for up to six modes
  int   mode[6];   // up to six decay modes
  int   pdgid;       // pdg code
  int   ecode;     // no clue...
};
extern "C" Agcpart_t* address_of_agcpart();
extern "C" void type_of_call axparticle();

/*
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       
     >AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  
     >AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         
     >AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    
     >AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          
     >AG_OPTION    
    */

struct Agcloca_t {

  int   begin;        // begining of common block scratch area
  int   ubuf[100];    // user buffer
  int   par[100];     // parameter list
  float aa[20];       // 
  float zz[20];
  float ww[20];
  int   nwbuf;        // Number of words in user buffer
  float xhmax;
  float yhmax;
  float zhmax;
  float rhmax;
  float fhmax;
  float fhmin;
  int   nbits;
  float bin;
  int   type;
  int   irot;
  int   npar;
  int   iset;
  int   idet;
  int   jdu;
  int   ireser;
  int   end;            // end of common block scratch area
  char  title[20];
  char  exname[20];
  char  parlist[20];
  char  material[20];
  char  mixture[20];
  char  component[20];
  char  medium[20];
  char  option[4];

};
extern "C" Agcloca_t* address_of_agcloca();


//____________________________________________________________________________________
void AgStarParticle::Add( const char* name, const int g3id, const int type, const float mass, const float charge, const float lifetime, const float* bratio, const int* mode, const int pdgid )
{
  Agcpart_t &agcpart = *(address_of_agcpart());
  Agcloca_t &ag      = *(address_of_agcloca());

  //sprintf( ag.title, "%-20s", name );
  strncpy( ag.title, name, 20 );
  
  agcpart.code = g3id;
  agcpart.type = type;
  agcpart.mass = mass;
  agcpart.charge = charge;
  agcpart.lifetime = lifetime;
  for ( int i=0;i<6;i++ ) {
    if ( bratio ) {
      agcpart.bratio[i] = bratio[i];
      agcpart.mode[i]   = mode[i];
    }
    else {
      agcpart.bratio[i] = 0;
      agcpart.mode[i]   = 0;      
    }
  }
  agcpart.pdgid = pdgid;
  axparticle();
  
};


