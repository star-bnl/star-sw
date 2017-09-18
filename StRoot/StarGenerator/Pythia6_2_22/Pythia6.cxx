#include "Pythia6.h"

//
// Declare several f77 functions/subroutines
//
#define pytune F77_NAME(pytune,PYTUNE) /* selects pythia tune */
#define pyinit F77_NAME(pyinit,PYINIT) /* pythia initialization */
#define pylist F77_NAME(pylist,PYLIST) /* pythia event recrd listing */
#define pyevnt F77_NAME(pyevnt,PYEVNT) /* generate a pythia event */
#define pyhepc F77_NAME(pyhepc,PYHEPC) /* copy to HEPEVT common... needed? */
#define pystat F77_NAME(pystat,PYSTAT) /* print end of run statistices */
#define pyr    F77_NAME(pyr,   PYR   ) /* pythia random numbers */

extern "C" void   type_of_call  pyevnt();
extern "C" void   type_of_call  pystat(int *key);
extern "C" void   type_of_call  pylist(int *key);
extern "C" int    type_of_call  pytune(int *itune);
extern "C" void   type_of_call  pyhepc(int *mconv);
extern "C" void   type_of_call  pyinit( const char *frame, const char *beam, const char *targ, double *ener, int nframe, int nbeam, int ntarg );

void PyEvnt(){ pyevnt(); }
void PyStat( int s ){ pystat( &s ); }
void PyList( int l ){ pylist( &l ); }
void PyTune( int t ){ pytune( &t ); }
void PyHepc( int m ){ pyhepc( &m ); }
void PyInit( string frame, string blue, string yellow, double energy ){ pyinit( frame.c_str(), blue.c_str(), yellow.c_str(), &energy, frame.size(), blue.size(), yellow.size() ); }
