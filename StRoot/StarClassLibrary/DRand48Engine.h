/***************************************************************************
 *
 * $Id: DRand48Engine.h,v 1.1 1999/01/30 03:58:59 fisyak Exp $
 *
 * Author:  Gabriele Cosmo - Created: 5th September 1995
 *          modified for SCL bl
 ***************************************************************************
 *
 * Description:
 *                 DRand48Engine.h,v 1.6 1998/02/02 10:05:05
 * -----------------------------------------------------------------------
 *                             HEP Random
 *                        --- DRand48Engine ---
 *                          class header file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 * Random engine using drand48() and srand48() functions from C standard
 * library to implement the flat() basic distribution and for setting
 * seeds.
 *
 ***************************************************************************
 *
 * $Log: DRand48Engine.h,v $
 * Revision 1.1  1999/01/30 03:58:59  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:27:32  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef DRand48Engine_h
#define DRand48Engine_h 1

#include "StGlobals.hh"
#include "RandomEngine.h"
#include <stdlib.h>        // drand48() etc., tu

#ifdef WIN32
   // ********************************************************************
   // Code extracted from GNU C Library 2.0.1

   /* Data structure for communication with thread safe versions.  */
   struct drand48_data
     {
       unsigned short int X[3];     /* Current state.  */
       unsigned short int a[3];     /* Factor in congruential formula.  */
       unsigned short int c;        /* Additive const. in congruential formula.  */
       unsigned short int old_X[3]; /* Old state.  */
       int init;                                /* Flag for initializing.  */
     };

   /* Internal function to compute next state of the generator.  */
   extern "C" { int drand48_iterate (unsigned short int xsubi[3],
                                                        struct drand48_data *buffer); }

   /* Return non-negative, double-precision floating-point value in [0.0,1.0).  */
   extern "C" { int drand48_r (struct drand48_data *buffer, double *result); }
   extern "C" { int erand48_r (unsigned short int xsubi[3],
                                            struct drand48_data *buffer, double *result); }

   /* Seed random number generator.  */
   extern "C" { int srand48_r (long seedval, struct drand48_data *buffer); }
   extern "C" { int seed48_r  (unsigned short int seed16v[3],
                                            struct drand48_data *buffer); }

   /* Return non-negative, double-precision floating-point value in [0.0,1.0).  */
   extern "C" { double drand48 (void); }
   extern "C" { double erand48 (unsigned short int xsubi[3]); }

   /* Seed random number generator.  */
   extern "C" { void srand48 (long seedval); }
   extern "C" { unsigned short int *seed48 (unsigned short int seed16v[3]);     }

   // End Code extracted from GNU C Library 2.0.1
   // ********************************************************************
#endif  /* WIN32 */

class DRand48Engine : public HepRandomEngine {

public:

  DRand48Engine(long seed = 19780503);
  ~DRand48Engine();
  // Constructor and destructor

  DRand48Engine(const DRand48Engine &p);
  // Copy constructor

  DRand48Engine & operator = (const DRand48Engine &p);
  // Overloaded assignment operator, to retrieve the engine status.

  HepDouble flat();
  // It returns a pseudo random number between 0 and 1,
  // according to the standard stdlib random function drand48()
  // but excluding the end points.

  void flatArray (const HepInt size, HepDouble* vect);
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    void flatArray (vector<HepDouble>&);
#else
    void flatArray (vector<HepDouble, allocator<HepDouble> >&);
#endif
  // Fills the array "vect" of specified size with flat random values.

  void setSeed(long seed, HepInt dum=0);
  // Sets the state of the algorithm according to seed.

  void setSeeds(const long * seeds, HepInt dum=0);
  // Sets the state of the algorithm according to the zero terminated
  // array of seeds. Only the first seed is used.

  void saveStatus() const;
  // Saves on file DRand48.conf the current engine status.

  void restoreStatus();
  // Reads from file DRand48.conf the last saved engine status
  // and restores it.

  void showStatus() const;
  // Dumps the engine status on the screen.

};

#endif
