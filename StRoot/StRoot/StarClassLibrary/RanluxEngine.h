/***************************************************************************
 *
 * $Id: RanluxEngine.h,v 1.1 1999/01/30 03:59:01 fisyak Exp $
 *
 * Author: Adeyemi Adesanya - Created: 6th November 1995
 *         modfied for SCL bl
 ***************************************************************************
 *
 * Description:
 *        RanluxEngine.h,v 1.3 1997/07/22 01:17:06
 * -----------------------------------------------------------------------
 *                             HEP Random
 *                        --- RanluxEngine ---
 *                          class header file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 * The algorithm for this random engine has been taken from the original
 * implementation in FORTRAN by Fred James as part of the MATHLIB HEP
 * library.
 * The initialisation is carried out using a Multiplicative Congruential
 * generator using formula constants of L'Ecuyer as described in "F.James,
 * Comp. Phys. Comm. 60 (1990) 329-344".
 *
 ***************************************************************************
 *
 * $Log: RanluxEngine.h,v $
 * Revision 1.1  1999/01/30 03:59:01  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:27:44  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef RanluxEngine_h
#define RanluxEngine_h 1

#include "RandomEngine.h"

class RanluxEngine : public HepRandomEngine {

public:

  RanluxEngine(long seed = 19780503, HepInt lux = 3);
  ~RanluxEngine();
  // Constructor and destructor

  RanluxEngine(const RanluxEngine &p);
  // Copy constructor

  RanluxEngine & operator = (const RanluxEngine &p);
  // Overloaded assignment operator, to retrieve the engine status.

// Luxury level is set in the same way as the original FORTRAN routine.
//  level 0  (p=24): equivalent to the original RCARRY of Marsaglia
//           and Zaman, very long period, but fails many tests.
//  level 1  (p=48): considerable improvement in quality over level 0,
//           now passes the gap test, but still fails spectral test.
//  level 2  (p=97): passes all known tests, but theoretically still
//           defective.
//  level 3  (p=223): DEFAULT VALUE.  Any theoretically possible
//           correlations have very small chance of being observed.
//  level 4  (p=389): highest possible luxury, all 24 bits chaotic.

  HepDouble flat();
  // It returns a pseudo random number between 0 and 1,
  // excluding the end points.

  void flatArray (const HepInt size, HepDouble* vect);
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    void flatArray (vector<HepDouble>&);
#else
    void flatArray (vector<HepDouble, allocator<HepDouble> >&);
#endif
  // Fills the array "vect" of specified size with flat random values.

  void setSeed(long seed, HepInt lux=3);
  // Sets the state of the algorithm according to seed.

  void setSeeds(const long * seeds, HepInt lux=3);
  // Sets the state of the algorithm according to the zero terminated
  // array of seeds. Only the first seed is used.

  void saveStatus() const;
  // Saves on file Ranlux.conf the current engine status.

  void restoreStatus();
  // Reads from file Ranlux.conf the last saved engine status
  // and restores it.

  void showStatus() const;
  // Dumps the engine status on the screen.

  HepInt getLuxury() const { return luxury; }
  // Gets the luxury level.

private:

  HepInt nskip, luxury;
  HepFloat float_seed_table[24];
  HepInt i_lag,j_lag;  
  HepFloat carry;
  HepInt count24;
  const HepInt int_modulus;
  const HepFloat mantissa_bit_24;
  const HepFloat mantissa_bit_12;

};

#endif
