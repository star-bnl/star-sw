/***************************************************************************
 *
 * $Id: RandFlat.h,v 1.2 1999/09/02 11:35:25 ullrich Exp $
 *
 * Author: Gabriele Cosmo - Created: 5th September 1995
 *         modified for SCL bl
 ***************************************************************************
 *
 * Description:
 *           RandFlat.h,v 1.4 1997/08/12 00:38:41
 * -----------------------------------------------------------------------
 *                             HEP Random
 *                           --- RandFlat ---
 *                          class header file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 * Class defining methods for shooting flat random numbers, double or
 * integers.
 * It provides also static methods to fill with double flat values arrays
 * of specified size.
 * Default boundaries ]0.1[ for operator()().
 *
 ***************************************************************************
 *
 * $Log: RandFlat.h,v $
 * Revision 1.2  1999/09/02 11:35:25  ullrich
 * Changed order of data member to avoid warnings on Linux
 *
 * Revision 1.1  1999/01/30 03:59:00  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:27:37  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef RandFlat_h
#define RandFlat_h 1

#include "Random.h"

class RandFlat : public HepRandom {

public:

    inline RandFlat ( HepRandomEngine& anEngine );
    inline RandFlat ( HepRandomEngine* anEngine );
    // These constructors should be used to instantiate a RandFlat
    // distribution object defining a local engine for it.
    // The static generator will be skeeped using the non-static methods
    // defined below.
    // If the engine is passed by pointer the corresponding engine object
    // will be deleted by the RandFlat destructor.
    // If the engine is passed by reference the corresponding engine object
    // will not be deleted by the RandFlat destructor.

    virtual ~RandFlat();
    // Destructor

    // Static methods to shoot random values using the static generator

    static  inline HepDouble shoot();
    static  inline HepDouble shoot( HepDouble width );
    static  inline HepDouble shoot( HepDouble a, HepDouble b );

    static  inline long shootInt( long n );
    static  inline long shootInt( long m, long n );

    static  inline HepInt shootBit();

    static  inline void shootArray ( const HepInt size, HepDouble* vect );
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    static  inline void shootArray (vector<HepDouble>&);
#else
    static  inline void shootArray (vector<HepDouble, allocator<HepDouble> >&);
#endif
    static  void shootArray ( const HepInt size, HepDouble* vect,
			      HepDouble lx, HepDouble dx );
#ifndef ST_NO_TEMPLATE_DEF_ARGS
     static  void shootArray ( vector<HepDouble>&, HepDouble, HepDouble );
#else
    static  void shootArray ( vector<HepDouble, allocator<HepDouble> >&,
			      HepDouble, HepDouble );
#endif
  //  Static methods to shoot random values using a given engine
  //  by-passing the static generator.

  static  inline HepDouble shoot ( HepRandomEngine* anEngine );

  static  inline HepDouble shoot( HepRandomEngine* anEngine, HepDouble width );

  static  inline HepDouble shoot( HepRandomEngine* anEngine,
                                  HepDouble a, HepDouble b );
  static  inline long shootInt( HepRandomEngine* anEngine, long n );
  
  static  inline long shootInt( HepRandomEngine* anEngine, long m, long n );
  
  static  inline HepInt shootBit( HepRandomEngine* );

  static  inline void shootArray ( HepRandomEngine* anEngine,
                                   const HepInt size, HepDouble* vect );
#ifndef ST_NO_TEMPLATE_DEF_ARGS
     static  inline void shootArray ( HepRandomEngine*, vector<HepDouble>& );
#else
    static  inline void shootArray ( HepRandomEngine*, vector<HepDouble,allocator<HepDouble> >& );
#endif
    
  static  void shootArray ( HepRandomEngine* anEngine, 
                            const HepInt size, HepDouble* vect,
                            HepDouble lx, HepDouble dx );
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    static  void shootArray ( HepRandomEngine*, 
			      vector<HepDouble>&,
			      HepDouble, HepDouble );
#else
static  void shootArray ( HepRandomEngine*, 
			  vector<HepDouble,allocator<HepDouble> >&,
			  HepDouble, HepDouble );
#endif
  //  Methods using the localEngine to shoot random values, by-passing
  //  the static generator.

  inline HepDouble fire();

  inline HepDouble fire( HepDouble width );

  inline HepDouble fire( HepDouble a, HepDouble b );

  inline long fireInt( long n );

  inline long fireInt( long m, long n );

  inline HepInt fireBit();

  inline void fireArray (const HepInt size, HepDouble* vect);
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    inline void fireArray (vector<HepDouble>&);
#else
    inline void fireArray (vector<HepDouble,allocator<HepDouble> >&);
#endif
    
  void fireArray (const HepInt size, HepDouble* vect,
                  HepDouble lx, HepDouble dx);
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    void fireArray (vector<HepDouble>&, HepDouble, HepDouble);
#else
    void fireArray (vector<HepDouble,allocator<HepDouble> >&, HepDouble, HepDouble);
#endif
  HepDouble operator()();
  
private:

  // ShootBits generates an integer random number,
  // which is used by fireBit().
  // The number is stored in randomInt and firstUnusedBit

  inline void fireBits();
  static inline void shootBits();
  static inline void shootBits(HepRandomEngine*);

  // In MSB, the most significant bit of the integer random number
  // generated by ShootBits() is set.
  // Note:
  //   the number of significant bits must be chosen so that
  //   - an unsigned long can hold it
  //   - and it should be less than the number of bits returned 
  //     by Shoot() which are not affected by precision problems
  //     on _each_ architecture.
  //   (Aim: the random generators should be machine-independent).

  HepRandomEngine* localEngine;
  HepBoolean deleteEngine;

  unsigned long randomInt;
  unsigned long firstUnusedBit;

  static const HepInt MSBBits;
  static const unsigned long MSB; 

  static unsigned long staticRandomInt;
  static unsigned long staticFirstUnusedBit;  
};

inline RandFlat::RandFlat(HepRandomEngine & anEngine)
: localEngine(&anEngine), deleteEngine(false), firstUnusedBit(0) {}

inline RandFlat::RandFlat(HepRandomEngine * anEngine)
: localEngine(anEngine), deleteEngine(true), firstUnusedBit(0) {}

inline HepDouble RandFlat::shoot() {
  return HepRandom::getTheGenerator()->flat();
}

inline HepDouble RandFlat::shoot(HepDouble a, HepDouble b) {
  return (b-a)* shoot() + a;
}

inline HepDouble RandFlat::shoot(HepDouble width) {
  return width * shoot();
}

inline long RandFlat::shootInt(long n) {
  return long(shoot()*HepDouble(n));
}

inline long RandFlat::shootInt(long m, long n) {
  return long(shoot()*HepDouble(n-m)) + m;
}

inline void RandFlat::shootArray(const HepInt size, HepDouble* vect) {
  HepRandom::getTheGenerator()->flatArray(size,vect);
}

#ifndef ST_NO_TEMPLATE_DEF_ARGS
inline void RandFlat::shootArray(vector<HepDouble>& vec)
#else
inline void RandFlat::shootArray(vector<HepDouble,allocator<HepDouble> >& vec)
#endif
{
    HepRandom::getTheGenerator()->flatArray(vec);
}

inline void RandFlat::shootBits() {
  const HepDouble factor= 2.0*MSB; // this should fit into a double! 
  staticFirstUnusedBit= MSB;
  staticRandomInt= (unsigned long)(factor*shoot());  
}

inline HepInt RandFlat::shootBit() {
  if (staticFirstUnusedBit==0)
    shootBits();
  unsigned long temp= staticFirstUnusedBit&staticRandomInt;
  staticFirstUnusedBit>>= 1;
  return temp!=0;   
}

//---------------------

inline HepDouble RandFlat::shoot(HepRandomEngine* anEngine) {
  return anEngine->flat();
}


inline HepDouble RandFlat::shoot(HepRandomEngine* anEngine,
                                 HepDouble a, HepDouble b) {
  return (b-a)* anEngine->flat() + a;
}

inline HepDouble RandFlat::shoot(HepRandomEngine* anEngine,
                                 HepDouble width) {
  return width * anEngine->flat();
}

inline long RandFlat::shootInt(HepRandomEngine* anEngine,
                                  long n) {
  return long(anEngine->flat()*HepDouble(n));
}

inline long RandFlat::shootInt(HepRandomEngine* anEngine,
                                  long m, long n) {
  return long(HepDouble(n-m)*anEngine->flat()) + m;
}

inline void RandFlat::shootArray(HepRandomEngine* anEngine,
                                 const HepInt size, HepDouble* vect) {
  anEngine->flatArray(size,vect);
}

#ifndef ST_NO_TEMPLATE_DEF_ARGS
inline void RandFlat::shootArray(HepRandomEngine* anEngine,
                                 vector<HepDouble>& vec)
#else
inline void RandFlat::shootArray(HepRandomEngine* anEngine,
                                 vector<HepDouble,allocator<HepDouble> >& vec)
#endif
{
  anEngine->flatArray(vec);
}

inline void RandFlat::shootBits(HepRandomEngine* engine) {
  const HepDouble factor= 2.0*MSB; // this should fit into a double! 
  staticFirstUnusedBit= MSB;
  staticRandomInt= (unsigned long)(factor*shoot(engine));  
}

inline HepInt RandFlat::shootBit(HepRandomEngine* engine) {
  if (staticFirstUnusedBit==0)
    shootBits(engine);
  unsigned long temp= staticFirstUnusedBit&staticRandomInt;
  staticFirstUnusedBit>>= 1;
  return temp!=0;   
}

//---------------------

inline HepDouble RandFlat::fire() {
  return localEngine->flat();
}

inline HepDouble RandFlat::fire(HepDouble a, HepDouble b) {
  return (b-a)* fire() + a;
}

inline HepDouble RandFlat::fire(HepDouble width) {
  return width * fire();
}

inline long RandFlat::fireInt(long n) {
  return long(fire()*HepDouble(n));
}

inline long RandFlat::fireInt(long m, long n) {
  return long(fire()*HepDouble(n-m)) + m;
}

inline void RandFlat::fireArray(const HepInt size, HepDouble* vect) {
  flatArray(localEngine,size,vect);
}

#ifndef ST_NO_TEMPLATE_DEF_ARGS
inline void RandFlat::fireArray(vector<HepDouble>&vec)
#else
inline void RandFlat::fireArray(vector<HepDouble,allocator<HepDouble> >&vec)
#endif
{
  flatArray(localEngine,vec);
}

inline void RandFlat::fireBits() {
  const HepDouble factor= 2.0*MSB; // this should fit into a HepDouble! 
  firstUnusedBit= MSB;
  randomInt= (unsigned long)(factor*fire());  
}

inline HepInt RandFlat::fireBit() {
  if (firstUnusedBit==0)
    fireBits();
  unsigned long temp= firstUnusedBit&randomInt;
  firstUnusedBit>>= 1;
  return temp!=0;   
}
#endif
