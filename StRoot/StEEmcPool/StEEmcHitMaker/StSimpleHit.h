/*!
 *
 * \class StSimpleHit_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * The basic information needed to define a hit:
 *  - x, y, z position
 *  - Energy E
 *
 * This is designed as a storage class.  Information derived from the
 * above is not included, as that would cost extra storage space
 * (though reduce time cost).
 *
 * Note: all functions are inlined and default copy constructors,
 * equal operators, and deconstructors are used.
*/

#ifndef _ST_SIMPLE_HIT_H_
#define _ST_SIMPLE_HIT_H_

#include <vector>
#include <ostream>

#include <Rtypes.h>
#include <TVector3.h>

/// Forward declaration
class StSimpleHit_t;

/// A container
typedef std::vector< StSimpleHit_t > StSimpleHitVec_t;

/// The class
class StSimpleHit_t : public TObject {
 public:
   // constructor
   StSimpleHit_t( Short_t ID = -1 ) : mID( ID ), mX( 0 ), mY( 0 ), mZ( 0 ), mE( 0 ){ /* */ };

   // deconstructor
   ~StSimpleHit_t(){ /* */ };

   // accessors
   Short_t getID() const { return mID; };
   Float_t getEnergy() const { return mE; };
   Float_t getX() const { return mX; };
   Float_t getY() const { return mY; };
   Float_t getZ() const { return mZ; };
   TVector3 getPosition() const { return TVector3( mX, mY, mZ ); };

   // modifiers
   void setID( const Short_t ID ) { mID = ID; };
   void setEnergy( const Float_t E ) { mE = E; };
   void setX( const Float_t X ) { mX = X; };
   void setY( const Float_t Y ) { mY = Y; };
   void setZ( const Float_t Z ) { mZ = Z; };

   friend std::ostream &operator<<( std::ostream &out, const StSimpleHit_t &clus );

 protected:
   Short_t mID;
   Float_t mX, mY, mZ, mE;

 private:
   /// Make class available to root
   ClassDef(StSimpleHit_t,1);  // Simplest class to describe a hit
};

std::ostream &operator<<( std::ostream &out, const StSimpleHit_t &hit );

#endif

/*
 * $Id: StSimpleHit.h,v 1.1 2012/11/26 19:05:56 sgliske Exp $ 
 * $Log: StSimpleHit.h,v $
 * Revision 1.1  2012/11/26 19:05:56  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
