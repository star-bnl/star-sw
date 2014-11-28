/*!
 * 
 * StArchInfo provides information of OS and Architecture specific information
 * not contained in TSystem such as endianess and alignement.
 * 
 */ 


#include "StArchInfo.h"

#include <iostream>
using namespace std;


/// Constructor, initialize variables
StArchInfo::StArchInfo()
{
   // init vars
   fEndian = 0;
}

/// Destructore - NOOP
StArchInfo::~StArchInfo()
{
   // nothing for now
}


/// Returns true/false if endianess is/ins't Big
Bool_t StArchInfo::isBigEndian()
{
   return (Endian() == __BIG_ENDIAN);
}

/// Returns true/false if endianess is/ins't Little
Bool_t StArchInfo::isLittleEndian()
{
   return (Endian() == __LITTLE_ENDIAN);
}


/*
 * Returns predefined Endianess as 1234 for little and 4321 for big
 * This does not treat middle-endian.
 * 
 * Base principle: we use an array of two char (1 byte ecah) and force cast to
 * a short value and test.
 * 
 * If the system is little-endian, the 0 and 1 is interpreted
 * backwards and seen as if it is 0,1. Since the high byte is 0, it
 * doesn't matter and the low byte is 1, so x is equal to 1.
 * 
 * If it's a big-endian system, the high byte is 1.
 * 
 * 
 */ 
Int_t StArchInfo::Endian()
{

   if ( fEndian == 0){
      // avoid using int32 to char but use instead short versus
      // unsigned char trick likely more robust
      unsigned char endian[2] = {1, 0};
      short x;
  
      x = * ((short *) endian);

      if ( x == 1) {
	 // little
	 fEndian = __LITTLE_ENDIAN;
      } else {
	 // big or middle
	 fEndian = __BIG_ENDIAN;

	 //
	 // One possible problem ... We do not detect middle-edian 
	 // This test ould need to be extended for middle-edian arch (rare)
	 // 
      }

   }
   return fEndian;
}
