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


/// Returns predefined Endianess as 1234for little and 4321 for big
Int_t StArchInfo::Endian()
{

   if ( fEndian == 0){
      // avoid using int32 to char but use instead short versus
      // unsigned char trick likely more robust
      unsigned char endian[2] = {1, 0};
      short x;
  
      x = * ((short *) endian);

      if ( x == 1) {
	 // If this is a little-endian system, the 0 and 1 is interpreted 
	 // backwards and seen as if it is 0,1. Since the high byte is 0, it 
	 // doesn't matter and the low byte is 1, so x is equal to 1.
	 fEndian = __LITTLE_ENDIAN;
      } else {
	 // If it's a big-endian system, the high byte is 1
	 fEndian = __BIG_ENDIAN;

	 // One possible problem ... We cannot detect middle-edian with the 1,0 0,1 
	 // combo but would need at least 4 bytes to test the 3412 ordering.
	 // 
	 // This test is hence not suitable if we get into middle-edian arch.
	 // 
      }

   }
   return fEndian;
}
