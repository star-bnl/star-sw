/*!
 * \class StArchInfo
 * \author Jerome Lauret, 2009
 *
 * Singleton based class providing information on
 * OS information
 * 
 */

#ifndef StArchInfo_h
#define StArchInfo_h

#include <stdlib.h>

#include "TNamed.h"

#if !defined(__LITTLE_ENDIAN)
# define __LITTLE_ENDIAN 1234
#endif
#if !defined(__BIG_ENDIAN)
# define __BIG_ENDIAN    4321
#endif
#if !defined(__PDP_ENDIAN)
# define __PDP_ENDIAN    3412
#endif



class StArchInfo :public TNamed {
 public:
   StArchInfo();
   ~StArchInfo();


   Bool_t isBigEndian();
   Bool_t isLittleEndian();
   Int_t  Endian();
   
 private:
   Int_t fEndian;
   
   ClassDef(StArchInfo,0)
};


#endif
