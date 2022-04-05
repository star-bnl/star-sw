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
   
//! Returns the number of bytes the type \a T has to be align to
   template<typename T> static long align(const T &)
   {
       return align<T>();
   }
   
//! This is an overloaded member function, provided for convenience.
/*!
    Returns the number of bytes the type \a T has to be align to
    \author Valeri Fine, 2009
    \sa AligmentTest.C
 */
   template<typename T> static int align()
   {
      static int done = -1;
      if (done<0) {
         struct { char c; T test; } probe;
         done =  ((char*)&probe.test)-&probe.c;
      }
      return done;
   }
   
//! Returns the number of bytes to be added to the \a offset of the \a T type to get the \a C type proper aligned 
/*!
   \author Valeri Fine, 2009
   \sa AligmentTest.C
   \sa http://en.wikipedia.org/wiki/Data_structure_alignment#Computing_padding
 */
   template <typename C, typename T> static int padding(const T&offset)
   {
      long offchar = (long)&offset;
      long  alg = align<C>();
      return  int((alg + ((offchar - 1) & ~(alg - 1))) - offchar);
   }

//! This is an overloaded member function, provided for convenience.
/*!
   Returns the number of bytes to be added to the \a offset of the \a T type to get the \a C type proper aligned 
   \author Valeri Fine, 2009
   \sa AligmentTest.C
*/
   template <typename T, typename C> static int padding(const T&offset, const C&)
   {
      return padding<C,T>(offset);
   }
 private:
   Int_t fEndian;
   
   ClassDef(StArchInfo,0)
};

#endif
