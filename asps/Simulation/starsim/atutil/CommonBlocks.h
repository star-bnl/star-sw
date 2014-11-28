#ifndef __CommonBlocks_h__
#define __CommonBlocks_h__
#include <map>
#include <string>
using namespace std;

/**
   /class CommonBlocks
   /author Jason C. Webb
   
   CommonBlocks provides lookuptable whereby users can register the location
   of a common block in memory and retrieve the location of that common
   block.  It is useful for the case of mixed-language programming where
   one wants to make use of a fortran common block from within c or c++.
   
   To utilize, one makes a call to register common block in fortran, e.g.

      Subroutine MyCommonBlocks
         COMMON /MyBlock/ a, b, c, i, j, k
	 REAL a, b, c
	 INTEGER i, j, k
	 call register_common('MyBlock',a)
      End

   On the c/c++ side, one can then obtain a pointer to the common block
   and recast it to a struct

   struct MyBlock_t {
      float a, b, c;
      int   i, h, k;
   };

   MyBlock_t *myblock = (MyBlock_t *)address_of_common( "MyBlock" ); 

   if address_of_common is declared as extern "C", or
   
   MyBlock_t *myblock = (MyBlock_t *)CommonBlocks::Address("MyBlock");

   note... may not be 64-bit compatible
   
 */
class CommonBlocks 
{
 public:
  static CommonBlocks &instance();
  static void  Register( string name, int   *ptr );
  static void *Address ( string name );

 private:
 protected:

  static CommonBlocks sInstance;
  map<string, void *> mTable;

};

#endif
