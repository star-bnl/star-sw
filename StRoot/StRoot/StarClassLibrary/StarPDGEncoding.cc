
#include "StarClassLibrary/StarPDGEncoding.hh"

int hid( int z, int a, int l )                                                                                                                                                                   
{                                                                                                                                                                                                          
  //         10LZZZAAAI                                                                                                                                                                                    
  return (   1000000000                                                                                                                                                                                    
         +     10000000*l                                                                                                                                                                                  
         +        10000*z                                                                                                                                                                                  
	     +           10*a );                                                                                                                                                                               
}          
