//:>------------------------------------------------------------------
//: FILE:       FtfBaseHit.cxx
//: HISTORY:
//:              1sep1999 ppy first version
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: CLASS:       FtfBaseHit
//: DESCRIPTION: Functions associated with this class
//: AUTHOR:      ppy - Pablo Yepes, yepes@physics.rice.edu
//:>------------------------------------------------------------------
#include "Stl3Util/ftf/FtfBaseHit.h"


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//  
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void FtfBaseHit::print (  ) { print (11) ; } ; 
void FtfBaseHit::print ( int point_level ) 
{

   if ( point_level > 9 ) 
      printf ( "hit Row    x      y     z\n" ) ;

   if ( fmod((float)point_level,10.) > 0 )
        printf ( "%3d %2d %6.2f %6.2f %6.2f \n", 
                  id, row, x, y, z ) ;
}
