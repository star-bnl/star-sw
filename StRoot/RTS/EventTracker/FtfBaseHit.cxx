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
#include "FtfBaseHit.h"

#include "FtfGeneral.h"
#include "rtsLog.h"

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//  
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void FtfBaseHit::print (  ) { print (11) ; } ; 
void FtfBaseHit::print ( int point_level ) 
{
    
    if ( point_level > 9 ) 
	LOG(NOTE, "hit Row    x      y     z\n" ) ;
    
    if ( fmod((double)point_level,10.) > 0 )
	LOG(NOTE, "%3d %2d %6.2f %6.2f %6.2f \n", 
		id, row, x, y, z ) ;
}
