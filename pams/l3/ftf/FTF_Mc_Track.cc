//:>------------------------------------------------------------------
//: FILE:       FTF_Mc_track.cpp
//: HISTORY:
//:             30oct1996 version 1.00
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: CLASS:       FTF_Mc_Track
//: DESCRIPTION: Functions associated with this class
//: AUTHOR:      ppy - Pablo Yepes, yepes@physics.rice.edu
//:>------------------------------------------------------------------
#include "FTF_Mc_Track.h"

FTF_Mc_Track::FTF_Mc_Track ( ) {
	hit = 0 ;
}
FTF_Mc_Track::~FTF_Mc_Track  ( ) {
	if ( hit != 0 ) delete []hit ;
}
