//:>------------------------------------------------------------------
//: FILE:       FtfMcTrack.cpp
//: HISTORY:
//:             30oct1996 version 1.00
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: CLASS:       FtfMcTrack
//: DESCRIPTION: Functions associated with this class
//: AUTHOR:      ppy - Pablo Yepes, yepes@physics.rice.edu
//:>------------------------------------------------------------------
#include "FtfMcTrack.h"

FtfMcTrack::FtfMcTrack ( ) {
	hit = 0 ;
}
FtfMcTrack::~FtfMcTrack  ( ) {
	if ( hit != 0 ) delete []hit ;
}
