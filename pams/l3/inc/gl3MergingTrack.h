//:>------------------------------------------------------------------
//: FILE:       gl3Tracks.h
//: HISTORY:
//:             18jan2000 version 1.00
//:<------------------------------------------------------------------
#include <stdio.h>
#include "daqFormats.h"
#include "L3Formats.h"
#include "FtfTrack.h"

#ifndef GL3MERGINGTRACK
#define GL3MERGINGTRACK

class gl3MergingTrack: public FtfTrack {
public:
   void addTrack ( short sector, type1_track* thisTrack ) ;
};

#endif
