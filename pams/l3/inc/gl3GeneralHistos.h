//:>------------------------------------------------------------------
//: FILE:       gl3GeneralHistos.h
//: HISTORY:
//:              1feb2000 version 1.00
//:<------------------------------------------------------------------
#include "gl3Analysis.h"

#ifndef GL3GENERALHISTOS
#define GL3GENERALHISTOS


class gl3GeneralHistos: public gl3Analysis {
public:
    int init    ( l3List*  ) ;
    int process ( gl3Event* ) ;
    //
    //   Histograms
    //
    gl3Histo* hPt  ;
    gl3Histo* hPsi ;
    gl3Histo* hEta ;
    gl3Histo* hR0  ;
    gl3Histo* hZ0  ;
    gl3Histo* hPhi0 ;
    gl3Histo* hNHitsTrack ;
    gl3Histo* hNHitsSector ;
    gl3Histo* hNTracks ;
    gl3Histo* hRealTime ;
    gl3Histo* hCpuTime ;

};
#endif
