#ifndef FTFGRAPHIC
#define FTFGRAPHIC
#include "FtfSl3.h"
#include "TCanvas.h"

#ifdef SL3ROOT
#include "Rtypes.h"
#else
#define ClassDef(a,b)
#endif

class FtfGraphic: public FtfSl3 {
public:   
   TCanvas *ftfCanvas ;
   float  bField ;
   float  phi   ;
   float  theta ;
   float  psi   ;
   float  phiMin ;
   float  phiMax ;
   float  etaMin ;
   float  etaMax ;
   float  xMin ;
   float  xMax ;
   float  yMin ;
   float  yMax ;
   float  zMin ;
   float  zMax ; 
   int    hitColor ;
   int    hitMarker ; 
   int    trackColor ;
   int    trackWidth  ;
   int    fitColor ;
   int    fitWidth  ;
//
   FtfGraphic ( ) ;
   ~FtfGraphic ( ) ;
//
   int  plotDetector ( ) ;
   int  plotHits     ( ) ;
   int  plotTracks   ( ) ;
   int  plotTracks   ( int thisTrack ) ;
   int  plotTracks   ( int firstTrack, int lastTrack ) ;
   int  plotFits     ( ) ;
   int  plotFits     ( int thisTrack ) ;
   int  plotFits     ( int firstTrack, int lastTrack ) ;
   int  plotFitsa    ( int thisTrack ) ;
   int  plotFitsa    ( int firstTrack, int lastTrack ) ;
   int  plotFit      ( FtfTrack *lTrack, float rMin, float rMax ) ;

   long setDefaults  ( ) ;
   void setXy ( ) { phi = 180. ; theta =  0. ; psi = 90. ; } ;
   void setYz ( ) { phi = 180. ; theta = 90. ; psi = 90. ; } ;
//
  ClassDef(FtfGraphic,1)
};
#endif
