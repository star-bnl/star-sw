#ifndef TRGGRAPHIC
#define TRGGRAPHIC

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "TrgGraphicHit.h"
#include "TrgGraphicTrack.h"
#include "TrgGraphicMcTrack.h"

#define min(a,b)    ( ( (a) < (b) ) ? (a) : (b) )
#define max(a,b)    ( ( (a) > (b) ) ? (a) : (b) )
#define toDeg 57.295779513
#define pi     3.141592654
#define twoPi  6.283185307
//
int const maxCylinders = 10 ;
int const maxBoxes     = 10 ;
//
//    Define cylinder class
//
class TrgCylinder {
public:
   TrgCylinder ( ) ;
   void fill    ( float in_radius, float in_length ) ;
   void plot    ( short view ) ;
private:
   float radius ;
   float length ;
} ;
//
//     Define box class
//
class TrgBox { 
public:
   TrgBox ( ) ;
   void fill    ( float x1, float x2,
	          float y1, float y2,
		  float z1, float z2 ) ;
   void plot ( short view ) ;
private:	
   float x1, x2 ;
   float y1, y2 ;
   float z1, z2 ;
} ;
//
//       Graphic parameters                  
//
class TrgGraphic {
// 
      
public:

   TrgGraphic ( ) : hit(0), track(0), mcTrack(0), nHits(0), nTracks(0), nMcTracks(0) {} ;
   void     dataSet       ( )   ;
   void     init          ( )   ;
   void     pickPoint     ( int point_level )   ;
   void     plotDetector  ( short lview ) ;
   void     plotFits      ( int   fst_track, int   no_tracks,      
				    float r_min,     float r_max,      int color ) ;
   void     plotGrid       ( ) ;
   void     plotHits       ( )   ;
   void     plotMcTracks   ( int fst_track, int no_tracks,     float pt_cut,
				    int color,     float label_scale, int label_pos  ) ;
   void     plotMcTracksMc( int fst_track, int no_tracks,     float pt_cut,
	                        int color,     int pid,           int   ppid ) ;
   void     plotTracks    ( int fst_track, int no_tracks,      float pt_cut,
		   		int color,     float label_scale,  int label_pos ) ;

   void     reset         ( )   ;
   void     selectArea    ( )   ;
   void     setPhaseSpace ( float etamin, float etamax, float phimin, float phimax ) ;
   void     zoom          ( float factor ) ;
//
//   Some utilities
//
// void   Evaluation                ( ) ;
// void   Fill_Track_Ntuple         ( int hid, FTF_Track *track ) ;
// void   Fill_Performance_Ntuple   ( int hid ) ; 
//
//     Graphic dependent operations
//
   void     clear ( ) ;
   void     getPoint            ( float *x, float *y ) ;
   void     defineArea          ( float x1, float x2, float y1, float y2 ) ;
   void     defineSize          ( float x1, float x2, float y1, float y2 ) ;
   void     plotCurve           ( int no_hit, float *x, float *y, float *z, float *r ) ; 
   void     plotPolymarker      ( int   n,  float *x, float *y ) ;
   void     plotPolyline        ( int   n,  float *x, float *y ) ;
   void     plotText            ( float *x, char *text, int color ) ;
   int      select              ( float x1, float y1, float x2, float y2 ) ;
   void     setPolymarkerColor  ( int color ) ;
   void     setPolylineColor    ( int color ) ;
   void     setTextHeight       ( float height ) ;
//
//    Variable access
//
   short    view          ;  // xy=1, xz=2, yz=3, rz=4  
   float    BField()  { return bField ; } ;
   float    EtaMin( ) { return etaMin ; } ;
   float    EtaMax( ) { return etaMax ; } ;
   short    HitErrors() { return hitErrors ; } ;
   float    PhiMin( ) { return phiMin ; } ;
   float    PhiMax( ) { return phiMax ; } ;
   float    XMinWn( ) { return xMinWn ; } ;
   float    XMaxWn( ) { return xMaxWn ; } ;
   float    YMinWn( ) { return yMinWn ; } ;
   float    YMaxWn( ) { return yMaxWn ; } ;
   float    ZMinWn( ) { return zMinWn ; } ;
   float    ZMaxWn( ) { return zMaxWn ; } ;
   float    RMinWn( ) { return rMinWn ; } ;
   float    RMaxWn( ) { return rMaxWn ; } ;
//
//    Variable setup
//
   void     addHitToTrack ( int hitIndex, int trackIndex ) ;
   void     fillHit ( int   id,
                      float  x, float  y, float z,
                      float dx, float dy, float dz,
                      int row ) ;
   void     fillTrack ( int lid,   int   lnhits, int   charge,
                        float lpt, float lpsi,   float tanl,
                        float lr0, float lphi0,  float z0,
                        float chi2_xy, float chi2_sz ) ;

   void     HitErrors      ( short in ) { hitErrors    = in ; } ;
   void     NHits          ( long  in ) { nHits        = in ; } ;
   void     NTracks        ( long  in ) { nTracks      = in ; } ;
   void     PlotGridFlag   ( short in ) { plotGridFlag = in ; } ;
   void     resetHitsTracks( int nh, int nt  ) ;
   void     XMinWn ( float in ) { xMinWn = in ; } ;
   void     XMaxWn ( float in ) { xMaxWn = in ; } ;
   void     YMinWn ( float in ) { yMinWn = in ; } ;
   void     YMaxWn ( float in ) { yMaxWn = in ; } ;
   void     ZMinWn ( float in ) { zMinWn = in ; } ;
   void     ZMaxWn ( float in ) { zMaxWn = in ; } ;

   long               NHits()     { return nHits ; }  ;
   long               NTracks()   { return nTracks ; } ;
   long               NMcTracks() { return nMcTracks ; } ;
   TrgGraphicHit*     Hit()       { return hit     ; } ;
   TrgGraphicTrack*   Track()     { return track   ; } ;
   TrgGraphicMcTrack* McTrack()   { return mcTrack ; } ;
//
//      Cylinder properties
//
   int nCylinders ;
   int nBoxes ;
   TrgCylinder cylinder[maxCylinders] ;
   TrgBox      box     [maxBoxes] ;

private:
   short    plotGridFlag  ;  /*   Plot grid flag     */
   short    mHitColor     ;
   short    hitErrors     ;
   short    detectorColor ;
   int      kwktyp       ;  /*   Work station type  */
   int      kwkid1       ;  /*   Work station id    */

   long     rowStart;
   long     rowEnd ;
   float    phiMin       ; /* Minimum phi displayed */
   float    phiMax       ; /* Minimum phi displayed */
   float    etaMin       ; /* Minimum eta displayed */
   float    etaMax       ; /* Maximum eta displayed */
//
//     Tracks and hits
//
   long              nHits ;
   long              nTracks ;
   long              nMcTracks ;
   long              maxHits ;
   long              maxTracks ;
   long              maxMcTracks ;
   TrgGraphicHit     *hit ;
   TrgGraphicTrack   *track ;
   TrgGraphicMcTrack *mcTrack ;
//
//   Parameters
//
   float    bField ;
//
//---> View port coordinates
//
   float   xMinVp  ;
   float   xMaxVp  ;
   float   yMinVp  ;
   float   yMaxVp  ;

   float   xMinWn ;
   float   xMaxWn ;
   float   yMinWn ;
   float   yMaxWn ;
   float   zMinWn ;
   float   zMaxWn ;
   float   rMinWn ;
   float   rMaxWn ;
};
#endif

