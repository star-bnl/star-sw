/*:>-------------------------------------------------------------------
**: FILE:       sl3StandAlone.cc
**: HISTORY:
**:   
**: 10/12/99 ppy Demo version 
**: 10/18/99 ppy Check in reading loop to turn all phi angle to positive values
**:              It causes problems when reading hits with phi=(0,2*pi)
**: 11/05/99 ppy Add line "tracker.reset()" after parameter modification
**:  
**:<------------------------------------------------------------------*/
#include "FtfSl3.h"

FtfSl3     tracker ;

void setParameters ( FtfPara* para ) ;

int main ( ) {
//
//    Set parameters
//
  tracker.setup (  ) ;
//
//    Open input file
//
  FILE* datafile = fopen("TpcSectorOneForAuAuCentralHijing.txt", "r");
//FILE* datafile = fopen("/star/u2c/yepes/jpsi1000rest.xyzdat", "r");
//FILE* datafile = fopen("/star/u2e/soeren/track_data/jpsi.dat", "r");
  if (datafile == NULL)
  {
    printf ( " \n Error opening input file \n " ) ;
    return 1 ;
  }
//
//   Read file
//
  float x, y, z, r, theta, phi, eta ;
  int   row ;
//
//   
  tracker.para.phiMin = 45.F * pi / 180.F ;
  tracker.para.phiMax = 75.F * pi / 180.F  ;
//tracker.para.phiMin =  0.F * pi / 180.F ;
//tracker.para.phiMax = 360.F * pi / 180.F  ;
  tracker.para.etaMin =  0.0F ;
  tracker.para.etaMax =  2.0F ;
  tracker.para.fillTracks = 1 ; 
#ifdef TRDEBUG
  tracker.para.trackDebug = 24 ;
  tracker.para.debugLevel =  1 ;
#endif
// !!! IMPORTANT !!!! tracker needs to be reset 
// when parameters are changed
  tracker.reset(); 
//
  int i ;
  int counter = 0 ;
  for ( i=0 ;  ; i++ )
  {
    if ( fscanf ( datafile, "%f %f %f %d", &x,&y,&z,&row) == EOF ) break ;
//
//    Calculate phi and look for min and max
//
    phi = (float)atan2(y,x) ;
    if ( phi < 0 ) phi += 2. * M_PI ;

    if ( phi < tracker.para.phiMin ) continue ;
    if ( phi > tracker.para.phiMax ) continue ;
//
//    Calculate eta
//
    r     = (float)sqrt(x*x+y*y) ;
    theta = (float)atan2(r,z) ;
    eta   = -(float)log(tan(theta/2.));

    if ( eta < tracker.para.etaMin ) continue ;
    if ( eta > tracker.para.etaMax ) continue ;
//
    tracker.hit[counter].id       = i ;
    tracker.hit[counter].row      = (int)fmod(row,100);
    tracker.hit[counter].x        = x;
    tracker.hit[counter].y        = y;
    tracker.hit[counter].z        = z;
    tracker.hit[counter].dx       = 0.2F ;
    tracker.hit[counter].dy       = 0.2F ;
    tracker.hit[counter].dz       = 0.2F ;

    counter++ ;
    if ( counter >= tracker.maxHits ) {
       printf ( "sl3StandAlone: max no Hits %d reached \n", tracker.maxHits ) ;
       break ;
    }
  }


  tracker.nHits = counter ;

  int nLoops = 1 ;
  int   lastNTracks = 0 ;
  float sectorTime ;
  float averageTime       = 0.F ;
  float averageSquareTime = 0.F ;
//
  printf ( " \n +++++++++++++++++++++++++++++++++++ " ) ;
  printf ( " \n ++ Start   benchmarking program ++ " ) ;
  printf ( " \n ++ Number of loops %d ", nLoops  ) ;
  printf ( " \n +++++++++++++++++++++++++++++++++++ " ) ;
//
  for ( int loop = 0 ; loop < nLoops ; loop++ ) {
     for ( int h = 0 ; h < tracker.nHits ; h++ ) {
         tracker.hit[h].track = 0 ;
     }
     tracker.para.eventReset = 1 ;
     tracker.nTracks         = 0 ;

     sectorTime = tracker.process ( ) ;

     if ( loop > 0 && lastNTracks != tracker.nTracks ) {
        printf ( "\n Warning variation in number of found tracks !!!! " ) ;
     }
     lastNTracks = tracker.nTracks ;

 //    printf ( " \n Loop %d Number of tracks %d, Time %8.4f", 
 //             loop, tracker.nTracks, sectorTime ) ;

     averageTime       += sectorTime ;
     averageSquareTime += sectorTime * sectorTime ;
  }
#ifdef PRINT
  for ( int i = 0 ; i < tracker.nTracks ; i++ ) {
      printf ( " %d pt %f tanl %f psi %f nHits %d \n ", i, 
                                           tracker.track[i].pt, 
                                           tracker.track[i].tanl,
                                           tracker.track[i].psi, 
                                           tracker.track[i].nHits );
  }
#endif
  averageTime       /= (float)nLoops ;
  averageSquareTime /= (float)nLoops ;

  float timeRMS = (float)sqrt(averageSquareTime - averageTime * averageTime) ;

  printf ( "\n **************************************************** " ) ;
  printf ( "\n ** Number of loops:                %d ", nLoops ) ;
  printf ( "\n ** Number of reconstructed tracks: %d ", tracker.nTracks ) ;
  printf ( "\n Sector Time in seconds:            %8.4f +/- %8.4f ", averageTime, timeRMS ) ;
  printf ( "\n **************************************************** " ) ;
  printf ( "\n " ) ;
//
  printf ( "before nPhi %d \n", tracker.para.nPhi ) ;
  tracker.para.read ( "parameters.txt" ) ; 
  printf ( "nPhi %d \n", tracker.para.nPhi ) ;
  tracker.para.write( "parameter2.txt" ) ; 
//
//
   return 1;
}
//
//   Set Default parameters
//
void setParameters ( FtfPara* para ) {


   para->hitChi2Cut        = 200.F  ;
   para->goodHitChi2       =  10.F  ;
   para->trackChi2Cut      = 100.F  ;
   para->segmentRowSearchRange = 2      ;
   para->trackRowSearchRange = 3    ;
   para->dphi              = 0.08F  ;
   para->deta              = 0.04F  ;
   para->dphiMerge        = 0.01F  ;
   para->detaMerge        = 0.02F  ;
   para->etaMinTrack      = -2.2F  ;
   para->etaMaxTrack      =  2.2F  ;
 
   para->getErrors        =  0     ;
   para->goBackwards      =  1     ;
   para->goodDistance     = 50.F   ;
   para->mergePrimaries   =  1    ;
   para->maxDistanceSegment = 50.F ;
   para->minHitsPerTrack  = 5      ;
   para->nHitsForSegment  = 2      ;
   para->nEta             = 40    ;
   para->nEtaTrack        = 40     ;
   para->nPhi             = 10     ;
   para->nPhiTrack        = 40     ;
   para->nPrimaryPasses   = 1      ;
   para->nSecondaryPasses = 0      ;
   para->xyErrorScale     = 1.0F   ;
   para->szErrorScale     = 1.0F   ;
   para->phiClosed        = 0      ;
  
   para->ptMinHelixFit    = 0.F  ;
   para->rVertex          = 0.F    ;
   para->xVertex          =10.F    ;
   para->yVertex          = 0.F    ;
   para->zVertex          =10.F    ;
   para->dxVertex         = 0.005F ;
   para->dyVertex         = 0.005F ;
   para->phiVertex        = 0.F    ;

}
