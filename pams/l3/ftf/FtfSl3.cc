/*:>-------------------------------------------------------------------
**: FILE:  
**: HISTORY:
**:   
**:  
**:<------------------------------------------------------------------*/
#include "FtfSl3.h"

//******************************************************************
//   fill tracks
//******************************************************************
int FtfSl3::fillTracks ( int maxBytes, int* buff ) {
//
//   Loop over tracks
//
    for ( int i = 0 ; i < nTracks ; i++ ) {
       sl3Track[i].id    = track[i].id ;
       sl3Track[i].nHits = track[i].nHits ;
       sl3Track[i].s11Xy = track[i].s11Xy ;
       sl3Track[i].s12Xy = track[i].s12Xy ;
       sl3Track[i].s22Xy = track[i].s22Xy ;
       sl3Track[i].g1Xy  = track[i].g1Xy  ;
       sl3Track[i].g2Xy  = track[i].g2Xy  ;
       sl3Track[i].s11Sz = track[i].s11Sz ;
       sl3Track[i].s12Sz = track[i].s12Sz ;
       sl3Track[i].s22Sz = track[i].s22Sz ;
       sl3Track[i].g1Sz  = track[i].g1Sz  ;
       sl3Track[i].g2Sz  = track[i].g2Sz  ;
       sl3Track[i].trackLength  = track[i].trackLength  ;
    }
//
//  Fill header
//
    TrackHeader head ;
    head.cpuTime  = cpuTime ;
    head.realTime = realTime ;
    head.nHits    = nHits ;
    head.nTracks  = nTracks ;
//
//   copy track info
//
    int *iP = buff+1;
    int *iPLast = buff + maxBytes ;
    int *iPNext =  iP + sizeof(TrackHeader) ;
    if ( iPNext > iPLast ) {
       printf ( " FtfSl3:fillTracks: buffer too short, maxBytes %d \n", maxBytes ) ; 
       return -1 ;
    }
    memcpy ( iP, (int *)&(head), sizeof(TrackHeader) ) ;
//
    iP     = iPNext ;
    iPNext = iP + nTracks*sizeof(sl3MPTrack) ;
    if ( iPNext > iPLast ) {
       printf ( " FtfSl3:fillTracks: buffer too short, maxBytes %d \n", maxBytes ) ;
       return -1 ;
    }
    memcpy ( iP, (int *)sl3Track, nTracks*sizeof(sl3MPTrack) ) ;
//
    int nBytes = (int)(iPNext-buff) ; 
    buff[0] = nBytes ;
//
    return nBytes ;
//
 }
//******************************************************************
//   Initialize sl3 tracker
//******************************************************************
int FtfSl3::setup ( ) {
//
//    Set parameters
//
  para.setDefaults ( ) ;
//
//    Set Parameters
//
  setParameters ( ) ;
//
//    Reset tracker
//
  reset ( ) ;
//
//     Allocate memory 
//
  maxHits        = 30000 ;
  maxTracks      =  1000 ;

  hit        = new FtfHit[maxHits] ;
  track      = new FtfTrack[maxTracks] ;
  sl3Track   = new sl3MPTrack[maxTracks] ;
//  l3Point hit[maxHits];
//
  para.phiMin = 75.F * pi / 180.F ;
  para.phiMax =105.F * pi / 180.F  ;
  para.etaMin = 0.F ;
  para.etaMax = 2.F ;
  para.mergePrimaries = 0 ;
  para.fillTracks     = 0 ;
#ifdef TRDEBUG
  para.trackDebug = 24 ;
  para.debugLevel =  1 ;
#endif
  return 0 ;
}
//************************************************************
//  Set input for hit array
//************************************************************
int FtfSl3::read ( int nPoints, l3Point *point ) {
//
  int i ;
  float x, y, z, r, theta, phi, eta ;
  int   row ;
  int counter = 0 ;
//
  for ( i = 0 ; i < nPoints ; i++ ) {
    x = point[i].X();
    y = point[i].Y();
    z = point[i].Z();
    if ( x > 200 ) continue ;
    row = point[i].Row();
//
//    Calculate phi and look for min and max
//
    phi = (float)atan2(y,x) ;
//
    if ( phi < para.phiMin ) continue ;
    if ( phi > para.phiMax ) continue ;
//
//    Calculate eta
//
    r     = (float)sqrt(x*x+y*y) ;
    theta = (float)atan2(r,z) ;
    eta   = -(float)log(tan(theta/2.));

    if ( eta < para.etaMin ) continue ;
    if ( eta > para.etaMax ) continue ;
//
    hit[counter].id       = i ;
    hit[counter].row      = (int)fmod(row,100);
    hit[counter].x        = x;
    hit[counter].y        = y;
    hit[counter].z        = z;
    hit[counter].dx       = 0.2F ;
    hit[counter].dy       = 0.2F ;
    hit[counter].dz       = 0.2F ;

    counter++ ;
  }
  printf ( " FtfSl3::read: hits %d read %d used \n ", nPoints, counter ) ;

  nHits = counter ;

  return counter ;
}
//******************************************************************
//    Read from cluster buffer 
//******************************************************************
int FtfSl3::read ( int nBytes, int* buff  ) {
//
   int counter = 0 ;
   int rowSize, overflow ;
   l3Cluster* pCluster ;
   int row, nPads ;
   FtfHit *hitP = hit ;
   char *cP = (char *)buff ;
   char *cPFirst = cP ;
   char *cPLast = cP + nBytes ;
//
   int nRows = *((int *)cP ) ;
   cP+=4;
   //
   while ( cP < cPLast ) {
      row = *((int *)cP ) ;
      if ( row < 1 || row > 45 ) {
         printf ( " FtfSl3::read:: cP %x cpLast %x  \n ", cP, cPLast ) ;
         printf ( " FtfSl3::read:: %d crazy row number, quit  \n ", row ) ;
         return 1 ;
      }
      int p = row-1;
      cP+=4;
      nPads = *((int *)cP ) ;
//    printf ( " Row %d with %d pads \n ", row, nPads ) ;
      cP+=4;
      pCluster = (l3Cluster *)(cP) ;
      for ( int i = 0 ; i < nPads ; i++ ) {
         hitP->id  = counter ;
         hitP->row = row ;
         hitP->x   = (float)(((float)(pCluster[i].pad))/64. - (nPadsInRow[p] >> 1)+.5) 
                          * padSpacing[p];
         hitP->y   = padrowOffset[p];
         hitP->z   = (float)(offset + driftLength - (((double)pCluster[i].time)/64.) * timeScale);
         hitP->dx  = 0.2F ;
         hitP->dy  = 0.2F ;
         hitP->dz  = 0.2F ;
//
         counter++;
         hitP++ ;
         if ( counter > maxHits ) {
             printf ( " FtfSl3:read: Hit array too small: counter %d maxHits %d \n ",
                       counter, maxHits ) ;
             break ;
          }
      }
      if ( counter > maxHits ) break ;
      rowSize = nPads * sizeof(l3Cluster) ;
      overflow    = fmod(rowSize,4);
      if ( overflow != 0 ) rowSize += 4 - overflow ;
      cP += rowSize ;
   }
   nHits = counter ;
   if ( debugLevel > 1 )
   printf ( " FtfSl3::read: nRows %d nBytes %d nHits %d \n ", nRows, nBytes, counter ) ;

   return counter;
}

//*******************************************************************
//    Process
//*******************************************************************
int FtfSl3::processSector ( ){ 
//
//   Reset hit track assignment
//
   for ( int h = 0 ; h < nHits ; h++ ) {
       hit[h].track = 0 ;
   }
   para.eventReset = 1 ;
   nTracks         = 0 ;
   process ( ) ;
//
   if ( debugLevel > 0 ) 
   printf ( " FtfSl3::process: tracks %i Time: real %f cpu %f\n ", 
                    nTracks, realTime, cpuTime ) ;
//
//
   return 1;
}
//***************************************************************
//   Set Default parameters
//***************************************************************
int FtfSl3::setParameters ( ) {

// FtfPara* para = &(para) ;

   para.hitChi2Cut        = 200.F  ;
   para.goodHitChi2       =  20.F  ;
   para.trackChi2Cut      = 100.F  ;
   para.segmentRowSearchRange = 2      ;
   para.trackRowSearchRange = 3    ;
   para.dphi              = 0.08F  ;
   para.deta              = 0.08F  ;
   para.dphiMerge        = 0.01F  ;
   para.detaMerge        = 0.02F  ;
   para.etaMinTrack      = -2.2F  ;
   para.etaMaxTrack      =  2.2F  ;
 
   para.getErrors        =  0     ;
   para.goBackwards      =  1     ;
   para.goodDistance     = 50.F   ;
   para.mergePrimaries   =  1    ;
   para.maxDistanceSegment = 50.F ;
   para.minHitsPerTrack  = 5      ;
   para.nHitsForSegment  = 2      ;
   para.nEta             = 40     ;
   para.nEtaTrack        = 40     ;
   para.nPhi             = 10     ;
   para.nPhiTrack        = 40     ;
   para.nPrimaryPasses   = 1      ;
   para.nSecondaryPasses = 0      ;
   para.xyErrorScale     = 1.0F   ;
   para.szErrorScale     = 1.0F   ;
   para.phiClosed        = 0      ;
  
   para.ptMinHelixFit     = 100.F  ;
   para.rVertex          = 0.F    ;
   para.xVertex          = 0.F    ;
   para.yVertex          = 0.F    ;
   para.zVertex          = 0.F    ;
   para.dxVertex         = 0.005F ;
   para.dyVertex         = 0.005F ;
   para.phiVertex        = 0.F    ;
//
   return 0 ;
}
