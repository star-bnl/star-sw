//:>------------------------------------------------------------------
//: FILE:       gl3GeneralHistos.cc
//: HISTORY:
//:              1feb2000 version 1.00
//:<------------------------------------------------------------------
#include "gl3GeneralHistos.h"


//####################################################################
//
//####################################################################
int gl3GeneralHistos::init ( l3List* histoList ) {
#ifdef GL3ONLINE
   char hid[50] ;
   char title[100] ;
//
//   Book histos
//
   strcpy ( hid, "pt" ) ;
   strcpy ( title, "pt" ) ;
   hPt = new gl3Histo ( hid, title, 100, 0., 2. ) ; 
   histoList->append ( (void *)hPt ) ;
   //
   strcpy ( hid,   "eta" ) ;
   strcpy ( title, "eta" ) ;
   hEta = new gl3Histo ( hid, title, 100, -2., 2. ) ; 
   histoList->append ( (void *)hEta ) ;
   //
   strcpy ( hid,   "psi" ) ;
   strcpy ( title, "psi" ) ;
   hPsi = new gl3Histo ( hid, title, 60, 0., 360. ) ; 
   histoList->append ( (void *)hPsi ) ;
   //
   strcpy ( hid,   "r0" ) ;
   strcpy ( title, "r0" ) ;
   hR0  = new gl3Histo ( hid, title, 50,  0., 200. ) ; 
   histoList->append ( (void *)hR0 ) ;
   //
   strcpy ( hid,   "z0" ) ;
   strcpy ( title, "z0" ) ;
   hZ0  = new gl3Histo ( hid, title, 80, -20., 20. ) ; 
   histoList->append ( (void *)hZ0 ) ;
   //
   strcpy ( hid,   "phi0" ) ;
   strcpy ( title, "phi0" ) ;
   hPhi0  = new gl3Histo ( hid, title, 60, 0., 360. ) ; 
   histoList->append ( (void *)hPhi0 ) ;
   //
   strcpy ( hid,   "nHitsPerTrack" ) ;
   strcpy ( title, "nHitsPerTrack" ) ;
   hNHitsTrack  = new gl3Histo ( hid, title, 60, -0.5 , 59.5 ) ; 
   histoList->append ( (void *)hNHitsTrack ) ;
   //
   strcpy ( hid,   "nHitsPerSector" ) ;
   strcpy ( title, "nHitsPerSector" ) ;
   hNHitsSector  = new gl3Histo ( hid, title, 100, -0.5 , 9999.5 ) ; 
   histoList->append ( (void *)hNHitsSector ) ;
   //
   strcpy ( hid,   "nTracks" ) ;
   strcpy ( title, "nTracks" ) ;
   hNTracks      = new gl3Histo ( hid, title, 100, -0.5 , 999.5 ) ; 
   histoList->append ( (void *)hNTracks ) ;
   //
   strcpy ( hid,   "RealTime" ) ;
   strcpy ( title, "RealTime" ) ;
   hRealTime  = new gl3Histo ( hid, title, 50, 0. , 200. ) ; 
   histoList->append ( (void *)hRealTime ) ;
   //
   strcpy ( hid,   "CpuTime" ) ;
   strcpy ( title, "CpuTime" ) ;
   hCpuTime  = new gl3Histo ( hid, title, 50, 0. , 200. ) ; 
   histoList->append ( (void *)hCpuTime ) ;
#endif
   return 0 ;
}
//####################################################################
//
//####################################################################
int gl3GeneralHistos::process ( gl3Event* event ) {
#ifdef GL3ONLINE
   int i ;
   short sector = 0 ;
   gl3Track* tTrack ;
   for ( i = 0 ; i < event->getNTracks() ; i++ ) { 
      tTrack = event->getTrack(i);
      sector = tTrack->id / 10000 ;  
      if ( sector > NSECTORS || sector <= 0 ) {
         fprintf ( stderr, " gl3Event:fillHistos:: wrong sector %d \n", sector ) ;
         continue ;
      }
      hPt->Fill     ( fabs(tTrack->pt),   1. ) ;
      hEta->Fill    ( tTrack->eta(),      1. ) ;
      hPsi->Fill    ( tTrack->psi*toDeg,  1. ) ;
      hR0->Fill     ( tTrack->r0,         1. ) ;
      hZ0->Fill     ( tTrack->z0,         1. ) ;
      hPhi0->Fill   ( tTrack->phi0*toDeg, 1. ) ;
      hNHitsTrack->Fill  ( tTrack->nHits, 1. ) ;
   }
//
//   Fill sector info
//
   gl3Sector* sectorInfo ;
   for ( i = 0 ; i < NSECTORS ; i++ ) {
      sectorInfo = event->getSector(i); 
      hNHitsSector->Fill ( sectorInfo->nHits,    1. ) ;
      hNTracks->Fill     ( sectorInfo->nTracks,  1. ) ;
      hCpuTime->Fill     ( float(sectorInfo->cpuTime)/1000.,  1. ) ;
      hRealTime->Fill    ( float(sectorInfo->realTime)/1000., 1. ) ;
   }
#endif
   return 0 ;
}
