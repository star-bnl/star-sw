//:>------------------------------------------------------------------
//: FILE:       gl3Event.cc
//: HISTORY:
//:              3dec1999 version 1.00
//:<------------------------------------------------------------------
#include "gl3Event.h"


//####################################################################
//
//####################################################################
void gl3Event::addTracks ( short sector, int nTrk, type1_track* track1 ) {
//
   gl3Track*    lTrack = &(track[nTracks]) ;
   type1_track *trk    = track1 ;
//
   for ( int i = 0 ; i < nTrk ; i++ ) { 
      lTrack->set ( trk, bField,
                      sectorInfo[sector].xVert, sectorInfo[sector].yVert, 
		      sectorInfo[sector].rVert, sectorInfo[sector].phiVert ) ; 
//    lTrack->print();
      trk++ ;
      lTrack++ ;
      nTracks++ ;
   }
}
//####################################################################
//
//####################################################################
void gl3Event::addTracks ( short sector, int nTrk, type2_track* track2 ) {
//
   gl3Track*    lTrack = &(track[nTracks]) ;
   type2_track *trk    = track2 ;
//
   for ( int i = 0 ; i < nTrk ; i++ ) { 
      lTrack->set ( trk, 
		      sectorInfo[sector].rVert, sectorInfo[sector].phiVert ) ; 
//    lTrack->print();
      trk++ ;
      lTrack++ ;
      nTracks++ ;
   }
}
//####################################################################
//
//####################################################################
void gl3Event::addTracks ( int nTrk, type3_track* track3 ) {
//
   gl3Track*    lTrack = &(track[nTracks]) ;
   type3_track *trk    = track3 ;
//
   for ( int i = 0 ; i < nTrk ; i++ ) { 
      lTrack->set ( trk ) ; 
//    lTrack->print();
      trk++ ;
      lTrack++ ;
      nTracks++ ;
   }
}
//####################################################################
//
//####################################################################
int gl3Event::readEvent  ( int maxLength, char* buffer ){
   maxLength = 0 ;
   return 0 ;
}
//####################################################################
//
//####################################################################
int gl3Event::readSector ( int maxLength, char* buffer ){

   struct L3_SECTP *head = (struct L3_SECTP *)buffer ;
   int sizeWord = 4; // 4 bytes per dword in raw formats

   int sector = head->bh.bank_id ;
// printf ( "gl3Event::readSector: sector %d \n",sector ) ;
   if ( sector < 0 || sector > NSECTORS ) {
      fprintf ( stderr, " gl3Event::readSector: %d wrong sector \n", sector ) ;
      return 1 ;
   }

   gl3Sector* sectorP = &(sectorInfo[sector]) ;
   sectorP->filled = 1 ;
   sectorP->nHits     = head->nHits ;
   sectorP->nTracks   = head->nTracks ;
   sectorP->cpuTime   = head->cpuTime ;
   sectorP->realTime  = head->realTime ;
   sectorP->xVert     = head->xVert ;
   sectorP->yVert     = head->yVert ;
   sectorP->zVert     = head->zVert ;
   sectorP->rVert     = sqrt((double)(head->xVert*head->xVert+head->yVert*head->yVert)) ;
   sectorP->phiVert   = atan2((double)head->yVert,(double)head->xVert) ;
   if ( sectorP->phiVert < 0 ) sectorP->phiVert += 2. * M_PI ;

 //sectorP->print();


   char* pointer1 = head->banks[0].off * sizeWord + buffer ;
   int nTracks1, nTracks2, nTracks3 ;
   if ( head->banks[0].len > 0 ) {
//    printf ( "banks[0].len %d\n", head->banks[0].len ) ;
      nTracks1 = (sizeWord*head->banks[0].len-sizeof(struct bankHeader))
	         /sizeof(struct type1_track);
   }
   else nTracks1 = 0 ;
   char* pointer2 = head->banks[1].off * sizeWord + buffer ;
   if ( head->banks[1].len > 0 ) {
//    printf ( "banks[1].len %d\n", sizeWord*head->banks[1].len ) ;

      nTracks2 = (sizeWord*head->banks[1].len-sizeof(struct bankHeader))
	         /sizeof(struct type2_track);
   }
   else nTracks2 = 0 ;
   char* pointer3 = head->banks[2].off * sizeWord + buffer ;
   if ( head->banks[2].len > 0 ) {
//    printf ( "banks[2].len %d\n", head->banks[2].len ) ;
      nTracks3 = (sizeWord*head->banks[2].len-sizeof(struct bankHeader))
	         /sizeof(struct type3_track);
   }
   else nTracks3 = 0 ;
// printf ( "nTracks 1 2 3 %d %d %d\n",nTracks1, nTracks2, nTracks3 ) ;
//
//   Add different types of tracks
//
   if ( nTracks1 > 0 ) {
      L3_STK1D* headerType1 = (struct L3_STK1D*)pointer1 ;
      type1_track* track1 = headerType1->track ;
      addTracks ( sector, nTracks1, track1 ) ;
   }
//
   if ( nTracks2 > 0 ) {
      L3_STK2D* headerType2 = (struct L3_STK2D*)pointer2 ;
      type2_track* track2 = headerType2->track ;
      addTracks ( sector, nTracks2, track2 ) ;
   }
//
   if ( nTracks3 > 0 ) {
      L3_STK3D* headerType3 = (struct L3_STK3D*)pointer3 ;
      type3_track* track3 = headerType3->track ;
      addTracks ( nTracks3, track3 ) ;
   }
// 
   return 0 ;
}
//####################################################################
//
//####################################################################
int gl3Event::setup ( int mxTracks ){

   if ( mxTracks < 0 || mxTracks > 1000000 ) {
      fprintf ( stderr, " gl3Event::setup: maxTrack %d out of range \n",
                        maxTracks ) ;
      mxTracks = 50000 ;
   }
   maxTracks = mxTracks ;
   track = new gl3Track[maxTracks] ;
   nTracks = 0 ;
   return 0 ;
}
