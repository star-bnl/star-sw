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
   gl3MergingTrack* lTrack = &(mergingTrack[nMergingTracks]) ;
   gl3MergingTrack* pTrack = lTrack ;
   type1_track *trk    = track1 ;
//
   for ( int i = 0 ; i < nTrk ; i++ ) { 
      pTrack->para = &para ;
      pTrack->addTrack ( sector, trk ) ;
      trk++ ;
      if ( pTrack->mergePrimary ( trackContainer ) ) continue ;
      pTrack++ ;
      nMergingTracks++ ;
      if ( nMergingTracks > maxMergingTracks ) {
         fprintf ( stderr, 
        "gl3Event::addTracks: max number of merging tracks %d reached\n", 
         maxMergingTracks ) ;
	 nMergingTracks-- ;
         break ;
      }
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
      lTrack->set ( sector, trk, 
		    sectorInfo[sector-1].rVert, sectorInfo[sector-1].phiVert ) ; 
//    lTrack->print();
      trk++ ;
      lTrack++ ;
      nTracks++ ;
      if ( nTracks > maxTracks ) {
         fprintf ( stderr, 
         "gl3Event::addTracks: max number of tracks %d reached\n", maxTracks ) ;
	 nTracks-- ;
         break ;
      }
   }
}
//####################################################################
//
//####################################################################
void gl3Event::addTracks ( short sector, int nTrk, type3_track* track3 ) {
//
   gl3Track*    lTrack = &(track[nTracks]) ;
   type3_track *trk    = track3 ;
//
   for ( int i = 0 ; i < nTrk ; i++ ) { 
      lTrack->set ( sector, trk ) ; 
//    lTrack->print();
      trk++ ;
      lTrack++ ;
      nTracks++ ;
      if ( nTracks > maxTracks ) {
         fprintf ( stderr, 
        " gl3Event::addTracks: max number of tracks %d reached\n", 
         maxTracks ) ;
	 nTracks-- ;
         break ;
      }
   }
}

//####################################################################
//
//####################################################################
int gl3Event::readEvent  ( int maxLength, char* buffer ){
   maxLength = 0 ;
   L3_P* header = (L3_P *)buffer ;

   int length, offset ;
   char* pointer ;

   nTracks = 0 ;
   nMergingTracks = 0 ;
   memset ( trackContainer, 0, para.nPhiTrackPlusOne*para.nEtaTrackPlusOne*sizeof(FtfContainer) ) ;

   int i ;
   L3_SECP* sectorP ;
   for ( i = 0 ; i < NSECTORS ; i++ ) {
      length = header->sector[i].len ;
      if ( !length ) continue ;
      offset = 4 * header->sector[i].off ;
      sectorP  = (L3_SECP *)&(buffer[offset]);
      pointer  = (char *)sectorP + sectorP->trackp.off * 4 ;
      printf ( "FtfSl3:readEvent:reading sector %d\n", i+1 ) ;
      if ( readSector ( pointer ) ) {
         printf ( "FtfSl3::readEvent: problems reading sector %d, quit\n", i ) ;
         break ;
      }
   }
   //
   //   Copy merging tracks to gl3Tracks
   //
   for ( i = 0 ; i < nMergingTracks ; i++ ) {
      track[nTracks].set ( &(mergingTrack[i]) ) ;
      nTracks++ ;
      if ( nTracks > maxTracks ) {
	 fprintf ( stderr, "gl3Event::readEvent: max number tracks %d reached \n",
	       nTracks ) ;
	 break ;
      }
   }
   //
   return 0 ;
}
//####################################################################
//
//####################################################################
int gl3Event::readSector ( char* buffer ){

   struct L3_SECTP *head = (struct L3_SECTP *)buffer ;
   int sizeWord = 4; // 4 bytes per dword in raw formats

   int sector = head->bh.bank_id ;
// printf ( "gl3Event::readSector: sector %d \n",sector ) ;
   if ( sector < 0 || sector > NSECTORS ) {
      fprintf ( stderr, " gl3Event::readSector: %d wrong sector \n", sector ) ;
      return 1 ;
   }

   gl3Sector* sectorP = &(sectorInfo[sector-1]) ;
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
//
//   Set vertex parameters for track merging 
//
   para.xVertex   = sectorP->xVert ;
   para.yVertex   = sectorP->yVert ;
   para.zVertex   = sectorP->zVert ;
   para.rVertex   = sectorP->rVert ;
   para.phiVertex = sectorP->phiVert ;
//
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
      addTracks ( sector, nTracks3, track3 ) ;
   }
//
//   declare event as busy
//
   busy = 1 ;
// 
   return 0 ;
}
//####################################################################
//
//####################################################################
int gl3Event::resetEvent  (  ){
   nTracks = 0 ;
   busy    = 0 ;
   return 0 ;
}

//####################################################################
//
//####################################################################
int gl3Event::setup ( int mxTracks, int mxMergingTracks ){

   if ( mxTracks < 0 || mxTracks > 1000000 ) {
      fprintf ( stderr, " gl3Event::setup: maxTrack %d out of range \n",
                        maxTracks ) ;
      mxTracks = 50000 ;
   }
   maxTracks = mxTracks ;
   track = new gl3Track[maxTracks] ;
//
//   Merging variables
//
   maxMergingTracks = mxMergingTracks ;
   nMergingTracks   = 0 ;
   mergingTrack = new gl3MergingTrack[maxMergingTracks];


   para.nPhiTrackPlusOne = para.nPhiTrack + 1 ;
   para.nEtaTrackPlusOne = para.nEtaTrack + 1 ;
//-------------------------------------------------------------------------
//         If needed calculate track area dimensions
//-------------------------------------------------------------------------
   para.phiSliceTrack   = (para.phiMaxTrack - para.phiMinTrack)/para.nPhiTrack ;
   para.etaSliceTrack   = (para.etaMaxTrack - para.etaMinTrack)/para.nEtaTrack ;

   int nTrackVolumes = para.nPhiTrackPlusOne* para.nEtaTrackPlusOne ;
   trackContainer = new FtfContainer[nTrackVolumes];
   if(trackContainer == NULL) {
      fprintf ( stderr, "Problem with memory allocation... exiting\n") ;
      return 1 ;
   }
   para.primaries = 1 ;

   nTracks = 0 ;
//
//

   return 0 ;
}
 
