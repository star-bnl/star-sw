//:>------------------------------------------------------------------
//: FILE:       gl3Event.cc
//: HISTORY:
//:             3 dec 1999 version 1.00
//:             8 apr 2000 include modifications form Clemens
//:<------------------------------------------------------------------
#include "gl3Event.h"


//####################################################################
//
//####################################################################
void gl3Event::addTracks ( short sector, int nTrk, local_track* localTrack ) {
//
   gl3Track*    lTrack = &(track[nTracks]) ;
   local_track *trk    = localTrack ;
   int indexStore = -1 ;
//
   int idTrack ;
   for ( int i = 0 ; i < nTrk ; i++ ) { 
      lTrack->set ( sector, trk ) ; 
      lTrack->id     = nTracks + 1 ;
      lTrack->para   = &para ;
      lTrack->sector = sector ;
      lTrack->bField = bField ;
      idTrack        = trk->id ; 
      //
      //  Check where to store relation orig track<->final track
      //  but only if requested
      //
      if ( hitProcessing ) {
	 indexStore = -1 ;
	 if ( abs(idTrack) < maxTracksSector ) 
	    indexStore = (sector-1)*maxTracksSector + abs(idTrack) ;
	 else {   
	    printf ( " gl3Event::addTracks: max number of tracks per Sector reached %d reached\n", 
		  idTrack ) ;
	 } 
      }
      //
      //  if id from buffer is negative track is mergable
      //
      gl3Track* fatterTrack = 0 ;
      if ( idTrack < 0 ) {
         fatterTrack = lTrack->merge ( trackContainer ) ;
         if ( fatterTrack ) {
            if ( hitProcessing && indexStore > 0 ) {
                trackIndex[indexStore] = fatterTrack->id  ;
            }
            trk++ ;
            continue ;
         }
      }
//
//   Store new track in trackIndex array
//
      if ( hitProcessing && indexStore > 0 ) 
         trackIndex[indexStore] = lTrack->id  ; 
//
//   Increment counters
//
      lTrack++ ;
      nTracks++ ;
      trk++ ;
      if ( nTracks+1 >= maxTracks ) {
         printf ( " gl3Event::addTracks: max number of tracks %d reached\n", 
         maxTracks ) ;
	 nTracks-- ;
      }
   }
}
//####################################################################
//
//####################################################################
int gl3Event::fillTracks ( int maxBytes, char* buffer, unsigned int token ){
   //
   //   Check enough space
   //
   int nBytesNeeded = sizeof(L3_GTD) + (nTracks-1) * sizeof(global_track) ; 
   if ( nBytesNeeded > maxBytes ) {
      printf ( " gl3Event::writeTracks: %d bytes needed less than max = %d \n",
	    nBytesNeeded, maxBytes ) ;
      return 0 ;
   }

   L3_GTD* head = (L3_GTD *)buffer ;

   head->nTracks = nTracks ;
   head->nHits   = nHits  ; 
   head->xVert   = 0      ; // each sector has a different vertex???
   head->yVert   = 0      ; 
   head->zVert   = 0      ; 
   // bankHeader
   // 
   memcpy(head->bh.bank_type,CHAR_L3_GTD,8);
   head->bh.bank_id    = 1;
   head->bh.format_ver = DAQ_RAW_FORMAT_VERSION ; 
   head->bh.byte_order = DAQ_RAW_FORMAT_ORDER ;
   head->bh.format_number = 0;
   head->bh.token      = token; 
   head->bh.w9         = DAQ_RAW_FORMAT_WORD9;
   head->bh.crc        = 0; //don't know yet....
   //
   //   Loop over tracks now
   //
   global_track* oTrack = (global_track *)head->track ;
   for ( int i = 0 ; i < nTracks ; i++ ) {
      oTrack->id   = track[i].id ;
      oTrack->flag = track[i].flag ;
      oTrack->innerMostRow = track[i].innerMostRow ;
      oTrack->outerMostRow = track[i].outerMostRow ;
      oTrack->nHits        = track[i].nHits        ;
      oTrack->ndedx        = track[i].nDedx        ;
      oTrack->q            = track[i].q            ;
      oTrack->chi2[0]      = track[i].chi2[0]      ;
      oTrack->chi2[1]      = track[i].chi2[1]      ;
      oTrack->dedx         = track[i].dedx         ;
      oTrack->pt           = track[i].pt           ;
      oTrack->phi0         = track[i].phi0         ;
      oTrack->r0           = track[i].r0           ;
      oTrack->z0           = track[i].z0           ;
      oTrack->psi          = track[i].psi          ;
      oTrack->tanl         = track[i].tanl         ;
      oTrack->length       = track[i].length       ;
      oTrack->dpt          = track[i].dpt          ;
      oTrack->dpsi         = track[i].dpsi         ;
      oTrack->dz0          = track[i].dz0          ;
      oTrack->dtanl        = track[i].dtanl        ;
      oTrack++ ;
   }
//
//   return number of bytes
//
   return ((char *)oTrack-buffer) ;
}
//####################################################################
//
//####################################################################
int gl3Event::readEvent  ( int maxLength, char* buffer ){
   maxLength = 0 ;
   L3_P* header = (L3_P *)buffer ;

   int length, offset ;
   char* trackPointer ;
   char* hitPointer ;

   resetEvent ( );
   int i ;
   L3_SECP* sectorP ; 
   for ( i = 0 ; i < NSECTORS ; i++ ) {
      length = header->sector[i].len ;
      if ( !length ) continue ;
      offset = 4 * header->sector[i].off ;
      sectorP  = (L3_SECP *)&(buffer[offset]);

      trackPointer  = (char *)sectorP + sectorP->trackp.off * 4 ; 
      int nSectorTracks = readSectorTracks ( trackPointer ) ;
      if ( hitProcessing && sectorP->sl3clusterp.off ) {
         hitPointer    = (char *)sectorP + sectorP->sl3clusterp.off * 4 ; 
         readSectorHits   ( hitPointer, nSectorTracks ) ;
      }
   }
   //
   return 0 ;
}
//####################################################################
//
//####################################################################
int gl3Event::readSectorHits ( char* buffer, int nSectorTracks ){
   L3_SECCD* head = (L3_SECCD *)buffer ;
//
//   Construct coordinate transformer class
//
   St_l3_Coordinate_Transformer transformer;
//
//   Check bank header type
//
  if ( strncmp(head->bh.bank_type,CHAR_L3_SECCD,8) ) {
      printf ( "gl3Event::readSectorHits, wrong bank type %s\n", head->bh.bank_type ) ;
      printf ( " right bank type %s \n", CHAR_L3_SECCD ) ;
      return 0 ;
   } 
   int sector = head->bh.bank_id;
   int nSectorHits = head->nrClusters_in_sector ;
//
//    Check number of hits
//
   if ( nHits + nSectorHits > maxHits ) {
      printf ( "gl3Event:readSectorHits: not enough space for hits in sector %d\n",
                sector ) ;
      printf ( "         maxHits %d nSectorHits %d nHits %d \n", maxHits,
                         nSectorHits, nHits ) ;
      return 0 ;
   }

   l3_cluster* cluster = (l3_cluster *)head->cluster ;
   l3_cluster* hitP ;
   gl3Hit*     gHitP = 0  ;
   
   for ( int i = 0 ; i < nSectorHits ; i++ ) {
      hitP = &(cluster[i]) ;
      //
      //   Only if hits are going to be used for analysis unpack them
      //
      if ( hitProcessing > 1 ) {
         gHitP = &(hit[nHits+i]);
         gHitP->set ( &transformer, sector, hitP ) ;
      }
      //
      //  Now logic to reset trackId in hits
      //  This is to take care of track merging
      //
      int trkId = hitP->trackId ;
      if ( trkId < 0 || trkId > nSectorTracks ) { 
         printf ( "gl3Event:readSectorHits: %d wrong track id in hit of sector %d \n", 
                   trkId, sector ) ;
         continue ;
      }
      int indexStore = (sector-1)*maxTracksSector+trkId ;
      if ( indexStore < 0 || indexStore > NSECTORS*maxTracksSector ) { 
         printf ( "gl3Event:readSectorHits: %d wrong indexStore\n", indexStore ) ;
         continue ;
      }
      int index = trackIndex[indexStore] - 1  ;
      if ( index < 0 || index > nTracks ) continue ;
      //
      //   Only if hits are gonig to be used the
      //   linked lists are set
      //
      if ( hitProcessing > 1 ) {
	 if ( track[index].firstHit == 0 )
	    track[index].firstHit = (void *)gHitP ;
	 else
	    ((gl3Hit *)(track[index].lastHit))->nextHit = (void *)gHitP ;
	 track[index].lastHit = (void *)gHitP ;
         gHitP->trackId = track[index].id ;
      }
      //
      //   Modify trackId of clusters got from sl3
      //
      hitP->trackId = track[index].id ;
//    printf ( "hit trackId %d \n", track[index].id  ) ;

   }
   nHits += nSectorHits ;

   return nSectorHits ;
}
//####################################################################
//
//####################################################################
int gl3Event::readSectorTracks ( char* buffer ){

   struct L3_SECTP *head = (struct L3_SECTP *)buffer ;

   if ( strncmp(head->bh.bank_type,CHAR_L3_SECTP,8) ) {
      printf ( "gl3Event::readSectorTracks, wrong bank type %s\n", head->bh.bank_type ) ;
      return 0 ;
   }

   int sector = head->bh.bank_id ;
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
   sectorP->xVert     = float(head->xVert)/1000000 ;
   sectorP->yVert     = float(head->yVert)/1000000 ;
   sectorP->zVert     = float(head->zVert)/1000000 ;
   sectorP->rVert     = sqrt((double)(sectorP->xVert*sectorP->xVert+sectorP->yVert*sectorP->yVert)) ;
   sectorP->phiVert   = atan2((double)sectorP->yVert,(double)sectorP->xVert) ;
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
   char* pointer = head->banks[0].off * 4 + buffer ;
   int nSectorTracks ;
//
   if ( (head->banks[0].len > 0) &&  (head->bh.format_number > 0) ) {
     //    printf ( "banks[0].len %d\n", head->banks[0].len ) ;
     nSectorTracks = (4 * head->banks[0].len - sizeof(struct bankHeader))
       /sizeof(struct local_track);
   }
   else nSectorTracks = 0 ;
//
//   Add tracks
//
   if ( nSectorTracks > 0 ) {
     struct L3_LTD* headerLocal = (struct L3_LTD*)pointer ;
     local_track* localTrack = headerLocal->track ;
     addTracks ( sector, nSectorTracks, localTrack ) ;
   }
//
//   declare event as busy
//
   busy = 1 ;
//
   return sectorP->nTracks ;
}
//####################################################################
//
//####################################################################
int gl3Event::resetEvent  (  ){
   nHits         = 0 ;
   nTracks       = 0 ;
   nMergedTracks = 0 ;
   busy          = 0 ;

   memset ( trackContainer, 0, para.nPhiTrackPlusOne*para.nEtaTrackPlusOne*sizeof(FtfContainer) ) ;
   if ( hitProcessing )
   memset ( trackIndex, 0, maxTracksSector*NSECTORS*sizeof(int) ) ;
/*
   for ( int i = 0 ; i < nTracks ; i++ ) {
      track[i].firstHit = 0 ;
      track[i].lastHit = 0 ;
      track[i].nextTrack = 0 ;
   }
   */

   return 0 ;
}

//####################################################################
//
//####################################################################
int gl3Event::setup ( int mxHits, int mxTracks ){

   if ( mxHits < 0 || mxHits > 1000000 ) {
      fprintf ( stderr, " gl3Event::setup: maxHits %d out of range \n",
                        maxHits ) ;
      mxHits = 500000 ;
   }

   if ( mxTracks < 0 || mxTracks > 1000000 ) {
      fprintf ( stderr, " gl3Event::setup: maxTracks %d out of range \n",
                        maxTracks ) ;
      mxTracks = 50000 ;
   }


   maxHits    = mxHits ;
   maxTracks  = mxTracks ;
   maxTracksSector = maxTracks*2/ NSECTORS ;
   hit        = new gl3Hit[maxHits] ;
   track      = new gl3Track[maxTracks] ;
   trackIndex = new int[maxTracksSector*NSECTORS];
//
//   Merging variables
//
   nMergedTracks   = 0 ;

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
   para.ptMinHelixFit = 1.e60 ;

   nTracks = 0 ;
//
   para.bField = 0.5 ;
//

   return 0 ;
}
