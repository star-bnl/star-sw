/*:>-------------------------------------------------------------------
**: FILE:  
**: HISTORY:
**:           aug 23, 1999  add setup with number of hits and tracks
**:           oct  6, 1999  ppy new version from Christof
**:   
**:  
**:<------------------------------------------------------------------*/
#include "FtfSl3.h"

UINT32 swap32(unsigned int in);

#ifdef SL3ROOT
ClassImp(FtfSl3)
#endif

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
    }//
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
       printf ( "FtfSl3:fillTracks: buffer too short, maxBytes %d \n", maxBytes ) ; 
       return -1 ;
    }
    memcpy ( iP, (int *)&(head), sizeof(TrackHeader) ) ;
//
    iP     = iPNext ;
    iPNext = iP + nTracks*sizeof(sl3MPTrack) ;
    if ( iPNext > iPLast ) {
       printf ( "FtfSl3:fillTracks: buffer too short, maxBytes %d \n", maxBytes ) ;
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
//   fill tracks
//******************************************************************
int FtfSl3::fillUSTracks ( int maxBytes, int* buff ) {
//
    long nBytes = sizeof(TrackHeader) - sizeof(int) + nTracks*sizeof(sl3USTrack) ;
    if ( nBytes > maxBytes ) {
       printf ( "FtfSl3::fillUSTracks: %d too short a buffer \n", maxBytes ) ;
       return 1 ;
    }
//
//  Fill header
//
    TrackHeader *head = (TrackHeader *)buff ;
    head->cpuTime  = cpuTime ;
    head->realTime = realTime ;
    head->nHits    = nHits ;
    head->nTracks  = nTracks ;
//
//   copy track info
//
    sl3USTrack *bTrack = (sl3USTrack *)&(head->bTrack) ;
//
//   Loop over tracks
//
   for ( int i = 0 ; i < nTracks ; i++ ) {
       bTrack[i].id    = track[i].id ;
       bTrack[i].pt    = track[i].pt    ;
       bTrack[i].psi   = track[i].psi   ;
       bTrack[i].tanl  = track[i].tanl  ;
       bTrack[i].r0    = track[i].r0    ;
       bTrack[i].phi0  = track[i].phi0  ;
       bTrack[i].z0    = track[i].z0    ;
       bTrack[i].nHits = track[i].nHits ;
       bTrack[i].q     = track[i].q     ;
   }
#ifdef DEBUG
   if ( debugLevel > 1 ) {
      printf ( "FtfSl3::fillUSTracks: %d bytes of track data\n",nBytes ) ;
   }
#endif
   return nBytes ;
//
}
//******************************************************************
//   Initialize sl3 tracker
//******************************************************************
int FtfSl3::setup ( int maxHitsIn, int maxTracksIn ) {
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
  maxHits    = maxHitsIn ;
  maxTracks  = maxTracksIn ;
  hit        =  (FtfHit*) new FtfHit[maxHits] ;
  FtfHit* head = (FtfHit*) new FtfHit[maxHits] ;
  track      = new FtfTrack[maxTracks] ;
  sl3Track   = new sl3MPTrack[maxTracks] ;
//
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
//******************************************************************
//    Read cluster data from TPCMZCLD bank (using daqFormats.h)
//
//    returns number of hits from this mezzanine
//
//     !! ONLY FOR ALPHA/PC !!
//     !! no byte swapping hardcoded !!
//
//     author: christof, derived from tonko's 'sl3'
//******************************************************************
int FtfSl3::readMezzanine (int sector, struct TPCMZCLD_local *mzcld) {

   unsigned int *tmp32;
   int row, rows;
   int len;
   int i, j;
   int counter;

   FtfHit *hitP = &hit[nHits];

   counter = 0;

   tmp32 = mzcld->padrowFiller ;

   rows = *tmp32++ ;

   //fprintf(stderr,"       Found %d rows.\n",rows) ;

   for (i=0; i<rows; i++) {
           row = *tmp32++ ;
	   len = *tmp32++ ;
	   //fprintf(stderr,"        Row# %d: clusters %d\n",row,len) ;

	   for ( j=0; j<len; j++) {
	           double fp, ft ;
		   double x;
		   double y;
		   double z;

		   struct xt {
		         unsigned short x ;
		         unsigned short t ;
		   } *xt ;
		   struct c {
		         unsigned short f ;
		         unsigned short c ;
		   } *c ;

		   xt = (struct xt *) tmp32++ ;
		   c = (struct c *) tmp32++ ;

		   fp = (double) xt->x / 64.0 ;
		   ft = (double) xt->t / 64.0 ;
		   //printf("%02d %02d %9.5f %9.5f %6d %3d\n", SB, row,
		   //       fp, ft, c->c , c->f) ;
		   rawToGlobal(sector, row, fp, ft, &x, &y, &z);

		   //printf(" %d  %d  %f  %f  %f  %d  %d\n",
		   //	  sector, row, x, y, z, c->c, c->f);

		   hitP->id  = counter ;
		   hitP->row = row ;
		   hitP->x   = (float) x;
		   hitP->y   = (float) y;
		   hitP->z   = (float) z;
		   hitP->dx  = 0.2F ;
		   hitP->dy  = 0.2F ;
		   hitP->dz  = 0.2F ;

		   hitP++;

		   counter++;
		   if ( (nHits+counter)>maxHits ) {
		           fprintf (stderr, "Error - FtfSl3:read: Hit array too small: counter %d maxHits %d \n",
				    counter, maxHits ) ;
			   return -1;
		   }
	   }
   }
   return counter;
}

//******************************************************************
//     Read Sector from buffer (using daqFormats.h)
//
//     !! ONLY FOR ALPHA/PC !!
//     !! byte swapping hardcoded !!
//
//     author: christof, derived from tonko's 'sl3'
//******************************************************************
int FtfSl3::readSector ( struct TPCSECLP *seclp ) {

    struct TPCRBCLP *rbclp ;
    struct TPCMZCLD_local *mzcld ;
    int iRb, kMz;
    int sector;
    int nHitsOfMz;

    // reset sector-hit counter
    nHits = 0;

    // check byte order of SECLP bank
    // byte swapping is needed
    if (checkByteOrder(swap32((unsigned int)seclp->bh.byte_order))) {
            ;
    }
    else {
            fprintf(stderr, "Error - FtfSl3::readSector: Wrong byte order in SECLP bank!\n");
	    return -1;
    }

    nHitsOfMz = 0;
    sector = swap32((unsigned int)seclp->bh.bank_id) ;

    // run over receiver boards
    for (iRb=0; iRb<SB_RB_NUM; iRb++) {

            if (iRb==6) sector++ ;	// the other Sector

	    if (swap32((unsigned int)seclp->rb[iRb].off)) {
#ifdef TRDEBUG
	            if ( debugLevel > 1 ) {
		            fprintf (stderr, "FtfSl3::readSector:     RBCLP %d exists!\n", iRb+1) ;
		    }
#endif
	    }
	    else continue ;

	    rbclp = (struct TPCRBCLP *)((char *)seclp + swap32((unsigned int)seclp->rb[iRb].off) * 4) ;

	    // run over the 3 mezzanines
	    for (kMz=0; kMz<RB_MZ_NUM; kMz++) {

	            if(rbclp->mz[kMz].off) {
#ifdef TRDEBUG
		            if ( debugLevel > 1 ) {
			            fprintf (stderr, "FtfSl3::readSector:       MZCLD %d exists!\n", kMz+1) ;
			    }
#endif
		    }
		    else continue ;

		    mzcld = (struct TPCMZCLD_local *) ((char *)rbclp + rbclp->mz[kMz].off*4) ;

		    if (mzcld) {
		            nHitsOfMz = readMezzanine (sector, mzcld);
			    if (nHitsOfMz<0) {
			            return -1;
			    }
			    nHits += nHitsOfMz;
		    }
		    
	    }
    }
    return 0;
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
   printf ( " FtfSl3::process: tracks %i Time: real %f cpu %f\n", 
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

   return 0 ;
}
