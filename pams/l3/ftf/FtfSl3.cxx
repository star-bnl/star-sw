/*:>-------------------------------------------------------------------
**: FILE:     FtfSl3.cxx 
**: HISTORY:
**:           aug 23, 1999  add setup with number of hits and tracks
**:           oct  6, 1999  ppy new version from Christof
**:           oct 11, 1999  ppy call rawToLocal with variables instead of pointers
**:           oct 11, 1999  ppy phiMin, phiMax changed to 0 and 2 pi
**:           oct 21, 1999  ppy call to rawToGlobal with variables rather than pointers
**:           oct 22, 1999  ppy back to pointers to make Christof happy
**:
**:           nov 24, 1999  cle extra method for filling US tracks deleted
**:                         (FtfSl3::fillUSTracks) 
**:                         added 'token' as argument for fill method
**:                         changed formats to official L3 formats
**:           dec  2, 1999  ppy setup and readSector modified
**:                         sectorGeo class added to keep sector phase space
**:                         readSector uses it to define tracker phase space
**:           dec  6, 1999  ppy add xLastHit and yLastHit to type1_track
**:           dec  6, 1999  ppy provisional patch to avoid generaring type1 tracks
**:           dec  6, 1999  ppy pt in type 1 & 2 tracks is pt * charge  
**:           dec  6, 1999  ppy method CanItBeMerged added
**:           dec 16, 1999  ppy readSector and readMezzaninne look at
**:                             byte order now
**:           dec 20, 1999  ppy sectorGeo[23].phiMax set to correct value in setup 
**:           dec 21, 1999  ppy fillTracks=0 deleted, leave default value (1)
**:           dec 21, 1999  ppy bug corrected phiShift = 0 zero for non-border sectors
**:           dec 21, 1999  ppy ptHelixFit set to zero in setParameters
**:           dec 21, 1999  ppy maxChi2Primary =10 in setParameters
**:           jan 27, 2000  ppy canItMerged beefed up
**:<------------------------------------------------------------------*/
#include "FtfSl3.h"
#include <iostream.h>

UINT32 swap32(unsigned int   in);
UINT32 swap16(unsigned short in);

#ifdef SL3ROOT
ClassImp(FtfSl3)
#endif

//******************************************************************
//   fill tracks
//******************************************************************
int FtfSl3::canItBeMerged ( FtfTrack* tTrack ) {
// if ( thisTrack->nHits < 45 - para.minHitsPerTrack ) return 1 ; // no type 1 for now
// float pt = thisTrack->pt ;
   double rTpcMin =  50. ;
   double rTpcMax = 210. ;
//
//   Get circle parameters
//
   double rc = tTrack->pt / ( 2.9979e-3 * para.bField );
   double xc = para.xVertex - tTrack->a2Xy / ( 2. * tTrack->a1Xy ) ;
   double yc = para.yVertex - 1.   /  ( 2. * tTrack->a1Xy ) ;
//   
//   Calculate intersection with tracking area (sector or supersector) boundaries
//
   double localPhi[2] ;
   localPhi[0] = para.phiMin ;
   localPhi[1] = para.phiMax ;

// printf ( " mergable track pt %f \n", tTrack->pt ) ;
   double sinPhi, cosPhi, tanPhi ;  
   double a, b, c, b2minus4ac ;
   double x1, y1, x2, y2, r1, r2 ;
   c = xc * xc + yc * yc - rc * rc ;
   for ( int i = 0 ; i < 2 ; i++ ) {
      sinPhi = sin(localPhi[i]); 
      cosPhi = cos(localPhi[i]); 
      tanPhi = tan(localPhi[i]); 
      a = 1. + tanPhi * tanPhi ;
      b = -2. * xc - 2. * yc * tanPhi ;

      b2minus4ac = b * b - 4. * a * c ;
      if ( b2minus4ac > 0 ) {
	 double rootB2Minus4ac = sqrt(b2minus4ac);

	 x1 = 0.5 * (-b + rootB2Minus4ac) / a ;
	 y1 = x1 * tanPhi ;
	 r1 = sqrt(x1*x1+y1*y1);

//	 printf ( " x1 y1 %e %e cos sin %e %e\n", x1, y1, cosPhi, sinPhi ) ; 
//	 double ratiox = x1/cosPhi ;
//	 double ratioy = y1/sinPhi ;
//	 printf ( " ratiox ratioy %e %e \n", ratiox, ratioy ) ;
	 if ( (x1/cosPhi)> 0 && (y1/sinPhi)> 0 && 
	      r1 > rTpcMin   &&  r1 < rTpcMax ) {
//	    printf ( "phi %f boundary crossed !!!!\n", localPhi[i] ) ;
	    return 1 ;
	 }

	 x2 = 0.5 * (-b - rootB2Minus4ac) / a ;
	 y2 = x2 * tanPhi ;
	 r2 = sqrt(x2*x2+y2*y2);
//	 ratiox = x2/cosPhi ;
//	 ratioy = y2/sinPhi ;
//	 printf ( " ratiox ratioy %e %e \n", ratiox, ratioy ) ;
//	 printf ( " x2 y2 %e %e cos sin %e %e\n", x2, y2, cosPhi, sinPhi ) ; 
	 if ( (x2/cosPhi) > 0 && (y2/sinPhi) > 0 &&  
	      r2 > rTpcMin   &&  r2 < rTpcMax ) {
//	    printf ( "phi %f boundary crossed !!!!\n", localPhi[i] ) ;
	    return 1 ;
	 }
      }

   }
   return 0 ;
}
//******************************************************************
//   fill tracks
//******************************************************************
int FtfSl3::fillTracks ( int maxBytes, char* buff, unsigned int token ) {
//
//   Loop over tracks
//
    int counter1 = 0 ;
    int counter2 = 0 ;
    int counter3 = 0 ;

    short* mergeFlag = new short[nTracks] ;
//
//   Count tracks of each type
//
    int i ;
    for ( i = 0 ; i < nTracks ; i++ ) {
       if ( track[i].flag == 1 ) {
	  if ( canItBeMerged ( &(track[i]) ) ) {
	     counter1++ ;
	     mergeFlag[i] = 1 ;
	  }
	  else { 
	     counter2++ ;
	     mergeFlag[i] = 0 ;
	  }
       } //  Secondaries now 
       else counter3++ ;
    }    
//
    // this is done to ease calcultion of offsets further down.
    unsigned int headSize1, headSize2, headSize3;
    if(counter1) {
	headSize1 = sizeof(struct bankHeader); }
    else { headSize1 = 0; }
    
    if(counter2) {
	headSize2 = sizeof(struct bankHeader); }
    else { headSize2 = 0; }

    if(counter3) {
	headSize3 = sizeof(struct bankHeader); }
    else { headSize3 = 0; }


    char *pointer1 = buff + sizeof(struct L3_SECTP) ;  // type I start

    char *pointer2 = pointer1 + headSize1 
	               + counter1 * sizeof(struct type1_track) ; //type II
    char *pointer3 = pointer2 + headSize2
	               + counter2 * sizeof(struct type2_track) ; //typeIII
    char *pointer4 = pointer3 + headSize3
	               + counter3 * sizeof(struct type3_track) ; // end

    int nBytes = (pointer4-buff) ; 
    if ( nBytes > maxBytes ) {
       fprintf ( stderr, "FtfSl3::fillTracks: %d bytes needed, %d max, too short a buffer \n ",  nBytes, maxBytes ) ;
    }
//
//  Fill header
//

    struct L3_SECTP *head = (struct L3_SECTP *)buff ;
    int sizeWord = 4; // 4 bytes per dword in raw formats
// bankHeader
    //  head->bh.bank_type = CHAR_L3_SECTP;
    memcpy(head->bh.bank_type,CHAR_L3_SECTP,8);
    head->bh.length     = sizeof(struct L3_SECTP) / 4 ;
    head->bh.bank_id    = sectorNr;
    head->bh.format_ver = DAQ_RAW_FORMAT_VERSION ;
    head->bh.byte_order = DAQ_RAW_FORMAT_ORDER ;
    head->bh.format_number = 0;
    head->bh.token      = token;
    head->bh.w9         = DAQ_RAW_FORMAT_WORD9;
    head->bh.crc        = 0; //don't know yet....

    head->nHits    = (unsigned int)nHits ;
    head->nTracks  = (unsigned int)nTracks ;
    head->cpuTime  = (unsigned int)rint(cpuTime * 1000000);   // to get microsec
    head->realTime = (unsigned int)rint(realTime * 1000000) ; // same here
    head->xVert  = (int)rint(para.xVertex * 1000000);   // to get cm*10**-6
    head->yVert  = (int)rint(para.yVertex * 1000000); 
    head->zVert  = (int)rint(para.zVertex * 1000000); 
    head->para = 1 ;
    head->banks[0].off = (pointer1-buff)/sizeWord; ;
    head->banks[0].len = (pointer2-pointer1)/sizeWord; ;
    head->banks[1].off = (pointer2-buff)/sizeWord; ;
    head->banks[1].len = (pointer3-pointer2)/sizeWord; ;
    head->banks[2].off = (pointer3-buff)/sizeWord; ;
    head->banks[2].len = (pointer4-pointer3)/sizeWord; ;
// done with L3_SECTP


//  L3_STKXD banks:

    struct L3_STK1D* STK1D = (struct L3_STK1D*)pointer1;
    struct L3_STK2D* STK2D = (struct L3_STK2D*)pointer2;
    struct L3_STK3D* STK3D = (struct L3_STK3D*)pointer3;

    struct type1_track* mPTrack = 
	(struct type1_track *)(pointer1 + headSize1) ;
    struct type2_track* uPTrack = 
	(struct type2_track *)(pointer2 + headSize2) ;
    struct type3_track* uSTrack = 
	(struct type3_track *)(pointer3 + headSize3) ;

    if(headSize1) {
	memcpy(STK1D->bh.bank_type, CHAR_L3_STK1D, 8);
	STK1D->bh.length     = (sizeof(struct bankHeader) + 
	                              counter1 * sizeof(type1_track))/4 ;
	STK1D->bh.bank_id    = sectorNr ;
	STK1D->bh.format_ver = DAQ_RAW_FORMAT_VERSION ;
	STK1D->bh.byte_order = DAQ_RAW_FORMAT_ORDER ;
	STK1D->bh.format_number = 0 ;
	STK1D->bh.token      = token ;
	STK1D->bh.w9         = DAQ_RAW_FORMAT_WORD9 ;
	STK1D->bh.crc = 0; // for now!!!
    }
    
    if(headSize2) {
	memcpy(STK2D->bh.bank_type, CHAR_L3_STK2D, 8);
	STK2D->bh.length     = (sizeof(struct bankHeader) + 
	                              counter2 * sizeof(type2_track))/4 ;
	STK2D->bh.bank_id    = sectorNr ;
	STK2D->bh.format_ver = DAQ_RAW_FORMAT_VERSION ;
	STK2D->bh.byte_order = DAQ_RAW_FORMAT_ORDER ;
	STK2D->bh.format_number = 0 ;
	STK2D->bh.token      = token ;
	STK2D->bh.w9         = DAQ_RAW_FORMAT_WORD9 ;
	STK2D->bh.crc = 0; // for now!!!
    }

    if(headSize3) {
	memcpy(STK3D->bh.bank_type, CHAR_L3_STK3D, 8);
	STK3D->bh.length     = (sizeof(struct bankHeader) + 
	                              counter3 * sizeof(type3_track))/4 ;
	STK3D->bh.bank_id    = sectorNr ;
	STK3D->bh.format_ver = DAQ_RAW_FORMAT_VERSION ;
	STK3D->bh.byte_order = DAQ_RAW_FORMAT_ORDER ;
	STK3D->bh.format_number = 0 ;
	STK3D->bh.token      = token ;
	STK3D->bh.w9         = DAQ_RAW_FORMAT_WORD9 ;
	STK3D->bh.crc = 0; // for now!!!
    }   
//
    for ( i = 0 ; i < nTracks ; i++ ) {
       if ( track[i].flag == 1 ) {
	  if ( mergeFlag[i] ) { 
	     mPTrack->id    = track[i].id ;
	     mPTrack->nHits = track[i].nHits ;
	     mPTrack->s11Xy = track[i].s11Xy ;
	     mPTrack->s12Xy = track[i].s12Xy ;
	     mPTrack->s22Xy = track[i].s22Xy ;
	     mPTrack->g1Xy  = track[i].g1Xy  ;
	     mPTrack->g2Xy  = track[i].g2Xy  ;
	     mPTrack->s11Sz = track[i].s11Sz ;
	     mPTrack->s12Sz = track[i].s12Sz ;
	     mPTrack->s22Sz = track[i].s22Sz ;
	     mPTrack->g1Sz  = track[i].g1Sz  ;
	     mPTrack->g2Sz  = track[i].g2Sz  ;
	     mPTrack->trackLength  = track[i].trackLength  ;
	     mPTrack->xLastHit = track[i].lastHit->x  ;
	     mPTrack->yLastHit = track[i].lastHit->y  ;
	     mPTrack->innerMostRow = track[i].innerMostRow ;
	     mPTrack->outerMostRow = track[i].outerMostRow ;
	     mPTrack++ ;
	  }
	  else {
	     uPTrack->id    = track[i].id ;
	     uPTrack->nrec = track[i].nHits ;
	     // chi2 * 10!!!! 
	     uPTrack->xy_chisq = (unsigned short)rint(10 * track[i].chi2[0]
						      /float(track[i].nHits));
	     uPTrack->sz_chisq = (unsigned short)rint(10 * track[i].chi2[1]
						      /float(track[i].nHits));
	     uPTrack->dedx  = track[i].dedx ; 
	     uPTrack->pt    = track[i].pt * float(track[i].q);   ; 
	     uPTrack->psi   = track[i].psi  ; 
	     uPTrack->tanl  = track[i].tanl  ; 
	     uPTrack->z0    = track[i].z0    ; 
	     uPTrack->Errors= 0 ; // to be filled
	     uPTrack++ ;
	  }
       }
       //
       //  Secondaries now 
       //
       else {
	  uSTrack->id    = track[i].id ;
	  uSTrack->nrec = track[i].nHits ;
	  uSTrack->xy_chisq = (unsigned short)rint(10 * track[i].chi2[0]
						   /float(track[i].nHits));
	  uSTrack->sz_chisq = (unsigned short)rint(10 * track[i].chi2[1]
						      /float(track[i].nHits));
	  uSTrack->dedx  = track[i].dedx  ; 
	  uSTrack->pt    = track[i].pt * float(track[i].q) ; 
	  uSTrack->psi   = track[i].psi   ; 
	  uSTrack->tanl  = track[i].tanl  ; 
	  uSTrack->r0    = track[i].r0    ; 
	  uSTrack->phi0  = track[i].phi0  ; 
	  uSTrack->z0    = track[i].z0    ; 
	  uSTrack->Errors= 0 ; // to be filled
	  uSTrack++ ;
       }
    }
//  delete array

    delete []mergeFlag ;
//
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
//    Set sector geometry parameters
//
  double toRad = acos(-1.)/180. ;
  sectorGeo[ 0].phiMin =  45. * toRad ;
  sectorGeo[ 1].phiMin =  15. * toRad ;
  sectorGeo[ 2].phiMin = 345. * toRad ;
  sectorGeo[ 3].phiMin = 315. * toRad ;
  sectorGeo[ 4].phiMin = 285. * toRad ;
  sectorGeo[ 5].phiMin = 255. * toRad ;
  sectorGeo[ 6].phiMin = 225. * toRad ;
  sectorGeo[ 7].phiMin = 195. * toRad ;
  sectorGeo[ 8].phiMin = 165. * toRad ;
  sectorGeo[ 9].phiMin = 135. * toRad ;
  sectorGeo[10].phiMin = 105. * toRad ;
  sectorGeo[11].phiMin =  75. * toRad ;
  sectorGeo[12].phiMin = 105. * toRad ;
  sectorGeo[13].phiMin = 135. * toRad ;
  sectorGeo[14].phiMin = 165. * toRad ;
  sectorGeo[15].phiMin = 195. * toRad ;
  sectorGeo[16].phiMin = 225. * toRad ;
  sectorGeo[17].phiMin = 255. * toRad ;
  sectorGeo[18].phiMin = 285. * toRad ;
  sectorGeo[19].phiMin = 315. * toRad ;
  sectorGeo[20].phiMin = 345. * toRad ;
  sectorGeo[21].phiMin =  15. * toRad ;
  sectorGeo[22].phiMin =  45. * toRad ;
  sectorGeo[23].phiMin =  75. * toRad ;
//
  sectorGeo[ 0].phiMax =  75. * toRad ;
  sectorGeo[ 1].phiMax =  45. * toRad ;
  sectorGeo[ 2].phiMax =  15. * toRad ;
  sectorGeo[ 3].phiMax = 345. * toRad ;
  sectorGeo[ 4].phiMax = 315. * toRad ;
  sectorGeo[ 5].phiMax = 285. * toRad ;
  sectorGeo[ 6].phiMax = 255. * toRad ;
  sectorGeo[ 7].phiMax = 225. * toRad ;
  sectorGeo[ 8].phiMax = 195. * toRad ;
  sectorGeo[ 9].phiMax = 165. * toRad ;
  sectorGeo[10].phiMax = 135. * toRad ;
  sectorGeo[11].phiMax = 105. * toRad ;
  sectorGeo[12].phiMax = 135. * toRad ;
  sectorGeo[13].phiMax = 165. * toRad ;
  sectorGeo[14].phiMax = 195. * toRad ;
  sectorGeo[15].phiMax = 225. * toRad ;
  sectorGeo[16].phiMax = 255. * toRad ;
  sectorGeo[17].phiMax = 285. * toRad ;
  sectorGeo[18].phiMax = 315. * toRad ;
  sectorGeo[19].phiMax = 345. * toRad ;
  sectorGeo[20].phiMax =  15. * toRad ;
  sectorGeo[21].phiMax =  45. * toRad ;
  sectorGeo[22].phiMax =  75. * toRad ;
  sectorGeo[23].phiMax = 105. * toRad ;
//
  double etaMin = 0.4 ;
  double etaMax = 2.3 ;
//
  for ( int sector = 0 ; sector < NSECTORS ; sector++ ) {
     sectorGeo[sector].phiShift = 0 ;
     if ( sector < 12 ) {
        sectorGeo[sector].etaMin = -1. * etaMin ;
        sectorGeo[sector].etaMax = etaMax ;
     }
     else {
        sectorGeo[sector].etaMin = -1. * etaMax ;
        sectorGeo[sector].etaMax = etaMin ;
     }
  }
  sectorGeo[ 2].phiShift = 16. * toRad ;
  sectorGeo[20].phiShift = 16. * toRad ; 
//
//    Reset tracker
//
  reset ( ) ;
//
//     Allocate memory 
//
  maxHits    = maxHitsIn ;
  maxTracks  = maxTracksIn ;
  hit        = new FtfHit[maxHits] ;
  track      = new FtfTrack[maxTracks] ;
//
  para.phiMin =  0.F * pi / 180.F ;
  para.phiMax =360.F * pi / 180.F  ;
  para.etaMin =-0.4F ;
  para.etaMax = 2.2F ;
#ifdef TRDEBUG
  para.trackDebug = 24 ;
  para.debugLevel =  1 ;
#endif
  return 0;
}
//******************************************************************
//    Read cluster data from TPCMZCLD bank (using daqFormats.h)
//
//    returns number of hits from this mezzanine
//
//
//     author: christof, derived from tonko's 'sl3'
//     ppy:    modified to look at byte order      
//******************************************************************
//#define TRDEBUG
int FtfSl3::readMezzanine (int sector, struct TPCMZCLD_local *mzcld) {

   unsigned int *tmp32;
   int row, rows;
   int len;
   int i, j;
   int counter;

   FtfHit *hitP = &hit[nHits];

   counter = 0;

   short swapByte = 0 ;
   if     ( !checkByteOrder(mzcld->bh.byte_order) ) swapByte = 1 ;

   tmp32 = mzcld->padrowFiller ;

   rows = mzcld->rows;
   if ( swapByte ) rows = swap32(rows);

   // fprintf(stderr,"       Found %d rows.\n",rows) ;

   for (i=0; i<rows; i++) {
      row = *tmp32++ ;
      len = *tmp32++ ;
      if ( swapByte ) {
	 row = swap32(row);
	 len = swap32(len);
      }
      //  fprintf(stderr,"        Row# %d: clusters %d\n",row,len) ;

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

	 if ( !swapByte ) {
	    fp = (double) xt->x / 64.0 ;
	    ft = (double) xt->t / 64.0 ;
	 }
	 else {
	    fp = (double) swap16(xt->x) / 64.0 ;
	    ft = (double) swap16(xt->t) / 64.0 ;
	 }
	 //  printf("%02d %9.5f %9.5f %6d %3d\n", row,
	 //         fp, ft, c->c , c->f) ;
	 rawToGlobal(sector, row, fp, ft, &x, &y, &z);

//	  printf(" %d  %d  %f  %f  %f  %f  %f\n",
//	   	  sector, row, x, y, z, fp, ft);

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
//
//     author: christof, derived from tonko's 'sl3'
//     ppy:    modified to look at byte order      
//******************************************************************
int FtfSl3::readSector ( struct TPCSECLP *seclp ) {

   struct TPCRBCLP *rbclp ;
   struct TPCMZCLD_local *mzcld ;
   int iRb, kMz;
   int sector; 
   int nHitsOfMz;

   // reset sector-hit counter
   nHits = 0;
   //

   // check byte order of SECLP bank
   // byte swapping is needed
   short swapByte = 0 ;
   if     ( !checkByteOrder(seclp->bh.byte_order) ) swapByte = 1 ;

   nHitsOfMz = 0;

   sector = (unsigned int)seclp->bh.bank_id ;
   if ( swapByte ) sector = swap32(sector) ;

   sectorNr = sector;
   para.phiMin = sectorGeo[sector-1].phiMin ;
   para.phiMax = sectorGeo[sector-1].phiMax ;
   para.etaMin = sectorGeo[sector-1].etaMin ;
   para.etaMax = sectorGeo[sector-1].etaMax ;
   //
   //   Check Sector 
   //
   if ( sectorNr < 0 && sectorNr > 24 ) {
      fprintf(stderr, "Error - FtfSl3::readSector: Wrong sector %d!\n",sectorNr);
      return -1 ;
   }

   // run over receiver boards
   for (iRb=0; iRb<SB_RB_NUM; iRb++) {

      if (iRb==6) { 
	 sector++ ;	// the other Sector
	 if ( sector == 4 ) { 
	    para.phiMin = 0. ;
	    para.phiMax = 1.1 ; // 60 degress a bit more than a rad
	    para.phiShift = 0.8 ; // ~45 degrees
	 }
	 else if ( sector == 22 ) {
	    para.phiMin = 0. ;
	    para.phiMax = 1.1 ; // 60 degress a bit more than a rad
	    para.phiShift = 0.27 ; // ~15 degrees
	 }
	 else { 
	    if ( sectorGeo[sector-1].phiMin < para.phiMin ) para.phiMin = sectorGeo[sector-1].phiMin ;
	    if ( sectorGeo[sector-1].phiMax > para.phiMax ) para.phiMax = sectorGeo[sector-1].phiMax ;
	 }
	 if ( sectorGeo[sector-1].etaMin < para.etaMin ) para.etaMin = sectorGeo[sector-1].etaMin ;
	 if ( sectorGeo[sector-1].etaMax > para.etaMax ) para.etaMax = sectorGeo[sector-1].etaMax ;
         para.phiShift = 0 ;
      }


      if ( !(unsigned int)seclp->rb[iRb].off) continue ;

      int off = (unsigned int)seclp->rb[iRb].off;
      if ( swapByte ) off = swap32(off);
      rbclp = (struct TPCRBCLP *)((char *)seclp + off * 4) ;
      int swapByteMezzaninne = 0 ;
      if     ( !checkByteOrder(rbclp->bh.byte_order) ) swapByteMezzaninne = 1 ;

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

	 off = rbclp->mz[kMz].off ;
	 if ( swapByteMezzaninne ) off = swap32(off);
	 mzcld = (struct TPCMZCLD_local *) ((char *)rbclp + off*4) ;

	 if (mzcld) {
	    nHitsOfMz = readMezzanine (sector, mzcld);
	    if (nHitsOfMz<0) {
	       return -1;
	    }
	    nHits += nHitsOfMz;
	 }

      }
   }
   //
   //   Since phiMin/Max etaMin/etaMax may have changed reset tracker
   //
   reset();
   //
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

   para.hitChi2Cut        = 400.F  ;
   para.goodHitChi2       =  50.F  ;
   para.trackChi2Cut      = 200.F  ;
   para.maxChi2Primary    = 50.F   ;
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
   para.goodDistance     =  5.F   ;
   para.mergePrimaries   =  1     ;
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

   para.ptMinHelixFit     = 0.F  ;
   para.rVertex          = 0.F    ;
   para.xVertex          = 0.F    ;
   para.yVertex          = 0.F    ;
   para.zVertex          = 0.F    ;
   para.dxVertex         = 0.005F ;
   para.dyVertex         = 0.005F ;
   para.phiVertex        = 0.F    ;
   return 0;
}





