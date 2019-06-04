/*:>-------------------------------------------------------------------
**: File:     FtfSl3.cxx 
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
**:           feb 10, 2000  ppy add xyError and zError
**:           feb 10, 2000  ppy add xyError and zError
**:           feb 14, 2000  ppy track length filled for tracks type 2 and 3
**:           feb 17, 2000  ppy check ratiox and ratioy are no division by zero
**:           feb 24, 2000  ppy hit id in readMezzaninne should be nHits+counter
**:           mar 09, 2000  ppy trackLength changed to length
**:           mar 09, 2000  ppy some changes to accomodate void pointers
**:           mar 11, 2000  ppy fill hits
**:           mar 29, 2000  ppy add errors to tracks in daq format
**:           apr 06, 2000  cle change L3_CLUSTER to l3_cluster
**:           apr 08, 2000  ppy add variables to cut cluster for high/low charge/time bin
**:           apr 18, 2000  ppy include readout board and mezzannine in readMezzanine    
**:                             arguments
**:           apr 19, 2000  cs  add dEdx from Christof
**:           apr 19, 2000  cs  discard one-apd cluster (flag == 1) in readMezzanine(...)
**:           jun 28, 2000  ppy replace sl3CoordinateTransform with class
**:                             l3CoordinateTransformer
**:           jul 12, 2000  ppy get rid of mergeFlag in fillTracks (not used)
**:           aug 21, 2000  ppy replace printf with l3Log
**:           aug 10, 2001  ppy get rid of bFieldPolarity, this should be handle somewhere else
**:<------------------------------------------------------------------*/


#ifndef TRG_VERSION
#define TRG_VERSION 0x32
#endif

#include <stdlib.h>
#include "FtfSl3.h"
#include "rts.h"
#include "l3Swap.h"
#include "fcfClass.hh"
#ifdef OLD_DAQ_READER
#include <evpReader.hh>
#else /* OLD_DAQ_READER */
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>
#endif /* OLD_DAQ_READER */
#include <rtsSystems.h>
#include <daqFormats.h>
#ifndef OLD_DAQ_READER
#include <DAQ_TPC/daq_tpc.h>
#include "gl3Event.h"

extern tpc_t *pTPC;
#endif /* OLD_DAQ_READER */

//******************************************************************
//   Check whether tracs are mergable
//******************************************************************
int FtfSl3::canItBeMerged ( FtfTrack* tTrack ) {
   double rTpcMin =  50. ;
   double rTpcMax = 210. ;

   if ( nHits < 15 ) return 0 ;
//
//   Get circle parameters
//
// double rc = tTrack->pt / ( 2.9979e-3 * para.bField );
// double xc = para.xVertex - tTrack->a2Xy / ( 2. * tTrack->a1Xy ) ;
// double yc = para.yVertex - 1.   /  ( 2. * tTrack->a1Xy ) ;
   double x0 = tTrack->r0 * cos (tTrack->phi0);
   double y0 = tTrack->r0 * sin (tTrack->phi0);
   double rc = tTrack->pt / (bFactor * para.bField);
   double trackPhi0 = tTrack->psi + tTrack->q * 0.5 * M_PI / fabs ((double) tTrack->q);
   double xc = x0 - rc * cos (trackPhi0);
   double yc = y0 - rc * sin (trackPhi0);
//   
//   Calculate intersection with tracking area (sector or supersector) boundaries
//
   double localPhi[2] ;
   localPhi[0] = para.phiMin ;
   localPhi[1] = para.phiMax ;

// l3Log ( " mergable track pt %f \n", tTrack->pt ) ;
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

//	 l3Log ( " x1 y1 %e %e cos sin %e %e\n", x1, y1, cosPhi, sinPhi ) ; 
  	 double ratiox = 0 ;
         if ( cosPhi != 0 ) ratiox = x1/cosPhi ;
  	 double ratioy = 0 ;
         if ( sinPhi != 0 ) ratiox = y1/sinPhi ;
//	 l3Log ( " ratiox ratioy %e %e \n", ratiox, ratioy ) ;
	 if ( ratiox >= 0 && ratioy >= 0 && 
	      r1 > rTpcMin   &&  r1 < rTpcMax ) {
//	    l3Log ( "phi %f boundary crossed !!!!\n", localPhi[i] ) ;
	    return 1 ;
	 }

	 x2 = 0.5 * (-b - rootB2Minus4ac) / a ;
	 y2 = x2 * tanPhi ;
	 r2 = sqrt(x2*x2+y2*y2);
         ratiox = 0 ;
         if ( cosPhi != 0 ) ratiox = x2/cosPhi ;
  	 ratioy = 0 ;
         if ( sinPhi != 0 ) ratioy = y2/sinPhi ;
//	 l3Log ( " ratiox ratioy %e %e \n", ratiox, ratioy ) ;
//	 l3Log ( " x2 y2 %e %e cos sin %e %e\n", x2, y2, cosPhi, sinPhi ) ; 
	 if ( ratiox >= 0 && ratioy >= 0 &&  
	      r2 > rTpcMin   &&  r2 < rTpcMax ) {
//	    l3Log ( "phi %f boundary crossed !!!!\n", localPhi[i] ) ;
	    return 1 ;
	 }
      }

   }
   //
   //  Check whether particle cross membrane
   //
   double zMembrane = 0 ;
   double sToMembrane = (zMembrane - tTrack->z0 ) / tTrack->tanl ;   
   double dangle      =  sToMembrane / rc ;    
   double angle       = dangle + trackPhi0  ; 
   double xMembrane   = xc + rc * cos(angle) ;
   double yMembrane   = yc + rc * sin(angle) ;
   double rMembrane   = sqrt(xMembrane*xMembrane+yMembrane*yMembrane);
   if ( rMembrane > rTpcMin   &&  rMembrane < rTpcMax ) {
//    l3Log ( "Membrane crossed at r %f !!!!\n", rMembrane ) ;
      return 1 ;
   }
//
   return 0 ;
}
//******************************************************************
//   fill Hits
//******************************************************************
int FtfSl3::fillHits ( unsigned int maxBytes, char* buff, unsigned int token ) {
//
//   Check enough space
//
   if ( (nHits-1) * sizeof(l3_cluster) + sizeof(L3_SECCD) > maxBytes ) {
      LOG (ERR, "FtfSl3::fillHits: buffer with %d bytes too small \n", maxBytes ) ;
      return 0 ;
   }
//
//   Loop over tracks
//
   L3_SECCD* head = (L3_SECCD *)buff ;

   head->nrClusters_in_sector = nHits ;
   // bankHeader
   memcpy(head->bh.bank_type,CHAR_L3_SECCD,8);
   head->bh.bank_id    = sectorNr;
   head->bh.format_ver = DAQ_RAW_FORMAT_VERSION ;
   head->bh.byte_order = DAQ_RAW_FORMAT_ORDER ;
   head->bh.format_number = 0;
   head->bh.token      = token;
   head->bh.w9         = DAQ_RAW_FORMAT_WORD9;
   head->bh.crc        = 0; //don't know yet....

    
   l3_cluster* hitP    = (l3_cluster *)(head->cluster);
   FtfHit *nextHit ;
   int i ;
   int counter = 0 ;


   for ( i = 0 ; i < nTracks ; i++ ) {
      for  ( nextHit = (FtfHit *)(track[i].firstHit) ;
	    nextHit != 0 ;
	    nextHit = (FtfHit *)nextHit->nextTrackHit) {
         hitP->padrow  = nextHit->row ;
         hitP->pad     = nextHit->buffer1 ;
         hitP->time    = nextHit->buffer2 ;
         hitP->flags   = nextHit->flags   ;
         hitP->charge  = (short)nextHit->q  ;
         hitP->RB_MZ   = (short)nextHit->hardwareId  ;
         hitP->trackId = track[i].id ;
         if ( !(nextHit->track) ) {
            LOG (ERR, "FtfSl3:fillHits: we got a problem track %d row hit %d \n",
	              track[i].id, hitP->padrow ) ;
         }
	 hitP++ ;
         counter++ ;
      }
   }
//
//   Loop over hits
//
   for ( i = 0 ; i < nHits ; i++ ) {
      nextHit = &(hit[i]) ;
      if ( nextHit->track ) continue ; 
      hitP->padrow  = nextHit->row ;
      hitP->pad     = nextHit->buffer1 ;
      hitP->time    = nextHit->buffer2 ;
      hitP->flags   = nextHit->flags   ;
      hitP->charge  = (short)nextHit->q       ;
      hitP->RB_MZ   = (short)nextHit->hardwareId  ;
      hitP->trackId = 0 ;
      hitP++ ;
      counter++ ;
   }

   if ( counter != nHits ) {
      LOG (ERR, "FtfSl3:fillHits: Warning! Actual number of hits written %d total nHits %d \n", 
                                 counter, nHits );
   }

   head->bh.length = ((char *)hitP-buff) / 4 ;

   return ((char *)hitP-buff) ;
}
//******************************************************************
//   fill tracks
//******************************************************************
int FtfSl3::fillTracks ( int maxBytes, char* buff, unsigned int token ) {


    if(buff == NULL) {
	LOG(ERR,"ERROR: trying to fill NULL buffer");
	return 0;
    }

    LOG(DBG, "fill nTracks = %d", nTracks);
    int counter = nTracks ;

    unsigned int headSize;

    if(counter) 
      headSize = sizeof(struct bankHeader); 
    else 
      headSize = 0; 


    char *pointer = buff + sizeof(struct L3_SECTP) ;  

    char *endOfData = pointer + headSize
	               + counter * sizeof(struct local_track) ; // end

    int nBytes = (endOfData-buff) ; 
    if ( nBytes > maxBytes ) {
      LOG(ERR, "FtfSl3::fillTracks: %d bytes needed, %d max, too short a buffer \n ",  
	       nBytes, maxBytes ) ;
       return -1 ;
    }
//
//  Fill header
//

    struct L3_SECTP *head = (struct L3_SECTP *)buff ;

    // bankHeader
    //  head->bh.bank_type = CHAR_L3_SECTP;
    memcpy(head->bh.bank_type,CHAR_L3_SECTP,8);
    head->bh.length     = sizeof(struct L3_SECTP) / 4 ;
    head->bh.bank_id    = sectorNr;
    LOG(DBG, "set bank_id to %d", sectorNr);
    head->bh.format_ver = DAQ_RAW_FORMAT_VERSION ;
    head->bh.byte_order = DAQ_RAW_FORMAT_ORDER ;
    head->bh.format_number = 1; // 1 means only pointing to local_tracks
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
    head->banks[0].off = (pointer-buff) / 4;
    head->banks[0].len = (endOfData-pointer)/ 4 ;
    head->banks[1].off = 0 ;
    head->banks[1].len = 0 ;
    head->banks[2].off = 0 ;
    head->banks[2].len = 0 ;

// done with L3_SECTP


//  L3_STKXD banks:

    struct L3_LTD* LTD = (struct L3_LTD*)pointer;

    struct local_track* uSTrack = 
	(struct local_track *)(pointer + headSize) ;


    if(headSize) {
	memcpy(LTD->bh.bank_type, CHAR_L3_LTD, 8);
	LTD->bh.length     = (sizeof(struct bankHeader) + 
	                              counter * sizeof(local_track))/4 ;
	LTD->bh.bank_id    = sectorNr ;
	LTD->bh.format_ver = DAQ_RAW_FORMAT_VERSION ;
	LTD->bh.byte_order = DAQ_RAW_FORMAT_ORDER ;
	LTD->bh.format_number = 0 ;
	LTD->bh.token      = token ;
	LTD->bh.w9         = DAQ_RAW_FORMAT_WORD9 ;
	LTD->bh.crc = 0; // for now!!!
    }   
//
    for ( int i = 0 ; i < nTracks ; i++ ) {
       //
       uSTrack->id    = track[i].id ;
       if ( canItBeMerged ( &(track[i]) ) ) {
          uSTrack->id *= -1 ;
       }
       uSTrack->nHits = track[i].nHits ;
       uSTrack->ndedx = track[i].nDedx;
       uSTrack->innerMostRow = track[i].innerMostRow    ; 
       uSTrack->outerMostRow = track[i].outerMostRow    ; 
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
       uSTrack->trackLength  = track[i].length  ;
       uSTrack->dpt          = (short)(32768. * track[i].dpt / track[i].pt ) ; 
       uSTrack->dpsi         = track[i].CompressOver1(track[i].dpsi,track[i].psi);
       uSTrack->dtanl        = track[i].CompressOver1(64.*track[i].dtanl,track[i].tanl);
       uSTrack->dz0          = (short)(1024. * track[i].dz0 ) ;

//        if (uSTrack->trackLength < 0) {
// 	   printf("ERROR: writing tracks(sl3): length = %f < 0\n",
// 		  uSTrack->trackLength);
//        }
       
       uSTrack++ ;
    }
//  delete array

//
    return nBytes ;
//
}
//******************************************************************
//   get nr of tracks
//******************************************************************
int FtfSl3::getNrTracks (void) {
   return nTracks;
}           
//******************************************************************
//   Initialize sl3 tracker
//******************************************************************
int FtfSl3::setup ( int maxHitsIn, int maxTracksIn ) {
//
//    Set parameters
//
 // l3Log("setting up tracker\n");
  para.setDefaults ( ) ;
//
//    Set Parameters
//
  setParameters ( ) ;
//
//    Set sector geometry parameters
//
  double toRad = acos(-1.)/180. ;
  sectorGeometry* sectorG = (sectorGeometry *)sectorGeo ;
  sectorG[ 0].phiMin =  45. * toRad ;
  sectorG[ 1].phiMin =  15. * toRad ;
  sectorG[ 2].phiMin = 345. * toRad ;
  sectorG[ 3].phiMin = 315. * toRad ;
  sectorG[ 4].phiMin = 285. * toRad ;
  sectorG[ 5].phiMin = 255. * toRad ;
  sectorG[ 6].phiMin = 225. * toRad ;
  sectorG[ 7].phiMin = 195. * toRad ;
  sectorG[ 8].phiMin = 165. * toRad ;
  sectorG[ 9].phiMin = 135. * toRad ;
  sectorG[10].phiMin = 105. * toRad ;
  sectorG[11].phiMin =  75. * toRad ;
  sectorG[12].phiMin = 105. * toRad ;
  sectorG[13].phiMin = 135. * toRad ;
  sectorG[14].phiMin = 165. * toRad ;
  sectorG[15].phiMin = 195. * toRad ;
  sectorG[16].phiMin = 225. * toRad ;
  sectorG[17].phiMin = 255. * toRad ;
  sectorG[18].phiMin = 285. * toRad ;
  sectorG[19].phiMin = 315. * toRad ;
  sectorG[20].phiMin = 345. * toRad ;
  sectorG[21].phiMin =  15. * toRad ;
  sectorG[22].phiMin =  45. * toRad ;
  sectorG[23].phiMin =  75. * toRad ;
//
  sectorG[ 0].phiMax =  75. * toRad ;
  sectorG[ 1].phiMax =  45. * toRad ;
  sectorG[ 2].phiMax =  15. * toRad ;
  sectorG[ 3].phiMax = 345. * toRad ;
  sectorG[ 4].phiMax = 315. * toRad ;
  sectorG[ 5].phiMax = 285. * toRad ;
  sectorG[ 6].phiMax = 255. * toRad ;
  sectorG[ 7].phiMax = 225. * toRad ;
  sectorG[ 8].phiMax = 195. * toRad ;
  sectorG[ 9].phiMax = 165. * toRad ;
  sectorG[10].phiMax = 135. * toRad ;
  sectorG[11].phiMax = 105. * toRad ;
  sectorG[12].phiMax = 135. * toRad ;
  sectorG[13].phiMax = 165. * toRad ;
  sectorG[14].phiMax = 195. * toRad ;
  sectorG[15].phiMax = 225. * toRad ;
  sectorG[16].phiMax = 255. * toRad ;
  sectorG[17].phiMax = 285. * toRad ;
  sectorG[18].phiMax = 315. * toRad ;
  sectorG[19].phiMax = 345. * toRad ;
  sectorG[20].phiMax =  15. * toRad ;
  sectorG[21].phiMax =  45. * toRad ;
  sectorG[22].phiMax =  75. * toRad ;
  sectorG[23].phiMax = 105. * toRad ;
//
  double etaMin = 0.4 ;
  double etaMax = 2.3 ;
//
  for ( int sector = 0 ; sector < 24 ; sector++ ) {
     sectorG[sector].phiShift = 0 ;
     if ( sector < 12 ) {
        sectorG[sector].etaMin = -1. * etaMin ;
        sectorG[sector].etaMax = etaMax ;
     }
     else {
        sectorG[sector].etaMin = -1. * etaMax ;
        sectorG[sector].etaMax = etaMin ;
     }
  }
  sectorG[ 2].phiShift = 16. * toRad ;
  sectorG[20].phiShift = 16. * toRad ; 
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
int FtfSl3::readMezzanine (int sector,        int readOutBoard,
                           int mezzanninneNr, struct TPCMZCLD_local *mzcld) {

  unsigned int *tmp32;
  int row, rows;
  int len;
  int i, j;
  int counter;

  if(mzcld->bh.length <= 10) {
    return 0;
  }
   
 
  FtfHit *hitP = &hit[nHits];

  // Prepare transformation
  l3xyzCoordinate XYZ(0,0,0) ;
  l3ptrsCoordinate PTRS(0,0,0,0) ;


  counter = 0;
  short swapByte = 0 ;
  if     ( !checkByteOrder(mzcld->bh.byte_order) ) swapByte = 1 ;

  tmp32 = mzcld->padrowFiller ;

  rows = mzcld->rows;
  if ( swapByte ) rows = swap32(rows);

  // LOG(ERR, "FtfSl3::readMezzaninne:  Found %d rows.\n",rows) ;

  for (i=0; i<rows; i++) {
    row = *tmp32++ ;
    len = *tmp32++ ;
    if ( swapByte ) {
      row = swap32(row);
      len = swap32(len);
    }
    //l3Log("        Row# %d: clusters %d\n",row,len) ;

    if (len == 0) {
      // No data in this row, go to next row
      continue;
    }


    if((row<0) || (row>45)){
      LOG(ERR, "ERROR: row (%i) is wrong in TPCMZCLD, len=%d\n",
	  row,len);
      continue;
    }

    for ( j=0; j<len; j++) {
      double fp, ft ;
      unsigned short pad ;
      unsigned short time ;
      unsigned short charge;
      unsigned short fla;

      unsigned short flags = 0;

      //l3Log("FtfSl3: Read cluster %i\n", j);

      struct xt {
	unsigned short x ;
	unsigned short t ;
      } *xt ;
      struct c {
	unsigned short flags ;
	unsigned short charge ;
      } *c ;

      xt = (struct xt *) tmp32++ ;
      c = (struct c *) tmp32++ ;
      //
      //   Discard 1 pad clusters
      // cs: discard one-pad cluster marked by the cluster finder as such (set first bit)
      //l3Log ( "flags %d \n", c->flags ) ;
      //if ( (c->flags & 1) == 1) continue;
	 
      if ( !swapByte ) {
	pad  = xt->x  ;
	time = xt->t ;

	fla  = c->flags ; 
	charge = c->charge ;
      }
      else {
	pad  = swap16(xt->x)  ;
	time = swap16(xt->t)  ;

	fla  = swap16(c->flags) ; 
	charge = swap16(c->charge) ;
      }


      // TODO: extract some flags from pad and time
      if(pad  & 0x8000) flags |= FCF_DOUBLE_PAD ;
      if(pad  & 0x4000) flags |= FCF_DEAD_EDGE ;
      if(time & 0x8000) flags |= FCF_ONEPAD ;
      if(fla & 0x8000)  flags |= FCF_ROW_EDGE ;
      if(fla & 0x4000)  flags |= FCF_BROKEN_EDGE ;
	 
      pad  &= 0x3FFF;
      time &= 0x7FFF;

      fp = (double)pad / 64. ;
      ft = (double)time/ 64. ;
	 

      LOG(DBG, "**************************************MZ*********************************");
      // Cut on timebins and clustercharge added by cle 02/21/00
      if ( ft < minTimeBin ) continue ;
      if ( ft > maxTimeBin ) continue ;
      if ( c->charge < minClusterCharge ) continue ;
      if ( c->charge > maxClusterCharge ) continue ;
      //#define MDEBUG
#ifdef MDEBUG
      LOG(ERR, "FtfSl3:readMezzaninne row %d \n", row ) ;
      LOG (ERR, "ft %f min max %d %d \n", ft, minTimeBin, maxTimeBin ) ;
      LOG (ERR "charge %d min max %d %d \n", c->charge, minClusterCharge, maxClusterCharge ) ;
#endif
      //
      //l3Log("sector:%i RB:%i pad: %f time: %f row: %i\n",
      //sector, readOutBoard,fp,ft,row);
      //if((sector == 1)) {
      //	printf("cl old: %4.2f %4.2f %f %f\n",fp, ft, (float)row, (float)sector);
      //}



      PTRS.Setptrs((double)fp, (double)ft,(double)row, (double)sector) ;
      getCoordinateTransformer()->raw_to_global(PTRS,XYZ) ;

      //printf("hitP = 0x%x\n",hitP);

      hitP->id  = nHits+counter ;
      hitP->row = row ;
      hitP->sector = sector ;

      hitP->x   = (float) XYZ.Getx();
      hitP->y   = (float) XYZ.Gety();
      hitP->z   = (float) XYZ.Getz();
      //	 l3Log ( "x %f y %f z %f \n", hitP->x, hitP->y, hitP->z ) ;

      hitP->dx  = xyError ;
      hitP->dy  = xyError ;
      hitP->dz  = zError ;
      hitP->buffer1 = pad ;
      hitP->buffer2 = time ;
      if(embedded)
	hitP->flags   = (flags | (1<<7) );
      else
	hitP->flags   = flags ;
      hitP->q       = charge ;
      hitP->hardwareId = readOutBoard * 16 + mezzanninneNr ;

      //l3Log("x: %f y: %f z: %f", hitP->x, hitP->y, hitP->z);
	 

      hitP++;

      counter++;

      // DEBUGGING: log cluster info
      //l3Log("pad: %f  time: %f  charge: %i flags: %x aaa\n",
      //      fp, ft, charge, flags);

      if ( (nHits+counter)>maxHits ) {
	LOG(ERR, "Error - FtfSl3:read: Hit array too small: %d + %d > %d \n",
	    nHits, counter, maxHits ) ;
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
int FtfSl3::readSector ( struct bankHeader *bank ) {

  struct TPCSECLP *seclp;
  struct TPCRBCLP *rbclp ;
  struct TPCMZCLD_local *mzcld ;
  int iRb, kMz;
  int sector; 
  int nHitsOfMz;
  embedded = 0;

  // until 2002, the DETs sent us TPCSECLPs, but from 2003, we
  // get a TPCSECP bank header that has the offset to the TPCSECLP
  // in w9
  // So, we have to check, if the bank header is a SECP or a SECLP,
  // and extract the SECLP that we need for further processing
  //l3Log("readSector: start %x\n", bank);

  if ( 0 == strncmp(bank->bank_type, CHAR_TPCSECLP, 8 ) ){
    // Old case, we get only the TPCSECLP
    //l3Log("readSector: old\n");

    seclp = (TPCSECLP *)bank;

  } else if ( 0 == strncmp(bank->bank_type, CHAR_TPCSECP, 8 ) ){
    // New case: we have to look into w9 to get the TPCSECLP
    //l3Log("readSector: new, w9=0x%x\n", bank->w9);
	
    if(bank->byte_order == DAQ_RAW_FORMAT_ORDER)
      seclp = (TPCSECLP *)((char*)bank + 4*bank->w9);
    else
      seclp = (TPCSECLP *)((char*)bank + 4*swap32(bank->w9));

  } else {
    // We don't know that type

    char type[9];
    strncpy(type, bank->bank_type, 8);
    type[8] = '\0';
    LOG(ERR,"readSector: Unknown bank type: '%s'\n", type);

    return -1;
  }
  
    
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
  LOG(DBG, "Set sectorNr to %d", sectorNr);
  if ( sectorNr < 1 || sectorNr > 24 ) {
    LOG(ERR, "Error - FtfSl3::readSector: Wrong sector %d!\n",sectorNr);
    return -1 ;
  }
    
  sectorGeometry* sectorG = (sectorGeometry *)sectorGeo ;
  para.phiMin = sectorG[sector-1].phiMin ;
  para.phiMax = sectorG[sector-1].phiMax ;
  para.etaMin = sectorG[sector-1].etaMin ;
  para.etaMax = sectorG[sector-1].etaMax ;
    
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
	if ( sectorG[sector-1].phiMin < para.phiMin ) 
	  para.phiMin = sectorG[sector-1].phiMin ;
	if ( sectorG[sector-1].phiMax > para.phiMax ) 
	  para.phiMax = sectorG[sector-1].phiMax ;
	para.phiShift = 0 ;
      }
      if ( sectorG[sector-1].etaMin < para.etaMin ) 
	para.etaMin = sectorG[sector-1].etaMin ;
      if ( sectorG[sector-1].etaMax > para.etaMax ) 
	para.etaMax = sectorG[sector-1].etaMax ;
    }
    //l3Log ( "sector %d rb %d phi min/max %f %f \n", 
    //        sector, iRb, para.phiMin, para.phiMax ) ;

    // Get pointer to RPCLP
    int off = (unsigned int)seclp->rb[iRb].off;
    if ( swapByte ) off = swap32(off);
    if ( !off ) continue ;
    rbclp = (struct TPCRBCLP *)((char *)seclp + off * 4) ;

    int swapByteMezzaninne = 0 ;
    if ( !checkByteOrder(rbclp->bh.byte_order) ) swapByteMezzaninne = 1 ;
	
    // run over the 3 mezzanines
    for (kMz=0; kMz<RB_MZ_NUM; kMz++) {

      if (!rbclp->mz[kMz].off) continue;
      off = rbclp->mz[kMz].off ;
      if ( swapByteMezzaninne ) off = swap32(off);
      mzcld = (struct TPCMZCLD_local *) ((char *)rbclp + off*4) ;
	    
      if(strncmp((char*)mzcld, "TPCMZCLD",8)!=0){
	if (swapByteMezzaninne) {
	  LOG(ERR, "ERROR: TPCMZCLD header not found off=%d len=%d\n",
	      swap32(rbclp->mz[kMz].off), swap32(rbclp->mz[kMz].len));
	} else {
	  LOG(ERR, "ERROR: TPCMZCLD header not found off=%d len=%d\n",
	      rbclp->mz[kMz].off, rbclp->mz[kMz].len);
	}
	continue; 
      }
      //  printf("sector=%d %d %d 0x%x\n",sector,iRb,kMz,mzcld);
      nHitsOfMz = readMezzanine (sector, iRb, kMz, mzcld);
      if (nHitsOfMz<0) {
	if (swapByteMezzaninne) {
	  LOG(ERR, "ERROR: wrong reading mezzanine off=%d len=%d\n",
	      swap32(rbclp->mz[kMz].off), swap32(rbclp->mz[kMz].len));
	} else {
	  LOG(ERR,"ERROR: wrong reading mezzanine off=%d len=%d\n",
	      rbclp->mz[kMz].off, rbclp->mz[kMz].len);
	}
	return -1;
      }
      nHits += nHitsOfMz;
	    
    }
  }
  //
  //   Since phiMin/Max etaMin/etaMax may have changed reset tracker
  //
  reset();
  //
  return 0;
} 


// Need to set the angles by sector... lost of special cases.  Don't know why!
// perhaps its because have a region -x to +x, move to 0 ... 2x with "shift" of x
//
// sector is the hypersector, counting from 1
//
int FtfSl3::setTrackingAngles(int sector)
{
  if(sector < 1 || sector > 24) {
    LOG(ERR, "Bad sector = %d",sector,0,0,0,0);
    return -1;
  }

  // This is basically it....
  sectorGeometry* sectorG = (sectorGeometry *)sectorGeo ;
  para.phiMin = sectorG[sector-1].phiMin ;
  para.phiMax = sectorG[sector-1].phiMax ;
  para.etaMin = sectorG[sector-1].etaMin ;
  para.etaMax = sectorG[sector-1].etaMax ;
    
  // Now do the next sector...
  sector++ ;   
  if ( sector == 4 ) {          // What's different?
    para.phiMin = 0.;
    para.phiMax = 1.1; 
    para.phiShift = 0.8; 
  }
  else if ( sector == 22 ) {    // What's different?
    para.phiMin = 0.;
    para.phiMax = 1.1; 
    para.phiShift = 0.27;
  }
  else { 
    if ( sectorG[sector-1].phiMin < para.phiMin ) 
      para.phiMin = sectorG[sector-1].phiMin ;
    if ( sectorG[sector-1].phiMax > para.phiMax ) 
      para.phiMax = sectorG[sector-1].phiMax ;
    para.phiShift = 0 ;
  }

  if ( sectorG[sector-1].etaMin < para.etaMin ) 
    para.etaMin = sectorG[sector-1].etaMin ;
  if ( sectorG[sector-1].etaMax > para.etaMax ) 
    para.etaMax = sectorG[sector-1].etaMax ;  

  reset();    // this is needed I think!
  return 0;
}

///////////////////////////////////
// Read sector from evpReader
//
// Assumes that tpcReader(mem,sector) has already been called
// so that tpc structure contains data for sector already
//


void FtfSl3::setClustersFromGl3Event(gl3Event *event, int sector) {
    int filtered_out = 0;

    sectorNr = sector;
    LOG(DBG, "set sectorNr to %d", sector);
    
    for(int i=event->sectorFirstHit[sector]; i < event->nHits; i++) {
	gl3Hit *gl3 = &event->hit[i];
	if((gl3->rowSector / 100) != sector) return;
	
	FtfHit *hitP = &hit[nHits];

	hitP->id = nHits;
	hitP->row = gl3->rowSector % 100;
	hitP->sector = sector;
	hitP->x = gl3->getX();
	hitP->y = gl3->getY();
	hitP->z = gl3->getZ();
	hitP->dx = xyError;
	hitP->dy = xyError;
	hitP->dz = zError;
	hitP->buffer1 = 0; //(int)(pad * 64);
	hitP->buffer2 = 0; //(int)(tb * 64);
		    
	hitP->flags = gl3->flags;
		    
	hitP->q = gl3->charge;
	hitP->hardwareId = 0;
		    

	//if(c->t < minTimeBin) continue;
	//if(c->t > maxTimeBin) continue;
	if(hitP->q < minClusterCharge) { filtered_out++; continue; }
	if(hitP->q > maxClusterCharge) { filtered_out++; continue; }

	nHits++;

	//printf("z: %f\n", hitP->z);
    }

    LOG(DBG, "sect = %d (%d) nhits = %d   (%d)", sector, event->sectorFirstHit[sector], nHits, filtered_out);
}

/*
int FtfSl3::readSectorFromEvpReader(int sector) {

    embedded = 0;

    l3xyzCoordinate XYZ(0,0,0);
    l3ptrsCoordinate PTRS(0,0,0,0);
    
    sectorNr = sector;
    LOG("JEFF", "set sectorNr to %d", sector);
    if ( sectorNr < 1 || sectorNr > 24 ) {
	LOG(ERR, "Error - FtfSl3::readSector: Wrong sector %d!\n",sectorNr);
	return -1 ;
    }
    
    // read data...
    
    FtfHit *hitP;

    // Read TPC clusters in (doesn't use gl3 event, reads it again!
    // First do iTPC

    if(rdr != NULL) {
	daq_dta *dd;
	dd = rdr->det("itpc")->get("cld", sector);
	if(dd) {
	    while(dd->iterate()) {
		int padrow = dd->row;
		int sec = dd->sec;
		
		for(u_int i=0;i<dd->ncontent;i++) {
		    float pad = dd->cld[i].pad;
		    float tb = dd->cld[i].tb;
		    int charge = dd->cld[i].charge;
		    int flags = dd->cld[i].flags;
		    int padrow = dd->row;
		    
		    
		    //printf("nHits = %d\n", nHits);
		    hitP = &hit[nHits];
		    
		    l3ptrsCoordinate raw;
		    l3xyzCoordinate local;
		    l3xyzCoordinate global;
		    
		    int pads = 2 * (padrow + 25 - (int)((double)padrow/7.0));
		    int padpos = pad - pads/2;
		    
		    raw.Sets(sector);
		    local.Setx(padpos * .5);
		    local.Sety(55.80 + 1.6 * (padrow - 1));
		    local.Setz(getCoordinateTransformer()->drift_length_inner - tb * getCoordinateTransformer()->lengthPerTb);
		    getCoordinateTransformer()->local_to_global(raw, local, global);
		    
		    hitP->id = nHits;
		    hitP->row = padrow;
		    hitP->sector = sector;
		    hitP->x = (float)global.Getx();
		    hitP->y = (float)global.Gety();
		    hitP->z = (float)global.Getz();
		    hitP->dx = xyError;
		    hitP->dy = xyError;
		    hitP->dz = zError;
		    hitP->buffer1 = (int)(pad * 64);
		    hitP->buffer2 = (int)(tb * 64);
		    
		    if(embedded) 
			hitP->flags = (flags | (1<<7));
		    else 
			hitP->flags = flags;
		    
		    hitP->q = charge;
		    hitP->hardwareId = 0;
		    
		    nHits++;
		}
	    }
	    
	    
	    
	    
	    //gl3c->setITPCHit(coordinateTransformer, sec, padrow, pad, tb, charge, flags);
	}
    }

    //LOG("JEFF","itpc nHits=%d", nHits);
    int filtered_out = 0;

    // Now do TPC!
    if(pTPC) {
	for(int r=0;r<45;r++) {
	    
	    for(int j=0;j<pTPC->cl_counts[r];j++) {
		tpc_cl *c = &pTPC->cl[r][j];

		
		hitP = &hit[nHits];
		
		
		// Some cuts...
		if(c->t < minTimeBin) { filtered_out++; continue; }
		if(c->t > maxTimeBin) { filtered_out++; continue; }
		if(c->charge < minClusterCharge) { filtered_out++; continue; }
		if(c->charge > maxClusterCharge) { filtered_out++; continue; }
		
		//if((sector == 1)) {
		//printf("cl new: %4.2f %4.2f %f %f\n",c->p - .5, c->t - .5, (float)r+1, (float)sector);
		//}
		
		// The subtractions are to handle the pad offsets
		// My choice at the momement is to make L3 tracking identical to 
		// old versions.   The difference is because Tonko includes the pad centroid
		// shifts in the calculation of c->t & c->p.   It may be that in the old
		// L3 code this shift was included, but in the transform code instead.
		// Untill I know, I leave it as it was before...
		//
		PTRS.Setptrs(c->p - .5 , c->t - .5, r+1, sector);
		getCoordinateTransformer()->raw_to_global(PTRS, XYZ);
		
		hitP->id = nHits;
		hitP->row = r+1;
		hitP->sector = sector;
		hitP->x = (float)XYZ.Getx();
		hitP->y = (float)XYZ.Gety();
		hitP->z = (float)XYZ.Getz();
		hitP->dx = xyError;
		hitP->dy = xyError;
		hitP->dz = zError;
		hitP->buffer1 = (int)((c->p - 0.5) * 64);
		hitP->buffer2 = (int)((c->t - 0.5) * 64);
		
		if(embedded) 
		    hitP->flags = (c->flags | (1<<7));
		else 
		    hitP->flags = c->flags;
		
		hitP->q = c->charge;
		hitP->hardwareId = 0;
		
		nHits++;
	    }
	}
    }

    LOG(DBG, "sector = %d nHits = %d  (%d)", sector, nHits, filtered_out);

    return 0;
} 
*/



// Done with read sector from evpReader
///////////////////////////////////////



// Read 1 sector, starting from DATAP.
// sector is hypersector, numbered 1..23
//
int FtfSl3::readSector (DATAP *datap, int sector) {

  if ( sector < 1 && sector > 24 ) {
    LOG(ERR, "Error - FtfSl3::readSector: Wrong sector %d!\n",sectorNr);
    return -1 ;
  }
  
  int off;
  int len;

  int swapdatap=0;
  if(datap->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swapdatap = 1;
  len = qswap32(swapdatap, datap->det[TPC_ID].len);
  off = qswap32(swapdatap, datap->det[TPC_ID].off);
  if(len == 0) return -1;
  if(off == 0) return -1;

  TPCP *tpcp = (TPCP *)((u_int *)datap + off) ;
  int swaptpcp = 0;
  if(checkBank((char *)tpcp,"TPCP") < 0) return -1;
  if(tpcp->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swaptpcp = 1;
  len = qswap32(swaptpcp, tpcp->sb[sector-1].len);
  off = qswap32(swaptpcp, tpcp->sb[sector-1].off);
  if(len == 0) return -1;
  if(off == 0) return -1;

  TPCSECP *secp = (TPCSECP *)((u_int *)tpcp + off);
  int swapsecp = 0;
  if(checkBank((char *)secp,"TPCSECP") < 0) return -1;
  if(secp->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swapsecp = 1;
  off = qswap32(swapsecp, secp->bh.w9);
  if(off == 0) return -1;
  
  TPCSECLP *seclp = (TPCSECLP *)((u_int *)secp + off);
  if(checkBank((char *)seclp,"TPCSECLP") < 0) return -1;
  readSector((bankHeader *)seclp);

  return 0;
} 

//******************************************************************
//     Read Sector from buffer (using daqFormats.h)
//
//     function to read in 2 sectors from different event and merge them 
//     to be able to embed one event into another one
//     
//     
//******************************************************************
int FtfSl3::readSector ( struct TPCSECLP *seclp1, struct TPCSECLP *seclp2 ) {

   struct TPCRBCLP *rbclp ;
   struct TPCMZCLD_local *mzcld ;
   int iRb, kMz;
   int sector,tmpsector; 
   int nHitsOfMz;
   struct TPCSECLP *seclp; // 'generic' sector pointer
   short swapByte ; 

   // reset sector-hit counter
   nHits = 0;
   //
   // check byte order of SECLP bank
   // byte swapping is needed
   short swapByte1 = 0 ;
   short swapByte2 = 0;
   if     ( !checkByteOrder(seclp1->bh.byte_order) ) swapByte1 = 1 ;
   if     ( !checkByteOrder(seclp2->bh.byte_order) ) swapByte2 = 1 ;

   //   nHitsOfMz = 0;

   sector = (unsigned int)seclp1->bh.bank_id ;
   if ( swapByte1 ) sector = swap32(sector) ;
   tmpsector = (unsigned int)seclp2->bh.bank_id ;
   if ( swapByte2 ) tmpsector = swap32(tmpsector) ;
   // check that both sectors are consistent
   if(tmpsector != sector){
       LOG(ERR,"Error in embedding got different sectors to read in\n");
       exit(1);
   }
   LOG(ERR,"sector: %i\n",sector);

   sectorNr = sector;
   sectorGeometry* sectorG = (sectorGeometry *)sectorGeo ;
   para.phiMin = sectorG[sector-1].phiMin ;
   para.phiMax = sectorG[sector-1].phiMax ;
   para.etaMin = sectorG[sector-1].etaMin ;
   para.etaMax = sectorG[sector-1].etaMax ;
   //
   //   Check Sector 
   //
   if ( sectorNr < 0 && sectorNr > 24 ) {
      LOG(ERR,"Error - FtfSl3::readSector: Wrong sector %d!\n",sectorNr);
      return -1 ;
   }

   seclp = seclp1;
   swapByte = swapByte1;
   embedded = 0;

   for(int embed=0;embed<2;embed++){
       // two  loops first set the pointers to event one then after that is 
       // done to event 2, at end of for loop
       
       
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
		   if ( sectorG[sector-1].phiMin < para.phiMin ) para.phiMin = sectorG[sector-1].phiMin ;
		   if ( sectorG[sector-1].phiMax > para.phiMax ) para.phiMax = sectorG[sector-1].phiMax ;
		   para.phiShift = 0 ;
	       }
	       if ( sectorG[sector-1].etaMin < para.etaMin ) para.etaMin = sectorG[sector-1].etaMin ;
	       if ( sectorG[sector-1].etaMax > para.etaMax ) para.etaMax = sectorG[sector-1].etaMax ;
	   }
	   //l3Log ( "sector %d rb %d phi min/max %f %f \n", sector, iRb, para.phiMin, para.phiMax ) ;
	   
	   
	   if ( !(unsigned int)seclp->rb[iRb].off) continue ;
	   
	   int off = (unsigned int)seclp->rb[iRb].off;
	   if ( swapByte ) off = swap32(off);
	   rbclp = (struct TPCRBCLP *)((char *)seclp + off * 4) ;
	   int swapByteMezzaninne = 0 ;
	   if     ( !checkByteOrder(rbclp->bh.byte_order) ) swapByteMezzaninne = 1 ;
	   
	   // run over the 3 mezzanines
	   for (kMz=0; kMz<RB_MZ_NUM; kMz++) {
	       
	       if( ! rbclp->mz[kMz].off) {
		   continue;
	       }
#ifdef TRDEBUG
	       else {
		   if ( debugLevel > 1 ) {
		       LOG(ERR, "FtfSl3::readSector:       MZCLD %d exists!\n", kMz+1) ;
		   }
	       }
#endif
	       
	       off = rbclp->mz[kMz].off ;
	       if ( swapByteMezzaninne ) off = swap32(off);
	       if (off == 0) continue;
	       mzcld = (struct TPCMZCLD_local *) ((char *)rbclp + off*4) ;
	       
	       if (mzcld) {
		     if(strncmp((char*)mzcld, "TPCMZCLD",8)!=0){
			   LOG(ERR, "ERROR: TPCMZCLD header not found\n");
			   continue; 
		     }
		    		     
		     nHitsOfMz = readMezzanine (sector, iRb, kMz, mzcld);
		     if (nHitsOfMz<0) {
			   LOG(ERR, "FtfSl3:readSector: wrong reading mezzanine \n" ) ;
			   return -1;
		     }
		     nHits += nHitsOfMz;
	       }
	       
	   }
       }

       seclp = seclp2;
       swapByte = swapByte2;
       embedded = 1;
       // reset sector
       sector = sectorNr;
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
   if (para.dEdx) dEdx();
   //
   if ( debugLevel > 0 ) 
      LOG(ERR," FtfSl3::process: tracks %i Time: real %f cpu %f\n", 
	    nTracks, realTime, cpuTime ) ;
   //
   //
   return 0;
}
//********************************************************************
//     Prcess, encapsulates readSector(); processSector(); fillTracks/Hits()
//********************************************************************
#if 0
int FtfSl3::processData(TPCSECLP* seclp, char* trackBuffer, 
			int maxTrackBytes, int& nTrackBytes,
			char* hitBuffer, int maxHitBytes, int& nHitBytes)
{
  // first: get token number
  short swapByte = 0;
  if (!checkByteOrder(seclp->bh.byte_order)) swapByte = 1;

  int token = (unsigned int)seclp->bh.token;
  if (swapByte) token = swap32(token);

  readSector(seclp);
  processSector();

  // write out tracks only if trackBuffer != 0
  if (trackBuffer) {
	int ret = fillTracks(maxTrackBytes, trackBuffer, token);
	if (ret>0) nTrackBytes = ret;
	else return 1;
  }
  // write out hits only if hitBuffer != 0
  if (hitBuffer) {
	int ret = fillHits(maxHitBytes, hitBuffer, token);
	if (ret>0) nHitBytes = ret;
	else return 1;
  }
  return 0;
}
#endif

//********************************************************************
//     Calculates deposited Energy
//********************************************************************
int FtfSl3::dEdx ( ) {

   for ( int i = 0 ; i<nTracks ; i++ ){
         if (track[i].nHits<para.minHitsForDedx) {
               track[i].nDedx = 0;
               track[i].dedx  = 0;
               continue;
         }
         fDedx->TruncatedMean(&track[i]);
   }
  return 0;
}
//***************************************************************
//   Set Default parameters
//***************************************************************
int FtfSl3::setParameters ( ) {

   // FtfPara* para = &(para) ;

   xyError                = 0.12 ;
   zError                 = 0.24 ;
   para.hitChi2Cut        =  50.  ;
   para.goodHitChi2       =  20.  ;
   para.trackChi2Cut      =  10.  ;
   para.maxChi2Primary    = 50.   ; // track with vertex if refit and minPtHelix is reasonable.
   para.segmentRowSearchRange = 2   ;
   para.trackRowSearchRange = 3    ;
   para.dphi              = 0.08F  ;
   para.deta              = 0.08F  ;
   para.dphiMerge        = 0.01F  ;
   para.detaMerge        = 0.02F  ;
   para.etaMinTrack      = -2.2F  ;
   para.etaMaxTrack      =  2.2F  ;

   para.dEdx             =  1     ;
   // get Errors = 1 makes FPE's
   para.getErrors        =  0     ;
   para.goBackwards      =  1     ;
   para.goodDistance     =  5.F   ;
   para.mergePrimaries   =  0     ;
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

//   para.ptMinHelixFit    = 100.F  ;
   para.ptMinHelixFit    = 0.1  ; // enables refit 
   para.rVertex          = 0.F    ;
   para.xVertex          = 0.F    ;
   para.yVertex          = 0.F    ;
   para.zVertex          = 0.F    ;
   para.dxVertex         = 0.05F ;
   para.dyVertex         = 0.05F ;
   para.phiVertex        = 0.F    ;

   para.zMax             = 205. ;
   return 0;
}





