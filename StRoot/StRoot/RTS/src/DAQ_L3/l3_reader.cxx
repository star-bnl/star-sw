#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>

#include <rtsLog.h>
#include <rtsSystems.h>
#include <rts.h>

#include "daq_l3.h"


inline float fswap(float swapped)
{
        unsigned int* uintptr = (unsigned int*) &swapped;
        unsigned int uintvar = l2h32(*uintptr);
        float* floatvar = (float*)&uintvar;
        return *floatvar;
}


#ifdef RTS_LITTLE_ENDIAN
#define l2hfloat(x) (x)
#define b2hfloat(x) (fswap(x))
#else
#define l2hfloat(x) (fswap(x))
#define b2hfloat(x) (x)
#endif


int l3_reader(char *m, struct l3_t *l3, u_int driver)
{
	struct DATAP *datap = (struct DATAP *)m ;
	struct L3_P *l3p ;
	int len, off ;
	char *l3_gtd_start = 0 ;
	int fy09_format = 0 ;

	l3->max_channels = 1280000 ;
	l3->channels = 0 ;
	l3->mode = 1 ;

// Tonko, zero this out and make non-sensical in case of further
// problems down the road
	l3->tracks_num = 0 ;
	l3->cluster_num = 0 ;
	l3->xVertex = -1000.0 ;
	l3->yVertex = -1000.0 ;
	l3->zVertex = -1000.0 ;


	if(datap == NULL) return 0 ;

	if(driver) {	// "m" points directly to L3_GTD! Used in FY09 HLT data
		l3_gtd_start = m ;	// directly points to L3_GTD!		
		fy09_format = 1 ;
		
		l3->mode = 0 ;	// this holds the trigger def
		l3->channels = 0 ;	// this holds the sequnce number
	}
	else {

		len = ntohl(datap->det[L3_ID].len) * 4 ;
		if(len == 0) return 0 ;

		off = ntohl(datap->det[L3_ID].off) ;
		if(off == 0) return 0 ;

		l3p = (struct L3_P *)((u_int *)m+off) ;


		len = l3p->bh.length;


		LOG(DBG,"L3_P bytes %d",len,0,0,0) ;

		if(checkBank(l3p->bh.bank_type,"L3_P") < 0) {
			return -1 ;
		}


		if(l3p->tracks.len && l3p->tracks.off){
			l3_gtd_start = ((char*)l3p + l2h32(l3p->tracks.off)*4) ;
		}
	}


	if(!l3_gtd_start) return 0 ;

//	if(l3p->tracks.len && l3p->tracks.off){
//	      struct L3_GTD* l3gtd = (struct L3_GTD*)((char*)l3p + l2h32(l3p->tracks.off)*4) ;

	if(l3_gtd_start) {
	      struct L3_GTD* l3gtd = (struct L3_GTD *) l3_gtd_start ;

// Tonko, sanity check
		if(checkBank(l3gtd->bh.bank_type,"L3_GTD") < 0) {
			return -1 ;
		}

		if(fy09_format) {	// override stuff
			l3->mode = l2h32(l3gtd->bh.crc) ;	// contains the decision!
			l3->channels = l2h32(l3gtd->bh.w9) ;	// contains the event sequence number from trigger!
		}

	      l3->tracks_num = l2h32(l3gtd->nTracks);
	      l3->cluster_num = l2h32(l3gtd->nHits);
	      l3->xVertex = l2hfloat(l3gtd->xVert);
	      l3->yVertex = l2hfloat(l3gtd->yVert);
	      l3->zVertex = l2hfloat(l3gtd->zVert);

// Tonko, sanity check
		if(l3->tracks_num >= L3_MAX_NR_TRACKS) {
			LOG(ERR,"L3 track number %d > %d! Maxing them out...",l3->tracks_num,L3_MAX_NR_TRACKS ,0,0,0) ;
			l3->tracks_num = L3_MAX_NR_TRACKS ;
		}


	      for (unsigned int i=0; i<l3->tracks_num; i++) {
		  global_track *tr = &(l3gtd->track[i]);

		  l3->track[i].id = l2h32(tr->id);

#ifndef UNIX_LITTLE_ENIDAN
		  l3->track[i].flag         = tr->flag;
		  l3->track[i].innerMostRow = tr->innerMostRow;
		  l3->track[i].outerMostRow = tr->outerMostRow;

		  l3->track[i].nHits = tr->nHits;
		  l3->track[i].ndedx = tr->ndedx;
		  l3->track[i].q     = tr->q;
#else
		  l3->track[i].flag = ( ((unsigned short)tr->innerMostRow) |
				       ((unsigned short)tr->outerMostRow)<<8);
		  l3->track[i].innerMostRow = (char)( tr->flag & 0x00ff );
		  l3->track[i].outerMostRow = (char)((tr->flag & 0xff00)>>8);

		  l3->track[i].nHits    = (unsigned char)tr->q;
		  l3->track[i].reserved = (char)tr->ndedx;
		  l3->track[i].ndedx    = (unsigned char)tr->reserved;
		  l3->track[i].q        = (char)tr->nHits;
#endif
		  l3->track[i].chi2[0] = l2hfloat(tr->chi2[0]);
		  l3->track[i].chi2[1] = l2hfloat(tr->chi2[1]);
		  l3->track[i].dedx    = l2hfloat(tr->dedx);
		  l3->track[i].pt      = l2hfloat(tr->pt);
		  l3->track[i].phi0    = l2hfloat(tr->phi0);
		  l3->track[i].psi     = l2hfloat(tr->psi);
		  l3->track[i].r0      = l2hfloat(tr->r0);
		  l3->track[i].tanl    = l2hfloat(tr->tanl);
		  l3->track[i].z0      = l2hfloat(tr->z0);
		  l3->track[i].length  = l2hfloat(tr->length);
		  l3->track[i].dpt     = l2hfloat(tr->dpt);
		  l3->track[i].dpsi    = l2hfloat(tr->dpsi);
		  l3->track[i].dz0     = l2hfloat(tr->dz0);
		  l3->track[i].dtanl   = l2hfloat(tr->dtanl);

	      }
	      

	}


#ifdef SHOW_DEBUG_INFO

#ifdef UNIX_LITTLE_ENDIAN
	printf("Running on LITTLE endian machine\n");
#else
	printf("Running on BIG endian machine\n");
#endif

	printf("\nVertex: (%6.2f/%6.2f/%6.2f)\n",
	       l3->xVertex, l3->yVertex, l3->zVertex);

	printf("Tracks: %5d   Clusters %7d\n",
	       l3->tracks_num, l3->cluster_num);
	
	for (unsigned int i=0; i<l3->tracks_num; i++) {
	  printf("%5d: pt=%5.3f z0=%7.2f q=%2d nHits=%2d ndedx=%2d ",
		 i, l3->track[i].pt, l3->track[i].z0, l3->track[i].q, 
		 l3->track[i].nHits, l3->track[i].ndedx); 

	  printf("flag=0x%04x iRow=%2d oRow=%2d\n",
		 l3->track[i].flag, l3->track[i].innerMostRow, 
		 l3->track[i].outerMostRow); 
	}

#endif

	return 1 ; // anything positive...

}

