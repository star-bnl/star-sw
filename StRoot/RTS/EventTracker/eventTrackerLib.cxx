// Switch between l3t and ftf in sl3.h!!!

#ifndef TRG_VERSION
#define TRG_VERSION 0x32
#endif

#include <unistd.h>
#include <string.h>
#include <setjmp.h>
#include <daqFormats.h>
#include "l3CoordinateTransformer.h"
#include <rts.h>
#include "rtsLog.h"
#include "FtfSl3.h"
#ifdef OLD_DAQ_READER
#include <evpReader.hh>
#else /* OLD_DAQ_READER */
#include <DAQ_READER/daqReader.h>
#endif /* OLD_DAQ_READER */
#include "gl3Event.h"
#include "eventTrackerLib.hh"
#include <DAQ_L3/daq_l3.h>


#ifdef OLD_DAQ_READER
int EventTracker::trackEvent(evpReader *evp, char *mem, L3_P *l3p, int max_size)
#else /* OLD_DAQ_READER */
int EventTracker::trackEvent(daqReader *rdr, char *mem, L3_P *l3p, int max_size)
#endif /* OLD_DAQ_READER */
{
  DATAP *datap = (DATAP *)mem;
  // Build L3_p
  int ret=0;
  char *buff = (char *)l3p;

  // First do tracking...
  L3_GTD *gtd = (L3_GTD *)(buff + sizeof(L3_P));
#ifdef OLD_DAQ_READER
  ret = trackTPC(evp, mem, gtd, max_size - sizeof(L3_P));
#else /* OLD_DAQ_READER */
  ret = trackTPC(rdr, mem, gtd, max_size - sizeof(L3_P));
#endif /* OLD_DAQ_READER */
 
  LOG(DBG, "gtd: nHits=%d", gtd->nHits);

   // Now build the L3_P bank...
  memset(l3p, 0, sizeof(L3_P));  

  memcpy(l3p->bh.bank_type, CHAR_L3_P, 8);
  l3p->bh.length = sizeof(L3_P) / 4;
  l3p->bh.bank_id = 0;
  l3p->bh.format_ver = DAQ_RAW_FORMAT_VERSION;
  l3p->bh.byte_order = DAQ_RAW_FORMAT_ORDER;
  l3p->bh.format_number = 0;
  l3p->bh.token = 0;
  l3p->bh.w9 = DAQ_RAW_FORMAT_WORD9;
  l3p->bh.crc = 0;

  // Pick up trigger info from datap if possible...
  l3p->len = (sizeof(L3_P) + gtd->bh.length * 4) / 4;
  
  // do we need to swap datap?
  int sdatap = (datap->bh.byte_order == DAQ_RAW_FORMAT_ORDER) ? 0 : 1;

  
  l3p->time = qswap32(sdatap, datap->time);
  l3p->gl3Id = 0;
  l3p->trg_word = qswap32(sdatap, datap->trg_word);
  l3p->trg_in_word = qswap32(sdatap, datap->trg_in_word);

  // Currently only fill tracks...
  l3p->tracks.off = sizeof(L3_P) / 4;
  l3p->tracks.len = gtd->bh.length;

  return ret;
}

#ifdef OLD_DAQ_READER
int EventTracker::trackTPC(evpReader *rdr, char *mem, L3_GTD *gtd, int max_size)
#else /* OLD_DAQ_READER */
int EventTracker::trackTPC(daqReader *rdr, char *mem, L3_GTD *gtd, int max_size)
#endif /* OLD_DAQ_READER */
{
  // bField != 1000 means use it, 1000 means take from file...
  gl3->readFromEvpReader(rdr, 1000);

  return gl3Event_to_GTD(gtd, max_size);
}


int EventTracker::gl3Event_to_GTD(L3_GTD *gtd,  u_int max_size)
{
  // Build GTD...
  memcpy(gtd->bh.bank_type, CHAR_L3_GTD, 8);
  gtd->bh.bank_id = 1;
  gtd->bh.format_ver = DAQ_RAW_FORMAT_VERSION;
  gtd->bh.byte_order = DAQ_RAW_FORMAT_ORDER;
  gtd->bh.format_number = 0;
  gtd->bh.token = 0;
  gtd->bh.w9 = DAQ_RAW_FORMAT_WORD9;
  gtd->bh.crc = 0;
  
  // GTD structure already includes one track
  gtd->bh.length = (sizeof(L3_GTD) + (gl3->getNTracks() - 1) * sizeof(global_track))/4;
  
  if(gtd->bh.length * 4 > max_size) {
    LOG(ERR, "GDT length needs to be %d bytes, but max buffers size only %d bytes",
	gtd->bh.length * 4, max_size,0,0,0);
    
    return -1;
  }
  
  // This stuff need to figure out later.  
  gtd->nHits = 0;
  
  gtd->xVert = gl3->getVertex().Getx();
  gtd->yVert = gl3->getVertex().Gety();
  gtd->zVert = gl3->getVertex().Getz();
  gtd->nTracks = gl3->getNTracks();

  for(uint i=0;i<gtd->nTracks;i++) {

    global_track gtrack;
    gl3Track *gl3track = gl3->getTrack(i);

    gtrack.id = gl3track->id;
    gtrack.flag = gl3track->flag;                                // Primaries flag=1, Secondaries flag=0
    gtrack.innerMostRow = gl3track->innerMostRow;
    gtrack.outerMostRow = gl3track->outerMostRow;

    gtrack.nHits = gl3track->nHits;                  // Number of points assigned to that track
    gtd->nHits += gtrack.nHits;

    //gtrack->reserved ; 
    gtrack.ndedx = gl3track->nDedx;                  // nr of clusters contributing to the dedx value
    gtrack.q = gl3track->q;              // charge
    gtrack.chi2[0] = gl3track->chi2[0];         // chi squared of the momentum fit
    gtrack.chi2[1] = gl3track->chi2[1];
    gtrack.dedx = gl3track->dedx;                    // dE/dx information
    gtrack.pt = gl3track->pt;                  // pt (transverse momentum) at (r,phi,z)
    gtrack.phi0 = gl3track->phi0;                    // azimuthal angle of the first point
    gtrack.psi = gl3track->psi;   // azimuthal angle of the momentum at (r,..
    gtrack.r0 = gl3track->r0;                                    // r (in cyl. coord.) for the first point
    gtrack.tanl = gl3track->tanl;                                   // tg of the dip angle at (r,phi,z)
    gtrack.z0 = gl3track->z0;                                    // z coordinate of the first point
    gtrack.length = gl3track->length;
    gtrack.dpt = gl3track->dpt;
    gtrack.dpsi = gl3track->dpsi;
    gtrack.dz0 = gl3track->dz0;
    gtrack.dtanl = gl3track->dtanl;

    
    memcpy(&gtd->track[i], &gtrack, sizeof(global_track));
  }  

  return 0;
}

void EventTracker::dumpGTD(L3_GTD *gtd)
{
  if(memcmp(gtd->bh.bank_type, "L3_GTD", 5) != 0) {
    printf("Not a L3_GTD Bank: %s\n",gtd->bh.bank_type);
    return;
  }

  printf("Computed:   Tracks: %5d:    Vertex: (%6.2f/%6.2f/%6.2f)\n",
	 gtd->nTracks,gtd->xVert, gtd->yVert, gtd->zVert);
  
  for(u_int i=0;i<gtd->nTracks;i++) {
    global_track *track = &(gtd->track[i]);
    
    
    printf("%5d: pt=%5.3f z0=%7.2f q=%2d nHits=%2d ndedx=%2d ",
	   i, track->pt, track->z0, track->q, 
	   track->nHits, track->ndedx); 
    
    printf("flag=0x%04x iRow=%2d oRow=%2d\n",
	   track->flag, track->innerMostRow, 
	   track->outerMostRow); 
  }
}


int EventTracker::copyl3_t(l3_t &l3, L3_P *l3p)
{
  int len = l3p->bh.length;

  l3.max_channels = 1280000 ;
  l3.channels = 0 ;
  l3.mode = 1 ;

  // Tonko, zero this out and make non-sensical in case of further
  // problems down the road
  l3.tracks_num = 0 ;
  l3.cluster_num = 0 ;
  l3.xVertex = -1000.0 ;
  l3.yVertex = -1000.0 ;
  l3.zVertex = -1000.0 ;

  LOG(DBG,"L3_P bytes %d",len,0,0,0) ;

  if(checkBank(l3p->bh.bank_type,"L3_P") != 0) {
    return -1 ;
  }


  if(l3p->tracks.len && l3p->tracks.off){
    struct L3_GTD* l3gtd = 
      (struct L3_GTD*)((char*)l3p + l2h32(l3p->tracks.off)*4) ;

    // Tonko, sanity check
    if(checkBank(l3gtd->bh.bank_type,"L3_GTD") < 0) {
      return -1 ;
    }

    LOG(NOTE, "l3gtd->nTracks=%d nHits=%d", l2h32(l3gtd->nTracks), l2h32(l3gtd->nHits));
    l3.tracks_num = l2h32(l3gtd->nTracks);
    l3.cluster_num = l2h32(l3gtd->nHits);
    l3.xVertex = l2hfloat(l3gtd->xVert);
    l3.yVertex = l2hfloat(l3gtd->yVert);
    l3.zVertex = l2hfloat(l3gtd->zVert);

    // Tonko, sanity check
    if(l3.tracks_num >= L3_MAX_NR_TRACKS) {
      LOG(ERR,"L3 track number %d > %d!",l3.tracks_num,L3_MAX_NR_TRACKS ,0,0,0) ;
      return -1 ;
    }


    for (unsigned int i=0; i<l3.tracks_num; i++) {
      global_track *tr = &(l3gtd->track[i]);

      l3.track[i].id = l2h32(tr->id);

#ifndef UNIX_LITTLE_ENIDAN
      l3.track[i].flag         = tr->flag;
      l3.track[i].innerMostRow = tr->innerMostRow;
      l3.track[i].outerMostRow = tr->outerMostRow;

      l3.track[i].nHits = tr->nHits;
      l3.track[i].ndedx = tr->ndedx;
      l3.track[i].q     = tr->q;
#else
      l3.track[i].flag = ( ((unsigned short)tr->innerMostRow) |
			   ((unsigned short)tr->outerMostRow)<<8);
      l3.track[i].innerMostRow = (char)( tr->flag & 0x00ff );
      l3.track[i].outerMostRow = (char)((tr->flag & 0xff00)>>8);

      l3.track[i].nHits    = (unsigned char)tr->q;
      l3.track[i].reserved = (char)tr->ndedx;
      l3.track[i].ndedx    = (unsigned char)tr->reserved;
      l3.track[i].q        = (char)tr->nHits;
#endif
      l3.track[i].chi2[0] = l2hfloat(tr->chi2[0]);
      l3.track[i].chi2[1] = l2hfloat(tr->chi2[1]);
      l3.track[i].dedx    = l2hfloat(tr->dedx);
      l3.track[i].pt      = l2hfloat(tr->pt);
      l3.track[i].phi0    = l2hfloat(tr->phi0);
      l3.track[i].psi     = l2hfloat(tr->psi);
      l3.track[i].r0      = l2hfloat(tr->r0);
      l3.track[i].tanl    = l2hfloat(tr->tanl);
      l3.track[i].z0      = l2hfloat(tr->z0);
      l3.track[i].length  = l2hfloat(tr->length);
      l3.track[i].dpt     = l2hfloat(tr->dpt);
      l3.track[i].dpsi    = l2hfloat(tr->dpsi);
      l3.track[i].dz0     = l2hfloat(tr->dz0);
      l3.track[i].dtanl   = l2hfloat(tr->dtanl);

    }
	      

  }


#ifdef SHOW_DEBUG_INFO

#ifdef UNIX_LITTLE_ENDIAN
  printf("Running on LITTLE endian machine\n");
#else
  printf("Running on BIG endian machine\n");
#endif

  printf("\nVertex: (%6.2f/%6.2f/%6.2f)\n",
	 l3.xVertex, l3.yVertex, l3.zVertex);

  printf("Tracks: %5d   Clusters %7d\n",
	 l3.tracks_num, l3.cluster_num);
	
  for (unsigned int i=0; i<l3.tracks_num; i++) {
    printf("%5d: pt=%5.3f z0=%7.2f q=%2d nHits=%2d ndedx=%2d ",
	   i, l3.track[i].pt, l3.track[i].z0, l3.track[i].q, 
	   l3.track[i].nHits, l3.track[i].ndedx); 

    printf("flag=0x%04x iRow=%2d oRow=%2d\n",
	   l3.track[i].flag, l3.track[i].innerMostRow, 
	   l3.track[i].outerMostRow); 
  }

#endif

  return len ;

}
