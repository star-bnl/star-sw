// Switch between l3t and ftf in sl3.h!!!

#include <unistd.h>
#include <string.h>
#include <setjmp.h>
#include <daqFormats.h>
#include "l3CoordinateTransformer.h"
#include "rtsLog.h"
#include "FtfSl3.h"
#include <evpReader.hh>
#include "gl3Event.h"
#include "eventTrackerLib.hh"

int EventTracker::trackEvent(evpReader *evp, char *mem, L3_P *l3p, int max_size)
{
  DATAP *datap = (DATAP *)mem;
  // Build L3_p
  int ret=0;
  uint buff = (uint)l3p;

  // First do tracking...
  L3_GTD *gtd = (L3_GTD *)(buff + sizeof(L3_P));
  ret = trackTPC(evp, mem, gtd, max_size - sizeof(L3_P));
 
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

int EventTracker::trackTPC(evpReader *evp, char *mem, L3_GTD *gtd, int max_size)
{
  // bField != 1000 means use it, 1000 means take from file...
  gl3->readFromEvpReader(evp, mem, defaultBField, bField, GL3_READ_TPC_TRACKS);
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
