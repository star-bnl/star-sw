// $Id: StFtpcGeantReader.cc,v 1.4 2007/01/15 07:49:22 jcs Exp $
//
// $Log: StFtpcGeantReader.cc,v $
// Revision 1.4  2007/01/15 07:49:22  jcs
// replace printf, cout and gMesMgr with Logger
//
// Revision 1.3  2004/01/28 01:41:15  jeromel
// Change OST to OS everywhere since defaultoption is now not to print
// the date.
//
// Revision 1.2  2003/10/07 12:40:29  jcs
// created member functions to extract ftpc plane and sector number from the GEANT volume id
//
// Revision 1.1  2000/09/18 14:27:42  hummler
// add StFtpcGeantReader
//
//

#include "StFtpcGeantReader.hh"

#include <math.h>
#include "StMessMgr.h"

StFtpcGeantReader::StFtpcGeantReader(St_g2t_vertex *vertex,
				     St_g2t_track *track,
				     St_g2t_ftp_hit *hit)
{
  vertexTable = vertex->GetTable();
  nVertices = vertex->GetNRows();
  trackTable = track->GetTable();
  nTracks = track->GetNRows();
  hitTable = hit->GetTable();
  nHits = hit->GetNRows();
}

StFtpcGeantReader::~StFtpcGeantReader()
{
}

Int_t StFtpcGeantReader::nextTrackHit(Int_t i)
{
  if(i>=0 && i<nHits)
    {
      return hitTable[i].next_tr_hit_p-1;
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: nextTrackHit index out of range, using 0" << endm;
      return hitTable[0].next_tr_hit_p-1;
    }
}

Int_t StFtpcGeantReader::track(Int_t i)
{
  if(i>=0 && i<nHits)
    {
      return hitTable[i].track_p-1;
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: track index out of range, using 0" << endm;
      return hitTable[0].track_p-1;
    }
}

Int_t StFtpcGeantReader::geantVolume(Int_t i)
{
  if(i>=0 && i<nHits)
    {
      return hitTable[i].volume_id;
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: geantVolume index out of range, using 0" << endm;
      return hitTable[0].volume_id;
    }
}
Int_t StFtpcGeantReader::geantPlane(Int_t GeantVolume)
{
  // from the Geant VolumeId, the plane number {1,20} (West,East)

  return (GeantVolume/1000 -1)*10 + GeantVolume/10 % 100; 
}
Int_t StFtpcGeantReader::geantSector(Int_t GeantVolume)
{
  // from the Geant VolumeId, the sector number {1,6} 
  return GeantVolume%10 ; 
}

Float_t StFtpcGeantReader::energyLoss(Int_t i)
{
  if(i>=0 && i<nHits)
    {
      return hitTable[i].de;
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: energyLoss index out of range, using 0" << endm;
      return hitTable[0].de;
    }
}

Float_t StFtpcGeantReader::segmentLength(Int_t i)
{
  if(i>=0 && i<nHits)
    {
      return hitTable[i].ds;
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: segmentLength index out of range, using 0" << endm;
      return hitTable[0].ds;
    }
}

Float_t StFtpcGeantReader::pLocalX(Int_t i)
{
  if(i>=0 && i<nHits)
    {
      return hitTable[i].p[0];
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: pLocalX index out of range, using 0" << endm;
      return hitTable[0].p[0];
    }
}

Float_t StFtpcGeantReader::pLocalY(Int_t i)
{
  if(i>=0 && i<nHits)
    {
      return hitTable[i].p[1];
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: pLocalY index out of range, using 0" << endm;
      return hitTable[0].p[1];
    }
}

Float_t StFtpcGeantReader::pLocalZ(Int_t i)
{
  if(i>=0 && i<nHits)
    {
      return hitTable[i].p[2];
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: pLocalZ index out of range, using 0" << endm;
      return hitTable[0].p[2];
    }
}

Float_t StFtpcGeantReader::timeOfFlight(Int_t i)
{
  if(i>=0 && i<nHits)
    {
      return hitTable[i].tof;
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: timeOfFlight index out of range, using 0" << endm;
      return hitTable[0].tof;
    }
}

Float_t StFtpcGeantReader::x(Int_t i)
{
  if(i>=0 && i<nHits)
    {
      return hitTable[i].x[0];
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: x index out of range, using 0" << endm;
      return hitTable[0].x[0];
    }
}

Float_t StFtpcGeantReader::y(Int_t i)
{
  if(i>=0 && i<nHits)
    {
      return hitTable[i].x[1];
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: y index out of range, using 0" << endm;
      return hitTable[0].x[1];
    }
}

Float_t StFtpcGeantReader::z(Int_t i)
{
  if(i>=0 && i<nHits)
    {
      return hitTable[i].x[2];
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: z index out of range, using 0" << endm;
      return hitTable[0].x[2];
    }
}

Int_t StFtpcGeantReader::trackType(Int_t i)
{
  if(i>=0 && i<nHits)
    {
      return hitTable[i].track_type;
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: trackType index out of range, using 0" << endm;
      return hitTable[0].track_type;
    }
}

Int_t StFtpcGeantReader::trackPid(Int_t i)
{
  if(i>=0 && i<nHits && hitTable[i].track_p>0 && hitTable[i].track_p<=nTracks)
    {
      return trackTable[hitTable[i].track_p-1].ge_pid;
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: geantPid index out of range, using 0" << endm;
      return trackTable[0].ge_pid;
    }
}

Int_t StFtpcGeantReader::hitsOnTrack(Int_t i)
{
  if(i>=0 && i<nHits && hitTable[i].track_p>0 && hitTable[i].track_p<=nTracks)
    {
      return trackTable[hitTable[i].track_p-1].n_ftp_hit;
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: hitsOnTrack index out of range, using 0" << endm;
      return trackTable[0].n_ftp_hit;
    }
}

Int_t StFtpcGeantReader::firstHitOnTrack(Int_t i)
{
  if(i>=0 && i<nHits && hitTable[i].track_p>0 && hitTable[i].track_p<=nTracks)
    {
      return trackTable[hitTable[i].track_p-1].hit_ftp_p-1;
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: firstHitOnTrack index out of range, using 0" << endm;
      return trackTable[0].hit_ftp_p-1;
    }
}

Int_t StFtpcGeantReader::isShower(Int_t i)
{
  if(i>=0 && i<nHits && hitTable[i].track_p>0 && hitTable[i].track_p<=nTracks)
    {
      return trackTable[hitTable[i].track_p-1].is_shower;
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: isShower index out of range, using 0" << endm;
      return trackTable[0].is_shower;
    }
}

Int_t StFtpcGeantReader::trackVertex(Int_t i)
{
  if(i>=0 && i<nHits && hitTable[i].track_p>0 && hitTable[i].track_p<=nTracks)
    {
      return trackTable[hitTable[i].track_p-1].start_vertex_p-1;
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: trackVertex index out of range, using 0" << endm;
      return trackTable[0].start_vertex_p-1;
    }
}

Float_t StFtpcGeantReader::trackCharge(Int_t i)
{
  if(i>=0 && i<nHits && hitTable[i].track_p>0 && hitTable[i].track_p<=nTracks)
    {
      return trackTable[hitTable[i].track_p-1].charge;
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: trackCharge index out of range, using 0" << endm;
      return trackTable[0].charge;
    }
}

Float_t StFtpcGeantReader::trackEnergy(Int_t i)
{
  if(i>=0 && i<nHits && hitTable[i].track_p>0 && hitTable[i].track_p<=nTracks)
    {
      return trackTable[hitTable[i].track_p-1].e;
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: trackEnergy index out of range, using 0" << endm;
      return trackTable[0].e;
    }
}

Float_t StFtpcGeantReader::trackEta(Int_t i)
{
  if(i>=0 && i<nHits && hitTable[i].track_p>0 && hitTable[i].track_p<=nTracks)
    {
      return trackTable[hitTable[i].track_p-1].eta;
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: trackEta index out of range, using 0" << endm;
      return trackTable[0].eta;
    }
}

Float_t StFtpcGeantReader::pVertexX(Int_t i)
{
  if(i>=0 && i<nHits && hitTable[i].track_p>0 && hitTable[i].track_p<=nTracks)
    {
      return trackTable[hitTable[i].track_p-1].p[0];
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: pVertexX index out of range, using 0" << endm;
      return trackTable[0].p[0];
    }
}

Float_t StFtpcGeantReader::pVertexY(Int_t i)
{
  if(i>=0 && i<nHits && hitTable[i].track_p>0 && hitTable[i].track_p<=nTracks)
    {
      return trackTable[hitTable[i].track_p-1].p[1];
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: pVertexY index out of range, using 0" << endm;
      return trackTable[0].p[1];
    }
}

Float_t StFtpcGeantReader::pVertexZ(Int_t i)
{
  if(i>=0 && i<nHits && hitTable[i].track_p>0 && hitTable[i].track_p<=nTracks)
    {
      return trackTable[hitTable[i].track_p-1].p[2];
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: pVertexZ index out of range, using 0" << endm;
      return trackTable[0].p[2];
    }
}

Float_t StFtpcGeantReader::pTVertex(Int_t i)
{
  if(i>=0 && i<nHits && hitTable[i].track_p>0 && hitTable[i].track_p<=nTracks)
    {
      return trackTable[hitTable[i].track_p-1].pt;
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: pTVertex index out of range, using 0" << endm;
      return trackTable[0].pt;
    }
}

Float_t StFtpcGeantReader::pTotalVertex(Int_t i)
{
  if(i>=0 && i<nHits && hitTable[i].track_p>0 && hitTable[i].track_p<=nTracks)
    {
      return trackTable[hitTable[i].track_p-1].ptot;
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: pTotalVertex index out of range, using 0" << endm;
      return trackTable[0].ptot;
    }
}

Float_t StFtpcGeantReader::rapidityVertex(Int_t i)
{
  if(i>=0 && i<nHits && hitTable[i].track_p>0 && hitTable[i].track_p<=nTracks)
    {
      return trackTable[hitTable[i].track_p-1].rapidity;
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: rapidityVertex index out of range, using 0" << endm;
      return trackTable[0].rapidity;
    }
}

Int_t StFtpcGeantReader::productionProcess(Int_t i)
{
  if(i>=0 && i<nHits && hitTable[i].track_p>0 && hitTable[i].track_p<=nTracks
     && trackTable[hitTable[i].track_p-1].start_vertex_p>0
     && trackTable[hitTable[i].track_p-1].start_vertex_p<=nVertices)
    {
      return vertexTable[trackTable[hitTable[i].track_p-1].start_vertex_p-1].ge_proc;
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: productionProcess index out of range, using 0" << endm;
      return vertexTable[0].ge_proc;
    }
}

Float_t StFtpcGeantReader::vertexX(Int_t i)
{
  if(i>=0 && i<nHits && hitTable[i].track_p>0 && hitTable[i].track_p<=nTracks
     && trackTable[hitTable[i].track_p-1].start_vertex_p>0
     && trackTable[hitTable[i].track_p-1].start_vertex_p<=nVertices)
    {
      return vertexTable[trackTable[hitTable[i].track_p-1].start_vertex_p-1].ge_x[0];
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: vertexX index out of range, using 0" << endm;
      return vertexTable[0].ge_x[0];
    }
}

Float_t StFtpcGeantReader::vertexY(Int_t i)
{
  if(i>=0 && i<nHits && hitTable[i].track_p>0 && hitTable[i].track_p<=nTracks
     && trackTable[hitTable[i].track_p-1].start_vertex_p>0
     && trackTable[hitTable[i].track_p-1].start_vertex_p<=nVertices)
    {
      return vertexTable[trackTable[hitTable[i].track_p-1].start_vertex_p-1].ge_x[1];
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: vertexY index out of range, using 0" << endm;
      return vertexTable[0].ge_x[1];
    }
}

Float_t StFtpcGeantReader::vertexZ(Int_t i)
{
  if(i>=0 && i<nHits && hitTable[i].track_p>0 && hitTable[i].track_p<=nTracks
     && trackTable[hitTable[i].track_p-1].start_vertex_p>0
     && trackTable[hitTable[i].track_p-1].start_vertex_p<=nVertices)
    {
      return vertexTable[trackTable[hitTable[i].track_p-1].start_vertex_p-1].ge_x[2];
    }
  else
    {
      LOG_WARN << "StFtpcGeantReader: vertexZ index out of range, using 0" << endm;
      return vertexTable[0].ge_x[2];
    }
}


