// $Id: StFtpcGeantReader.cc,v 1.1 2000/09/18 14:27:42 hummler Exp $
//
// $Log: StFtpcGeantReader.cc,v $
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
      gMessMgr->Message("StFtpcGeantReader: nextTrackHit index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: track index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: geantVolume index out of range, using 0", "W", "OST");
      return hitTable[0].volume_id;
    }
}

Float_t StFtpcGeantReader::energyLoss(Int_t i)
{
  if(i>=0 && i<nHits)
    {
      return hitTable[i].de;
    }
  else
    {
      gMessMgr->Message("StFtpcGeantReader: energyLoss index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: segmentLength index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: pLocalX index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: pLocalY index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: pLocalZ index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: timeOfFlight index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: x index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: y index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: z index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: trackType index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: geantPid index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: hitsOnTrack index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: firstHitOnTrack index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: isShower index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: trackVertex index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: trackCharge index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: trackEnergy index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: trackEta index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: pVertexX index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: pVertexY index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: pVertexZ index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: pTVertex index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: pTotalVertex index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: rapidityVertex index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: productionProcess index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: vertexX index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: vertexY index out of range, using 0", "W", "OST");
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
      gMessMgr->Message("StFtpcGeantReader: vertexZ index out of range, using 0", "W", "OST");
      return vertexTable[0].ge_x[2];
    }
}


