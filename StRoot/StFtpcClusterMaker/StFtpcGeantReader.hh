// $Id: StFtpcGeantReader.hh,v 1.2 2003/10/07 12:40:29 jcs Exp $
//
// $Log: StFtpcGeantReader.hh,v $
// Revision 1.2  2003/10/07 12:40:29  jcs
// created member functions to extract ftpc plane and sector number from the GEANT volume id
//
// Revision 1.1  2000/09/18 14:27:46  hummler
// add StFtpcGeantReader
//
//

#ifndef STAR_StFtpcGeantReader
#define STAR_StFtpcGeantReader


#include <sys/types.h>
#include "TObject.h"

#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_ftp_hit_Table.h"

class StFtpcGeantReader : public TObject 
{
  
protected:
  G2T_VERTEX_ST *vertexTable;
  G2T_TRACK_ST *trackTable;
  G2T_FTP_HIT_ST *hitTable;
  Int_t nVertices;
  Int_t nTracks;
  Int_t nHits;
  
public:
  StFtpcGeantReader(St_g2t_vertex *vertex,
		    St_g2t_track *track,
		    St_g2t_ftp_hit *hit);
  ~StFtpcGeantReader();

  Int_t numberOfVertices() {return nVertices;}
  Int_t numberOfTracks() {return nTracks;}
  Int_t numberOfHits() {return nHits;}

  Int_t nextTrackHit(Int_t i);
  Int_t track(Int_t i);
  Int_t geantVolume(Int_t i);
  Int_t geantPlane(Int_t geantVolume); // from the Geant VolumeId, the plane number {1,20} (West,East)
  Int_t geantSector(Int_t geantVolume);// from the Geant VolumeId, the sector number {1,6} (West,East)
  Float_t energyLoss(Int_t i);
  Float_t segmentLength(Int_t i);
  Float_t pLocalX(Int_t i);
  Float_t pLocalY(Int_t i);
  Float_t pLocalZ(Int_t i);
  Float_t timeOfFlight(Int_t i);
  Float_t x(Int_t i);
  Float_t y(Int_t i);
  Float_t z(Int_t i);
  Int_t trackType(Int_t i);
  Int_t trackPid(Int_t i);
  Int_t hitsOnTrack(Int_t i);
  Int_t firstHitOnTrack(Int_t i);
  Int_t isShower(Int_t i);
  Int_t trackVertex(Int_t i);
  Float_t trackCharge(Int_t i);
  Float_t trackEnergy(Int_t i);
  Float_t trackEta(Int_t i);
  Float_t pVertexX(Int_t i);
  Float_t pVertexY(Int_t i);
  Float_t pVertexZ(Int_t i);
  Float_t pTVertex(Int_t i);
  Float_t pTotalVertex(Int_t i);
  Float_t rapidityVertex(Int_t i);
  Int_t productionProcess(Int_t i);
  Float_t vertexX(Int_t i);
  Float_t vertexY(Int_t i);
  Float_t vertexZ(Int_t i);
};

#endif
