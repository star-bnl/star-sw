// -*- mode: c++;-*-
// $Id: TrackList.h,v 1.7 2008/07/18 04:11:59 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TRACKLIST_H
#define TRACKLIST_H

#include <ostream>
#include <vector>

namespace StSpinJet {

struct Track {
  int            runNumber;
  int            eventId;
  int            detectorId; // 1: TPC
  double         pt;
  double         eta;
  double         phi;
  short          flag;
  unsigned short nHits;
  short          charge;
  unsigned short nHitsPoss;
  unsigned short nHitsDedx;
  unsigned short nHitsFit;
  double         nSigmaPion;
  double         Tdca;
  float          dcaZ;
  float          dcaD;
  double         BField;
  double         bemcRadius;
  double         vertexZ;
  int            exitDetectorId; // 9: BEMC, 13: EEMC
  int            exitTowerId;
  double         exitEta;
  double         exitPhi;
  double         dEdx;
  int            trackIndex;
  short          id;
};

typedef std::vector<Track> TrackList;

inline bool operator==(const Track& v1, const Track& v2)
{
  if(v1.runNumber      != v2.runNumber)      return false;
  if(v1.eventId        != v2.eventId)        return false;   
  if(v1.detectorId     != v2.detectorId)     return false;   
  if(v1.pt             != v2.pt)             return false;  
  if(v1.eta            != v2.eta)            return false;   
  if(v1.phi            != v2.phi)            return false;   
  if(v1.flag           != v2.flag)           return false;   
  if(v1.nHits          != v2.nHits)          return false;   
  if(v1.charge         != v2.charge)         return false;   
  if(v1.nHitsPoss      != v2.nHitsPoss)      return false;   
  if(v1.nHitsDedx      != v2.nHitsDedx)      return false;   
  if(v1.nHitsFit       != v2.nHitsFit)       return false;   
  if(v1.nSigmaPion     != v2.nSigmaPion)     return false;   
  if(v1.Tdca           != v2.Tdca)           return false;   
  if(v1.dcaZ           != v2.dcaZ)           return false;   
  if(v1.dcaD           != v2.dcaD)           return false;   
  if(v1.BField         != v2.BField)         return false;   
  if(v1.bemcRadius     != v2.bemcRadius)     return false;   
  if(v1.exitDetectorId != v2.exitDetectorId) return false;   
  if(v1.exitTowerId    != v2.exitTowerId)    return false;   
  if(v1.exitEta        != v2.exitEta)        return false;   
  if(v1.exitPhi        != v2.exitPhi)        return false;   
  if(v1.dEdx           != v2.dEdx)           return false;   
  if(v1.trackIndex     != v2.trackIndex)     return false;   
  if(v1.id             != v2.id)             return false;   
  return true;
  }

inline bool operator!=(const Track& v1, const Track& v2)
{
  return(!(v1 == v2));
}

inline bool operator==(const TrackList& v1, const TrackList& v2){
  if(v1.size() != v2.size()) return false;
  for(size_t i = 0; i < v1.size(); ++i) if(v1[i] != v2[i]) return false;
  return true;
}


inline std::ostream& operator<<(std::ostream& out, const Track& v)
{
  out << "trackId: " << v.id << ", pt: " << v.pt << ", .... ";
  return out;
}

inline std::ostream& operator<<(std::ostream& out, const TrackList& v)
{
  out << "TrackList size: " << v.size();
  return out;
}

}

#endif // TRACKLIST_H
