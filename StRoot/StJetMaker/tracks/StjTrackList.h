// -*- mode: c++;-*-
// $Id: StjTrackList.h,v 1.6 2016/01/06 22:00:17 gdwebb Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKLIST_H
#define STJTRACKLIST_H

#include <ostream>
#include <vector>
#include "TObject.h"
#include "TVector3.h"

class StjTrack : public TObject {
public:
  virtual ~StjTrack() {}
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
  double         nSigmaKaon;
  double         nSigmaProton;
  double         nSigmaElectron;
  double         Tdca;
  double         dcaX;
  double         dcaY;
  double         dcaZ;
  double         dcaD;
  double         chi2;
  double         chi2prob;
  double         BField;
  double         bemcRadius;
  double         vertexZ;
  int            exitDetectorId; // 9: BEMC, 13: EEMC
  int            exitTowerId;
  double         exitEta;
  double         exitPhi;
  double         dEdx;
  double         beta;
  int            trackIndex;
  short          id;
  TVector3       firstPoint;
  TVector3       lastPoint;
  ClassDef(StjTrack,4)
};

typedef std::vector<StjTrack> StjTrackList;

inline bool operator==(const StjTrack& v1, const StjTrack& v2)
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
  if(v1.dcaX           != v2.dcaX)           return false;   
  if(v1.dcaY           != v2.dcaY)           return false;   
  if(v1.dcaZ           != v2.dcaZ)           return false;   
  if(v1.dcaD           != v2.dcaD)           return false;   
  if(v1.chi2           != v2.chi2)           return false;
  if(v1.chi2prob       != v2.chi2prob)       return false;
  if(v1.BField         != v2.BField)         return false;   
  if(v1.bemcRadius     != v2.bemcRadius)     return false;   
  if(v1.exitDetectorId != v2.exitDetectorId) return false;   
  if(v1.exitTowerId    != v2.exitTowerId)    return false;   
  if(v1.exitEta        != v2.exitEta)        return false;   
  if(v1.exitPhi        != v2.exitPhi)        return false;   
  if(v1.dEdx           != v2.dEdx)           return false;   
  if(v1.beta           != v2.beta)           return false;   
  if(v1.trackIndex     != v2.trackIndex)     return false;   
  if(v1.id             != v2.id)             return false;   
  if(v1.firstPoint     != v2.firstPoint)     return false;
  if(v1.lastPoint      != v2.lastPoint)      return false;
  return true;
  }

inline bool operator!=(const StjTrack& v1, const StjTrack& v2)
{
  return(!(v1 == v2));
}

inline bool operator==(const StjTrackList& v1, const StjTrackList& v2){
  if(v1.size() != v2.size()) return false;
  for(size_t i = 0; i < v1.size(); ++i) if(v1[i] != v2[i]) return false;
  return true;
}


inline std::ostream& operator<<(std::ostream& out, const StjTrack& v)
{
  out << "trackId: " << v.id << ", pt: " << v.pt << ", .... ";
  return out;
}

inline std::ostream& operator<<(std::ostream& out, const StjTrackList& v)
{
  out << "TrackList size: " << v.size();
  return out;
}

#endif // STJTRACKLIST_H
