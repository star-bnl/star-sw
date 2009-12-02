// $Id: CtbMatching.h,v 1.2 2009/12/02 16:35:58 fine Exp $

#ifndef MUDST_CTBMATCHING_H
#define MUDST_CTBMATCHING_H
#include <vector>
using std::vector;

class StMuEvent;
class StMuTrack;
struct ctbHit{
  double phi;
  double eta;
  float adc;
};

class CtbMatching {
  int aa;
  vector<ctbHit> *ctbHits;
  float etaToll;
  float phiToll;  

public: 
  CtbMatching();
  void loadHits(StMuEvent* muEve);
  void ctb_get_slat_from_data(int slat, int tray, double & ctbphi, double & ctbeta);
  unsigned int match(const StMuTrack* rTrack);
  };

#endif


// $Log: CtbMatching.h,v $
// Revision 1.2  2009/12/02 16:35:58  fine
// Fix StMuTrack interface
//
// Revision 1.1  2003/10/20 17:04:39  balewski
// LCP analysis code
//
// Revision 1.1  2003/09/16 19:18:36  balewski
// matching  muDst tracks to CTB
//
