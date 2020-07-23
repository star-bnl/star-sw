#ifndef __TrackMatch__
#define __TrackMatch__
#include "TObject.h"
class TrackParameters : public TObject {
 public:
  TrackParameters() {}
  virtual ~TrackParameters() {}
  void set() {memset(&begin, 0, &end-&begin);}
  Bool_t IsEmpty()    {return MatchStatus == 0;}
  Bool_t IsMatch()    {return MatchStatus & 1;}
  Bool_t IsClone()    {return MatchStatus & 2;}
  Bool_t IsSplitted() {return MatchStatus & 4;}
  Bool_t IsLost()     {return MatchStatus & 8;}
  Int_t  charge()     {return (Charge > 0) ? 0 : 1;}
  Char_t begin;
  Int_t   Id;
  Float_t PtGl;
  Float_t EtaGl;
  Float_t PhiGl;
  Float_t PGl;
  Float_t FitPtsGl;
  Float_t PtPr;
  Float_t EtaPr;
  Float_t PhiPr;
  Float_t PPr;
  Float_t FitPtsPr;
  Float_t Dedx;
  Float_t Charge;
  Float_t Prim;
  Float_t Chi2Gl0;
  Float_t Chi2Gl1;
  Float_t Chi2Pr0;
  Float_t Chi2Pr1;
  Float_t FirstPointX;
  Float_t FirstPointY;
  Float_t FirstPointZ;
  Float_t LastPointX;
  Float_t LastPointY;
  Float_t LastPointZ;
  Float_t PrimX;
  Float_t PrimY;
  Float_t PrimZ;
/*   Float_t SecF;  // first and last hit sector */
/*   Float_t SecL; */
  Int_t   hitMap; // HFT hits pxl + 10 * ist + 100 *Ssd
  Int_t   MatchStatus; // 1 - ReCo (1 to 1 match), 2 - CLone, 4 - Splitted, 8 - Lost 
  Char_t end;
  ClassDef(TrackParameters,3)
};
class TrackMatch : public TObject {
 public:
 TrackMatch() {}
 virtual ~TrackMatch() {}
  TrackParameters newP;
  TrackParameters oldP;
  ClassDef(TrackMatch,3)
};
#endif
