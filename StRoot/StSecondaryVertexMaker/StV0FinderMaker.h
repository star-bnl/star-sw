/*
  \class StV0FinderMaker
  
  StV0FinderMaker finds V0 secondary vertices

*/

#ifndef StV0FinderMaker_hh
#define StV0FinderMaker_hh

#include "StMaker.h"
#include "StThreeVectorD.hh"

class St_ev0_ev0par2;
class ev0_ev0par2_st;
class StEvent;
class StV0Vertex;
class StTrack;
class StPhysicalHelixD;


class StV0FinderMaker : public StMaker {

 public:
  StV0FinderMaker(const char* name="V0FinderMaker");
  virtual ~StV0FinderMaker();

  virtual void   GetPars();
  virtual Int_t  Make();
  virtual void   Clear(Option_t *option="") { prepared = kFALSE; }
  virtual void   UseExistingV0s(Bool_t opt=kTRUE) { useExistingV0s = opt; }
  virtual void   DontZapV0s(Bool_t opt=kTRUE) { dontZapV0s = opt; }
  virtual void   UseITTFTracks(Bool_t opt=kFALSE) { useITTFTracks = opt; }
  virtual Bool_t UseV0() { return kFALSE; }
  virtual Bool_t UsingITTFTracks() {return useITTFTracks;}
  virtual void   Trim();

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StV0FinderMaker.h,v 1.1 2003/04/09 16:44:05 faivre Exp $ built "__DATE__" "__TIME__ ; return cvs;}

 protected:
  virtual Int_t Prepare();
  St_ev0_ev0par2* ev0par2;         //!
  ev0_ev0par2_st* pars;            //!
  ev0_ev0par2_st* pars2;           //!
  StEvent* event;                  //!
  StV0Vertex* v0Vertex;            //!
  double ptV0sq;
  double Bfield;
  unsigned short trks;
  unsigned short ptrks;
  unsigned short ntrks;
  Bool_t prepared;
  Bool_t useExistingV0s;
  Bool_t dontZapV0s;
  Bool_t useITTFTracks;
  int det_id_v0;

  int maxtracks;
  StTrack** trk;                   //!
  unsigned short* ntrk;            //!
  unsigned short* ptrk;            //!
  short* hits;                     //!
  short* detId;                    //!
  double* pt;                      //!
  double* ptot;                    //!
  unsigned short *trkID;           //!
  StPhysicalHelixD* heli;          //!
  StThreeVectorD mainv;

  ClassDef(StV0FinderMaker,0)

};

#endif

//_____________________________________________________________________________
// $Id: StV0FinderMaker.h,v 1.1 2003/04/09 16:44:05 faivre Exp $
// $Log: StV0FinderMaker.h,v $
// Revision 1.1  2003/04/09 16:44:05  faivre
// First version of xxx
//
//
