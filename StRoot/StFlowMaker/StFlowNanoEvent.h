//////////////////////////////////////////////////////////////////////////
// 
// $Id: StFlowNanoEvent.h,v 1.4 2000/05/12 22:42:04 snelling Exp $
//
// Author: Sergei Voloshin and Raimond Snellings, March 2000
//
// Description:  A persistent Flow nano DST
// 
//////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowNanoEvent.h,v $
// Revision 1.4  2000/05/12 22:42:04  snelling
// Additions for persistency and minor fix
//
// Revision 1.3  2000/05/11 20:00:37  posk
// Preparation for micro and nano DSTs.
//
// Revision 1.2  2000/03/08 15:10:50  posk
// Added $Id: StFlowNanoEvent.h,v 1.4 2000/05/12 22:42:04 snelling Exp $ and $Log: StFlowNanoEvent.h,v $
// Added $Id: StFlowNanoEvent.h,v 1.3 2000/05/11 20:00:37 posk Exp $ and Revision 1.4  2000/05/12 22:42:04  snelling
// Added $Id: StFlowNanoEvent.h,v 1.3 2000/05/11 20:00:37 posk Exp $ and Additions for persistency and minor fix
// Added $Id: StFlowNanoEvent.h,v 1.3 2000/05/11 20:00:37 posk Exp $ and
// Added $Id: StFlowNanoEvent.h,v 1.4 2000/05/12 22:42:04 snelling Exp $ and Revision 1.3  2000/05/11 20:00:37  posk
// Added $Id: StFlowNanoEvent.h,v 1.4 2000/05/12 22:42:04 snelling Exp $ and Preparation for micro and nano DSTs.
// Added $Id: StFlowNanoEvent.h,v 1.4 2000/05/12 22:42:04 snelling Exp $ and.
//
//
//////////////////////////////////////////////////////////////////////////
#ifndef StFlowNanoEvent__h
#define StFlowNanoEvent__h

#include <iostream.h>
#include "TObject.h"
#include "TClonesArray.h"


class StFlowNanoEventHeader {
  
 private:
  Int_t   fEvtNum;
  Int_t   fRun;
  Int_t   fDate;
  
 public:
  StFlowNanoEventHeader() : fEvtNum(0), fRun(0), fDate(0) { }
  virtual ~StFlowNanoEventHeader() { }
  void    Set(Int_t i, Int_t r, Int_t d) { fEvtNum = i; fRun = r; fDate = d; }
  Int_t   GetEvtNum() const { return fEvtNum; }
  Int_t   GetRun() const { return fRun; }
  Int_t   GetDate() const { return fDate; }
  
  ClassDef(StFlowNanoEventHeader,1)  //Event Header
};
    
    
class StFlowNanoEvent : public TObject {
      
 private:
  Int_t                  fNtrack;
  StFlowNanoEventHeader  fEvtHdr;
  TClonesArray           *fTracks;
  static TClonesArray    *fgTracks;

 public:
  StFlowNanoEvent();
  virtual       ~StFlowNanoEvent();
  void          Clear(Option_t *option ="");
  static void   Reset(Option_t *option ="");
  void          SetNtrack(Int_t n) { fNtrack = n; }
  void          SetHeader(Int_t i, Int_t run, Int_t date);
  void          AddTrack(Float_t pt, Float_t phi, Float_t eta );
  
  Int_t         GetNtrack() const { return fNtrack; }
  StFlowNanoEventHeader  *GetHeader() { return &fEvtHdr; }
  TClonesArray *GetTracks() const { return fTracks; }

  ClassDef(StFlowNanoEvent,1)  //Event structure
};


class StFlowNanoTrack : public TObject {

 private:
  Float_t  fPt;         
  Float_t  fPhi;         
  Float_t  fEta;         
  
 public:
  StFlowNanoTrack() { }
  StFlowNanoTrack(Float_t pt, Float_t phi, Float_t eta);
  virtual  ~StFlowNanoTrack() { }
  Float_t  GetPt() const { return fPt; }
  Float_t  GetPhi() const { return fPhi; }
  Float_t  GetEta() const { return fEta; }
  
  ClassDef(StFlowNanoTrack,1)  //A track segment
};

#endif
