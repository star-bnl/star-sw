#ifndef StHeader_H
#define StHeader_H

#include "TObject.h"
 
class StHeader : public TObject {
protected:
  Int_t         fRun;         //Run number
  Int_t         fNvertex;     //Number of vertices
  Int_t         fNprimary;    //Number of primary tracks
  Int_t         fNtrack;      //Number of tracks
  Int_t         fEvent;       //Event number

public:
  StHeader();
  StHeader(Int_t run, Int_t event);
  ~StHeader() {;}

  virtual void Reset(Int_t run, Int_t event);

  virtual  void  SetRun(Int_t run) {fRun = run;}
  virtual  Int_t GetRun() {return fRun;}
  
  virtual  void  SetNprimary(Int_t nprimary) {fNprimary = nprimary;}
  virtual  Int_t GetNprimary() {return fNprimary;}
  
  virtual  void  SetNvertex(Int_t vertex) {fNvertex = vertex;}
  virtual  Int_t GetNvertex() {return fNvertex;}
  
  virtual  void  SetNtrack(Int_t ntrack) {fNtrack = ntrack;}
  virtual  Int_t GetNtrack() {return fNtrack;}
  
  virtual  void  SetEvent(Int_t event) {fEvent = event;}
  virtual  Int_t GetEvent() {return fEvent;}

  virtual void Dump();
  
  ClassDef(StHeader,1) //STAR event header
    
};

#endif
