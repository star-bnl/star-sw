// $Id: StFtpcGeantPoint.hh,v 1.2 2001/01/25 15:25:52 oldi Exp $
// $Log: StFtpcGeantPoint.hh,v $
// Revision 1.2  2001/01/25 15:25:52  oldi
// Fix of several bugs which caused memory leaks:
//  - Some arrays were not allocated and/or deleted properly.
//  - TClonesArray seems to have a problem (it could be that I used it in a
//    wrong way in StFtpcTrackMaker form where Holm cut and pasted it).
//    I changed all occurences to TObjArray which makes the program slightly
//    slower but much more save (in terms of memory usage).
//
// Revision 1.1  2000/11/24 15:02:34  hummler
// commit changes omitted in last commit
//

///////////////////////////////////////////////////////////////////////////////////
//                                                                               //
// StFtpcGeantPoint class - one FTPC cluster for the FTPC evaluation.            //
//                                                                               //
///////////////////////////////////////////////////////////////////////////////////

#ifndef STAR_StFtpcGeantPoint
#define STAR_StFtpcGeantPoint

#include "TObject.h"

#include "tables/St_ffs_gepoint_Table.h"

class StFtpcGeantPoint : public TObject {
  
private:

  Int_t mTrackPointer;
  Int_t mGeantPID;
  Int_t mPrimaryTag;
  Int_t mGeantProcess;

  Double_t  mVertexMomentum[3];         // vector of geant track vertex momentum
  Double_t  mLocalMomentum[3];          // vector of geant track local momentum
  Double_t  mVertexPosition[3];         // vector of geant vertex position

public:
  
                 StFtpcGeantPoint();                  // default constructor
  virtual       ~StFtpcGeantPoint();                  // destructor
  virtual Int_t  ToTable(ffs_gepoint_st *geant_st);   // writes cluster to STAF table
  
  // getter
  Int_t    GetTrackPointer() {return mTrackPointer;}
  Int_t    GetGeantPID() {return mGeantPID;}
  Int_t    GetPrimaryTag() {return mPrimaryTag;}
  Int_t    GetGeantProcess() {return mGeantProcess;}

  Double_t GetVertexMomentum(Int_t i)  { return mVertexMomentum[i];}
  Double_t GetLocalMomentum(Int_t i)  { return mLocalMomentum[i];}
  Double_t GetVertexPosition(Int_t i)  { return mVertexPosition[i];}

  // setter
  void SetTrackPointer(Int_t d) {mTrackPointer =d;}
  void SetGeantPID(Int_t d) {mGeantPID =d;}
  void SetPrimaryTag(Int_t d) {mPrimaryTag =d;}
  void SetGeantProcess(Int_t d) {mGeantProcess =d;}
  
  void SetVertexMomentum(Int_t i, Double_t f) {if(i>=0&&i<3) mVertexMomentum[i]=f;}
  void SetLocalMomentum(Int_t i, Double_t f) {if(i>=0&&i<3) mLocalMomentum[i]=f;}
  void SetVertexPosition(Int_t i, Double_t f) {if(i>=0&&i<3) mVertexPosition[i]=f;}

  void SetVertexMomentum(Double_t f1,Double_t f2,Double_t f3) 
  {mVertexMomentum[0]=f1;mVertexMomentum[1]=f2;mVertexMomentum[2]=f3;}
  void SetLocalMomentum(Double_t f1,Double_t f2,Double_t f3) 
  {mLocalMomentum[0]=f1;mLocalMomentum[1]=f2;mLocalMomentum[2]=f3;}
  void SetVertexPosition(Double_t f1,Double_t f2,Double_t f3) 
  {mVertexPosition[0]=f1;mVertexPosition[1]=f2;mVertexPosition[2]=f3;}

  ClassDef(StFtpcGeantPoint, 1)   //Ftpc point class
};

#endif
