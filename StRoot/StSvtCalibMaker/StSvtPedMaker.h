/***************************************************************************
 *
 * $Id: StSvtPedMaker.h,v 1.1 2000/08/23 13:08:12 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Pedestal calculation Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtPedMaker.h,v $
 * Revision 1.1  2000/08/23 13:08:12  munhoz
 * SVT pedestal calculation
 *
 *
 **************************************************************************/

#ifndef STSVTPEDMAKER_H
#define STSVTPEDMAKER_H

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StSvtClassLibrary/StSvtEnumerations.hh"

class TObjectSet;
class StSvtHybridStat;
class StSvtHybridStat2;
class StSvtHybridPed;
class StSvtHybridPixels2;
class StSvtHybridCollection;
class StSvtHybridData;
class StSvtData;

class StSvtPedMaker : public StMaker {
 private:
  StSvtHybridStat        *fStat;     //!
  StSvtHybridStat2       *fStat2;    //!
  StSvtHybridPed         *fPed;      //!
  StSvtHybridPixels2     *fPed2;     //!
  StSvtHybridData        *fData;     //!
  StSvtHybridCollection  *fSvtStat;  //!
  StSvtHybridCollection  *fSvtStat2;  //!
  StSvtHybridCollection  *fSvtPed;   //!
  StSvtHybridCollection  *fSvtPed2;   //!
  StSvtData              *fSvtData;  //!

  TObjectSet           *fPedSet;   //! 
  TObjectSet           *fPedSet2;   //! 

  pedestalType fType;  //!                 

 protected:
 public: 
  StSvtPedMaker(const char *name="SvtPed", pedestalType type = kTime);
  virtual       ~StSvtPedMaker();
  virtual Int_t  Init();
  Int_t  SetType(pedestalType option);
  pedestalType  GetType(){return fType;}
  virtual Int_t  SetSvtData();
  virtual Int_t  SetSvtPed();
  virtual Int_t  SetSvtPed2ndOrd();
  virtual Int_t  AddStat();
  virtual Int_t  AddStat2ndOrd();
  virtual Int_t  Make();
  virtual Int_t  CalcPed();
  virtual Int_t  CalcPed2ndOrd();
  virtual Int_t  WriteToFile(char* fileName = "svtPedestal.root", char* option = "NEW");
  virtual Int_t  ReadFromFile(char* fileName = "svtPedestal.root");
  virtual Int_t  ResetStat();
  virtual Int_t  ResetPed();
  virtual Int_t  Finish();
  virtual void   PrintInfo();
  ClassDef(StSvtPedMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


