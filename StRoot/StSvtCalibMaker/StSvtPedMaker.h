/***************************************************************************
 *
 * $Id: StSvtPedMaker.h,v 1.5 2003/12/01 00:19:43 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Pedestal calculation Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtPedMaker.h,v $
 * Revision 1.5  2003/12/01 00:19:43  caines
 * Version of pedestal maker to go with EMbedding
 *
 * Revision 1.4  2003/09/10 19:47:34  perev
 * ansi corrs
 *
 * Revision 1.3  2001/10/24 16:47:52  munhoz
 * adding RMS methods
 *
 * Revision 1.2  2000/11/30 20:32:03  caines
 * Use MessMgr
 *
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
class StSvtHybridPixels;
class StSvtHybridPixels2;
class StSvtHybridCollection;
class StSvtHybridData;
class StSvtData;
class StSvtConfig;

class StSvtPedMaker : public StMaker {
 private:
  StSvtHybridStat        *fStat;     //!
  StSvtHybridStat2       *fStat2;    //!
  StSvtHybridPed         *fPed;      //!
  StSvtHybridPixels2     *fPed2;     //!
  StSvtHybridPixels      *fPedRms;   //!
  StSvtHybridData        *fData;     //!
  StSvtHybridCollection  *fSvtStat;  //!
  StSvtHybridCollection  *fSvtStat2;  //!
  StSvtHybridCollection  *fSvtPed;   //!
  StSvtHybridCollection  *fSvtPed2;   //!
  StSvtHybridCollection  *fSvtPedRms;//!
  StSvtData              *fSvtData;  //!

  TObjectSet           *fPedSet;   //! 
  TObjectSet           *fPedSet2;   //! 
  TObjectSet           *fPedRmsSet;     //! 

  StSvtConfig                  *mConfig; //!

  pedestalType fType;  //!                 

 protected:
 public: 
  StSvtPedMaker(const char *name="SvtPed", pedestalType type = kTime);
  virtual       ~StSvtPedMaker();
  virtual Int_t  Init();
  Int_t  SetType(pedestalType option);
  pedestalType  GetType(){return fType;}

  virtual Int_t  getConfig();
  virtual Int_t  SetSvtData();
  virtual Int_t  SetSvtPed();
  virtual Int_t  SetSvtPed2ndOrd();
  virtual Int_t  SetSvtRMSPed();
  
  virtual Int_t  AddStat();
  virtual Int_t  AddStat2ndOrd();
  virtual Int_t  Make();
  virtual Int_t  CalcPed();
  virtual Int_t  CalcPed2ndOrd();
  virtual Int_t  WriteToFile(const char* fileName = "svtPedestal.root", char* option = "NEW");
  virtual Int_t  WriteRMSToFile(const char* fileName = "svtPedestal.root", char* option = "NEW");
  virtual Int_t  ReadFromFile(const char* fileName = "svtPedestal.root");
  virtual Int_t  ReadRMSFromFile(const char* fileName = "svtPedestal.root");
  virtual Int_t  ResetStat();
  virtual Int_t  ResetPed();
  virtual Int_t  Finish();
  virtual void   PrintInfo();
  ClassDef(StSvtPedMaker,2)   //StAF chain virtual base class for Makers
};

#endif


