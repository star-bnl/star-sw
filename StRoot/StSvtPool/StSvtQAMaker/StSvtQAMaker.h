/***************************************************************************
 *
 * $Id: StSvtQAMaker.h,v 1.1 2004/02/06 02:30:36 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT QA Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtQAMaker.h,v $
 * Revision 1.1  2004/02/06 02:30:36  munhoz
 * inserting SVT online monitor
 *
 *
 **************************************************************************/

#ifndef STSVTQAMAKER_H
#define STSVTQAMAKER_H

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StChain;

class StSvtData;
class StSvtHybridCollection;
class StSvtMonitor;

class StSvtQAMaker : public StMaker {
 private:
  char                   *mDataType;       //!

  StSvtData              *mSvtData;        //!
  StSvtData              *mSvtRawData;     //!
  StSvtHybridCollection  *mSvtPed;         //!
  StSvtHybridCollection  *mSvtRMSPed;      //!
  StSvtMonitor           *mMonitor;        //!

 protected:

 public: 
  StSvtQAMaker(const char *name="SvtQA", StSvtMonitor* monitor=0, char* data="RAW");
  virtual       ~StSvtQAMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual void  Clear(const char *opt);
  virtual void   PrintInfo();

  void   SetDataType(char* data){mDataType = data;}
  void   SetMonitor(StSvtMonitor* monitor){mMonitor = monitor;}
  virtual Int_t  SetSvtData();
  virtual Int_t  SetSvtAdjData();
  virtual Int_t  SetSvtRawData();
  virtual Int_t  SetData();
  virtual Int_t  SetAdjData();
  virtual Int_t  SetRawData();
  virtual Int_t  SetPedestal();
  virtual Int_t  SetPedestal2ndOrd();
  virtual Int_t  SetRMSPedestal();
  virtual Int_t  FillHistograms();
  virtual Int_t  Reset();

  ClassDef(StSvtQAMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


