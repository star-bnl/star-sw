/***************************************************************************
 *
 * $Id: StSvtDaqMaker.h,v 1.2 2000/08/04 21:03:51 perev Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Library Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtDaqMaker.h,v $
 * Revision 1.2  2000/08/04 21:03:51  perev
 * Leaks + Clear() cleanup
 *
 * Revision 1.1  2000/06/13 20:42:05  caines
 * StRoot/StSvtDaqMaker
 *
 *
 **************************************************************************/

#ifndef STSVTDAQMAKER_H
#define STSVTDAQMAKER_H

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StSvtHybridDaqData;
class StSvtDaqData;
class StDAQReader;
class StSVTReader;
class TObjectSet;

class StSvtDaqMaker : public StMaker {

 private:
  char             *fConfig;   //!           
  char             *fDataType; //!
           
  StSvtHybridDaqData  *fData;     //!
  StSvtDaqData        *fSvtData;  //!

  TObjectSet     *fSvtSet;    //! pointer to StSvtEvent DataSet
  TObjectSet     *fHybridSet; //! pointer to StSvtEvent DataSet

  StDAQReader *daqReader;   //!
  StSVTReader *svtReader;   //!

 protected:

 public: 
  StSvtDaqMaker(const char *name="SvtDaq", char* config="Y1L", char* data="RAW");
  virtual       ~StSvtDaqMaker();
  void   SetConfiguration(char* config){fConfig = config;}
  Int_t  SetSvtData();
  Int_t  SetHybridData();
  Int_t  GetSvtData();
  Int_t  GetHybridData(int barrel, int ladder, int wafer, int hybrid);
  void   PrintInfo();
  void   PrintEventInfo();
  Int_t  Reset();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual void   Clear(const char *opt);
  virtual Int_t  Finish();
  ClassDef(StSvtDaqMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


