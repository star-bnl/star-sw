/***************************************************************************
 *
 * $Id: StSvtDaqMaker.h,v 1.11 2014/08/06 11:43:45 jeromel Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Library Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtDaqMaker.h,v $
 * Revision 1.11  2014/08/06 11:43:45  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.10  2004/01/30 00:14:42  munhoz
 * few corrections to pedestal reading and adding getDaqReader method
 *
 * Revision 1.9  2003/09/10 19:47:35  perev
 * ansi corrs
 *
 * Revision 1.8  2003/02/19 11:03:32  munhoz
 * adding getCVS line
 *
 * Revision 1.7  2002/02/15 02:39:28  munhoz
 * switching from .const to .data
 *
 * Revision 1.6  2001/10/24 16:49:43  munhoz
 * adding capability to retrieve t0 and first SCA
 *
 * Revision 1.5  2001/08/22 14:22:17  caines
 * Default to config to FULL and ZS
 *
 * Revision 1.4  2001/07/11 23:29:47  munhoz
 * adding capability for zero suppressed and pedestal reading
 *
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
class StSvtDaqPed;
class StSvtDaqData;
class StDAQReader;
class StSVTReader;
class TObjectSet;

class StSvtDaqMaker : public StMaker {

 private:
  char             *fConfig;   //!           
  char             *fDataType; //!
           
  StSvtHybridDaqData     *fData;     //!
  StSvtDaqData           *fSvtData;  //!
  StSvtDaqPed            *fSvtPed;   //!
  StSvtDaqPed            *fSvtRMSPed;   //!

  TObjectSet     *fSvtSet;    //! pointer to StSvtEvent DataSet
  TObjectSet     *fHybridSet; //! pointer to StSvtEvent DataSet
  TObjectSet     *fPedSet;    //! 
  TObjectSet     *fRMSPedSet;    //! 

  StDAQReader *daqReader;   //!
  StSVTReader *svtReader;   //!

 protected:

 public: 
  StSvtDaqMaker(const char *name="SvtDaq", char* config="FULL", char* data="ZS");
  virtual       ~StSvtDaqMaker();
  void   SetConfiguration(char* config){fConfig = config;}
  void   SetDataType(char* data){fDataType = data;}
  Int_t  SetSvtData();
  Int_t  SetSvtPed();
  Int_t  SetSvtRMSPed();
  Int_t  SetHybridData();
  Int_t  GetDaqReader();
  Int_t  GetSvtData();
  Int_t  GetSvtPed();
  Int_t  GetSvtRMSPed();
  Int_t  GetHybridData(int barrel, int ladder, int wafer, int hybrid);
  Int_t  GetUnixTime();
  void   PrintInfo();
  void   PrintEventInfo();
  Int_t  Reset();
  void   UpdateReader();
  Int_t  InitEvp(const char* option){return kStErr;}
  Int_t  GetSvtEvpData(){return kStErr;}

  virtual Int_t  Init();
  //  virtual Int_t  InitRun(int runnumber);
  virtual Int_t  Make();
  virtual void   Clear(const char *opt);
  virtual Int_t  Finish();

 virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StSvtDaqMaker.h,v 1.11 2014/08/06 11:43:45 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

  ClassDef(StSvtDaqMaker,0)   //StAF chain virtual base class for Makers
};

#endif


