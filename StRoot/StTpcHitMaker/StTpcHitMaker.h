#ifndef StTpcHitMaker_H
#define StTpcHitMaker_H

/***************************************************************************
 *
 * $Id: StTpcHitMaker.h,v 1.1.1.1 2008/05/27 14:22:41 fisyak Exp $
 * StTpcHitMaker - class to fill the StEvent with TPC clusters from DAQ reader
 * $Log: StTpcHitMaker.h,v $
 * Revision 1.1.1.1  2008/05/27 14:22:41  fisyak
 * Maker to access TPC DAQ information via EVP_READER
 *
 * Revision 1.3  2008/05/27 14:18:18  fisyak
 * Freeze before moving to STAR repository
 *
 * Revision 1.2  2008/04/28 14:37:15  fisyak
 * Rearrage TpcHitMaker to make it run for parallel taks, add the first version of online clustering
 *
 * Revision 1.1.1.1  2008/04/03 20:16:39  fisyak
 * Initial version
 *
 *--------------------------------------------------------------------------
 *
 ***************************************************************************/

#include "StRTSBaseMaker.h"

class StEvent;
class StTpcCoordinateTransform;
class StTpcHitCollection;
class StTpcHit;
class StDaqTpcClusterInterface;
class StTpcDigitalSector;

class evpReader; 
class rts_reader;

struct tpc_cl;
class StTpcHitCollection;

class StTpcHitMaker : public StRTSBaseMaker {
 public:
  enum EMode {kUndefined, kTpx, kTpxPulser, kTpxPadMonitor, kTpxDumpPxls2Nt, kTpxRaw, kAll};
  StTpcHitMaker(const char *name="tpc_hits") : StRTSBaseMaker(name), mStEvent(0), kMode(kUndefined) {}
  virtual ~StTpcHitMaker() {}

  Int_t   Init();
  Int_t   Make();
  void    DoPulser(Int_t sector);
  void    PadMonitor(Int_t sector);
  void    UpdateHitCollection(Int_t sector);
  void    DumpPixels2Ntuple(Int_t sector);
  void    PrintSpecial(Int_t sector);
  void    RawData(Int_t sector);
  StTpcDigitalSector *GetDigitalSector(Int_t sector);
 private:
  static evpReader  *fDaqReader;
  static rts_reader *fRtsReader;

  StEvent *mStEvent;
  EMode   kMode;
 protected:
    evpReader *InitReader();
    Int_t MakeSector(Int_t sector);
    StTpcHitCollection *GetHitCollection();
    static StTpcHit *StTpcHitMaker::CreateTpcHit(const StDaqTpcClusterInterface &cluster, Int_t sector, Int_t row);
    
 public:

  static Float_t fgDp;             // hardcoded errors
  static Float_t fgDt;
  static Float_t fgDperp;

 public:

  // cvs
  virtual const char *GetCVS() const    {
    static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
  }
  ClassDef(StTpcHitMaker, 1)    //StTpcHitMaker - class to fille the StEvewnt from DAQ reader
};

#endif
