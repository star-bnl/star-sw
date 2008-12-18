#ifndef StTpcHitMaker_H
#define StTpcHitMaker_H

/***************************************************************************
 *
 * $Id: StTpcHitMaker.h,v 1.5 2008/12/18 20:20:26 fine Exp $
 * StTpcHitMaker - class to fill the StEvent with TPC clusters from DAQ reader
 * $Log: StTpcHitMaker.h,v $
 * Revision 1.5  2008/12/18 20:20:26  fine
 * access two different detectors tpx/tpc
 *
 * Revision 1.4  2008/12/15 21:04:01  fine
 * For for the NEW_DAQ_READER
 *
 * Revision 1.3  2008/07/31 20:45:27  fisyak
 * Add TpcMixer
 *
 * Revision 1.2  2008/06/23 20:13:53  fisyak
 * Add real data pixel annotation
 *
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

#ifndef NEW_DAQ_READER
class evpReader; 
class rts_reader;

struct tpc_cl;
#else /* NEW_DAQ_READER */
struct tpc_t;
#endif /* NEW_DAQ_READER */
class StTpcHitCollection;

class StTpcHitMaker : public StRTSBaseMaker {
 public:
  enum EMode {kUndefined, kTpx, kTpxPulser, kTpxPadMonitor, kTpxDumpPxls2Nt, kTpxRaw, kAll};
#ifndef NEW_DAQ_READER
  StTpcHitMaker(const char *name="tpc_hits") : StRTSBaseMaker(name), mStEvent(0), kMode(kUndefined) {}
#else /* NEW_DAQ_READER */
  StTpcHitMaker(const char *name="tpc_hits") : StRTSBaseMaker("tpc",name), mStEvent(0), kMode(kUndefined)
        ,fTpc(0) {}
#endif /* NEW_DAQ_READER */
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
#ifndef NEW_DAQ_READER
  static evpReader  *fDaqReader;
  static rts_reader *fRtsReader;
#endif /* ! NEW_DAQ_READER */

  StEvent *mStEvent;
  EMode   kMode;
#ifdef NEW_DAQ_READER
  tpc_t   *fTpc;
#endif /* NEW_DAQ_READER */
 protected:
#ifndef NEW_DAQ_READER
    evpReader *InitReader();
#endif /* ! NEW_DAQ_READER */
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
