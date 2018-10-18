#ifndef StTpcHitMaker_H
#define StTpcHitMaker_H

/***************************************************************************
 *
 * $Id: StTpcHitMaker.h,v 1.28 2018/10/17 20:45:27 fisyak Exp $
 * StTpcHitMaker - class to fill the StEvent with TPC clusters from DAQ reader
 * $Log: StTpcHitMaker.h,v $
 * Revision 1.28  2018/10/17 20:45:27  fisyak
 * Restore update for Run XVIII dE/dx calibration removed by Gene on 08/07/2018
 *
 * Revision 1.26  2018/06/22 18:35:19  perev
 * Merging with TPC group code
 *
 * Revision 1.19  2014/08/06 11:43:50  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.18  2014/06/26 21:31:42  fisyak
 * New Tpc Alignment, v632
 *
 * Revision 1.16  2013/04/07 21:58:36  fisyak
 * Move selection of sector range from InitRun to Init, add cluster averaging based on TH3
 *
 * Revision 1.15  2012/09/13 21:00:04  fisyak
 * Corrections for iTpx, clean up
 *
 * Revision 1.14  2012/05/07 15:51:01  fisyak
 * Remove hard coded TPC numbers
 *
 * Revision 1.13  2011/06/09 20:52:08  genevb
 * Set sanity flag
 *
 * Revision 1.12  2011/03/08 18:20:44  genevb
 * Limit on number of hits starting at time bin 0
 *
 * Revision 1.11  2010/08/30 18:02:02  genevb
 * Introduce hit maxima for tracking
 *
 * Revision 1.10  2010/03/25 15:05:54  fisyak
 * Add AfterBurner
 *
 * Revision 1.9  2010/02/19 23:36:08  fisyak
 * Add hit Id
 *
 * Revision 1.8  2009/03/16 13:41:45  fisyak
 * Switch to new scheme (avoid legacy) for TPX cluster reading
 *
 * Revision 1.7  2009/03/11 18:38:20  fisyak
 * Add 22 time bins to account subtracted by Tonko, clean up
 *
 * Revision 1.6  2008/12/29 23:58:07  fine
 * Optimize the DAQ data access
 *
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
#include "TString.h"
#include "StThreeVectorF.hh"
//#define __USE__THnSparse__
#include "TH1.h"
#ifdef __USE__THnSparse__
#include "THnSparse.h"
#else /* ! __USE__THnSparse__ */
#include "TH3.h"
#endif /* __USE__THnSparse__ */
class StTpcDigitalSector;
class StTpcHit;
class tpc_cl;
class daq_cld;
class tpc_t;
class StTpcHitCollection;
class StTpcHitMaker : public StRTSBaseMaker {
 public:
  enum EReaderType {kUnknown, kLegacyTpc, kLegacyTpx, kStandardTpx, kStandardiTPC};
  enum EMode {kUndefined, 
	      kTpc, kTpx, kiTPC,
	      kTpcPulser, kTpxPulser, kiTPCPulser, 
	      kTpcDumpPxls2Nt, kTpxDumpPxls2Nt, 
	      kTpcRaw, kTpxRaw, kiTPCRaw,
	      kTpcAvLaser, kTpxAvLaser,      // averaging on pixel level
	      kAll};
  StTpcHitMaker(const char *name="tpc_hits");
  virtual ~StTpcHitMaker() {}

  Int_t   Init();
  Int_t   InitRun(Int_t runnumber);
  Int_t   Make();
  void    DoPulser(Int_t sector);
  void    TpxAvLaser(Int_t sector);
  void    TpcAvLaser(Int_t sector);
  void    PadMonitor(Int_t sector);
  Int_t   UpdateHitCollection(Int_t sector);
  void    DumpPixels2Ntuple(Int_t sector);
  void    PrintSpecial(Int_t sector);
  Int_t   RawTpcData(Int_t sector);
  Int_t   RawTpxData(Int_t sector);
  void    InitializeHistograms(Int_t token);
#ifdef __USE__THnSparse__
  THnSparseF *CompressTHn(THnSparseF *hist, Double_t compress = 1e4);
#endif /* __USE__THnSparse__ */
  StTpcDigitalSector *GetDigitalSector(Int_t sector);
  virtual Int_t        Finish();
 private:

  EMode   kMode;
  EReaderType kReaderType;
  TString mQuery;
  tpc_t   *fTpc;
  Short_t  ADCs[512];
  UShort_t IDTs[512];
  UShort_t fId; // current cluster Id
  Int_t    maxHits[24];
  Int_t    maxBin0Hits;
  Int_t    bin0Hits;
#ifdef __USE__THnSparse__
  THnSparseF **fAvLaser;
#else /* ! __USE__THnSparse__ */
  TH3F       **fAvLaser;
#endif /* __USE__THnSparse__ */
  TH1F        *fSectCounts;
  Int_t        RowNumber();
 protected:
  StTpcHit *CreateTpcHit(const tpc_cl &cluster, Int_t sector, Int_t row);
  StTpcHit *CreateTpcHit(const daq_cld  &cluster, Int_t sector, Int_t row);
    
 public:
  static void AfterBurner(StTpcHitCollection *hitCollection);
  static StTpcHit* StTpcHitFlag(const StThreeVectorF& p,
             const StThreeVectorF& e,
             UInt_t hw, float q, UChar_t c,
             UShort_t idTruth, UShort_t quality,
             UShort_t id,
             UShort_t mnpad, UShort_t mxpad, UShort_t mntmbk,
             UShort_t mxtmbk, Float_t cl_x, Float_t cl_t, UShort_t adc,
             UShort_t flag);
  static Float_t fgDp;             // hardcoded errors
  static Float_t fgDt;
  static Float_t fgDperp;

  // cvs
  virtual const char *GetCVS() const    {
    static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
  }
  ClassDef(StTpcHitMaker, 1)    //StTpcHitMaker - class to fille the StEvewnt from DAQ reader
};

#endif
