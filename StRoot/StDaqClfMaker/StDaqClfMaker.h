#ifndef StDaqClfMaker_H
#define StDaqClfMaker_H

/***************************************************************************
 *
 * $Id: StDaqClfMaker.h,v 1.6 2003/09/10 19:47:05 perev Exp $
 *
 *--------------------------------------------------------------------------
 *
 * $Log: StDaqClfMaker.h,v $
 * Revision 1.6  2003/09/10 19:47:05  perev
 * ansi corrs
 *
 * Revision 1.5  2002/09/05 15:48:07  jml
 * Now fill the deconvolution flag to help dedx
 *
 * Revision 1.4  2002/08/22 21:31:33  jml
 * Installed new version of fcfClass
 * This version is frozen for the 2002-2003 run and will be running on the i960s
 * (assuming no new bugs are found).  Also set best values for several
 * control flags
 *
 * Revision 1.3  2002/03/20 16:41:54  jml
 * Added pad by pad t0 corrections controlled by flags
 * 	no flag    -- full pad by pad corrections
 * 	'nopadt0'  -- no pad by pad corrections
 * 	'avgpadt0' -- correct according to clusters pad
 *
 *
 ***************************************************************************/

#include <StMaker.h>
#include "tables/St_raw_sec_m_Table.h"
#include "tables/St_raw_pad_Table.h"
#include "tables/St_raw_row_Table.h"
#include "tables/St_raw_seq_Table.h"
#include "tables/St_tcl_tphit_Table.h"
#include "tables/St_type_shortdata_Table.h"
#include <St_DataSet.h>
#include "fcfClass.hh"

class St_tss_tsspar;
class StEvent;
class StTpcCoordinateTransform;
class StTpcHitCollection;

#define MAX_PADS_EVER 256
#define MAX_TIMEBINS_EVER 512
#define MAX_CLUSTERS 6000

#ifndef MAX_PADROWS
#define MAX_PADROWS 45
#endif
#ifndef MAX_PADS
#define MAX_PADS 184
#endif
#ifndef MAX_SEQ
#define MAX_SEQ 32
#endif


// In the tpc_raw dataset, 3 clusters x,y & z the pixels are arranged in memory
// 
//         x1 x2 x3 y1 y2 y3 y4 z1 z2
//
// raw_seq[0] = 0,2 ; raw_seq[1] = 50,3, raw_seq[2] = 75,1
//
// The point here is that the start_bin is used for the calculation
// Of the time bin that the data came from.  The position in memory
// is calculated by summing up the lengths.
//
// This is different from the system used in the mezzanine where
// The raw bank is filled:
// 
//         x1 x2 0 ...0 y1 y2 y3 y4 0 ... 0 z1 z2 0 ...
//
// in such a way that the cpp banks in MZ contain start_bin,end_bin
// and the they each correspond to BOTH the time bin and the offset 
// in the pad data of the sequence.  
//
// The following is meant to be a convenient intermediary between the 
// two pictures.  The final translation will be done in the input
// routine of the DAQ clusterfinder for each padrow
//
struct StDaqClfcpp {
  unsigned short start_bin;
  unsigned short length;
  unsigned int offset;
}; 

struct StDaqClfCppRow {
  StDaqClfcpp r[MAX_PADS][MAX_SEQ];
};

class StDaqClfMaker:public StMaker
{
 private:
  StDaqClfCppRow cpp[MAX_PADROWS];       // PADROW, PAD, SEQUENCE (0 based)

  unsigned short croat_adc[MAX_PADS_EVER+1][MAX_TIMEBINS_EVER];
  unsigned short croat_cpp[MAX_PADS_EVER+1][64];
  unsigned int croat_adcOff[MAX_PADS_EVER+1];
  unsigned short croat_cppOff[MAX_PADS_EVER+1];   
  
  int t0Corr[MAX_PADS_EVER+1];
  unsigned int gainCorr[MAX_PADS_EVER+1];

 

  Int_t BuildCPP(int nrows, 
		 raw_row_st *row, 
		 raw_pad_st *pad, 
		 raw_seq_st *seq);

  StDaqClfCppRow *GetCPPRow(int r, int i, StDaqClfCppRow *storage);     // split the row along mezzanine banks

  void saveCluster(int cl_x, int cl_t, int cl_f, int cl_c, int r, int sector);
  void fillStEvent(tcl_tphit_st *hit);
  void filltphit(tcl_tphit_st *hit);

  void getPbPT0Corrections(int sector, int row);
  void getGainCorrections(int sector, int row);

  // Need my own coordinate transformations, because
  // TPC provided transforms only work on ints.
  double lxFromPad(int row, double pad);
  double lyFromRow(int row);
  double lzFromTB(double TB,int sector,int row,int pad);

  double mPrfin;
  double mTrfin;
  double mPrfout;
  double mTrfout;
  double mDp;
  double mDt;
  double mDperp;

  // control flags
  int splitRows;          // split rows as they are split on the i960
  int doT0Corrections;    // apply t0 corrections
  int doGainCorrections;  // apply gains in clf (rather than daqmaker)
  int doZeroTruncation;   // throw away hist beyond central membrane
  int fillDeconFlag;      // do we fill the deconvolution flag?

  int mFill_tphit;
  int mFill_stevent;
  int mCreate_stevent;
  int clustercount;

  double mDriftVelocity;

  St_tcl_tphit *mT_tphit;
  StEvent *mStEvent;
  StTpcHitCollection *mTpcHitColl;

  StTpcCoordinateTransform *mCTransform;
  fcfClass *fcf;
  
  // TPC Parameters...
  St_tss_tsspar *m_tsspar;

 public:    

  StDaqClfMaker(const char *name="tpc_hits");
  ~StDaqClfMaker() ;

  Int_t Init() ;
  Int_t Make() ;

  // cvs
  const char *GetCVS() const
    {
      static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
    }
  
  ClassDef(StDaqClfMaker,0)    //StAF chain virtual base class for Makers
};

#endif
