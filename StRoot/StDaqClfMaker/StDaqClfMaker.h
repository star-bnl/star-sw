#ifndef StDaqClfMaker_H
#define StDaqClfMaker_H

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
  unsigned int croat_cppOff[MAX_PADS_EVER+1];

  // control flags
  Int_t histSector;
  Int_t histPadrow;
  Int_t splitRows;          // split rows as they are split on the i960

  Int_t StDaqClfMaker::BuildHistogram(int padrow, 
				      unsigned short *adc_in, 
				      unsigned short *adc_out);

  Int_t BuildCPP(int nrows, 
		 raw_row_st *row, 
		 raw_pad_st *pad, 
		 raw_seq_st *seq);

  StDaqClfCppRow *GetCPPRow(int r, int i, StDaqClfCppRow *storage);     // split the row along mezzanine banks

  void saveCluster(int cl_x, int cl_t, int cl_f, int cl_c, int r, int sector);
  void fillStEvent(tcl_tphit_st *hit);
  void filltphit(tcl_tphit_st *hit);

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
  
  ClassDef(StDaqClfMaker, 1)    //StAF chain virtual base class for Makers
};

#endif
