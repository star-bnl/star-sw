#ifndef StRTSClientFCFMaker_H
#define StRTSClientFCFMaker_H

/***************************************************************************
 *
 * $Id: FCFMaker.h,v 1.6 2004/01/26 22:46:54 jml Exp $
 *
 *--------------------------------------------------------------------------
 *
 * $Log: FCFMaker.h,v $
 * Revision 1.6  2004/01/26 22:46:54  jml
 * debugging to see gains/t0, only get gains/t0 once, cleaner logging
 *
 * Revision 1.5  2004/01/22 18:36:11  jml
 * more updates to the logging
 *
 * Revision 1.4  2004/01/22 14:20:36  jml
 * Added cluster reading
 *
 * Revision 1.3  2003/11/17 18:53:00  jml
 * Preliminary tests look good
 *
 * Revision 1.2  2003/09/17 19:57:48  tonko
 * Changed name of the class from DaqClf to RTSClientFCF
 *
 * Revision 1.1  2003/09/17 18:22:26  tonko
 * First seemingly working set
 *
 * Revision 1.3  2003/09/16 15:35:40  tonko
 * Deleted misc. ifdef code
 *
 * Revision 1.2  2003/09/15 20:17:33  tonko
 * First compilable Tonkos version
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

#include <fcfClass.hh>

class St_tss_tsspar;
class StEvent;
class StTpcCoordinateTransform;
class StTpcHitCollection;
typedef u_int* j_uintptr;

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

class StRTSClientFCFMaker:public StMaker
{
 private:
  bool ignoreFileClusters;
  bool ignoreRawData;

  StDaqClfCppRow cpp[MAX_PADROWS];       // PADROW, PAD, SEQUENCE (0 based)

  unsigned short croat_adc[MAX_PADS_EVER+1][MAX_TIMEBINS_EVER];
  unsigned short croat_cpp[MAX_PADS_EVER+1][64];
  unsigned int croat_adcOff[MAX_PADS_EVER+1];
  unsigned short croat_cppOff[MAX_PADS_EVER+1];   
  
  int t0Corr[24][45][MAX_PADS_EVER+1];
  unsigned int gainCorr[24][45][MAX_PADS_EVER+1];
  unsigned short startFlags[MAX_PADS_EVER+1];

  Int_t BuildCPP(int nrows, 
		 raw_row_st *row, 
		 raw_pad_st *pad, 
		 raw_seq_st *seq,
		 int sector);

  StDaqClfCppRow *GetCPPRow(int r, int i, StDaqClfCppRow *storage);     // split the row along mezzanine banks

  void saveCluster(int cl_x, int cl_t, int cl_f, int cl_c, int p1, int p2, int t1, int t2, int r, int sector);
  void fillStEvent(tcl_tphit_st *hit);
  void filltphit(tcl_tphit_st *hit);

  void getCorrections(int sector, int row);

  // Need my own coordinate transformations, because
  // TPC provided transforms only work on ints.
  double lxFromPad(int row, double pad);
  double lyFromRow(int row);
  double lzFromTB(double TB,int sector,int row,int pad);

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

  void SetDAQFlag(Int_t mode);

  StRTSClientFCFMaker(const char *name="tpc_hits");
  ~StRTSClientFCFMaker() ;

  Int_t Init() ;
  Int_t Make() ;
  Int_t InitRun(int run)  ;

  int runClusterFinder(j_uintptr *result_mz_ptr,
		       u_int *result_buff,
		       int sector,
		       int row,
		       StDaqClfCppRow *cppRow,
		       unsigned short *adc);

  int build_croat_clusters();
  int build_daq_file_clusters();

  bool checkSwap(int x);
  u_int swap32(bool test, u_int x);

  // cvs
  const char *GetCVS() const
    {
      static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
    }
  
  ClassDef(StRTSClientFCFMaker, 1)    //StAF chain virtual base class for Makers
};

#endif
