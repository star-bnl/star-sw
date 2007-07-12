#ifndef StRTSClientFCFMaker_H
#define StRTSClientFCFMaker_H

/***************************************************************************
 *
 * $Id: FCFMaker.h,v 1.14 2007/07/12 20:31:42 fisyak Exp $
 *
 *--------------------------------------------------------------------------
 *
 * $Log: FCFMaker.h,v $
 * Revision 1.14  2007/07/12 20:31:42  fisyak
 * Use Drift velocity depending on sector
 *
 * Revision 1.13  2005/04/11 14:08:20  jml
 * flags to switch of EAST &/or WEST TPC
 *
 * Revision 1.12  2004/09/02 21:37:19  jml
 * Reduced memory footprint
 *
 * Revision 1.11  2004/08/05 20:08:06  jml
 * added support for StEvent
 *
 * Revision 1.10  2004/04/21 20:30:54  tonko
 * Added back-annotation and misc. cleanup
 *
 * Revision 1.9  2004/03/10 21:55:54  jml
 * support for simulations in FCFMaker
 *
 * Revision 1.8  2004/01/27 18:38:18  jeromel
 * Change SetDAQFlag to overloaded SetMode
 *
 * Revision 1.7  2004/01/27 16:30:15  jml
 * added FCF_ to defines
 *
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

#define FCF_MAX_PADS_EVER 256
#define FCF_MAX_TIMEBINS_EVER 512
#define FCF_MAX_CLUSTERS 6000

#define FCF_MAX_PADROWS 45
#define FCF_MAX_PADS 184
#define FCF_MAX_SEQ 32

class St_tss_tsspar;
class StEvent;
class StTpcCoordinateTransform;
class StTpcHitCollection;
typedef u_int* j_uintptr;

struct croat_out_s {
  u_int v[45][(FCF_MAX_CLUSTERS + 2) * 2];
};
struct daq_out_s {
  u_int v[6][3][(FCF_MAX_CLUSTERS + 2) * 2 * 6];
};
struct resptr_s {
  j_uintptr v[45][3];
};

typedef croat_out_s croat_out_t;
typedef daq_out_s daq_out_t;
typedef resptr_s resptr_t;

struct t0_corr_s {
  int v[24][45][FCF_MAX_PADS_EVER+1];
};
struct gain_corr_s {
  u_int v[24][45][FCF_MAX_PADS_EVER+1];
};

typedef t0_corr_s t0_corr_t;
typedef gain_corr_s gain_corr_t;

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
  StDaqClfcpp r[FCF_MAX_PADS][FCF_MAX_SEQ];
};

class StRTSClientFCFMaker:public StMaker
{
 private:
  bool ignoreFileClusters;
  bool ignoreRawData;
  bool m_EastOff;
  bool m_WestOff;

  StDaqClfCppRow cpp[FCF_MAX_PADROWS];       // PADROW, PAD, SEQUENCE (0 based)

  short croat_trk[FCF_MAX_PADS_EVER+1][FCF_MAX_TIMEBINS_EVER];
  unsigned short croat_adc[FCF_MAX_PADS_EVER+1][FCF_MAX_TIMEBINS_EVER];
  unsigned short croat_cpp[FCF_MAX_PADS_EVER+1][64];
  unsigned int croat_adcOff[FCF_MAX_PADS_EVER+1];
  unsigned short croat_cppOff[FCF_MAX_PADS_EVER+1];   
  
  t0_corr_t *t0Corr;
  gain_corr_t *gainCorr;
  unsigned short startFlags[FCF_MAX_PADS_EVER+1];

  Int_t BuildCPP(int nrows, 
		 raw_row_st *row, 
		 raw_pad_st *pad, 
		 raw_seq_st *seq,
		 int sector);

  StDaqClfCppRow *GetCPPRow(int r, int i, StDaqClfCppRow *storage);     // split the row along mezzanine banks

  void saveCluster(int cl_x, int cl_t, int cl_f, int cl_c, int p1, int p2, int t1, int t2, int r, int sector, int cl_id, int id_simtrk, int id_quality);
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

  int clustercount;
  int hasSim;

  St_tcl_tphit *mT_tphit;
  StEvent *mStEvent;
  StTpcHitCollection *mTpcHitColl;

  StTpcCoordinateTransform *mCTransform;
  fcfClass *fcf;
  
  // TPC Parameters...
  St_tss_tsspar *m_tsspar;

 public:    

  void SetMode(Int_t mode);

  StRTSClientFCFMaker(const char *name="tpc_hits");
  ~StRTSClientFCFMaker() ;

  Int_t Init() ;
  Int_t Make() ;
  Int_t InitRun(int run)  ;

  void EastOff() { 
    printf("<FCFMaker> Setting East TPC Off\n");
    m_EastOff = true; 
  }

  void WestOff() { 
    printf("<FCFMaker> Setting West TPC Off\n");
    m_WestOff = true; 
  }

  void AllOn() {
    printf("<FCFMaker> Setting East&West TPC On\n");
    m_WestOff = false;
    m_EastOff = false;
  }

  int runClusterFinder(j_uintptr *result_mz_ptr,
		       u_int *result_buff,
		       int sector,
		       int row,
		       StDaqClfCppRow *cppRow,
		       unsigned short *adc,
		       unsigned short *trk,
		       u_int *simu_result_buff,
		       j_uintptr *simu_mz_ptr);

  int build_croat_clusters(u_int sector, croat_out_t *croat_out, resptr_t *croat_res, croat_out_t *simu_out, resptr_t *simu_res);
  int build_daq_file_clusters(u_int sector, daq_out_t *, resptr_t *);

  int anyRawDataInFile();
  int anyClustersInFile();
  u_int *getMZCLD(u_int hsector, u_int rb, u_int mz, u_int *len);

  bool checkSwap(int x);
  u_int swap32(bool test, u_int x);

  // cvs
  virtual const char *GetCVS() const
    {
      static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
    }
  
  ClassDef(StRTSClientFCFMaker, 1)    //StAF chain virtual base class for Makers
};

#endif
