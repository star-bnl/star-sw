#ifndef _DAQ_FORMATS_H
#define _DAQ_FORMATS_H

#ifdef VXWORKS
#include <vxWorks.h>
#else
typedef unsigned int UINT32 ;
typedef unsigned short UINT16 ;
typedef unsigned char UINT8 ;
#endif

#include "sector.h"	/* for the constants */

#define FMT_ADCD	(1 << 0)
#define FMT_ADCR	(1 << 1)
#define FMT_ADCHUF	(1 << 2)
#define FMT_CPPR	(1 << 3)
#define FMT_CLD		(1 << 4)

#define FMT_NOT_FREE	(FMT_ADCR | FMT_ADCD | FMT_ADCHUF)

#define FMT_PEDR	(1 << 8)
#define FMT_RMSR	(1 << 9)
#define FMT_GAINR	(1 << 10)
#define FMT_BADR	(1 << 11)
#define FMT_CFGR	(1 << 12)


/* special events */
#define FMT_SPEC_PED	(1 << 0) 
#define FMT_SPEC_SIGMA	(1 << 1)
#define FMT_SPEC_GAIN	(1 << 2)
#define FMT_SPEC_BADC	(1 << 3)


/* Version below corresponds to "DAQ Raw Data Format" document's version */
#define DAQ_RAW_FORMAT_VERSION	0x00020002	/* 2.2 */

/* Define for linker-level checking only! See comment at the */
/* end of this file. */
/*#define VERIFY_SIZES */

#define DAQ_RAW_FORMAT_ORDER	0x04030201
#define DAQ_RAW_FORMAT_WORD9	0x9999c0de

/*
// ALL the structures and defines that share the same structure types as the TPC
// (i.e. SVT, FTPC, SSD) will be called, by definition, TPC_
*/

/* order in the mezzanine bank */
#define TPC_ADCD	0
#define TPC_SEQD	1
#define TPC_ADCX	2
#define TPC_PADK	3
#define TPC_CPPR	4
#define TPC_ADCR	5
#define TPC_MZCLD	6
#define TPC_CFGR	7
#define TPC_PEDR	8
#define TPC_RMSR	9
#define TPC_GAINR	10
#define TPC_BADR	11

#define TPC_MZP_BANKS_NUM	12

/* gloabal character names */
#define CHAR_LRHD	"LRHD    "
#define CHAR_BEGR	"BEGR    "
#define CHAR_ENDR	"ENDR    "
#define CHAR_DATA	"DATA    "
#define CHAR_SLOW	"SLOW    "

#define CHAR_DATAP	"DATAP   "

/* real TPC names    */
#define CHAR_TPCP	"TPCP    "

#define CHAR_TPCSECLP	"TPCSECLP"
#define CHAR_TPCRBCLP	"TPCRBCLP"
#define CHAR_TPCMZCLD	"TPCMZCLD"

#define CHAR_TPCSECP	"TPCSECP "
#define CHAR_TPCRBP	"TPCRBP  "
#define CHAR_TPCMZP	"TPCMZP  "

#define	CHAR_TPCADCD	"TPCADCD "
#define CHAR_TPCSEQD	"TPCSEQD "
#define CHAR_TPCADCX	"TPCADCX "
#define CHAR_TPCPADK	"TPCPADK "
#define CHAR_TPCCPPR	"TPCCPPR "
#define CHAR_TPCADCR	"TPCADCR "
#define CHAR_TPCCFGR	"TPCCFGR "
#define CHAR_TPCPEDR	"TPCPEDR "
#define CHAR_TPCRMSR	"TPCRMSR "
#define CHAR_TPCGAINR	"TPCGAINR"
#define CHAR_TPCBADR	"TPCBADR "

/* real SVT names */
#define CHAR_SVTP	"SVTP    "

#define CHAR_SVTSECLP	"SVTSECLP"
#define CHAR_SVTRBCLP	"SVTRBCLP"
#define CHAR_SVTMZCLD	"SVTMZCLD"

#define CHAR_SVTSECP	"SVTSECP "
#define CHAR_SVTRBP	"SVTRBP  "
#define CHAR_SVTMZP	"SVTMZP  "

#define	CHAR_SVTADCD	"SVTADCD "
#define CHAR_SVTSEQD	"SVTSEQD "
#define CHAR_SVTADCX	"SVTADCX "
#define CHAR_SVTANODK	"SVTANODK"
#define CHAR_SVTCPPR	"SVTCPPR "
#define CHAR_SVTADCR	"SVTADCR "
#define CHAR_SVTCFGR	"SVTCFGR "
#define CHAR_SVTPEDR	"SVTPEDR "
#define CHAR_SVTRMSR	"SVTRMSR "
#define CHAR_SVTGAINR	"SVTGAINR"
#define CHAR_SVTBADR	"SVTBADR "

/* real FTP names */
#define CHAR_FTPP	"FTPP    "

#define CHAR_FTPSECLP	"FTPSECLP"
#define CHAR_FTPRBCLP	"FTPRBCLP"
#define CHAR_FTPMZCLD	"FTPMZCLD"

#define CHAR_FTPSECP	"FTPSECP "
#define CHAR_FTPRBP	"FTPRBP  "
#define CHAR_FTPMZP	"FTPMZP  "

#define	CHAR_FTPADCD	"FTPADCD "
#define CHAR_FTPSEQD	"FTPSEQD "
#define CHAR_FTPADCX	"FTPADCX "
#define CHAR_FTPPADK	"FTPPADK "
#define CHAR_FTPCPPR	"FTPCPPR "
#define CHAR_FTPADCR	"FTPADCR "
#define CHAR_FTPCFGR	"FTPCFGR "
#define CHAR_FTPPEDR	"FTPPEDR "
#define CHAR_FTPRMSR	"FTPRMSR "
#define CHAR_FTPGAINR	"FTPGAINR"
#define CHAR_FTPBADR	"FTPBADR "

/* real RICH names   */
#define CHAR_RICHP	"RICHP   "
#define CHAR_RICHD	"RICHD   "
#define CHAR_RICHPED	"RICHPED "
#define CHAR_RICHRMS	"RICHRMS "
#define CHAR_RICHTHR	"RICHTHR "
#define CHAR_RICHBADC	"RICHBADC"

/*
//#pragma align 1
//#if (CPU == I960HX)
//#pragma pack 1
//#endif
*/

/* generic section for all of DAQ */
struct bankHeader {
        char bank_type[8] ;
        UINT32 length ;
        UINT32 bank_id ;
        UINT32 format_ver ;
        UINT32 byte_order ;
        UINT32 format_number ;
        UINT32 token ;
        UINT32 w9 ;
        UINT32 crc ;
} ;

struct logicalHeader {
	char bank_type[8] ;
	UINT32 length ;
	UINT32 run ;
	UINT32 format_ver ;
	UINT32 byte_order ;
	UINT32 w7 ;
	UINT32 w8 ;
	UINT32 w9 ;
	UINT32 crc ;
} ;

struct offlen {
	UINT32 off ;
	UINT32 len;
} ;

struct row_pad {
	UINT8 row ;
	UINT8 pad ;
} ;

struct LOGREC {
	struct logicalHeader lh ;
	UINT32 length ;
	UINT32 blocking ;
	char record_type[8] ;
	UINT32 crc ;
} ;


struct DATAP {
	struct bankHeader bh ;
	UINT32 len ;
	UINT32 time ;
	UINT32 seq ;
	UINT32 trg_word ;
	UINT32 trg_in_word ;
	UINT32 detector ;
	struct offlen det[61] ; /* total data len is 128 words */
} ;


struct TPCP {
	struct bankHeader bh ;
	struct offlen sb[24] ;	/* 24 sectors  */
} ;

/*
// Level 3 structures
// Level 3 banks - most are defined in  file included at end
*/

struct TPCSECLP {
	struct bankHeader bh ;
	struct offlen rb[SB_RB_NUM] ;
} ;

struct TPCRBCLP {
	struct bankHeader bh ;
	struct offlen mz[RB_MZ_NUM] ;
	UINT8	fiberHdr[RB_FIBER_HDR_LEN] ;
} ;

struct mzCentroid {
	UINT16 x ;
	UINT16 t ;
	UINT16 flags ;
	UINT16 charge ;
} ;





/* Formatting structures */
struct TPCSECP {
	struct bankHeader bh ;
	struct offlen rb[SB_RB_NUM] ;
} ;


struct TPCRBP {
	struct bankHeader bh ;
	struct offlen mz[RB_MZ_NUM] ;
	UINT8 fiberHdr[RB_FIBER_HDR_LEN] ;
} ;

struct TPCMZP {
	struct bankHeader bh ;
	struct offlen banks[TPC_MZP_BANKS_NUM] ;
} ;
/*
   this is a local structure that is just used to allocate space
   it will be recast to the real TPCMZCLD later...
*/
struct TPCMZCLD_local {
	struct bankHeader bh ;
	UINT32 rows ;	
	UINT32 padrowFiller[2*MZ_MAX_ROWS] ;
	struct mzCentroid centroidFiller[MZ_MAX_CPPS] ;
} ;

struct TPCADCX {
	struct bankHeader bh ;
	struct row {
		UINT32 row ;
		UINT32 offADC ;
		UINT32 offSEQ ;
	} lrow[MZ_MAX_ROWS] ;	/* MAX 6 padrows/hybrids in a mezzanine */
} ;

struct TPCADCD {
	struct bankHeader bh ;
	UINT8	adc[MZ_MAX_CHANNELS] ;	/* this is the worst case */
} ;

struct TPCSEQD {
	struct bankHeader bh ;
	UINT16	seq[MZ_MAX_CHANNELS/2] ;/* theoretical maximum */
					/* see TPCADCD... */
} ;

struct TPCPEDR {
	struct bankHeader bh ;
	UINT32 events ;
	UINT8 ped[MZ_MAX_CHANNELS] ;
} ;

struct TPCRMSR {
	struct bankHeader bh ;
	UINT32 events ;
	UINT8 rms[MZ_MAX_CHANNELS] ;
} ;

struct TPCADCR_l {
	struct bankHeader bh ;
} ;

struct TPCCPPR_l {
	struct bankHeader bh ;
	UINT8 thr_lo ;
	UINT8 thr_hi ;
	UINT8 seq_lo ;
	UINT8 seq_hi ;
} ;


/* structures that are detector dependent */

struct DETPADK {
	struct bankHeader bh ;
	UINT32	bytesADC ;
	UINT32	bytesCPP ;
	UINT32	bytesPED ;
	UINT32	bytesRMS ;
	UINT32	bytesCFG ;
	UINT32	bytesGAIN ;
	struct	row_pad rp[MZ_MAX_PADS] ;
} ;


struct TPCPADK {
	struct bankHeader bh ;
	UINT32	bytesADC ;
	UINT32	bytesCPP ;
	UINT32	bytesPED ;
	UINT32	bytesRMS ;
	UINT32	bytesCFG ;
	UINT32	bytesGAIN ;
	struct	row_pad rp[MZ_TPC_MAX_PADS_PER_MEZ] ;
} ;


struct SVTANODK {
	struct bankHeader bh ;
	UINT32	bytesADC ;
	UINT32	bytesCPP ;
	UINT32	bytesPED ;
	UINT32	bytesRMS ;
	UINT32	bytesCFG ;
	UINT32	bytesGAIN ;
	UINT32  hybrids[6] ;
#ifdef TONKO_ELABORATE
	struct	hybrids {
		UINT8	barrel ;
		UINT8	ladder ;
		UINT8   hy_wf ;
		UINT8	hybridID ;
	} hybrids[6] ;
#endif
} ;


struct DETCFGR {
	struct bankHeader bh ;
	UINT8	feeId[MZ_MAX_PADS] ;
} ;

struct TPCCFGR {
	struct bankHeader bh ;
	UINT8	feeId[MZ_TPC_MAX_PADS_PER_MEZ] ;
} ;

struct SVTCFGR {
	struct bankHeader bh ;
	UINT8	feeId[MZ_SVT_MAX_PADS_PER_MEZ] ;
} ;

struct gain_st {
	UINT16	t0 ;
	UINT8	t0_rms ;
	UINT8	rel_gain ;
}  ;

struct DETGAINR {
	struct bankHeader bh ;
	UINT32 events ;
	UINT32 meanGain ;
	struct gain_st gain[MZ_MAX_PADS] ;
	UINT8	trans_table[1024] ;
	UINT16  exp_table[256] ;
} ;


struct SVTGAINR {
	struct bankHeader bh ;
	UINT32 events ;
	UINT32 meanGain ;
	struct gain_st gain[MZ_SVT_MAX_PADS_PER_MEZ] ;
	UINT8	trans_table[1024] ;
	UINT16  exp_table[256] ;
} ;

struct TPCGAINR {
	struct bankHeader bh ;
	UINT32 events ;
	UINT32 meanGain ;
	struct gain_st gain[MZ_TPC_MAX_PADS_PER_MEZ] ;
	UINT8	trans_table[1024] ;
	UINT16  exp_table[256] ;
} ;


struct DETBADR {
	struct bankHeader bh ;
	struct row_pad rp[MZ_MAX_PADS] ;
} ;

struct TPCBADR {
	struct bankHeader bh ;
	struct row_pad rp[MZ_TPC_MAX_PADS_PER_MEZ] ;
} ;

struct SVTBADR {
	struct bankHeader bh ;
	struct row_pad rp[MZ_SVT_MAX_PADS_PER_MEZ] ;
} ;

/*
// Level 3 structures
// Level 3 banks
//#include "L3/L3Formats.h"
*/
#include "L3Formats.h"

/*
//#if (CPU == I960HX)
//#pragma pack 0
//#endif
//#pragma align 0
*/


#ifdef VERIFY_SIZES
/* 

If this is defined the follwing variables will show up
in the dissaemby of this file when compiled so one can
immediatelly check the sizes of these ovjects without
printf statements and actual code since the sizes (i.e. sizeof)
are knwon at compile time...

For normal compilation this should be undefined...

__alignof__ exists in GCC...
*/

static UINT32 sTPCSECP	= sizeof(struct TPCSECP) ;
static UINT32 aTPCSECP	= __alignof__(struct TPCSECP) ;

static UINT32 sTPCSECLP	= sizeof(struct TPCSECLP) ;
static UINT32 aTPCSECLP	= __alignof__(struct TPCSECLP) ;

static UINT32 sTPCRBCLP	= sizeof(struct TPCRBCLP) ;
static UINT32 aTPCRBCLP	= __alignof__(struct TPCRBCLP) ;

static UINT32 sTPCMZCLD_local	= sizeof(struct TPCMZCLD_local) ;
static UINT32 aTPCMZCLD_local	= __alignof__(struct TPCMZCLD_local) ;

static UINT32 sTPCRBP	= sizeof(struct TPCRBP) ;
static UINT32 aTPCRBP	= __alignof__(struct TPCRBP) ;

static UINT32 sTPCMZP	= sizeof(struct TPCMZP) ;
static UINT32 aTPCMZP	= __alignof__(struct TPCMZP) ;

static UINT32 sTPCADCX	= sizeof(struct TPCADCX) ;
static UINT32 aTPCADCX	= __alignof__(struct TPCADCX) ;

static UINT32 sTPCADCD	= sizeof(struct TPCADCD) ;
static UINT32 aTPCADCD	= __alignof__(struct TPCADCD) ;

static UINT32 sTPCSEQD	= sizeof(struct TPCSEQD) ;
static UINT32 aTPCSEQD	= __alignof__(struct TPCSEQD) ;

static UINT32 sTPCPADK	= sizeof(struct TPCPADK) ;
static UINT32 aTPCPADK	= __alignof__(struct TPCPADK) ;

static UINT32 sTPCCFGR	= sizeof(struct TPCCFGR) ;
static UINT32 aTPCCFGR	= __alignof__(struct TPCCFGR) ;

static UINT32 sTPCGAINR	= sizeof(struct TPCGAINR) ;
static UINT32 aTPCGAINR	= __alignof__(struct TPCGAINR) ;

static UINT32 sTPCPEDR	= sizeof(struct TPCPEDR) ;
static UINT32 aTPCPEDR	= __alignof__(struct TPCPEDR) ;

static UINT32 sTPCRMSR	= sizeof(struct TPCRMSR) ;
static UINT32 aTPCRMSR	= __alignof__(struct TPCRMSR) ;

static UINT32 sTPCBADR	= sizeof(struct TPCBADR) ;
static UINT32 aTPCBADR	= __alignof__(struct TPCBADR) ;

static UINT32 sTPCADCR_l	= sizeof(struct TPCADCR_l) ;
static UINT32 aTPCADCR_l	= __alignof__(struct TPCADCR_l) ;

static UINT32 sTPCCPPR_l	= sizeof(struct TPCCPPR_l) ;
static UINT32 aTPCCPPR_l	= __alignof__(struct TPCCPPR_l) ;


#endif

#endif

