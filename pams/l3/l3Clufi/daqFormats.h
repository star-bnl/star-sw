#ifndef _DAQ_FORMATS_H
#define _DAQ_FORMATS_H

#ifdef VXWORKS
#include <vxWorks.h>
#else
typedef unsigned int UINT32 ;
typedef unsigned short UINT16 ;
typedef unsigned char UINT8 ;
#endif

#include </star/u2a/struck/work/l3offline/CROAT/include/sector.h>	// for the constants

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


// special events
#define FMT_SPEC_PED	(1 << 0) 
#define FMT_SPEC_SIGMA	(1 << 1)
#define FMT_SPEC_GAIN	(1 << 2)
#define FMT_SPEC_BADC	(1 << 3)


// Version below corresponds to "DAQ Raw Data Format" document's version
#define DAQ_RAW_FORMAT_VERSION	0x00020000	// 2.0

// Define for linker-level checking only! See comment at the
// end of this file.
//#define VERIFY_SIZES

#define DAQ_RAW_FORMAT_ORDER	0x04030201
#define DAQ_RAW_FORMAT_WORD9	0x9999c0de

// order in the mezzanine bank
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

// character names
#define CHAR_LRHD	"LRHD    "
#define CHAR_BEGR	"BEGR    "
#define CHAR_ENDR	"ENDR    "
#define CHAR_DATA	"DATA    "
#define CHAR_SLOW	"SLOW    "

#define CHAR_DATAP	"DATAP   "
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


#define CHAR_L3_SECLP	"L3_SECLP"
#define CHAR_L3_SECLD	"L3_SECLD"

#define CHAR_RICHP	"RICHP   "
#define CHAR_RICHD	"RICHD   "
#define CHAR_RICHPED	"RICHPED "
#define CHAR_RICHRMS	"RICHRMS "
#define CHAR_RICHTHR	"RICHTHR "
#define CHAR_RICHBADC	"RICHBADC"


#pragma align 1
#if (CPU == I960HX)
#pragma pack 1
#endif

// generic section for all of DAQ
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

struct LOGREC {
	struct logicalHeader lh ;
	UINT32 length ;
	UINT32 blocking ;
	char record_type[8] ;
	UINT32 crc ;
} ;

// Level 3 structures
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



// this is a local structure that is just used to allocate space
// it will be recast to the real TPCMZCLD later...
struct TPCMZCLD_local {
	struct bankHeader bh ;
	UINT32 padrowFiller[2*6] ;	// max 4 padrows, each occupies 2 UINT32s...
	struct mzCentroid centroidFiller[MZ_TPC_CPP_PER_PAD*MZ_TPC_MAX_PADS_PER_MEZ] ; 
} ;


// Formatting structures
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

struct TPCADCX {
	struct bankHeader bh ;
	struct row {
		UINT32 row ;
		UINT32 offADC ;
		UINT32 offSEQ ;
	} lrow[6] ;	// MAX 6 padrows in a mezzanine
} ;

struct TPCADCD {
	struct bankHeader bh ;
	UINT8	adc[MZ_TPC_MAX_PADS_PER_MEZ*MZ_TPC_TIMEBINS] ;	// this is the worst case
				// 384 represents the maximum _logical_
				// number of pads per mezzanine
} ;

struct TPCSEQD {
	struct bankHeader bh ;
	UINT16	seq[MZ_TPC_MAX_PADS_PER_MEZ*(MZ_TPC_TIMEBINS/2)] ;	// theoretical maximum
					// see TPCADCD...
} ;

struct TPCPADK {
	struct bankHeader bh ;
	UINT32	bytesADC ;
	UINT32	bytesCPP ;
	UINT32	bytesPED ;
	UINT32	bytesRMS ;
	UINT32	bytesCFG ;
	UINT32	bytesGAIN ;
	UINT8	row_pad[2*MZ_ASIC_NUM*MZ_TPC_PADS_PER_ASIC] ;
} ;

struct TPCCFGR {
	struct bankHeader bh ;
	UINT8	feeId[MZ_ASIC_NUM*MZ_TPC_PADS_PER_ASIC] ;
} ;

struct TPCGAINR {
	struct bankHeader bh ;
	UINT32 events ;
	UINT32 meanGain ;
	struct gain {
		UINT16	t0 ;
		UINT8	t0_rms ;
		UINT8	rel_gain ;
	} gain[MZ_ASIC_NUM*MZ_TPC_PADS_PER_ASIC] ;
} ;

struct TPCPEDR {
	struct bankHeader bh ;
	UINT32 events ;
	UINT8 ped[MZ_ASIC_NUM*MZ_TPC_PADS_PER_ASIC*MZ_TPC_TIMEBINS] ;
} ;

struct TPCRMSR {
	struct bankHeader bh ;
	UINT32 events ;
	UINT8 rms[MZ_ASIC_NUM*MZ_TPC_PADS_PER_ASIC*MZ_TPC_TIMEBINS] ;
} ;

struct TPCBADR {
	struct bankHeader bh ;
	UINT32 row_ch[MZ_TPC_PADS_PER_ASIC*MZ_ASIC_NUM] ;
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


// Level 3 banks
struct L3_SECLP {
	struct bankHeader bh ;
} ;

struct L3_SECLD {
	struct bankHeader bh ;
} ;

struct DATAP {
	struct bankHeader bh ;
	UINT32 len ;
	UINT32 time ;
	UINT32 seq ;
	UINT32 trg_word ;
	UINT32 trg_in_word ;
	UINT32 detector ;
	struct offlen det[61] ; // total data len is 128 words
} ;


struct TPCP {
	struct bankHeader bh ;
	struct offlen sb[24] ;	// 24 sectors
} ;

#if (CPU == I960HX)
#pragma pack 0
#endif
#pragma align 0


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

