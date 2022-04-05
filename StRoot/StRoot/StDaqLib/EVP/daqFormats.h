#ifndef _DAQ_FORMATS_H
#define _DAQ_FORMATS_H

#ifdef VXWORKS
#include <vxWorks.h>
#else
typedef unsigned int UINT32 ;
typedef unsigned short UINT16 ;
typedef unsigned char UINT8 ;
#endif

#include "SECTOR/sector.h"	// for the size constants


#if (TRG_VERSION == 0x20)
#include "TRG/trgStructures_20.h"
#elif (TRG_VERSION == 0x12)
#include "TRG/trgStructures-Jan2002.h"
#elif (TRG_VERSION == 0x21)
#include "TRG/trgStructures_21.h"
#elif (TRG_VERSION == 0x22)
#include "TRG/trgStructures_v22.h"
#else	// default
//#define TRG_VERSION 0x30	// changed on Feb 22, 2007
//#include "TRG/trgStructures.h"
#define TRG_VERSION 0x32
#include "TRG/trgStructures2008.h"
#endif

#define FMT_ADCD	(1 << 0)
#define FMT_ADCR	(1 << 1)
#define FMT_ADCHUF	(1 << 2)
#define FMT_CPPR	(1 << 3)
#define FMT_CLD		(1 << 4)

#define FMT_PADK	(1 << 5) 
#define FMT_SEQD	(1 << 6)
#define FMT_ADCX	(1 << 7)

#define FMT_NOT_FREE	(FMT_ADCR | FMT_ADCD | FMT_ADCHUF)

#define FMT_PEDR	(1 << 8)
#define FMT_RMSR	(1 << 9)
#define FMT_GAINR	(1 << 10)
#define FMT_BADR	(1 << 11)
#define FMT_CFGR	(1 << 12)
#define FMT_NONE        (1 << 13)


// Version below corresponds to "DAQ Raw Data Format" document's version
// changed from:
//#define DAQ_RAW_FORMAT_VERSION	0x00020002	// 2.2
// on April 14, 2002. Tonko
#define DAQ_RAW_FORMAT_VERSION	0x00020003	// 2.30


#define DAQ_RAW_FORMAT_ORDER	0x04030201

// changed from
//#define DAQ_RAW_FORMAT_WORD9	0x9999c0de
// on April 14, 2002. Tonko
#define DAQ_RAW_FORMAT_WORD9	0x00000000

#define DATAP_FORMAT_NUMBER     1
#define SVTP_FORMAT_NUMBER      1 
#define TPCP_FORMAT_NUMBER      1
#define FTPCP_FORMAT_NUMBER     1
#define LRHD_FORMAT_NUMBER      1
#define TRGP_FORMAT_NUMBER      1
#define EMCP_FORMAT_NUMBER      1
#define SSDP_FORMAT_NUMBER      1
#define TRGID_FORMAT_NUMBER     1
#define TPXP_FORMAT_NUMBER      1

// ALL the structures and defines that share the same structure types as the TPC
// (i.e. SVT, FTPC, SSD) will be called, by definition, TPC_

// order in the mezzanine bank
#define TPC_ADCD	0
#define TPC_SEQD	1
#define TPC_ADCX	2
#define TPC_PADK	3
#define SVT_ANODK	3
#define TPC_CPPR	4
#define TPC_ADCR	5
#define TPC_MZCLD	6
#define TPC_CFGR	7
#define TPC_PEDR	8
#define TPC_RMSR	9
#define TPC_GAINR	10
#define TPC_GAIND	10
#define TPC_BADR	11

#define TPC_MZP_BANKS_NUM	12

// gloabal character names
#define CHAR_LRHD	"LRHD    "
#define CHAR_BEGR	"BEGR    "
#define CHAR_ENDR	"ENDR    "
#define CHAR_DATA	"DATA    "
#define CHAR_SLOW	"SLOW    "

#define CHAR_DATAP	"DATAP   "
#define CHAR_DATAPX	"DATAPX  "

// real TPC names
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
#define CHAR_TPCGAIND	"TPCGAIND"	// new, sane gain bank
#define CHAR_TPCBADR	"TPCBADR "	// this is obsolete...

// real SVT names
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

// real FTP names
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

// real SSD names
#define CHAR_SSDP	"SSDP    "

#define CHAR_SSDSECLP	"SSDSECLP"
#define CHAR_SSDRBCLP	"SSDRBCLP"
#define CHAR_SSDMZCLD	"SSDMZCLD"

#define CHAR_SSDSECP	"SSDSECP "
#define CHAR_SSDRBP	"SSDRBP  "
#define CHAR_SSDMZP	"SSDMZP  "

#define	CHAR_SSDADCD	"SSDADCD "
#define CHAR_SSDSEQD	"SSDSEQD "
#define CHAR_SSDADCX	"SSDADCX "
#define CHAR_SSDPADK	"SSDPADK "
#define CHAR_SSDCPPR	"SSDCPPR "
#define CHAR_SSDADCR	"SSDADCR "
#define CHAR_SSDCFGR	"SSDCFGR "
#define CHAR_SSDPEDR	"SSDPEDR "
#define CHAR_SSDRMSR	"SSDRMSR "
#define CHAR_SSDGAINR	"SSDGAINR"
#define CHAR_SSDBADR	"SSDBADR "


// real RICH names
#define CHAR_RICP	"RICP    "
#define CHAR_RICCRAMP	"RICCRAMP"
#define CHAR_RICDATAD	"RICDATAD"
#define CHAR_RICDATAR	"RICDATAR"
#define CHAR_RICPEDR	"RICPEDR "
#define CHAR_RICRMSR	"RICRMSR "
#define CHAR_RICTHRER	"RICTHRER"
#define CHAR_RICBADR	"RICBADR "

// real TOF names
#define CHAR_TOFP	"TOFP    "
#define CHAR_TOFADCD	"TOFADCD "
#define CHAR_TOFTDCD	"TOFTDCD "
#define CHAR_TOFA2DD	"TOFA2DD "
#define CHAR_TOFSCAD	"TOFSCAD "
#define CHAR_TOFDDLR	"TOFDDLR "

// real FPD names
#define CHAR_FPDP	"FPDP    "
#define CHAR_FPDADCD	"FPDADCD "
#define CHAR_FPDTDCD	"FPDTDCD "
#define CHAR_FPDREGD	"FPDREGD "
#define CHAR_FPDPEDR	"FPDPEDR "
#define CHAR_FPDSCL	"FPDSCL  "
// BBC comes under FPDP!
#define CHAR_BBCDAT	"BBCDAT  "
#define CHAR_BBCPED	"BBCPED  "
#define CHAR_BBCSCL	"BBCSCL  "


// "new" (2002-2003) FPD
#define CHAR_FP2P	"FP2P    "


// real EMC names
#define CHAR_EMCP	"EMCP    "
#define CHAR_EMCSECP	"EMCSECP "
#define CHAR_EMCRBP	"EMCRBP  "

#define	CHAR_EMCADCD	"EMCADCD "
#define CHAR_EMCSEQD	"EMCSEQD "
#define CHAR_EMCADCX	"EMCADCX "
#define CHAR_EMCPADK	"EMCPADK "
#define CHAR_EMCCPPR	"EMCCPPR "
#define CHAR_EMCADCR	"EMCADCR "
#define CHAR_EMCCFGR	"EMCCFGR "
#define CHAR_EMCPEDR	"EMCPEDR "
#define CHAR_EMCRMSR	"EMCRMSR "
#define CHAR_EMCGAINR	"EMCGAINR"
#define CHAR_EMCBADR	"EMCBADR "

// real EEC names
#define CHAR_EECP	"EECP    "
#define CHAR_EECSECP	"EECSECP "
#define CHAR_EECRBP	"EECRBP  "

#define	CHAR_EECADCD	"EECADCD "
#define CHAR_EECSEQD	"EECSEQD "
#define CHAR_EECADCX	"EECADCX "
#define CHAR_EECPADK	"EECPADK "
#define CHAR_EECCPPR	"EECCPPR "
#define CHAR_EECADCR	"EECADCR "
#define CHAR_EECCFGR	"EECCFGR "
#define CHAR_EECPEDR	"EECPEDR "
#define CHAR_EECRMSR	"EECRMSR "
#define CHAR_EECGAINR	"EECGAINR"
#define CHAR_EECBADR	"EECBADR "


// PMD
#define CHAR_PMDP	"PMDP    "
#define CHAR_PMDSECP	"PMDSECP "
#define CHAR_PMDADCD	"PMDADCD "
#define CHAR_PMDRMSR	"PMDRMSR "
#define CHAR_PMDPEDR	"PMDPEDR "
#define CHAR_PMDTHRR	"PMDTHRR "

// trigger
#define CHAR_TRGP       "TRGP    "
#define CHAR_TRGD       "TRGD    "
#define CHAR_TRGID      "TRGID   "

// PP2PP
#define CHAR_PP2PPP	"PP2PPP  "
#define CHAR_PP2PPR	"PP2PPR  "

#define CHAR_L3P	"L3P     "

// Slow Controls and friends
#define CHAR_SCD	"SCD     "

#define CHAR_TPXP       "TPXP    "

/*
#if __GNUC__ == 2 && __GNUC_MINOR__ < 96 && defined(__I960)
#pragma align 1
#pragma pack 1
#endif
*/


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



// Tonko, 4/11/2002, Changed the name of the EventDescriptor
// structure to EventDescriptor2001 since it is still used
// in DATAP (i.e.) for backward compatibility

// the EventDescriptor is passed to DAQ from Trigger
// Tonko, 7/13/00 noticed that detectorMask and TRG_DAQ_cmds were
// swapped in real trigger data. Swapped them!

#if defined(UNIX_LITTLE_ENDIAN) || defined(RTS_LITTLE_ENDIAN)
struct EventDescriptor2001 {
  UINT8  format_version;
  UINT8  tag;                   // 'E'
  UINT16 byteCount;             // 28
  UINT32  bx_hi;                // Bunch Xing hi 32 bits
  UINT32  bx_lo;                // Bunch Xing lo 32 bits
  UINT8  detectorMask;          // makes the Action Word with TRG_DAQ_cmds
  UINT8  TRG_DAQ_cmds;          // 16*TRG_cmd | DAQ_cmd
  UINT16  token;
  UINT16 dsm_address;           // address of raw data for this crossing
  UINT16 dsm_data;              // output of last DSM
  UINT16 TRG_word;
  UINT8 add_bits;               // bit 7 - fake data; bit 6 - L2.5 abort; bit 1 - priority;  bit 0 - pileup
  UINT8 busy;                   // BUSY at start of this Xing
  UINT16 npost;
  UINT16 npre;
};
#else
struct EventDescriptor2001 {
  UINT16 byteCount;		// 28
  UINT8  tag;			// 'E'
  UINT8  format_version;	
  UINT32  bx_hi;		// Bunch Xing hi 32 bits
  UINT32  bx_lo;		// Bunch Xing lo 32 bits
  UINT16  token; 
  UINT8  TRG_DAQ_cmds;		// 16*TRG_cmd | DAQ_cmd
  UINT8  detectorMask;		// makes the Action Word with TRG_DAQ_cmds
  UINT16 dsm_data;		// output of last DSM
  UINT16 dsm_address;		// address of raw data for this crossing
  UINT8 busy;			// BUSY at start of this Xing
  UINT8 add_bits;		// bit 7 - fake data; bit 6 - L2.5 abort; bit 1 - priority;  bit 0 - pileup
  UINT16 TRG_word;
  UINT16 npre;
  UINT16 npost;
} ;
#endif


struct DATAP {
	struct bankHeader bh ;
	UINT32 len ;
	UINT32 time ;
	UINT32 seq ;
	UINT32 trg_word ;
	UINT32 trg_in_word ;
	UINT32 detector ;
	struct offlen det[10] ;	// DET_ID "EXT_ID" points to DATAPX if it exists!
        UINT32 TRG_L1_summary[2];
        UINT32 TRG_L2_summary[2];
        UINT32 L3_Summary[4] ;
        struct EventDescriptor2001 evtdes ;
  // total data len is 40 words (header not included)
} ;

struct DATAPX {
	struct bankHeader bh ;
	struct offlen det[22] ;	// index 0 is det_id 10 !!!!
} ;

struct TRGID {
        struct bankHeader bh ;
        UINT32 triggerId[32] ;  // index is the l3 summary bit.
};

struct DUMMYDATA {
	struct bankHeader bh ;
} ;

struct TPCP {
	struct bankHeader bh ;
	struct offlen sb[24] ;	// 24 sectors
} ;

struct TPXP {
  struct bankHeader bh;
  struct offlen sb;
};

struct TPXD {
  struct bankHeader;
  UINT32 data[1];              // size is actually variable...
};


struct TRGP {
        struct bankHeader bh ;
        struct offlen trgData ;
        struct offlen trgId;
};

#if (TRG_VERSION < 0x32)
struct TRGD {
	struct bankHeader	bh ;
	EvtDescData	desc ;
	TrgSumData	sum ;
	RawTrgDet	raw[MAX_RAW_DATA_BLOCKS] ;
} ;
#else
struct TRGD {
	struct bankHeader	bh ;
  /*
	EvtDescData2008	desc ;
	TrgSumData2008	sum ;
	RawTrgDet2008	raw[y8MAX_RAW_DATA_BLOCKS] ;
  */
        TrgTowerTrnfer2008 tow ;
} ;
#endif

// Level 3 structures
// Level 3 banks - most are defined in  file included at end
//

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

// this is a local structure that is just used to allocate space
// it will be recast to the real TPCMZCLD later...
struct TPCMZCLD_local {
	struct bankHeader bh ;
	UINT32 rows ;	// how many "rowlike" objects follow...
	UINT32 padrowFiller[2*MZ_MAX_ROWS] ;	// max 6 padrows, each occupies 2 UINT32s...
	struct mzCentroid centroidFiller[MZ_MAX_CLUSTERS] ;
} ;

// change the name of the "struct row" to something else - Tonko, 2/22/2001
struct TPCADCX {
	struct bankHeader bh ;
	struct rowx {
		UINT32 row ;
		UINT32 offADC ;
		UINT32 offSEQ ;
	} lrow[MZ_MAX_ROWS] ;	// MAX 6 padrows/hybrids in a mezzanine
} ;

struct TPCADCD {
	struct bankHeader bh ;
	UINT8	adc[MZ_MAX_CHANNELS] ;	// this is the worst case
} ;

struct TPCSEQD {
	struct bankHeader bh ;
	UINT16	seq[MZ_MAX_CHANNELS/2] ;	// theoretical maximum
					// see TPCADCD...
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


// structures that are detector dependent

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

struct FTPPADK {
	struct bankHeader bh ;
	UINT32	bytesADC ;
	UINT32	bytesCPP ;
	UINT32	bytesPED ;
	UINT32	bytesRMS ;
	UINT32	bytesCFG ;
	UINT32	bytesGAIN ;
	struct	row_pad rp[MZ_FTP_PADS_PER_MEZ] ;
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
	// used in SVT specific processing...
	UINT32 pre ;
	UINT32 post ;
	UINT32 pedOff ;
} ;


struct DETCFGR {
	struct bankHeader bh ;
	UINT8	feeId[MZ_MAX_PADS] ;
} ;

struct TPCCFGR {
	struct bankHeader bh ;
	UINT8	feeId[MZ_TPC_MAX_PADS_PER_MEZ] ;
} ;

struct FTPCFGR {
	struct bankHeader bh ;
	UINT8	feeId[MZ_FTP_PADS_PER_MEZ] ;
} ;

struct SVTCFGR {
	struct bankHeader bh ;
	UINT8	feeId[MZ_SVT_MAX_PADS_PER_MEZ] ;
} ;


struct DETGAINR {
	struct bankHeader bh ;
	UINT32 events ;
	UINT32 meanGain ;
	struct gain {
		UINT16	t0 ;
		UINT8	t0_rms ;
		UINT8	rel_gain ;
	} gain[MZ_MAX_PADS] ;
	UINT8	trans_table[1024] ;
	UINT16  exp_table[256] ;
} ;

struct DETGAIND {
	struct bankHeader bh ;
	struct gain {
		short	t0 ;
		unsigned short  rel_gain ;
	} gain[MZ_MAX_PADS] ;
	UINT8	trans_table[1024] ;
	UINT16  exp_table[256] ;
} ;


struct SVTGAINR {
	struct bankHeader bh ;
	UINT32 events ;
	UINT32 meanGain ;
	struct gain {
		UINT16	t0 ;
		UINT8	t0_rms ;
		UINT8	rel_gain ;
	} gain[MZ_SVT_MAX_PADS_PER_MEZ] ;
	UINT8	trans_table[1024] ;
	UINT16  exp_table[256] ;
} ;

struct TPCGAINR {
	struct bankHeader bh ;
	UINT32 events ;
	UINT32 meanGain ;
	struct gain {
		UINT16	t0 ;
		UINT8	t0_rms ;
		UINT8	rel_gain ;
	} gain[MZ_TPC_MAX_PADS_PER_MEZ] ;
	UINT8	trans_table[1024] ;
	UINT16  exp_table[256] ;
} ;

struct FTPGAINR {
	struct bankHeader bh ;
	UINT32 events ;
	UINT32 meanGain ;
	struct gain {
		UINT16	t0 ;
		UINT8	t0_rms ;
		UINT8	rel_gain ;
	} gain[MZ_FTP_PADS_PER_MEZ] ;
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

struct FTPBADR {
	struct bankHeader bh ;
	struct row_pad rp[MZ_FTP_PADS_PER_MEZ] ;
} ;

struct SVTBADR {
	struct bankHeader bh ;
	struct row_pad rp[MZ_SVT_MAX_PADS_PER_MEZ] ;
} ;


// RIC stuff
#define RIC_BANK_DATAD	0
#define RIC_BANK_DATAR	1
#define RIC_BANK_PEDR	2
#define RIC_BANK_RMSR	3

struct RICP {
	struct bankHeader bh ;
	struct offlen crams[18] ;
} ;

struct RICCRAMP {
	struct bankHeader bh ;
	struct offlen banks[8] ;
} ;

struct RICDATAD {
	struct bankHeader bh ;
	unsigned short data[2*960] ;
} ;

struct RICDATAR {
	struct bankHeader bh ;
} ;

struct RICPEDR {
	struct bankHeader bh ;
}; 

struct RICRMSR {
	struct bankHeader bh ;
} ;

struct RICTHRER {
	struct bankHeader bh ;
} ;

struct RICBADR {
	struct bankHeader bh ;
} ;




// FPD
#define FPDP_MAX_BANKS	8
// index of the banks in FPDP
#define FPDP_ADCD	0
#define FPDP_TDCD	1
#define FPDP_REGD	2
#define FPDP_PEDR	3
#define FPDP_SCL	4
#define FPDP_BBCDAT	5
#define FPDP_BBCPED	6
#define FPDP_BBCSCL	7


struct FPDP {
	struct bankHeader bh ;
	struct offlen type[FPDP_MAX_BANKS] ;
} ;

struct FPDADCD {
	struct bankHeader bh ;
	unsigned short data[256] ;
} ;

struct FPDTDCD {
	struct bankHeader bh ;
	unsigned short data[8] ;
} ;

struct FPDREGD {
	struct bankHeader bh ;
	unsigned short data[4] ;
} ;

struct FPDPEDR {
	struct bankHeader bh ;
	unsigned short ped[256] ;
	unsigned short rms[256] ;
} ;

struct FPDSCL {
	struct bankHeader bh ;
	unsigned int scl[128] ;
} ;

struct BBCDAT {
	struct bankHeader bh ;
	unsigned short pulse[32] ;
	unsigned short time[32] ;
	unsigned short proof[2] ;
	unsigned short spare[6] ;
} ;

struct BBCPED {
	struct bankHeader bh ;
	unsigned short ped[32] ;
	unsigned short rms[32] ;
	unsigned short peaks[64] ;
} ;

struct BBCSCL {
	struct bankHeader bh ;
	unsigned int scl[32] ;
} ;



// TOF 
// Format Version 2 for FY03
// FOrmat Version ? for FY04
// Format Version 0x50000 for FY05 - RORC! - changed type count from 4 to 8
struct TOFP {
	struct bankHeader bh ;
	struct offlen type[8] ;
} ;

struct TOFADCD {
	struct bankHeader bh ;
	unsigned int data[180] ;	// 48 in FY02, 132 in FY03
} ;

struct TOFTDCD {
	struct bankHeader bh ;
	unsigned int data[184] ;	// 48 in FY02, 120 in FY03
} ;

struct TOFA2DD {
	struct bankHeader bh ;
	unsigned int data[32] ;		// 32 in FY02
} ;

struct TOFSCAD {
	struct bankHeader bh ;
	unsigned int data[12] ;		// 12 in FY02
} ;

struct TOFDDLR {
	struct bankHeader bh ;
	char data[0] ;
} ;

// PMP
// misc. counters
#define PMD_CRAMS_MAX		12
#define PMD_CRAMS_CH_MAX	1728

// bank seqeuence in the SECP bank
#define PMD_ADCD_N	0
#define PMD_PEDR_N	1
#define PMD_RMSR_N	2
#define PMD_THRR_N	3

struct PMDP {
	struct bankHeader bh ;
	struct offlen sec[2] ;
} ;

struct PMDSECP {
	struct bankHeader bh ;
	struct offlen type[4] ;
} ;

struct PMDADCD {
	struct bankHeader bh ;
	UINT32 data[PMD_CRAMS_MAX*2*(1+PMD_CRAMS_CH_MAX)] ;	// variable
} ;

struct PMDPEDR {
	struct bankHeader bh ;
	UINT16 data[PMD_CRAMS_MAX*2*(1+PMD_CRAMS_CH_MAX)] ;	// variable
	
} ;

struct PMDRMSR {
	struct bankHeader bh ;
	UINT16 data[PMD_CRAMS_MAX*2*(1+PMD_CRAMS_CH_MAX)] ;	// variable
} ;

struct PMDTHRR {
	struct bankHeader bh ;
	UINT16 data[PMD_CRAMS_MAX*2*(1+PMD_CRAMS_CH_MAX)] ;	// variable
} ;



// EMC
/*
 Tonko, Nov 2, 2004 -- the Barrel people _finally_ decided
 how the preshower looks like and it will be fibers 9-12
 in the BSMD. This means:
	EMC_FIBER_NUM increases from 8 to 12
	EMC_SEC_NUM stays at 6 but the B_PRE/E_PRE is unused

 NOTE: the FIBER_NUM increase may cause pain for Offline. *shrug*

*/
#define EMC_SEC_NUM	6	// num. of "sections" i.e. subparts
#define EMC_FIBER_NUM	12	// max num of fibers per subemc

// indices in the EMCP
#define EMC_B_TOW	0
#define EMC_B_SMD	1
#define EMC_B_PRE	2
#define EMC_E_TOW	3
#define EMC_E_SMD	4
#define EMC_E_PRE	5

struct EMCP {
	struct bankHeader bh ;
	struct offlen sec[EMC_SEC_NUM] ;	// 6 sections...
} ;

struct EMCSECP {
	struct bankHeader bh ;
	struct offlen fiber[EMC_FIBER_NUM] ;	// for 8 fibers contribs
} ;

struct EMCRBP {
	struct bankHeader bh ;
	struct offlen banks[TPC_MZP_BANKS_NUM] ;	// same number of banks as TPC
} ;


struct PP2PPP {	// main pointer bank
	struct bankHeader bh ;
	struct offlen sec[3] ;
} ;

// WARNING: sizeof() will not give the correct length!
struct PP2PPR {	// void data dump...
	struct bankHeader bh ;
	unsigned int type ;
	unsigned int xing ;
	unsigned int seq ;
        unsigned int hash ;   // adjusted seq * 1000 + adjusted xing 
        unsigned int seqAdj ; // sequence adjuster
        unsigned int xingAdj ; // xing adjuster
	unsigned int ms ;	// miliseconds from the beginning of the run
	unsigned int res[1] ; 
	char data[0] ;	// unknown at compile time...
} ;

// Slow Controls and Friends
#define SCD_FORMAT_VERSION	0x00050001	// Run FY05, version 1
struct SCD {
	struct bankHeader bh ;
	unsigned int time ;	// unix time
	int mag_field ;
	unsigned int rich_scalers[16] ;	// ask Jamie Dunlop
} ;

// Level 3 structures
// Level 3 banks
#include "L3/L3Formats.h"

/*
#if __GNUC__ == 2 && __GNUC_MINOR__ < 96 && defined(__I960)
#pragma pack 0
#pragma align 0
#endif
*/




// Define for linker-level checking only! See comment at the
// end of this file.
//#define VERIFY_SIZES

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

