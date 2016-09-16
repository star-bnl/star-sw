#ifndef _RTS_SYSTEMS_H_
#define _RTS_SYSTEMS_H_

#include <stdio.h>
#include <sys/types.h>
#include <string.h>


/*
	HISTORY:
	* 11/06/2000, Tonko, completelly revamped from daqSystems.h

*/



/* 
	Tonko, 7/13/00, added the Trigger's detector Bitmask (as found in EventDescriptor)
	THIS SHOULD BE MAINTAINED BY TRIGGER!!!!!

	Tonko, 5/8/01, added FPD and moved EMC to 6 and added SMD at old EMCs place
        John,  8/7/01, replaced RICH with SSD and moved down ETOW, ESMD and PMD into
                       region 0-13 which is the region of valid LIVE
        John, 10/23/02 Updated TRG_xxx_INSTANCE and TRG_xxx_NODEID (Jeff consulted)
	Tonko, 10/07/03 Untangled BTOW, BSMD, ETOW & ESMD and retired EMC names...
*/

/* This is _obsolete_ */
#define TRG_TPC_BIT     0

#define TRG_SVT_BIT     1

#define TRG_BSMD_BIT    2

#define TRG_FTPC_BIT    3

#define TRG_TOF_BIT     4

#define TRG_SSD_BIT     5              // used to be RICH

#define TRG_BTOW_BIT    6
#define TRG_EMC_BIT     TRG_BTOW_BIT    // use "BTOW"

#define TRG_FPD_BIT     7

#define TRG_ETOW_BIT    8
#define TRG_EEC_BIT     TRG_ETOW_BIT    // use "ETOW"

#define TRG_ESMD_BIT    9

#define TRG_PMD_BIT    10

//#define TRG_MWC_BIT	11

#define TRG_CTB_BIT    14               // does not have a LIVE bit
 
#define TRG_BBC_BIT    15               // does not have a LIVE bit



/* TCD IDs - slot positions in the TCD crate */
/* 
	SCRATCH that! The ID has nothing to do with the slot
	anymore but is kept for historical reasons.

	The TCD_xxx MUST code the board VME address in this form:

	TCD_ID 6 --> VME 0x10000000 
	TCD_ID 7 --> VME 0x11000000
	...
	TCD_ID 21 -> VME 0x1F000000
*/
#define TCD_ESMD        6	//0x10,
#define TCD_BBC         7	//0x11, trigger-only; unused
#define TCD_ETOW        8	//0x12,
#define TCD_MTD_QT      9	//0x13, trigger-only; unused
#define TCD_IST         10	//0x14, Jun 2013: was FGT before; Aug 26, 2009: was FPD's before
#define TCD_TOF         11      //0x15,
#define TCD_PP          12      //0x16
#define TCD_MTD         13      //0x17
#define TCD_TPX		14	//0x18
#define TCD_BSMD        15      //0x19
#define TCD_CTB         16	//0x1A, trigger-only; unused
#define TCD_BTOW        17      //0x1B
#define TCD_SST         18      //0x1C; was FTPC; gone in Sep '11
#define TCD_PXL         19      //0x1D; was PMD; gone in Sep '11
#define TCD_GMT         20      //0x1E; WAS: empty, Nov, 2008
#define TCD_VPD		21      //0x1F trigger-only; unused



// FY14 Group definitions; Tonko Nov 2013.
#define SST_GRP		0
#define PP_GRP	        1
#define ETOW_GRP	2   
#define BTOW_GRP	3
#define BSMD_GRP	4
#define TOF_GRP	        5
#define ESMD_GRP	6
#define TPX_GRP		7
#define PXL_GRP		8
#define IST_GRP         9
//#define xxx_GRP		10	// unused
//#define xxx_GRP		11	// but still unused
//#define xxx_GRP		12	// unused
//#define xxx_GRP		13	// unused
#define GMT_GRP		14
#define MTD_GRP		15


//FY15, new "evp group" concept; Tonko Feb 2015
#define EVP_GRP_FMS		0	//FMS,FPS: FMS_xxx triggers
#define EVP_GRP_RP		1	//RomanPots RP_xxx triggers
#define EVP_GRP_ZEROBIAS	2	// and similar;



/* RTS Node Id, Tonko, 11/06/2000

The RTS Node Id is a 16 bit identifier which is used to uniquelly determine
a particular node in the STAR RTS tree. At the same time it is used by the
myrinet libraries.

To make life sane these 16 bits are partitioned into 4 subfields according
to the bits:

SYSTEM <= 10 or SYSTEM=15 (regular)

15 14 13 12   11 10  9  8     7  6  5  4   3  2  1  0

SYSTEM--->    ROUTE  SUBSYS   INSTANCE-------------->

10 < SYSTEM < 20  (EXT)

15 14 13 12   11 10  9  8     7  6  5  4   3  2  1  0

EXT------->    x  x  System------------>   INSTANCE->  

20 <= SYSTEM < 32  (EXT2)

15 14 13 12   11 10  9  8     7  6  5  4   3  2  1  0

EXT2------>   SYSTEM---------->  INSTANCE----------->


SYSTEM is 4 bits, ROUTE is 2 bits, SUBSYSTEM is 2 bits and the particular
INSTANCE is 8 bits. The INSTANCE itself can be any number from (0-255) and
thus can additionally be sub-partitioned if this makes sense and adds to
human readability.

SYSTEM is either DAQ, TRG, SC or L3 as well as all the detectors. Generally
it should correspond to the bit in DAQ EVB's DATAP bank under "detectors
presence bits" word.

ROUTE is a Myrinet concept and should be 0 for all the systems where Myrinet
makes no sense.  SUBSYS depends on the SYSTEM and can be used if it makes
sense.  INSTANCE is just the count of nodes. This should generally _not_
start with 0 i.e. node called TPC01 should have a '1' as the INSTANCE.

There are 2 reserved NodeIds:
	0x0000
	0xFFFF

0x0000 is used to mean "self" in various subroutines and 0xFFFF is used to
mean ERROR or UNKNOWN in other misc. parts of code.

*/


/* special nodes */
#define LOCALHOST_NODE	0
#define MY_NODE		0
#define ERROR_NODE	0xFFFF	/* -1 */


/* 
Known RTS systems. For historical reasons, the detectors used to use XXX_ID
so we keep it here for source compatibility 
*/

#define TPC_SYSTEM	0
#define TPC_ID		TPC_SYSTEM	/* retired */

#define	SVT_SYSTEM	1
#define SVT_ID		SVT_SYSTEM	/* retired */

#define TOF_SYSTEM	2
#define TOF_ID		TOF_SYSTEM

/* this is just the Barrel Tower EMC! */
#define BTOW_SYSTEM	3
#define BTOW_ID		BTOW_SYSTEM


/* Frozen as of 8/2002, Tonko */
#define FPD_SYSTEM	4
#define FPD_ID		FPD_SYSTEM

#define FTP_SYSTEM	5
#define FTP_ID		FTP_SYSTEM

/* extendes the post-April 2002 systems */
#define EXT_SYSTEM	6
#define EXT_ID		EXT_SYSTEM

/* Frozen as of 8/2002, Tonko */
#define RIC_SYSTEM	7
#define RIC_ID		RIC_SYSTEM

#define TRG_SYSTEM	8
#define TRG_ID          TRG_SYSTEM

#define L3_SYSTEM	9
#define L3_ID           L3_SYSTEM

#define SC_SYSTEM	10	/* slow controls */
#define SC_ID           SC_SYSTEM

/* Reserver for super-future extensions */
#define EXT2_SYSTEM	11
#define EXT2_ID		EXT2_SYSTEM

#define PMD_SYSTEM	12
#define PMD_ID		PMD_SYSTEM

#define SSD_SYSTEM	13
#define SSD_ID		SSD_SYSTEM	/* retired */

/* Endcap Tower!*/
#define ETOW_SYSTEM	14
#define ETOW_ID		ETOW_SYSTEM

#define DAQ_SYSTEM	15
#define DAQ_ID          DAQ_SYSTEM
#define RTS_SYSTEM	DAQ_SYSTEM	/* global */

// the "new" (2002-03) FPD; Tonko: FP2 was NEVER used -- changing it to FGT in Feb 09!
#define FGT_SYSTEM	16
#define FGT_ID		FGT_SYSTEM

#define PP_SYSTEM       17              /* pp2pp */
#define PP_ID           PP_SYSTEM

#define BSMD_SYSTEM	18
#define BSMD_ID		BSMD_SYSTEM

#define ESMD_SYSTEM	19
#define ESMD_ID		ESMD_SYSTEM

#define TPX_SYSTEM      20
#define TPX_ID          TPX_SYSTEM    /* DAQ1000 detector */

#define PXL_SYSTEM      21
#define PXL_ID          PXL_SYSTEM     /* HFT's Pixel; Used to be HFT_ID */

#define MTD_SYSTEM      22
#define MTD_ID          MTD_SYSTEM     /* Muon Tracking Detector */

#define IST_SYSTEM      23
#define IST_ID          IST_SYSTEM     /* HFT's Inner Silicon Tracker */

#define SST_SYSTEM      24
#define SST_ID          SST_SYSTEM     /* HFT's SSD */

//#define RPII_SYSTEM      25
//#define RPII_ID          RPII_SYSTEM     /* Roman Pots, Phase II */

#define GMT_SYSTEM      26
#define GMT_ID          GMT_SYSTEM     /* GEM Monitor for TPC */

#define L4_SYSTEM       27
#define L4_ID           L4_SYSTEM

#define FPS_SYSTEM       28
#define FPS_ID           FPS_SYSTEM

#define STRXXX_SYSTEM       29
#define STRXXX_ID           STRXXX_SYSTEM

#define RTS_NUM_SYSTEMS	30	/* current maximum. Can not be greater than 32! */

#define PP_SEQE_INSTANCE  1
#define PP_SEQW_INSTANCE  2
#define PP_TRG_INSTANCE 3
#define PP_TEST_INSTANCE 4

// Trigger Detector Bit Mask
#define TRGDET_ZDC 0
#define TRGDET_BBC 1
#define TRGDET_VPD 2
#define TRGDET_TOF 3
#define TRGDET_ETOF 4
#define TRGDET_MTD 5
#define TRGDET_BEMC 6
#define TRGDET_EEMC 7
#define TRGDET_PP2PP 8
#define TRGDET_FMS 9
#define TRGDET_FPS 10

extern inline int getTrgDetBit(char *str) {
    if(strcmp(str, "zdc")==0) return TRGDET_ZDC;
    if(strcmp(str, "bbc")==0) return TRGDET_BBC;
    if(strcmp(str, "vpd")==0) return TRGDET_VPD;
    if(strcmp(str, "tof")==0) return TRGDET_TOF;
    if(strcmp(str, "etof")==0) return TRGDET_ETOF;
    if(strcmp(str, "mtd")==0) return TRGDET_MTD;
    if(strcmp(str, "bemc")==0) return TRGDET_BEMC;
    if(strcmp(str, "eemc")==0) return TRGDET_EEMC;
    if(strcmp(str, "pp2pp")==0) return TRGDET_PP2PP;
    if(strcmp(str, "fms")==0) return TRGDET_FMS;
    if(strcmp(str, "fps")==0) return TRGDET_FPS;
    return -1;
}

extern inline char *getTrgDetBitName(int x) {
    switch(x) {
    case TRGDET_ZDC: return "zdc";
    case TRGDET_BBC: return "bbc";
    case TRGDET_VPD: return "vpd";
    case TRGDET_TOF: return "tof";
    case TRGDET_ETOF: return "etof";
    case TRGDET_MTD: return "mtd";
    case TRGDET_BEMC: return "bemc";
    case TRGDET_EEMC: return "eemc";
    case TRGDET_PP2PP: return "pp2pp";
    case TRGDET_FMS: return "fms";
    case TRGDET_FPS: return "fps";
    }
    return NULL;
}

/*
  Subsystems (These are overloaded for each system)
*/

#define TRG_L1_SUBSYS     1
#define TRG_L2_SUBSYS     2

#define GL3_SUBSYS 1
#define SL3_SUBSYS 2

#define EVB_SUBSYS	1
#define GB_SUBSYS	2
#define EVBL_SUBSYS     3

/*
  Instances (overloaded for each system)
*/

#ifndef RTS_PROJECT_PP
#define DAQMAN_INSTANCE		1
#endif

#define BB_INSTANCE		2

#ifndef RTS_PROJECT_PP
#define EVP_INSTANCE		3
#endif

//#define GB_INSTANCE		4
#define BDB_INSTANCE		4	// we'll keep the BDB controller's node_id the same...

#define TOKEN_MANAGER_INSTANCE	5

#define TCD_LX_INSTANCE		6

#define CONTROLS_INSTANCE	7

//#define BB2_INSTANCE    5
//#define TM_INSTANCE     GB_INSTANCE
//#define EVB_INSTANCE    BB_INSTANCE
//#define EVB02_INSTANCE	BB2_INSTANCE

#define RC_CLIENT_INSTANCE	14
#define CLIENT_INSTANCE		15


#ifdef RTS_PROJECT_PP
#define DAQMAN_INSTANCE BB_INSTANCE
#define EVP_INSTANCE BB_INSTANCE
#endif

#define TRG_INSTANCE         1
#define TRG_L1_INSTANCE      2
#define TRG_L2_INSTANCE      3
#define TRG_TCD_INSTANCE     4
#define TRG_RCC_INSTANCE     5
#define TRG_CTB_INSTANCE     6
#define TRG_MIX_INSTANCE     6
// #define TRG_EEC_INSTANCE     7    jml Aug 21,03
#define TRG_SCALER48_INSTANCE 7
//#define TRG_MIX_INSTANCE     8     jml Nov 20, 2008
#define TRG_BC1_INSTANCE     9
#define TRG_BCE_INSTANCE    10  
#define TRG_BCW_INSTANCE    11
#define TRG_SCALER_INSTANCE 12
#define TRG_BBC_INSTANCE    13
//#define TRG_FPE_INSTANCE    14  
#define TRG_FMS_INSTANCE    15  
#define TRG_L0_INSTANCE		16 // Tonko. Feb25,03
#define  TRG_QT1_INSTANCE  17
#define  TRG_QT2_INSTANCE  18
#define  TRG_QT3_INSTANCE  19
#define  TRG_QT4_INSTANCE  20 
#define TRG_BBQ_INSTANCE 21
#define TRG_MXQ_INSTANCE 22
// #define TRG_FEQ_INSTANCE 23   // JML via JN.  8/29/16
#define TRG_EPQ_INSTANCE 23

#define TRG_TCD_NEW_INSTANCE	24

#define L3EVP_INSTANCE      1
#define L3DISP_INSTANCE     2

/******************** RC/DAQ nodes ****************/
/* Tonko, Jun 6, 2003 - EVB & GB extensions */
/* 
	Tonko Comments: 

	EVB) the Linux based EVBs will be EVB_NODES(1)... and
		we keep the old names...
	GB) the new Linux GB will be GB_NODES(1) and the old MVME
		in the main crate will continue to be called
		BDB_NODE
*/
//#define EVB_NODES(x)	((DAQ_SYSTEM<<12) | (EVB_SUBSYS<<8) | (x))
#define EVBL_NODES(x)   ((DAQ_SYSTEM<<12) | (EVBL_SUBSYS<<8) | (x))
#define EVBX_NODES(x)   ((DAQ_SYSTEM<<12) | (EVB_SUBSYS<<8) | (x))
#define EVBX_NODE_COUNT	10	// was 6 before FY13; in a run epoch, used by monitoring and RICH scalers!

#define MAX_EVB_NODES 10

#define GB_NODES(x)	((DAQ_SYSTEM<<12) | (GB_SUBSYS<<8 ) | (x))
#define GB_NODE		GB_NODES(1)


#define BB_NODE		((DAQ_SYSTEM<<12) | BB_INSTANCE) 

#ifdef RTS_PROJECT_PP

#define DAQMAN_NODE BB_NODE
#define EVP_NODE BB_NODE
#define BB2_NODE BB_NODE

#else

#define DAQMAN_NODE	((DAQ_SYSTEM<<12) | DAQMAN_INSTANCE) 
#define EVP_NODE	((DAQ_SYSTEM<<12) | EVP_INSTANCE) 
#define BB2_NODE	((DAQ_SYSTEM<<12) | BB2_INSTANCE) 


#endif

/* aliases and shortcuts */
#define RC_NODE		DAQMAN_NODE 		/* for the time being...*/
#define LOG_NODE        DAQMAN_NODE
#define MON_NODE        DAQMAN_NODE


#define RC_CLIENT_NODE ((DAQ_SYSTEM<<12) | RC_CLIENT_INSTANCE)
#define CLIENT_NODE     ((DAQ_SYSTEM<<12) | CLIENT_INSTANCE)

// Tonko: changed this, Nov 4, 2003
//#define EVB01_NODE      BB_NODE
//#define EVB02_NODE      BB2_NODE
//#define EVB_NODE	EVB01_NODE
// this will also go away!
//#define EVB_NODE	EVB_NODES(1)


#define BDB_NODE	((DAQ_SYSTEM<<12) | BDB_INSTANCE) // old GB...

#define TOKEN_MANAGER_NODE	((DAQ_SYSTEM<<12) | TOKEN_MANAGER_INSTANCE) 

#define TCD_LX_NODE	((DAQ_SYSTEM<<12) | TCD_LX_INSTANCE)

#define CONTROLS_NODE	((DAQ_SYSTEM<<12) | CONTROLS_INSTANCE)

/* singular detectors */
#define RIC01_NODE	((RIC_SYSTEM<<12) | 1)
#define RIC02_NODE	((RIC_SYSTEM<<12) | 2)
#define RIC_NODE	RIC01_NODE

#define TOF01_NODE	((TOF_SYSTEM<<12) | 1)	// DAQ end
#define TOF02_NODE	((TOF_SYSTEM<<12) | 2)	// Platform
#define TOF03_NODE	((TOF_SYSTEM<<12) | 3)	// new, test node
#define TOF_NODE	TOF01_NODE
#define TOF_NODES	((TOF_SYSTEM<<12) | (x))	// shorthand

#define FPD01_NODE	((FPD_SYSTEM<<12) | 1)
#define FPD02_NODE	((FPD_SYSTEM<<12) | 2)
#define FPD_NODE	FPD01_NODE
/* added IP steering */
#define FPD_DEST_HOST	"gb.daq.bnl.local"
#define FPD_PORT	5211

/* multi-node detectors */
#define TPC_NODES(x)	((TPC_SYSTEM<<12) | (x)) 

#define FTP_NODES(x)	((FTP_SYSTEM<<12) | (x)) 

#define SVT_NODES(x)	((SVT_SYSTEM<<12) | (x)) 

/* separated the Endcap (EEC) from the Barrel (EMC), Tonko, 4/4/2002 */
/* Barrel */

#define BTOW_NODE	((BTOW_SYSTEM<<12) | 1)

#define BSMD_NODE	((EXT_SYSTEM<<12)|(BSMD_SYSTEM<<4)|1)
#define BSMD_NODES(x)	((EXT_SYSTEM<<12)|(BSMD_SYSTEM<<4) | (x)) /* Tonko, split into 3 crates */

#define BPRE_NODE	((BTOW_SYSTEM<<12) | 2)	/* NOT really known yet! */

/* Extended (post April 2002) Detectors */
#define PMD01_NODE	((EXT_SYSTEM<<12)|(PMD_SYSTEM<<4) | 1)
#define PMD02_NODE	((EXT_SYSTEM<<12)|(PMD_SYSTEM<<4) | 2)
#define PMD03_NODE	((EXT_SYSTEM<<12)|(PMD_SYSTEM<<4) | 3)
#define PMD_NODES(x)	((EXT_SYSTEM<<12)|(PMD_SYSTEM<<4) | (x)) 
/* the main node is PMD03 */
#define PMD_NODE	PMD03_NODE
/* Added TCP/IP steering */
#define PMD_DEST_HOST	"gb.daq.bnl.local"
#define PMD_PORT_1	5201
#define PMD_PORT_2	5202	// becomes 5202 soon!

#define SSD01_NODE	((EXT_SYSTEM<<12)|(SSD_SYSTEM<<4) | 1)
#define SSD_NODE	SSD01_NODE


#define FGT01_NODE	((EXT_SYSTEM<<12)|(FGT_SYSTEM<<4) | 1)
#define FGT_NODE	FGT01_NODE


/* Endcap */

#define ETOW_NODE	((EXT_SYSTEM<<12)|(ETOW_SYSTEM<<4)|1)
#define ESMD_NODE	((EXT_SYSTEM<<12)|(ESMD_SYSTEM<<4)|1)


/* Level III */
#define L3_NODES(x)	((L3_SYSTEM<<12) | (SL3_SUBSYS<<8) | (x)) 
#define SL3_NODES(x)	((L3_SYSTEM<<12) | (SL3_SUBSYS<<8) | (x)) 
#define GL3_NODES(x)	((L3_SYSTEM<<12) | (GL3_SUBSYS<<8) | (x)) 
#define GL3_NODE	GL3_NODES(1) 
#define GL3_NODE_COUNT	10		// maximum count of nodes per run epoch

#define L3EVP_NODE      ((L3_SYSTEM<<12) | 1)
#define L3DISP_NODE     ((L3_SYSTEM<<12) | 2)

#define L4_NODES(x)     ((EXT2_SYSTEM<<12) | (L4_SYSTEM<<7) | (x))
#define L4_CAL_INSTANCE  120
#define L4_EVP_INSTANCE  121
#define L4_CAL_NODE     ((EXT2_SYSTEM<<12) | (L4_SYSTEM<<7) | L4_CAL_INSTANCE)
#define L4_EVP          ((EXT2_SYSTEM<<12) | (L4_SYSTEM<<7) | L4_EVP_INSTANCE)
#define L4_EVP_NODE     L4_EVP
#define L4_EVB(x)       ((EXT2_SYSTEM<<12) | (L4_SYSTEM<<7) | (L4_EVP_INSTANCE + 1 + x))

/* Trigger */
#define TRG_NODE        ((TRG_SYSTEM<<12) | TRG_INSTANCE)
#define TRG_L1_NODE         ((TRG_SYSTEM<<12) | TRG_L1_INSTANCE)
//#define TRG_L2_NODE         ((TRG_SYSTEM<<12) | TRG_L2_INSTANCE)
#define TRG_L2_NODE         ((TRG_SYSTEM<<12) | (TRG_L2_SUBSYS<<8) | (1))
#define TRG_TCD_NODE        ((TRG_SYSTEM<<12) | TRG_TCD_INSTANCE)
#define TRG_RCC_NODE        ((TRG_SYSTEM<<12) | TRG_RCC_INSTANCE)
#define TRG_CTB_NODE        ((TRG_SYSTEM<<12) | TRG_CTB_INSTANCE)
#define TRG_MIX_NODE       ((TRG_SYSTEM<<12) | TRG_MIX_INSTANCE)
#define TRG_L1_NODES(x)     ((TRG_SYSTEM<<12) | (TRG_L1_SUBSYS<<8) | (x))
#define TRG_L2_NODES(x)     ((TRG_SYSTEM<<12) | (TRG_L2_SUBSYS<<8) | (x))
// #define TRG_MIX_NODE	((TRG_SYSTEM<<12) | TRG_MIX_INSTANCE)     jml 
#define TRG_BC1_NODE        ((TRG_SYSTEM<<12) | TRG_BC1_INSTANCE)
#define TRG_BCE_NODE        ((TRG_SYSTEM<<12) | TRG_BCE_INSTANCE)
#define TRG_BCW_NODE        ((TRG_SYSTEM<<12) | TRG_BCW_INSTANCE)
#define TRG_SCALER_NODE ((TRG_SYSTEM<<12) | TRG_SCALER_INSTANCE)
#define TRG_BBC_NODE        ((TRG_SYSTEM<<12) | TRG_BBC_INSTANCE)
//#define TRG_FPE_NODE        ((TRG_SYSTEM<<12) | TRG_FPE_INSTANCE)
#define TRG_FMS_NODE        ((TRG_SYSTEM<<12) | TRG_FMS_INSTANCE)
#define TRG_L0_NODE		((TRG_SYSTEM<<12) | TRG_L0_INSTANCE)	// Tonko, Feb25,03
#define  TRG_QT1_NODE   ((TRG_SYSTEM<<12) | TRG_QT1_INSTANCE)
#define  TRG_QT2_NODE   ((TRG_SYSTEM<<12) | TRG_QT2_INSTANCE)
#define  TRG_QT3_NODE   ((TRG_SYSTEM<<12) | TRG_QT3_INSTANCE)
#define  TRG_QT4_NODE   ((TRG_SYSTEM<<12) | TRG_QT4_INSTANCE)
#define TRG_BBQ_NODE    ((TRG_SYSTEM<<12) | TRG_BBQ_INSTANCE)
#define TRG_MXQ_NODE    ((TRG_SYSTEM<<12) | TRG_MXQ_INSTANCE)
//#define TRG_FEQ_NODE    ((TRG_SYSTEM<<12) | TRG_FEQ_INSTANCE)
#define TRG_FEQ_NODE    ((TRG_SYSTEM<<12) | TRG_EPQ_INSTANCE)
#define TRG_FQ1_NODE    ((TRG_SYSTEM<<12) | TRG_FQ1_INSTANCE)
#define TRG_FQ2_NODE    ((TRG_SYSTEM<<12) | TRG_FQ2_INSTANCE)


#define TRG_SCALER48_NODE  ((TRG_SYSTEM<<12) | TRG_SCALER48_INSTANCE)

/* Temporary... for zoran...*/
/* #define TDI_NODE ((TRG_SYSTEM<<12) | TRG_TDI_INSTANCE) */

#define TRG_TCD_NEW_NODE ((TRG_SYSTEM<<12) | TRG_TCD_NEW_INSTANCE)

#define L201_NODE TRG_L2_NODES(1)

/* Slow Controls */
#define SC_NODE         ((SC_SYSTEM<<12) | 1)

/* PP2PP */ /* IGNORED for STAR 2007+ runs! */
#define PP_SEQE_NODE  ((EXT_SYSTEM<<12) | ((PP_SYSTEM)<<4) | PP_SEQE_INSTANCE)

/* this is known as Sector1 aka ppdaq2 aka Yellow */
#define PP_SEQ1_NODE	PP_SEQE_NODE

#define PP_SEQW_NODE  ((EXT_SYSTEM<<12) | ((PP_SYSTEM)<<4) | PP_SEQW_INSTANCE)
/* this is known as Sector2 aka ppdaq5 aka Blue */
#define PP_SEQ2_NODE	PP_SEQW_NODE

#define PP_TRG_NODE  ((EXT_SYSTEM<<12) | ((PP_SYSTEM)<<4) | PP_TRG_INSTANCE)
#define PP_TEST_NODE  ((EXT_SYSTEM<<12) | ((PP_SYSTEM)<<4) | PP_TEST_INSTANCE)

/* Tonko, Aug 2007, ready for 2007 */
#define PP_NODES(x) ((EXT_SYSTEM<<12) | ((PP_SYSTEM)<<4) | (x))

#define TPX_NODES(x)     ((EXT2_SYSTEM<<12) | (TPX_SYSTEM<<7) | (x))
#define MTD_NODES(x)	 ((EXT2_SYSTEM<<12) | (MTD_SYSTEM<<7) | (x))

#define PXL_NODES(x)     ((EXT2_SYSTEM<<12) | (PXL_SYSTEM<<7) | (x))
#define IST_NODES(x)     ((EXT2_SYSTEM<<12) | (IST_SYSTEM<<7) | (x))
#define SST_NODES(x)     ((EXT2_SYSTEM<<12) | (SST_SYSTEM<<7) | (x))
//#define RPII_NODES(x)     ((EXT2_SYSTEM<<12) | (RPII_SYSTEM<<7) | (x))

#define GMT_NODES(x)     ((EXT2_SYSTEM<<12) | (GMT_SYSTEM<<7) | (x))

#define FPS_NODES(x)     ((EXT2_SYSTEM<<12) | (FPS_SYSTEM<<7) | (x))

#define STRXXX_NODES(x)  ((EXT2_SYSTEM<<12) | (STRXXX_SYSTEM<<7) | (x))

extern inline const char *rts2name(int rts_id)
{
	switch(rts_id) {
	case TPC_SYSTEM :
		return "TPC" ;
	case SVT_SYSTEM :
		return "SVT" ;
	case TOF_SYSTEM :
		return "TOF" ;
	case BTOW_SYSTEM :
		return "BTOW" ;
	case FPD_SYSTEM :
		return "FPD" ;
	case FTP_SYSTEM :
		return "FTP" ;
	case PMD_SYSTEM :
		return "PMD" ;
	case SSD_SYSTEM :
		return "SSD" ;
	case ETOW_SYSTEM :
		return "ETOW" ;
	case FGT_SYSTEM :	// watch it!
		return "FGT" ;
	case BSMD_SYSTEM :
		return "BSMD" ;
	case ESMD_SYSTEM :
		return "ESMD" ;
	case DAQ_SYSTEM :
		return "DAQ" ;
	case TRG_SYSTEM :
		return "TRG" ;
	case L3_SYSTEM :
		return "L3" ;
	case SC_SYSTEM :
		return "SC" ;
	case TPX_SYSTEM :
	        return "TPX" ;
	case PXL_SYSTEM :
	        return "PXL" ;
	case PP_SYSTEM :
	        return "PP2PP" ;
	case RIC_SYSTEM :
		return "RICH" ;
	case MTD_SYSTEM :
		return "MTD" ;
	case IST_SYSTEM :
		return "IST" ;
	case SST_SYSTEM :
		return "SST" ;
//	case RPII_SYSTEM :
//		return "RPII" ;
	case GMT_SYSTEM :
		return "GMT" ;
	case L4_SYSTEM :
		return "L4" ;
	case FPS_SYSTEM :
		return "FPS" ;
	case STRXXX_SYSTEM :
		return "STRXXX" ;
	default :
	  return (const char *)NULL ;	// unknown!
	}
} ;

extern inline const char *rts2sfs_name(int rts_id)
{
	switch(rts_id) {
	case TPC_SYSTEM :
		return "tpc" ;
	case SVT_SYSTEM :
		return "SVT" ;
	case TOF_SYSTEM :
		return "tof" ;
	case BTOW_SYSTEM :
		return "btow" ;
	case FPD_SYSTEM :
		return "FPD" ;
	case FTP_SYSTEM :
		return "FTP" ;
	case PMD_SYSTEM :
		return "PMD" ;
	case SSD_SYSTEM :
		return "SSD" ;
	case ETOW_SYSTEM :
		return "etow" ;
	case FGT_SYSTEM :	
		return "fgt" ;
	case BSMD_SYSTEM :
		return "bsmd" ;
	case ESMD_SYSTEM :
		return "esmd" ;
	case DAQ_SYSTEM :
		return "DAQ" ;
	case TRG_SYSTEM :
		return "trg" ;
	case L3_SYSTEM :
		return "l3" ;
	case SC_SYSTEM :
		return "sc" ;
	case TPX_SYSTEM :
	        return "tpx" ;
	case PXL_SYSTEM :
	        return "pxl" ;
	case PP_SYSTEM :
	        return "pp2" ;
	case RIC_SYSTEM :
		return "RICH" ;
	case MTD_SYSTEM :
		return "mtd" ;
	case IST_SYSTEM :
		return "ist" ;
	case SST_SYSTEM :
		return "sst" ;
//	case RPII_SYSTEM :
//		return "rpii" ;
	case GMT_SYSTEM :
		return "gmt" ;
	case L4_SYSTEM :
		return "l4" ;
	case FPS_SYSTEM :
		return "fps" ;
	case STRXXX_SYSTEM :
		return "strxxx";
	default :
	  return (const char *)NULL ;	// unknown!
	}
} ;

#ifndef __vxworks
extern inline int name2rts(const char *name)
{
	for(int rts_id=0;rts_id<32;rts_id++) {
		const char *r_name = rts2name(rts_id) ;
		if(r_name && name && (strncasecmp(name,r_name,strlen(r_name))==0)) return rts_id ;
	}

	return -1 ;
}
#endif


/* return >= 0 only in case of real detectors */
/* Why do we need this?? Tonko. */
extern inline int rts2det(int ix)
{
	switch(ix) {
	case RIC_ID :
	case TPC_ID :
	case SVT_ID :
	case TOF_ID :
	case BTOW_ID :
	case FPD_ID :
	case FTP_ID :
	case PMD_ID :
	case SSD_ID :
	case ETOW_ID :
	case BSMD_ID :
	case ESMD_ID :
	case TPX_ID :
	case PXL_ID :
	case PP_ID :
	case FGT_ID :
	case MTD_ID :
	case IST_ID :
	case SST_ID :
//	case RPII_ID :
	case GMT_ID :
	case FPS_ID:
	case STRXXX_ID:
		return ix ;
	default :
		return -1 ;
	}

}

extern inline int rts2tcd(int rts)
{
	static const int map[32] = {
		-1,		//0 TPC gone...
		-1,		//1 SVT gone...
		TCD_TOF,	//2
		TCD_BTOW,	//3
		-1,		//4: FPD gone...
		-1,		//5: FTPC gone
		-1,		//6
		-1,		//7
		-1,		//8
		-1,		//9
		-1,		//10
		-1,		//11
		-1,		//12: PMD gone
		-1,		//13: SSD gone...
		TCD_ETOW,	//14
		-1,		//15: generic DAQ
		-1,		//16: was FGT...
		TCD_PP,		//17
		TCD_BSMD,	//18
		TCD_ESMD,	//19
		TCD_TPX,	//20
		TCD_PXL,	//21 PXL
		TCD_MTD,	//22 MTD
		TCD_IST,	//23 IST
		TCD_SST,	//24 SST
		-1,		//25 
		TCD_GMT,	//26 GMT
		-1,		//27
		-1,		//28
		-1,		//29
		-1,		//30
		-1,		//31
	} ;

	if(rts < 0) return -1 ;
	if(rts >31) return -1 ;

	return map[rts] ;

}

extern inline int tcd2rts(int tcd)
{
	static int map[32] = {
        -1,		//0
        -1,		//1
        -1,		//2
        -1,		//3
        -1,		//4
        -1,		//5
        ESMD_SYSTEM,	//6
        -1,		//7 BBC
        ETOW_SYSTEM,	//8
        -1,		//9 ; used for MTD_QT, was SSD?
        IST_SYSTEM,	//10 ; moved from FGT; moved from FPD
        TOF_SYSTEM,	//11
        PP_SYSTEM,	//12 ; moved from SVT_SYSTem to PP!
        MTD_SYSTEM,	//13 was EMPTY in FY10, MTD in FY11
        TPX_SYSTEM,	//14
        BSMD_SYSTEM,	//15
        -1,		//16 CTB aka ZDC
        BTOW_SYSTEM,	//17
        SST_SYSTEM,	//18
        PXL_SYSTEM,	//19
        GMT_SYSTEM,	//20 GMT; EMPTY until Aug 11; TPC was here... removed Sep 08
        -1,		//21 VPD
        -1,		//22
        -1,		//23
        -1,		//24
        -1,		//25
        -1,		//26
        -1,		//27
        -1,		//28
        -1,		//29
        -1,		//30
        -1		//31
	};

//	LOG(WARN,"tcd2rts: tcd %d, rts %d",tcd,map[tcd],0,0,0) ;
	return map[tcd] ;
} ;


// BTOW, ETOW now part of trigger:   jan 2008
#define LEGACY_DETS (1<<FTP_ID)
#define DAQ1000_DETS ((1<<TPX_ID) | (1<<TOF_ID) | (1<<PXL_ID) | (1<<PMD_ID) | (1<<ESMD_ID) | (1<<PP_ID) | (1<<FGT_ID) | \
		      (1<<L3_ID) | (1 << BSMD_ID) | (1 << MTD_ID) | (1<<IST_ID) | (1<<SST_ID) | (1<<GMT_ID) | (1<<BTOW_ID) | (1<<ETOW_ID)) | (1<<FPS_ID) |\
			(1<<STRXXX_ID) 

// 2009... unused dets:  SSD/SVT/TPC/PMD/HFT --->  FTPGROUP
extern inline u_int grp2rts_mask(int grp)
{
	u_int ret ;

	ret = 0 ;

	if(grp & (1<<SST_GRP)) {
	  ret  |= (1<<SST_SYSTEM) ;
	}
	if(grp & (1 << PP_GRP)) {
	  ret |= (1 << PP_SYSTEM);
	}
	if(grp & (1 << ETOW_GRP)) {
	  ret |= (1 << ETOW_SYSTEM) ;
	}
	if(grp & (1 << BTOW_GRP)) {
	  ret |= (1 << BTOW_SYSTEM) ;
	}
	if(grp & (1 << BSMD_GRP)) {
	  ret |= (1 << BSMD_SYSTEM) ;
	}
	if(grp & (1 << TOF_GRP)) {
	  ret |= (1 << TOF_SYSTEM)  ;	
	}
	if(grp & (1 << ESMD_GRP)) {
	  ret |= (1 << ESMD_SYSTEM) ;
	}
	if(grp & (1 << TPX_GRP)) {
	  ret |= (1 << TPX_SYSTEM);
	}
	if(grp & (1 << PXL_GRP)) {
	  ret |= (1 << PXL_SYSTEM);
	}
	if(grp & (1 << IST_GRP)) {
	  ret |= (1 << IST_SYSTEM);
	}
	if(grp & (1 << MTD_GRP)) {
	  ret |= (1 << MTD_SYSTEM);
	}
	if(grp & (1 << GMT_GRP)) {
	  ret |= (1 << GMT_SYSTEM);
	}

	return ret ;
}

// goes from the RTS_SYSTEM id to the GRP id
extern inline int rts2grp(int rts)
{
  switch(rts)
    {
	// Instance doesn't matter...
	case PP_ID:
		return PP_GRP;
	case ETOW_ID:
		return ETOW_GRP;
	case ESMD_ID:
		return ESMD_GRP;
	case BTOW_ID:
		return BTOW_GRP;
	case BSMD_ID:
		return BSMD_GRP;
	case TOF_ID:
		return TOF_GRP;
	case TPX_ID:
		return TPX_GRP;
	case FGT_ID:
		return IST_GRP;
	case MTD_ID:
		return MTD_GRP;
	case GMT_ID :
		return GMT_GRP;
	// Tonko, Aug 11: filled them all
	case SST_ID :
		return SST_GRP;
	case PXL_ID :
		return PXL_GRP;
	case IST_ID :
		return IST_GRP ;
//	case RPII_ID :
//		return RPII_GRP ;
	default:
		return 31 ;	// this is an ERROR since groups < 16
   }
}


/*
  Inverse's...

  MODIFIED FOR POST-APRIL02 EXT. DETECTORS! Tonko
*/

extern inline int GET_NODE(int sys, int subsys, int inst)
{
  int node ;

  if((sys <= 10) || (sys==15)) {
    node = (sys<<12) | (subsys<<8) | (inst);
  }
  else if (sys < 20) {
    node = (EXT_SYSTEM << 12) | (sys<<4) | (inst);   
  }
  else {
    node = (EXT2_SYSTEM << 12) | (sys<<7) | (inst);
  }

  return node ;
} ;

extern inline int GET_SYSTEM(unsigned short node)
{
  int id;
  id = (node & 0xf000) >> 12;
  
  if(id == EXT_SYSTEM) return (node & 0x03f0) >> 4;
  if(id == EXT2_SYSTEM) return (node & 0x0f80) >> 7;
  return id;
}

extern inline int GET_INSTANCE(unsigned short node)
{
  int id;

  id = (node & 0xf000) >> 12;
  if(id == EXT_SYSTEM) return node & 0xf;
  if(id == EXT2_SYSTEM) return node & 0x7f;
  return node & 0xff;
}

extern inline int GET_SUBSYSTEM(unsigned short node)
{
  int id = (node & 0xf000) >> 12;
  
  if(id == EXT_SYSTEM) return 0;
  if(id == EXT2_SYSTEM) return 0;
  return (node & 0x0300) >> 8;
}
	
#define GET_NODE_PRE_APR2002(sys,subsys,inst)  ((sys<<12) | (subsys<<8) | (inst))
#define GET_SYSTEM_PRE_APR2002(x)    (((x) >> 12) & 0xf)
#define GET_INSTANCE_PRE_APR2002(x)  ((x) & 0xff)
#define GET_SUBSYSTEM_PRE_APR2002(x) (((x) >> 8) & 0x3)

#define GET_ROUTE(x)     (((x) >> 10) & 0x3)

#endif /* _RTS_SYSTEMS_H_ */



