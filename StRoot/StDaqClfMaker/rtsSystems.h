#ifndef _RTS_SYSTEMS_H_
#define _RTS_SYSTEMS_H_

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
*/
#define TRG_TPC_BIT     0

#define TRG_SVT_BIT     1

#define TRG_BSMD_BIT    2
#define TRG_SMD_BIT     TRG_BSMD_BIT   // use "BSMD"

#define TRG_FTPC_BIT    3

#define TRG_TOF_BIT     4

#define TRG_SSD_BIT     5              // used to be RICH

#define TRG_BTOW_BIT    6
#define TRG_EMC_BIT     TRG_BTOW_BIT    // use "BTOW"

#define TRG_FPD_BIT     7

#define TRG_ETOW_BIT    8
#define TRG_ESMD_BIT    9
#define TRG_EEC_BIT     TRG_ETOW_BIT    // use "ETOW"

#define TRG_PMD_BIT    10

#define TRG_CTB_BIT    14               // does not have a LIVE bit
 
#define TRG_BBC_BIT    15               // does not have a LIVE bit


/* RTS Node Id, Tonko, 11/06/2000

The RTS Node Id is a 16 bit identifier which is used to uniquelly determine
a particular node in the STAR RTS tree. At the same time it is used by the
myrinet libraries.

To make life sane these 16 bits are partitioned into 4 subfields according
to the bits:

15 14 13 12   11 10  9  8     7  6  5  4   3  2  1  0

SYSTEM--->    ROUTE  SUBSYS   INSTANCE-------------->


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
#define TPC_ID		TPC_SYSTEM

#define	SVT_SYSTEM	1
#define SVT_ID		SVT_SYSTEM

#define TOF_SYSTEM	2
#define TOF_ID		TOF_SYSTEM

/* this is just the barrel EMC! */
#define EMC_SYSTEM	3
#define EMC_ID		EMC_SYSTEM

/* let's add the FPD too, Tonko, 1/11/01 */
#define FPD_SYSTEM	4
#define FPD_ID		FPD_SYSTEM

#define FTP_SYSTEM	5
#define FTP_ID		FTP_SYSTEM

/* extendes the post-April 2002 systems */
#define EXT_SYSTEM	6
#define EXT_ID		EXT_SYSTEM

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
#define SSD_ID		SSD_SYSTEM

/* Endcap */
#define EEC_SYSTEM	14
#define EEC_ID		EEC_SYSTEM


#define DAQ_SYSTEM	15
#define DAQ_ID          DAQ_SYSTEM
#define RTS_SYSTEM	DAQ_SYSTEM	/* global */

/*
  Subsystems (These are overloaded for each system)
*/

#define TRG_L1_SUBSYS     1
#define TRG_L2_SUBSYS     2

#define GL3_SUBSYS 1
#define SL3_SUBSYS 2


/*
  Instances (overloaded for each system)
*/

#define DAQMAN_INSTANCE 1
#define BB_INSTANCE     2
#define EVP_INSTANCE    3
#define GB_INSTANCE     4
#define BB2_INSTANCE    5
#define TM_INSTANCE     GB_INSTANCE
#define EVB_INSTANCE    BB_INSTANCE
#define EVB02_INSTANCE	BB2_INSTANCE

#define TRG_INSTANCE         1
#define TRG_L1_INSTANCE      2
#define TRG_L2_INSTANCE      3
#define TRG_TCD_INSTANCE     4
#define TRG_RCC_INSTANCE     5
#define TRG_CTB_INSTANCE     6
#define TRG_EMC_INSTANCE     7
#define TRG_MWC_INSTANCE     8
#define TRG_BMC_INSTANCE     9
#define TRG_BME_INSTANCE    10
#define TRG_BMW_INSTANCE    11 
#define TRG_SCALER_INSTANCE 12
#define TRG_BBC_INSTANCE    13
#define TRG_FPE_INSTANCE    14  
#define TRG_FPW_INSTANCE    15  

#define L3EVP_INSTANCE      1
#define L3DISP_INSTANCE     2


/******************** RC/DAQ nodes ****************/
#define DAQMAN_NODE	((DAQ_SYSTEM<<12) | DAQMAN_INSTANCE) 
#define BB_NODE		((DAQ_SYSTEM<<12) | BB_INSTANCE) 
#define BB2_NODE	((DAQ_SYSTEM<<12) | BB2_INSTANCE) 
#define EVP_NODE	((DAQ_SYSTEM<<12) | EVP_INSTANCE) 
#define GB_NODE		((DAQ_SYSTEM<<12) | GB_INSTANCE)

/* aliases and shortcuts */
#define RC_NODE		DAQMAN_NODE 		/* for the time being...*/
#define LOG_NODE        DAQMAN_NODE
#define MON_NODE        DAQMAN_NODE

#define TM_NODE		GB_NODE

#define EVB01_NODE      BB_NODE
#define EVB02_NODE      BB2_NODE
#define EVB_NODE	EVB01_NODE


/* singular detectors */
#define RIC01_NODE	((RIC_SYSTEM<<12) | 1)
#define RIC02_NODE	((RIC_SYSTEM<<12) | 2)
#define RIC_NODE	RIC01_NODE

#define TOF01_NODE	((TOF_SYSTEM<<12) | 1)
#define TOF02_NODE	((TOF_SYSTEM<<12) | 2)
#define TOF_NODE	TOF01_NODE


#define FPD01_NODE	((FPD_SYSTEM<<12) | 1)
#define FPD02_NODE	((FPD_SYSTEM<<12) | 2)
#define FPD_NODE	FPD01_NODE

/* multi-node detectors */
#define TPC_NODES(x)	((TPC_SYSTEM<<12) | (x)) 

#define FTP_NODES(x)	((FTP_SYSTEM<<12) | (x)) 

#define SVT_NODES(x)	((SVT_SYSTEM<<12) | (x)) 

/* separated the Endcap (EEC) from the Barrel (EMC), Tonko, 4/4/2002 */
/* Barrel */
#define BTOW_INSTANCE	1
#define BSMD_INSTANCE	2
#define BPRE_INSTANCE	3	/* preshower - doesn't exist yet... */

#define EMC_NODES(x)	((EMC_SYSTEM<<12) | (x))

#define BTOW_NODE	EMC_NODES(BTOW_INSTANCE)
#define BSMD_NODE	EMC_NODES(BSMD_INSTANCE)
#define BPRE_NODE	EMC_NODES(BPRE_INSTANCE)	

/* Extended (post April 2002) Detectors */
#define PMD01_NODE	((EXT_SYSTEM<<12)|(PMD_SYSTEM<<4) | 1)
#define PMD02_NODE	((EXT_SYSTEM<<12)|(PMD_SYSTEM<<4) | 2)
#define PMD03_NODE	((EXT_SYSTEM<<12)|(PMD_SYSTEM<<4) | 3)
#define PMD_NODES(x)	((EXT_SYSTEM<<12)|(PMD_SYSTEM<<4) | (x)) 
/* the main node is PMD03 */
#define PMD_NODE	PMD03_NODE

#define SSD01_NODE	((EXT_SYSTEM<<12)|(SSD_SYSTEM<<4) | 1)
#define SSD_NODE	SSD01_NODE

/* Endcap */
#define ETOW_INSTANCE	1
#define ESMD_INSTANCE	2

#define EEC_NODES(x)	((EXT_SYSTEM<<12)|(EEC_SYSTEM<<4) | (x))

#define ETOW_NODE	EEC_NODES(ETOW_INSTANCE)	// for _all_ Endcap
#define EEC_NODE	ETOW_NODE			// Endcap EMC shortcut if only one...

#define ESMD_NODE	EEC_NODES(ESMD_INSTANCE)	



/* Level III */
#define L3_NODES(x)	((L3_SYSTEM<<12) | (SL3_SUBSYS<<8) | (x)) 
#define GL3_NODES(x)	((L3_SYSTEM<<12) | (GL3_SUBSYS<<8) | (x)) 
#define L3EVP_NODE      ((L3_SYSTEM<<12) | 1)
#define L3DISP_NODE     ((L3_SYSTEM<<12) | 2)
#define GL3_NODE	GL3_NODES(1) 

/* Trigger */
#define TRG_NODE        ((TRG_SYSTEM<<12) | TRG_INSTANCE)
#define L1_NODE         ((TRG_SYSTEM<<12) | TRG_L1_INSTANCE)
#define L2_NODE         ((TRG_SYSTEM<<12) | TRG_L2_INSTANCE)
#define TCD_NODE        ((TRG_SYSTEM<<12) | TRG_TCD_INSTANCE)
#define RCC_NODE        ((TRG_SYSTEM<<12) | TRG_RCC_INSTANCE)
#define CTB_NODE        ((TRG_SYSTEM<<12) | TRG_CTB_INSTANCE)
#define L1_NODES(x)     ((TRG_SYSTEM<<12) | (TRG_L1_SUBSYS<<8) | (x))
#define L2_NODES(x)     ((TRG_SYSTEM<<12) | (TRG_L2_SUBSYS<<8) | (x))
#define EMC_NODE        ((TRG_SYSTEM<<12) | TRG_EMC_INSTANCE)
#define MWC_NODE	((TRG_SYSTEM<<12) | TRG_MWC_INSTANCE)
#define BMC_NODE        ((TRG_SYSTEM<<12) | TRG_BMC_INSTANCE)
#define BME_NODE        ((TRG_SYSTEM<<12) | TRG_BME_INSTANCE)
#define BMW_NODE        ((TRG_SYSTEM<<12) | TRG_BMW_INSTANCE)
#define TRG_SCALER_NODE ((TRG_SYSTEM<<12) | TRG_SCALER_INSTANCE)
#define BBC_NODE        ((TRG_SYSTEM<<12) | TRG_BBC_INSTANCE)
#define FPE_NODE        ((TRG_SYSTEM<<12) | TRG_FPE_INSTANCE)
#define FPW_NODE        ((TRG_SYSTEM<<12) | TRG_FPW_INSTANCE)

#define L201_NODE L2_NODES(1)

/* Slow Controls */
#define SC_NODE         ((SC_SYSTEM<<12) | 1)

/*
  Inverse's...

  MODIFIED FOR POST-APRIL02 EXT. DETECTORS! Tonko
*/
extern inline int GET_NODE(int sys, int subsys, int inst)
{
	int node ;

	if((sys <= 10) || (sys==15)) {	// pre-Apr2002
		node = ((sys<<12) | (subsys<<8) | (inst)) ;
	}
	else {
	  //	node = 0x6000 | (inst);    
	  node = 0x6000 | (sys<<4) | (inst);   // need sys -- 18APR02, jml
	}

	return node ;
} ;

extern inline int GET_SYSTEM(unsigned short node)
{
	int ret ;

	if((node & 0xF000) != (EXT_SYSTEM<<12)) {
		ret = ((node)>>12) & 0xF ;
	}
	else {
		ret = ((node)&0x03F0) >> 4 ;
	}

	return ret ;
}

extern inline int GET_INSTANCE(unsigned short node)
{
	int ret ;

	if((node & 0xF000) != (EXT_SYSTEM << 12)) {
		ret = (node) & 0xFF ;
	}
	else {
		ret = (node) &  0xF ;
	}

	return ret ;
}

extern inline int GET_SUBSYSTEM(unsigned short node)
{
	int ret ;

	if((node & 0xF000) != (EXT_SYSTEM<<12)) {
		ret = (((node) >> 8) & 0x3) ;
	}
	else {
		ret = 0 ;
	}

	return ret ;

}
	
#define GET_NODE_PRE_APR2002(sys,subsys,inst)  ((sys<<12) | (subsys<<8) | (inst))
#define GET_SYSTEM_PRE_APR2002(x)    (((x) >> 12) & 0xf)
#define GET_INSTANCE_PRE_APR2002(x)  ((x) & 0xff)
#define GET_SUBSYSTEM_PRE_APR2002(x) (((x) >> 8) & 0x3)

#define GET_ROUTE(x)     (((x) >> 10) & 0x3)

#endif /* _RTS_SYSTEMS_H_ */



