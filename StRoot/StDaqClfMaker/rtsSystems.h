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
*/
#define TRG_TPC_BIT	0
#define TRG_SVT_BIT	1


#define TRG_BSMD_BIT	2
#define TRG_SMD_BIT	TRG_BSMD_BIT	// use "BSMD"

#define TRG_FTPC_BIT	3
#define TRG_TOF_BIT	4
#define TRG_RICH_BIT	5

#define TRG_BTOW_BIT	6
#define TRG_EMC_BIT	TRG_BTOW_BIT	// use "BTOW"

#define TRG_FPD_BIT	7

/* Tonko, 6/11/01 we still need these. Numbers are INVENTED!!! */
#define TRG_EEMC_BIT	12
#define TRG_PMD_BIT	13



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
/* yet another EMC Change, Tonko, 6/11/01 */
#define EMC_SYSTEM	3
#define EMC_ID		EMC_SYSTEM
/* let's add the FPD too, Tonko, 1/11/01 */
#define FPD_SYSTEM	4
#define FPD_ID		FPD_SYSTEM

#define FTP_SYSTEM	5
#define FTP_ID		FTP_SYSTEM

#define PMD_SYSTEM	6			
#define PMD_ID		PMD_SYSTEM


#define RIC_SYSTEM	7
#define RIC_ID		RIC_SYSTEM
#define TRG_SYSTEM	8
#define TRG_ID          TRG_SYSTEM
#define L3_SYSTEM	9
#define L3_ID           L3_SYSTEM
#define SC_SYSTEM	10	/* slow controls */
#define SC_ID           SC_SYSTEM


#define EXT_SYSTEM	14	/* will use this when we run out of 4 bit
				numbers - "Extended System".
				*/
#define EXT_ID		EXT_SYSTEM


#define DAQ_SYSTEM	0xF
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
#define TM_INSTANCE     GB_INSTANCE
#define EVB_INSTANCE    BB_INSTANCE


#define TRG_INSTANCE         1
#define TRG_L1CTL_INSTANCE   2
#define TRG_L2CTL_INSTANCE   3
#define TRG_TCDCTL_INSTANCE  4
#define TRG_RCCCTL_INSTANCE  5
#define TRG_CTBCTL_INSTANCE  6
#define TRG_L1DC_INSTANCE    7
#define TRG_CDBCTL_INSTANCE  8
#define TRG_EEMCCTL_INSTANCE  9
#define TRG_DCCCTL_INSTANCE  10
#define TRG_MWCCTL_INSTANCE  11
#define TRG_BEMCCTL_INSTANCE 12
#define TRG_TDI_INSTANCE     13
#define TRG_SCALER_INSTANCE  14

#define L3EVP_INSTANCE      1
#define L3DISP_INSTANCE     2


/******************** RC/DAQ nodes ****************/
#define DAQMAN_NODE	((DAQ_SYSTEM<<12) | DAQMAN_INSTANCE) 
#define BB_NODE		((DAQ_SYSTEM<<12) | BB_INSTANCE) 
#define EVP_NODE	((DAQ_SYSTEM<<12) | EVP_INSTANCE) 
#define RC_NODE		DAQMAN_NODE 		/* for the time being...*/
#define LOG_NODE        DAQMAN_NODE
#define MON_NODE        DAQMAN_NODE

#define GB_NODE		((DAQ_SYSTEM<<12) | 4)
#define TM_NODE		GB_NODE

#define EVB01_NODE      BB_NODE
#define EVB_NODE	EVB01_NODE


/* singular detectors */
#define RIC01_NODE	((RIC_SYSTEM<<12) | 1)
#define RIC02_NODE	((RIC_SYSTEM<<12) | 2)
#define RIC_NODE	RIC01_NODE

#define TOF01_NODE	((TOF_SYSTEM<<12) | 1)
#define TOF02_NODE	((TOF_SYSTEM<<12) | 2)
#define TOF_NODE	TOF01_NODE

#define PMD01_NODE	((PMD_SYSTEM<<12) | 1)
#define PMD02_NODE	((PMD_SYSTEM<<12) | 2)
#define PMD_NODE	PMD01_NODE

#define FPD01_NODE	((FPD_SYSTEM<<12) | 1)
#define FPD02_NODE	((FPD_SYSTEM<<12) | 2)
#define FPD_NODE	FPD01_NODE

/* multi-node detectors */
#define TPC_NODES(x)	((TPC_SYSTEM<<12) | (x)) 

#define FTP_NODES(x)	((FTP_SYSTEM<<12) | (x)) 

#define SVT_NODES(x)	((SVT_SYSTEM<<12) | (x)) 

#define EMC_NODES(x)	((EMC_SYSTEM<<12) | (x))
#define BTOW_INSTANCE	1
#define BSMD_INSTANCE	2
#define EEMC_INSTANCE	4
#define BTOW_NODE	EMC_NODES(BTOW_INSTANCE)
#define BSMD_NODE	EMC_NODES(BSMD_INSTANCE)
#define BPRE_NODE	EMC_NODES(3)	// won't exist

#define ETOW_NODE	EMC_NODES(EEMC_INSTANCE)	// for _all_ Endcap
#define EEMC_NODE	ETOW_NODE		// Endcap EMC shortcut - use this!

#define ESMD_NODE	EMC_NODES(5)	// won't exist
#define EPRE_NODE	EMC_NODES(6)	// won't exist

/* Tonko, historical - should _not_ use them anymore */
#define EMC_NODE	BTOW_NODE
#define EMC01_NODE	BTOW_NODE
#define SMD_NODE	BSMD_NODE

/* Level III */
#define L3_NODES(x)	((L3_SYSTEM<<12) | (SL3_SUBSYS<<8) | (x)) 
#define GL3_NODES(x)	((L3_SYSTEM<<12) | (GL3_SUBSYS<<8) | (x)) 
#define L3EVP_NODE      ((L3_SYSTEM<<12) | 1)
#define L3DISP_NODE     ((L3_SYSTEM<<12) | 2)
#define GL3_NODE	GL3_NODES(1) 

/* Trigger */
#define TRG_NODE        ((TRG_SYSTEM<<12) | TRG_INSTANCE)
#define L1CTL_NODE      ((TRG_SYSTEM<<12) | TRG_L1CTL_INSTANCE)
#define L2CTL_NODE      ((TRG_SYSTEM<<12) | TRG_L2CTL_INSTANCE)
#define TCDCTL_NODE     ((TRG_SYSTEM<<12) | TRG_TCDCTL_INSTANCE)
#define RCCCTL_NODE     ((TRG_SYSTEM<<12) | TRG_RCCCTL_INSTANCE)
#define CTBCTL_NODE     ((TRG_SYSTEM<<12) | TRG_CTBCTL_INSTANCE)
#define L1DC_NODE       ((TRG_SYSTEM<<12) | TRG_L1DC_INSTANCE)
#define CDBCTL_NODE     ((TRG_SYSTEM<<12) | TRG_CDBCTL_INSTANCE)
#define L1_NODES(x)     ((TRG_SYSTEM<<12) | (TRG_L1_SUBSYS<<8) | (x))
#define L2_NODES(x)     ((TRG_SYSTEM<<12) | (TRG_L2_SUBSYS<<8) | (x))
#define EEMCCTL_NODE     ((TRG_SYSTEM<<12) | TRG_EEMCCTL_INSTANCE)
#define BEMCCTL_NODE     ((TRG_SYSTEM<<12) | TRG_BEMCCTL_INSTANCE)
#define DCCCTL_NODE     ((TRG_SYSTEM<<12) | TRG_DCCCTL_INSTANCE)
#define MWCCTL_NODE	((TRG_SYSTEM<<12) | TRG_MWCCTL_INSTANCE)
#define TDI_NODE	((TRG_SYSTEM<<12) | TRG_TDI_INSTANCE)
#define TRG_SCALER_NODE ((TRG_SYSTEM<<12) | TRG_SCALER_INSTANCE)


/* Slow Controls */
#define SC_NODE         ((SC_SYSTEM<<12) | 1)

/*
  Inverse's...
*/
#define GET_NODE(sys,subsys,inst)  ((sys<<12) | (subsys<<8) | (inst))
#define GET_INSTANCE(x)  ((x) & 0xff)
#define GET_SUBSYSTEM(x) (((x) >> 8) & 0x3)
#define GET_SYSTEM(x)    (((x) >> 12) & 0xf)
#define GET_ROUTE(x)     (((x) >> 10) & 0x3)

#endif /* _RTS_SYSTEMS_H_ */



