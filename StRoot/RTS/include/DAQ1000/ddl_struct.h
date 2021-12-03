#ifndef _DDL_STRUCT_H_
#define _DDL_STRUCT_H_


#include <stdint.h>

/*
	DDL events are little endian.

	Every DDL event starts with a type describing the event type of
	the hex form:


		0xvVRssrST

	where:
		v is the general version, 0 for this document
		V is the version
		R is reserved
		ss is the sector (from 1)
		r is the RDO (from 1)
		S is the subtype
		T is the type corresponding to the string

	The second word is the event counter which starts at the
	beginning of the RDO's lifetime (reconfiguration).

	The event ends with a word count of _all_ words
	(including the header and trailer) is it is known or
	0 if unknown. This word also includes flags such as
	error etc:

		0xFLLLLLLL
	
	where:
		F is the flags, 4th bit signifies some error
		LLLLLLL is the length in words



	The last word is the exact copy of the second word
	of the header namely version.

	Thus:

		0xvVRssrST
		event_count
		...
		0xFLLLLLLL
		0xvVRssrST
*/

#define DDL_STRUCT_VERSION	15			// test version...

#define DDL_TYPE_LOG		0			// log aka ascii data
#define DDL_TYPE_LOG_V		0

#define DDL_TYPE_DTA		1			// triggered data with altros
#define DDL_TYPE_DTA_V		0

#define DDL_TYPE_RC		2			// Start/Stop run events
#define DDL_TYPE_RC_V		0

#define DDL_TYPE_MSC		3			// anything, look at subtype
#define DDL_TYPE_MSC_V		0

#define DDL_HEADER(t,s)	((DDL_STRUCT_VERSION<<28)|(t<<0)|(s<<4))

struct ddl_header {
	uint32_t type ;	// 0xvRRVVSST
	uint32_t ev_cou ;	// event counter
} ;

struct ddl_trailer {
	uint32_t fl_wc ;	// 0xFLLLLLLL:	F: 8 event in error; LLLLLL wc, can be 0 if not know...
	uint32_t type ;	// 0xvRRVVSST	same as in the ddl_header
} ;

struct ddl_file_header {
	char ctype[4] ;	// LOG,DTA,RuC,MSC
	uint32_t wc ;		// word count of all data, including this header
	uint32_t res[4] ;
} ;



// data structures and types; some are just descriptive
#define DDL_DTA_NORMAL	0	// used for the ALTRO readout or for Trigger-only events
#define DDL_DTA_CFG	1
#define DDL_DTA_EMUL	2	// emulated data...

#define DDL_MSC_ANY	0
#define DDL_MSC_CONFIG	1
#define DDL_MSC_HEARTBEAT	2

/*
struct altro_ch_v0 {
	uint32_t d[] ;	// of the form 0x000ddddd 
	uint32_t last[2] ;	// 
}

struct altro_data {
	altro_ch_v0 d[] ;
} ;
*/

// TRG subdata
struct trg_data {
	uint32_t rhic_counter ;
	uint32_t csr ;		// if 0xFFFF0000 it is the prompt trigger
	uint32_t data ;	// 0xFFFTDttt
} ;

// structures used in the configuration event
struct fee_cfg {
	uint32_t fl_id ;	// 0xF0ii00II:	F flags, ii old TPC ID, II new FEE ID
	uint32_t b_x_s ;	// 0x00000BXS:	B bus [0..2], X RDO Xilinx [0..2], Side [0..1]

} ;

/*
struct cfg_data {
	uint32_t sec_rdo ;	// 0x0000SSRR ;
	uint32_t time_code ;	
	fee_cfg fee[] ;
	uint32_t fee_cou ;
} ;

struct data_event {
	ddl_header h ;
	union {
		cfg_data c ;
		altro_data a;
	} d ;
	uint32_t fee_mask[2] ;
	trg_data trg[] ;
	uint32_t trg_cou ;
	ddl_trailer t ;
} ;

*/

#endif 
