#ifndef _SECTOR_H
#define _SECTOR_H


// Sector Broker defines
#define SB_RB_NUM	12

// Receiver board defines
#define RB_MZ_NUM		3
#define RB_FIBER_HDR_LEN	64

// Mezzanine defines 
#define MZ_ASIC_NUM	6
#define MZ_BUFFER_NUM	12



#define MZ_TPC_PADS_PER_ASIC	64
#define MZ_TPC_TIMEBINS		512
#define MZ_TPC_CPP_PER_PAD	32
#define MZ_TPC_MAX_PADS_PER_MEZ	(MZ_TPC_PADS_PER_ASIC*MZ_ASIC_NUM)

#define MZ_FTP_PADS_PER_ASIC	64
#define MZ_FTP_TIMEBINS		512					// deal with this in software
#define MZ_FTP_CPP_PER_PAD	32
#define MZ_FTP_MAX_PADS_PER_MEZ	(MZ_TPC_PADS_PER_ASIC*MZ_ASIC_NUM)
#define MZ_FTP_PADS_PER_MEZ	320

#define MZ_SVT_PADS_PER_ASIC	256
#define MZ_SVT_TIMEBINS		128
#define MZ_SVT_CPP_PER_PAD	8
#define MZ_SVT_MAX_PADS_PER_MEZ	(MZ_SVT_PADS_PER_ASIC*MZ_ASIC_NUM)	// not really verified

// generic constants
#define MZ_MAX_PADS_PER_ASIC	256	// SVT case
#define MZ_MAX_TIMEBINS		512	// TPC
#define MZ_MAX_CPP_PER_PAD	32	// TPC
#define MZ_MAX_PADS		(256*MZ_ASIC_NUM)	// SVT
#define MZ_MAX_CHANNELS		(512*64*MZ_ASIC_NUM)	// all
#define MZ_MAX_CPPS		(8*256*MZ_ASIC_NUM)		// all
#define MZ_MAX_ROWS		6	// TPC, SVT
// maximum clusters per mezzanine
#define MZ_MAX_CLUSTERS		6000

#define MZ_RAW_DEBUG_BYTES		(0x40000 + 0x10000)	// ADC and CPP

// this shouldn't really be here...
// misc.
#define DMA_SL3_TYPE	(1 << 28)
#define DMA_FMT_TYPE	(2 << 28)



#endif
