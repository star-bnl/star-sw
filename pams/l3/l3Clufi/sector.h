#ifndef _SECTOR_H
#define _SECTOR_H


/* Sector Broker defines */ 
#define SB_RB_NUM	12

/* Receiver board defines */
#define RB_MZ_NUM	3
#define RB_FIBER_HDR_LEN	64

/* Mezzanine defines  */
#define MZ_ASIC_NUM	6
#define MZ_BUFFER_NUM	12

#define MZ_TPC_PADS_PER_ASIC	64
#define MZ_TPC_TIMEBINS		512
#define MZ_TPC_CPP_PER_PAD	31
#define MZ_TPC_MAX_PADS_PER_MEZ	384	/* calculated by Mike... */ 

#define MZ_SVT_PADS_PER_ASIC	256
#define MZ_SVT_TIMEBINS		126
#define MZ_SVT_CPP_PER_PAD	8
#define MZ_SVT_MAX_PADS_PER_MEZ	(256*6)	/* not really verified */


/* misc. */
#define DMA_SL3_TYPE	(1 << 28)
#define DMA_FMT_TYPE	(2 << 28)



#endif
