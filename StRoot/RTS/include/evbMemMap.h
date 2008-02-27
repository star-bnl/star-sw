#ifndef _MEMMAP_H_
#define _MEMMAP_H_

//---------------------------------
// Currently only the PP defines...
//---------------------------------
#ifdef RTS_PROJECT_PP

#define MAX_DET_SIZE            (32*1024)
#define MAX_EVENT_SIZE          (64*1024)
#define NUMBER_TCP_BUFFS        200       // 40

#define MAX_TOKENS              1000

#define EVB_MAX_DETS            5         // max dets in one event
#define EVB_MAX_BUFFS           MAX_TOKENS

#define EVB_DEFAULT_BUFF_SIZE   MAX_EVENT_SIZE
#define EVB_SEG_SIZE            (MAX_TOKENS*64*1024)

// File sizes....
#define MAX_FILE_SIZE (500*1024*1024)
#define RCF_MAX_FILE_SIZE MAX_FILE_SIZE

#define XING_MAX 128

#endif  // pp


#endif
