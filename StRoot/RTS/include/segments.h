#ifndef _SEGMENTS_H_
#define _SEGMENTS_H_

// shmLib Shared memory segments used by DAQ
//
// The segment number should be between 0...9

// Bufferbox:
#ifndef RTS_PROJECT_PP
#define EVB_MON_SEG              1
#define EVB_SUPERMON_SEG         3
#define EVB_CTL_SEG              4
#define EVB_SCA_SEG              5
#define SPOOL_CTL_SEG            8
#define DISK_MANAGER_SEG         9
#define MSG_Q_SEG                2
#define MSG_Q_SEG_SIZE           (1024 * 1024 * 20)
#else  // PP!
#define EVB_MON_SEG              1     
#define EVB_CTL_SEG              4
#define EVB_SCA_SEG              2
#define SPOOL_CTL_SEG            8
#endif

// evp:
#define EVP_SEG			5

#ifndef RTS_PROJECT_PP
#define EVP_SEG_SIZE		(300*1024*1024)
#else
#define EVP_SEG_SIZE            (64*1024)      // biggest pp event ~64k
#endif

#define VX_FILE_REQUEST_SEG     6
#define VX_FILE_REQUEST_SIZE    (1024 * 1024 * 12)

#define SC_SEG			7

// Some usefull keys for reference
//
//#define SHM_KEY_BASE     0xfd000000     
//#define SHM_KEY_SEM      0xfd001000
//#define SHM_KEY_QUEUES   0xfd002000

#endif
