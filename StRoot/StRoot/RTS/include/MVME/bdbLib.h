#ifndef _BDB_LIB_H_
#define _BDB_LIB_H_

#include <vxWorks.h>

#define BDB_MAX_NUM	21

#define BDB_MASK_REG	0x00
#define BDB_STAT_REG	0x02

// mask register
#define BDB_OVERRIDE_BUSY_BIT	15

// status register
#define BDB_BUSY_BIT		15
#define BDB_MANUAL_STATE_BIT	14

#define BDB_MASK_ALL_ENABLE	0x0
#define BDB_MASK_ALL_DISABLE	0x1FFF

// VME A16 addresses in the Main Crate
#define BDB_TPC_0		0x0000
#define BDB_TPC_1		0x0100
#define BDB_SVT			0x0200
#define BDB_FTPC		0x0300
// VME A16 address in the Sector Crate
#define BDB_SECTOR		0x0000


extern int bdbFind(UINT16 a16) ;
extern int bdbInit(int bdb) ;

extern int bdbSetMask(int bdb, UINT16 mask) ;
extern UINT16 bdbGetMask(int bdb) ;

extern int bdbSetBusy(int bdb) ;
extern int bdbClearBusy(int bdb) ;
extern int bdbGetBusy(int bdb) ;

extern int bdbGetRBBusy(int bdb, int rb) ;

extern int bdbSetRBMask(int bdb, int rb) ;
extern int bdbClearRBMask(int bdb, int rb) ;
extern int bdbGetRBMask(int bdb, int rb) ;

extern UINT32 *bdbTable ;






#endif	// _BDB_LIB_H_
