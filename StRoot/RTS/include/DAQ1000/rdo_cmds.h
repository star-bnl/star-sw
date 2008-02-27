#ifndef _RDO_CMDS_H_
#define _RDO_CMDS_H_



// the major parts (upper 4 bits)
#define MAJ_SET		0
#define MAJ_GET		1
#define MAJ_READ	2
#define MAJ_WRITE	3
#define MAJ_RUN		4
#define MAJ_TEST	5
#define MAJ_CONFIG	6
#define MAJ_MISC	7

// minor parts (lower 8 bits)
// set/get
#define MIN_ALTRO_START	0
#define ALTRO_K1        0x00
#define ALTRO_K2        0x01
#define ALTRO_K3        0x02
#define ALTRO_L1        0x03
#define ALTRO_L2        0x04
#define ALTRO_L3        0x05
#define ALTRO_VFPED     0x06
#define ALTRO_PMDTA     0x07
#define ALTRO_ZSTHR     0x08
#define ALTRO_BCTHR     0x09
#define ALTRO_TRCFG     0x0a
#define ALTRO_DPCFG     0x0b
#define ALTRO_DPCF2     0x0c
#define ALTRO_PMADD     0x0d
#define ALTRO_ERSTR     0x10
#define ALTRO_ADEVL     0x11
#define ALTRO_TRCNT     0x12
#define ALTRO_WPINC     0x18
#define ALTRO_RPINC     0x19
#define ALTRO_CHRDO     0x1a
#define ALTRO_SWTRG     0x1b
#define ALTRO_TRCLR     0x1c
#define ALTRO_ERCLR     0x1d
#define MIN_ALTRO_STOP	31
#define MIN_ID		32
#define MIN_RECONFIG	33
#define MIN_RUN_TYPE	34

// read/write
#define MIN_LOG		0
#define MIN_PED		1
#define MIN_MON		2
#define MIN_EXEC	3
#define MIN_FLASH	4
#define MIN_ANY		5
#define MIN_CONFIG	6
#define MIN_FLASH_ALT	0x30
#define MIN_FLASH_BOB	0x31
#define MIN_FLASH_FEE	0x32 

// run
#define MIN_START	0
#define MIN_STOP	1

// test
#define MIN_TRG		0
#define MIN_TEST_RDO	1

// config
#define MIN_EVTS	0
#define MIN_TIME	1
#define MIN_RDO		2

// misc
#define MIN_REINIT_LINK	0



#endif
