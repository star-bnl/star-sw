/*
 *  This file defines the QT board address map offsets (in bytes)
 *   
 *  J.M. Nelson       February 2007
 * 
 *  17Apr07 CWP Changed QT_MREG_BASE to 0x00804100
 *  06Jul07 JMN Added DCM_RESET register  
 */


/*#ifndef _QT_H_INCLUDED_*/

#define LUT_MAX_LEN     32*4096*2               /* Bytes for one Board's LUTs */

#define QT_LUT_BASE     0x00800000		/* Base address for first of 32 LUTs */ 
#define QT_MREG_BASE    0x00804100              /* Base address for Mother registers */ 
#define QT_DREG_MASK    0x009C4000		/* Register mask for Daughter boards */ 

#define QT_WRITE_ADDR   0x00804148              /* TCU address store */
#define QT_WRITE_BUSY   0x0080414C              /* Bit 0 low when data are ready */
#define QT_WORD_CNT     0x00804150              /* Number of data words available */
#define QT_DATA_ADDR    0x00804154              /* Address of first data word */

#define QT_DCM_RESET    0x24                    /* Daughter Board Status and Reset Register */

/*#define _QT_H_INCLUDED_
#endif*/

/* end of file QT.h */


