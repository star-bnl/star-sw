#ifndef _RORC_LIB_H_
#define _RORC_LIB_H_

/*
 ***************************************************
 * rorc_lib.h
 *
 * Header file for library routines for ALICE RORC programs
 *
 * last updated: 24/04/2007
 * written by: Peter Csato and Ervin Denes
 ***************************************************
 */

#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <linux/types.h>
//#include <sys/io.h>
#include <sys/mman.h>
//#include <asm/page.h>
#include <sys/ioctl.h>
#include <linux/pci.h>





//#include "rorc.h"
//#include "rorc_aux.h"
//#include "rorc_date.h"
#include "ddl_def.h"




/*
 * Defines ----------------------- D-RORC ----------------------------
 */

/* RORC's PCI revisions */
#define RORC_MAX_REVISION 4 /* revision 1: pRORC, 2: D-RORC with pluggable DIU
	      		                3: integrated D-RORC and DIU 
                                        4: second generation D-RORC
                            */
#define PRORC  1
#define DRORC  2
#define INTEG  3
#define DRORC2 4

/*
 * D-RORC REGISTERS
 */

#define DRORC_REG_NUM 32

#define RCSR       0     /* RORC Control and Status register      */
#define RERR       1     /* RORC Error register                   */
#define RFID       2     /* Firmware ID                           */
#define RHID       3     /* Hardware ID                           */
#define C_CSR      4     /* channel Control and Status register   */
#define C_ERR      5     /* channel Error register                */
#define C_DCR      6     /* channel DDL Command register          */
#define C_DSR      7     /* channel DDL Status register           */
#define C_DG1      8     /* channel Data Generator param 1        */
#define C_DG2      9     /* channel Data Generator param 2        */
#define C_DG3     10     /* channel Data Generator param 3        */
#define C_DG4     11     /* channel Data Generator param 4        */
#define C_DGS     12     /* channel Data Generator Status         */
#define C_RRBAR   13     /* channel Receive Report Base Address   */
#define C_RAFL    14     /* channel Receive Address FIFO Low      */
#define C_RAFH    15     /* channel Receive Address FIFO High     */
#define C_TRBAR   16     /* channel Transmit Report Base Address  */
#define C_TAFL    17     /* channel Transmit Address FIFO Low     */
#define C_TAFH    18     /* channel Transmit Address FIFO High    */
#define RESERVED  19
#define C_RDR1    20     /* channel Receive Data Rate register 1  */
#define C_RDR2    21     /* channel Receive Data Rate register 2  */
#define C_RDR3    22     /* channel Receive Data Rate register 3  */
#define C_RDR4    23     /* channel Receive Data Rate register 4  */
#define C_TDR1    24     /* channel Transmit Data Rate register 1 */
#define C_TDR2    25     /* channel Transmit Data Rate register 2 */
#define C_TDR3    26     /* channel Transmit Data Rate register 3 */
#define C_TDR4    27     /* channel Transmit Data Rate register 4 */
#define C_RXDC    28     /* channel Receive  DMA count register   */
#define C_TXDC    29     /* channel Transmit DMA count register   */
#define C_RXDA    30     /* channel Receive  DMA address register */
#define C_TXDA    31     /* channel Transmit DMA address register */

/* high addresses for 64 bit */
#define C_RAFX    32     /* Receive Address FIFO Extension register   */
#define C_RRBX    33     /* Rx Report Base Address Extension register */
#define C_TAFX    34     /* Transmit Address FIFO Extension register  */
#define C_TRBX    35     /* Tx Report Base Address Extension register */
#define C_RDAH    36     /* channel Receive  DMA address register High*/
#define C_TDAH    37     /* channel Transmit DMA address register High*/


/*
 * I2C bits (D-RORC HID register)
 */

#define I2C_DATA                    0x000000FF   // bits  7 ... 0
#define I2C_ADDRESS                 0x0000FF00   // bits 15 ... 8
#define I2C_WRITE                   0x00010000   // bit 16
#define I2C_DATA_VALID              0x10000000   // bit 28
#define I2C_READ_OPERATION_ACTIVE   0x20000000   // bit 29
#define I2C_WRITE_OPERATION_VALIDE  0x40000000   // bit 30
#define I2C_OPERATION_ACTIVE        0x80000000   // bit 31


/*
 * D-RORC commands
 */

/* RCSR controls */

#define DRORC_CMD_RESET_RORC       0x00000001   //bit  0
#define DRORC_CMD_RESET_CHAN       0x00000002   //bit  1
#define DRORC_CMD_CLEAR_RORC_ERROR 0x00000008   //bit  3

/* CCSR commands */

#define DRORC_CMD_RESET_DIU      0x00000001   //bit  0
#define DRORC_CMD_CLEAR_FIFOS    0x00000002   //bit  1
#define DRORC_CMD_CLEAR_RXFF     0x00000004   //bit  2
#define DRORC_CMD_CLEAR_TXFF     0x00000008   //bit  3
#define DRORC_CMD_CLEAR_ERROR    0x00000010   //bit  4
#define DRORC_CMD_CLEAR_COUNTERS 0x00000020   //bit  5

#define DRORC_CMD_JTAG_DOWN_ON   0x00000040   //bit  6
#define DRORC_CMD_JTAG_DOWN_OFF  0x00000080   //bit  7
#define DRORC_CMD_DATA_TX_ON_OFF 0x00000100   //bit  8
#define DRORC_CMD_DATA_RX_ON_OFF 0x00000200   //bit  9
#define DRORC_CMD_START_DG       0x00000400   //bit 10
#define DRORC_CMD_STOP_DG        0x00000800   //bit 11
#define DRORC_CMD_LOOPB_ON_OFF   0x00001000   //bit 12
#define DRORC_CMD_HLT_FLC_ON_OFF 0x20000000   //bit 29
#define DRORC_CMD_HLT_SPL_ON_OFF 0x40000000   //bit 30

/*
 * CCSR status bits 
 */

#define DRORC_STAT_LINK_DOWN          0x00002000   //bit 13
#define DRORC_STAT_LINK_FULL          0x00004000   //bit 14
#define DRORC_STAT_CMD_NOT_EMPTY      0x00010000   //bit 16

#define DRORC_STAT_RXRBAR_NOT_SET     0x00020000   //bit 17
#define DRORC_STAT_RXAFF_EMPTY        0x00040000   //bit 18
#define DRORC_STAT_RXAFF_FULL         0x00080000   //bit 19

#define DRORC_STAT_TXRBAR_NOT_SET     0x00100000   //bit 20
#define DRORC_STAT_TXAFF_EMPTY        0x00200000   //bit 21
#define DRORC_STAT_TXAFF_FULL         0x00400000   //bit 22

#define DRORC_STAT_RXSTAT_NOT_EMPTY   0x00800000   //bit 23
#define DRORC_STAT_RXDAT_ALMOST_FULL  0x01000000   //bit 24
#define DRORC_STAT_RXDAT_NOT_EMPTY    0x02000000   //bit 25

#define DRORC_STAT_TXDAT_NOT_EMPTY    0x04000000   //bit 26
#define DRORC_STAT_TXDAT_ALMOST_FULL  0x08000000   //bit 27

#define DRORC_STAT_ERR_NOT_EMPTY      0x80000000   //bit 31

#define DRORC_STAT_ANY                0xffffffc0   //bits 31-6 

/*
 * CCSR status text 
 */

#define DRORC_STAT_TEXT "0,0,0,0,0,0,\
JTAG download enabled,\
0,\
Data transfer (RDMA) enabled,\
Data receiver (WDMA) enabled,\
Data Generator started,0,\
Internal loop-back set,\
Link is down,\
Link is full,\
0,\
DDL command register not empty,\
Receive report base address not set,\
Receive address FIFO empty,\
Receive address FIFO full,\
Transmit report base address not set,\
Transmit address FIFO empty,\
Transmit address FIFO full,\
Receiver status FIFO not empty,\
Receive data FIFO almost full,\
Receive data FIFO not empty,\
Transmit data FIFO not empty,\
Transmit data FIFO almost full,\
0,\
HLT flow-control enabled,\
HLT splitter enabled,\
Error register is not empty"

#define DRORC_STAT_DEFAULT_TEXT "Reserved bit set"

/*------------------- pRORC ----------------------------------*/

/*
 * pRORC commands
 */

#define PRORC_CMD_RESET_RORC     0x0110
#define PRORC_CMD_RESET_DIU      0x0210
#define PRORC_CMD_RESET_RORC_DIU 0x0310
#define PRORC_CMD_RESET_SIU      0x00F1
#define PRORC_CMD_CLEAR_FIFOS    0x0410 
#define PRORC_CMD_CLEAR_FF       0x0810
#define PRORC_CMD_CLEAR_ERROR    0x1010
#define PRORC_CMD_CLEAR_COUNTERS 0x2010
#define PRORC_CMD_GET_STAT       0x020
#define PRORC_CMD_GET_ID         0x030
#define PRORC_CMD_PUSH_FF        0x040
#define PRORC_CMD_POP_FF         0x050
#define PRORC_CMD_PUT_READY_BASE 0x060
#define PRORC_CMD_DG_PARAM1      0x070
#define PRORC_CMD_DG_PARAM2      0x080
#define PRORC_CMD_DOWNL_DATA     0x090
#define PRORC_CMD_DOWNL_JTAG     0x190
#define PRORC_CMD_PARAM_RESET    0x0a0
#define PRORC_CMD_LOOPB_ON       0x1a0
#define PRORC_CMD_STOP_ERR_ON    0x2a0
#define PRORC_CMD_START_W_DMA    0x1b0
#define PRORC_CMD_STOP_W_DMA     0x0b0
#define PRORC_CMD_START_DG       0x3b0
#define PRORC_CMD_STOP_DG        0x2b0
#define PRORC_CMD_GET_IN_BYTES   0x0c0
#define PRORC_CMD_GET_OUT_BYTES  0x1c0

#define PRORC_PARAM_LOOPB        0x1
#define PRORC_PARAM_STOP_ERR     0x2

/*
 * Mailbox not empty bits
 */

#define PRORC_NE_OMB1  0x0000000f
#define PRORC_NE_OMB2  0x000000f0
#define PRORC_NE_OMB3  0x00000f00
#define PRORC_NE_OMB4  0x0000f000
#define PRORC_NE_OUTMB 0x0000ffff
#define PRORC_NE_IMB1  0x000f0000
#define PRORC_NE_IMB2  0x00f00000
#define PRORC_NE_IMB3  0x0f000000
#define PRORC_NE_IMB4  0x70000000
#define PRORC_NE_INMB  0x7fff0000

/*
 * Control bits of IMB4 high byte
 */

#define PRORC_BIT_LINK_DOWN 0x80000000
#define PRORC_BIT_FF_EMPTY  0x40000000
#define PRORC_BIT_CMD_RDY1  0x04000000
#define PRORC_BIT_CMD_RDY2  0x02000000
#define PRORC_BIT_CMD_PROC  0x01000000

/*
 * OCSR bits
 */

#define PRORC_STAT_LINK_DOWN          0x40000000   //bit 30
#define PRORC_STAT_LINK_FULL          0x20000000   //bit 29
#define PRORC_STAT_FIFO_NOT_EMPTY     0x10000000   //bit 28
#define PRORC_STAT_DC_NOT_EMPTY       0x08000000   //bit 27
#define PRORC_STAT_JTAG_NOT_EMPTY     0x04000000   //bit 26
#define PRORC_STAT_FED_NOT_EMPTY      0x02000000   //bit 25
#define PRORC_STAT_DIU_CMD_NOT_EMPTY  0x01000000   //bit 24
#define PRORC_STAT_DTSTW_NOT_EMPTY    0x00800000   //bit 23
#define PRORC_STAT_RFBAR_NOT_SET      0x00400000   //bit 22
#define PRORC_STAT_START_DATA_GEN     0x00200000   //bit 21
#define PRORC_STAT_LOOP_BACK          0x00100000   //bit 20
#define PRORC_STAT_STOP_ON_ERROR      0x00080000   //bit 19
#define PRORC_STAT_JTAG_DLOAD_ENABLE  0x00040000   //bit 18
#define PRORC_STAT_WAIT_DTSTW         0x00020000   //bit 17
#define PRORC_STAT_RDMA_RUNNING       0x00010000   //bit 16
#define PRORC_STAT_RDMA_SUSP_PCI      0x00008000   //bit 15
#define PRORC_STAT_RDMA_SUSP_LINK     0x00004000   //bit 14
#define PRORC_STAT_JTAG_REC_ENABLE    0x00002000   //bit 13
#define PRORC_STAT_WDMA_RUNNING       0x00001000   //bit 12
#define PRORC_STAT_WDMA_SUSP_PCI      0x00000800   //bit 11
#define PRORC_STAT_WDMA_SUSP_LINK     0x00000400   //bit 10
#define PRORC_STAT_FF_EMPTY           0x00000020   //bit  5
#define PRORC_STAT_FF_FULL            0x00000010   //bit  4
#define PRORC_STAT_FF_USEDW           0x0000000f   //bits 3-0
#define PRORC_STAT_ANY                0x7ffffff0   //bits 30-4

/*
 * OCSR status text 
 */

#define PRORC_STAT_TEXT "0,0,0,0,\
Free FIFO full,\
Free FIFO empty,\
0,0,0,0,\
W_DMA suspended because of the link (DC_EMPTY),\
W_DMA suspended because of the PCI (WRFULL),\
W_DMA running,\
JTAG receive enabled,\
R_DMA_SUSPENDED because of the link (FEDF_FULL),\
R_DMA suspended because of the PCI (RDEMPTY),\
R_DMA running,\
Waiting for DTSTW adfter R_DMA,\
JTAG download enabled,\
Stop on error is on,\
Internal loop-back set,\
Data Generator started,\
Ready FIFO base address not set,\
DTSTW register not empty,\
DIU command register not empty,\
FED FIFO not empty,\
JTAG FIFO not empty,\
DC FIFO not empty,\
Status FIFO not empty,\
Link is full,\
Link is down,\
0"

#define PRORC_STAT_DEFAULT_TEXT "Reserved bit set"

/*
 * RSER bits
 */

#define PRORC_ERROR              0x80000000
#define PRORC_ERR_INV_COMM       0x40000000
#define PRORC_ERR_MISS_PARM      0x20000000
#define PRORC_ERR_FF_OVWR        0x04000000
#define PRORC_ERR_DTSTW_OVWR     0x02000000
#define PRORC_ERR_DIU_OVWR       0x01000000
#define PRORC_ERR_LINK_DOWN      0x00800000
#define PRORC_ERR_STAT_FF_FULL   0x00000008
#define PRORC_ERR_FED_FULL       0x00000004
#define PRORC_ERR_DC_FULL        0x00000002
#define PRORC_ERR_JTAG_FULL      0x00000001

/* nvram constants */
#define MAX_WAIT 1000000

/*
 * DMA WAIT
 */

#define PRORC_DMA_WAIT 16

/*-----------------------------------------------*/

/*
 * Return values
 */

#define MAX_RETURN_CODE                  15
#define MAX_RETURN_TEXT                  32

#define RORC_STATUS_OK                    0
#define RORC_STATUS_ERROR                -1
#define RORC_INVALID_PARAM               -2

#define RORC_LINK_NOT_ON                 -4
#define RORC_CMD_NOT_ALLOWED             -8
#define RORC_NOT_ACCEPTED               -16
#define RORC_NOT_ABLE                   -32
#define RORC_TIMEOUT                    -64

#define RORC_FF_FULL                   -128
#define RORC_FF_EMPTY                  -256

#define RORC_NOT_ENOUGH_REPLY          -512 
#define RORC_TOO_MANY_REPLY           -1024

#define RORC_DATA_BLOCK_NOT_ARRIVED       0
#define RORC_NOT_END_OF_EVENT_ARRIVED     1
#define RORC_LAST_BLOCK_OF_EVENT_ARRIVED  2 

/*
 * pRORC initialization and reset options
 */

#define RORC_RESET_FF         1   /* reset Free FIFOs */
#define RORC_RESET_RORC       2   /* reset RORC */
#define RORC_RESET_DIU        4   /* reset DIU */
#define RORC_RESET_SIU        8   /* reset SIU */
#define RORC_LINK_UP         16   /* init link */
#define RORC_RESET_FEE       32   /* reset Front-End */
#define RORC_RESET_FIFOS     64   /* reset RORC's FIFOS (not Free FIFO) */
#define RORC_RESET_ERROR    128   /* reset RORC's error register */
#define RORC_RESET_COUNTERS 256   /* reset RORC's event number counters */

#define RORC_RESET_ALL      0x000001FF   //bits 8-0

/*
 * Data Generator patterns
 */

#define RORC_DG_CONST   1
#define RORC_DG_ALTER   2
#define RORC_DG_FLY0    3
#define RORC_DG_FLY1    4
#define RORC_DG_INCR    5
#define RORC_DG_DECR    6
#define RORC_DG_RANDOM  7

#define RORC_DG_NO_RANDOM_LEN  0
#define RORC_DG_INFINIT_EVENT  0

/*
 * pRORC clock time (1/(26.5625 MHz) = 3.764705882e-8 sec) * 2^18
 */

#define TWO_TO_THE_18 262144.0
#define TWO_TO_THE_20 1048576.0
#define TWO_TO_THE_32 4294967296.0
#define RORC_CLOCK_18 0.009868950588

/*
 * Macros ------------------- D-RORC --------------------------------------
 */

#define dRorcWriteReg(dev, reg_number, reg_value) \
    *(*(dev)).reg[reg_number] = reg_value

#define dRorcReadReg(dev, reg_number) (*(*(dev)).reg[reg_number])
    
#define dRorcPushRxFreeFifo(dev, blockAddress, blockLength, readyFifoIndex) \
    dRorcWriteReg (dev, C_RAFH, blockAddress); \
    dRorcWriteReg (dev, C_RAFL, ((blockLength) << 8) | (readyFifoIndex))

#define dRorcPushTxFreeFifo(dev, blockAddress, blockLength, readyFifoIndex) \
    dRorcWriteReg (dev, C_TAFH, blockAddress); \
    dRorcWriteReg (dev, C_TAFL, ((blockLength) << 8) | (readyFifoIndex))

#define dRorcCheckLoopBack(dev) (dRorcReadReg(dev, C_CSR) & \
		                                      DRORC_CMD_LOOPB_ON_OFF)
#define dRorcChangeLoopBack(dev) dRorcWriteReg(dev, C_CSR, \
		                                      DRORC_CMD_LOOPB_ON_OFF)
#define dRorcCheckHltFlctl(dev) (dRorcReadReg(dev, C_CSR) & \
		                                      DRORC_CMD_HLT_FLC_ON_OFF)
#define dRorcChangeHltFlctl(dev) dRorcWriteReg(dev, C_CSR, \
		                                      DRORC_CMD_HLT_FLC_ON_OFF)
#define dRorcCheckHltSplit(dev) (dRorcReadReg(dev, C_CSR) & \
		                                      DRORC_CMD_HLT_SPL_ON_OFF)
#define dRorcChangeHltSplit(dev) dRorcWriteReg(dev, C_CSR, \
		                                      DRORC_CMD_HLT_SPL_ON_OFF)
#define dRorcCheckRxStatus(dev) (dRorcReadReg(dev, C_CSR) & \
                                                    DRORC_STAT_RXSTAT_NOT_EMPTY)
#define dRorcCheckTxStatus(dev) (dRorcReadReg(dev, C_CSR) & \
                                                    DRORC_STAT_TXSTAT_NOT_EMPTY)
#define dRorcCheckRxData(dev) (dRorcReadReg(dev, C_CSR) & \
                                                    DRORC_STAT_RXDAT_NOT_EMPTY)
#define dRorcCheckTxData(dev) (dRorcReadReg(dev, C_CSR) & \
                                                    DRORC_STAT_TXDAT_NOT_EMPTY)
#define dRorcReadRxDmaCount(dev) (dRorcReadReg(dev, C_RXDC) & 0xFFFFFF)
#define dRorcReadTxDmaCount(dev) (dRorcReadReg(dev, C_TXDC) & 0xFFFFFF)

#define I2C_ACTIVE(dev) (dRorcReadReg(dev, RHID) & I2C_OPERATION_ACTIVE)

/*------------------------- pRORC ---------------------------------------*/

#define pRorcWriteMb(dev, mb_number, mb_value) \
    *(*(dev)).omb[mb_number] = mb_value

#define pRorcReadMb(dev, mb_number) (*(*(dev)).imb[mb_number])

#define pRorcReadRxDmaCount(dev) (*(*(dev)).mwtc)
#define pRorcReadTxDmaCount(dev) (*(*(dev)).mrtc)

#define pRorcPushOldFreeFifo(dev, blockAddress, blockLength, readyFifoIndex) \
    while ((*(*(dev)).mbef & 0xffff) != 0); \
    *(*(dev)).omb[3] = (blockLength); \
    *(*(dev)).omb[2] = (blockAddress); \
    *(*(dev)).omb[1] = ((readyFifoIndex) << 8) | PRORC_CMD_PUSH_FF

#define pRorcInitCmdProc(dev) (prorc_cmd_rdy = (pRorcReadMb(dev, 4) & PRORC_BIT_CMD_RDY1))

/* #define pRorcCheckMb(dev, mb_mask) ((*(*(dev)).mbef & mb_mask) == mb_mask) */
#define pRorcCheckMb(dev, mb_mask) (*(*(dev)).mbef & mb_mask)

#define NVRAM_BUSY_MEM(ptr_to_pci_mem) (*(ptr_to_pci_mem + 0x3F) & 0x80)

/*-----------------------------------------------------------------------*/

#define pRorc(dev) ((*(dev)).rorc_revision == PRORC)

#define rorcCheckLink(dev) \
  (pRorc(dev) ? \
    ((pRorcReadMb(dev, 4) & PRORC_BIT_LINK_DOWN) ? RORC_LINK_NOT_ON \
                                                 : RORC_STATUS_OK) \
  : \
    ((dRorcReadReg(dev, C_CSR) & DRORC_STAT_LINK_DOWN) ? RORC_LINK_NOT_ON \
                                                       : RORC_STATUS_OK))

#define rorcCheckStatus(dev) \
  (pRorc(dev) ? \
    pRorcCheckMb(dev, PRORC_NE_IMB1) : dRorcCheckRxStatus(dev))

#define rorcCheckCommandRegister(dev) \
  (pRorc(dev) ? \
    pRorcCheckMb(dev, PRORC_NE_OMB1) \
  : \
    dRorcReadReg(dev, C_CSR) & DRORC_STAT_CMD_NOT_EMPTY)

#define rorcPutCommandRegister(dev, com) \
  (pRorc(dev) ? \
    (pRorcInitCmdProc(dev), pRorcWriteMb(dev, 1, com), pRorcWaitCmdProc(dev)) \
  : \
    (dRorcWriteReg((dev), C_DCR, (com))))

#define rorcLoopBackOn(dev) rorcParamOn(dev, PRORC_PARAM_LOOPB)

#define rorcLoopBackOff(dev) rorcParamOff(dev);

#define rorcPushFreeFifo(dev, blockAddress, blockLength, readyFifoIndex) \
  if (pRorc(dev))  \
  { \
    while ((*(*(dev)).mbef & 0xffff) != 0); \
    *(*(dev)).omb[3] = (blockLength); \
    *(*(dev)).omb[2] = (blockAddress); \
    *(*(dev)).omb[1] = ((readyFifoIndex) << 8) | PRORC_CMD_PUSH_FF; \
  } \
  else \
  { \
    dRorcPushRxFreeFifo(dev, blockAddress, blockLength, readyFifoIndex); \
  }
    
#define rorcHasData(rf, index) \
  (((rf)[index].status == -1) ? RORC_DATA_BLOCK_NOT_ARRIVED : \
  (((rf)[index].status == 0)  ? RORC_NOT_END_OF_EVENT_ARRIVED : \
                                RORC_LAST_BLOCK_OF_EVENT_ARRIVED)) 

#define rorcFFSize(fw) ((fw & 0xff000000) >> 18)  /* (x >> 24) * 64 */
#define rorcFWVersMajor(fw) ((fw >> 20) & 0xf)
#define rorcFWVersMinor(fw) ((fw >> 13) & 0x7f)

/*
 * Type defs ----------------------------------------------------
 */

typedef struct
{
  int                    fd;
  int                    minor;
  volatile unsigned	*p2pci;
  unsigned               shift;
  unsigned short         vendor;
  unsigned short         device;
  unsigned               irq;
  unsigned		 base_address[6];
  unsigned		 rom_address;
  unsigned		 bus_speed_mode;
  int                    rorc_revision;
  int                    rorc_serial;
  int                    diu_version;
  int                    driver_major;
  int                    driver_minor;
  int                    driver_release;
  int                    ddl_channel;
  int                    fd_ch;
  volatile unsigned	 *reg[DRORC_REG_NUM];  /* D-RORC reg addresses */
  volatile unsigned	 *omb[5];              /* pRORC  reg addresses */
  volatile unsigned	 *imb[5];
  volatile unsigned      *mwar;
  volatile unsigned      *mwtc;
  volatile unsigned      *mrar;
  volatile unsigned      *mrtc;
  volatile unsigned      *mbef;
  volatile unsigned      *intcsr;
  volatile unsigned       *mcsr;
  long long int          loop_per_usec;      /* loop/us for the given machine */
  long long int          max_resp_time;      /* the corresponding max. time */
} rorc_pci_dev_t;

typedef rorc_pci_dev_t    rorcDescriptor_t;
typedef rorcDescriptor_t* rorcHandle_t;

typedef struct
{
  int minor;
  int channel;
} rorcChannelId_t;


typedef struct
{
  volatile unsigned int   length;
  volatile unsigned int   status;
} rorcReadyFifo_t;

typedef struct
{
  __u32 ccsr;
  __u32 cerr;
  __u32 cdgs;
} rorcStatus_t;

typedef struct
{
  unsigned long gbc;
  unsigned long mbc;
  unsigned long gtc;
  unsigned long lbc;
  double lspeed;
  double bytes;
  double time;
  double gspeed;
} rorcCounter_t;

typedef struct
{
  __u8 data[DDL_MAX_HW_ID];
  int version;
  int subversion;
  int serial;
} rorcHwSerial_t;

typedef struct
{
  char id_text[8];
  int sn_pos;         // serial number position
  int ch_pos;         // channel number position
  int ver_pos;        // hw version position
  int ld_pos;         // logical device position
} rorcId_t;

typedef struct
{
  int code;
  char text[MAX_RETURN_TEXT];
} rorcReturn_t;

/*
 * Global variables ------------------------------------------------
 */

extern volatile __u32 prorc_cmd_rdy;
extern volatile int interrupt_arrived;


/*
 * Prototypes ------------------------------------------------------
 */

void sprom_load_address_mem(volatile char *ptr_to_pci_mem, __u8 address);
__u8 sprom_readB_mem(volatile char *ptr_to_pci_mem, __u8 address);
int i2c_write_a_byte(rorcHandle_t dev, __u8 address, __u8 data, int timeout);
int i2c_read_a_byte(rorcHandle_t dev, __u8 address, __u8 *data, int timeout);
rorcHwSerial_t rorcSerial(rorcHandle_t handle);
void rorcBuildHwSerial(__u8 data[], int rorc_rev, int version_major,
                       int version_minor, __u8 c_pld[], int numb_chan,
                       int serial);
int rorcCheckOpen(int minor, unsigned int channel);
int rorcFind(int revision, int serial, int *minor);
int rorcFindAll(rorcHwSerial_t *hw, rorcHwSerial_t *diu_hw, 
                rorcChannelId_t *channel, int *rorc_revision, 
		                      int *diu_version, int max_dev);
int rorcQuickFind(int *rorc_minor, int *rorc_revision, int *pci_speed,
                  int *rorc_serial, int *rorc_fw_maj,int *rorc_fw_min,
                  int *max_chan, int *ch_pid0, int *ch_pid1, int max_dev);
int rorcMapChannel(rorcDescriptor_t *prorc, int minor, int channel);
int rorcMap(rorcDescriptor_t *prorc, int minor);
int rorcOpenChannel(rorcDescriptor_t *prorc, int minor, int channel);
int rorcOpen(rorcDescriptor_t *prorc, int minor);
int rorcClose(rorcDescriptor_t *prorc);
int physmemOpen (int *fd,
                 volatile unsigned long **user_addr,
                 unsigned long *phys_addr,
                 unsigned long *size_physmem);
int physmemCheck (char *devnam,
                  unsigned long *phys_addr,
                 unsigned long *size_physmem);
int physmemClose (int physmem_fd,
                  unsigned long *addr_user_physmem,
                  unsigned long phys_size_physmem);
int rorcCheckVersion(rorc_pci_dev_t *dev);
void rorcReset(rorc_pci_dev_t *dev, int prorc_cmd);
int rorcEmptyDataFifos(rorc_pci_dev_t *dev, int empty_time);
int dRorcWaitRxStatusNotEmpty(rorc_pci_dev_t *dev);
int dRorcCheckRxFreeFifo(rorc_pci_dev_t *dev);
int dRorcCheckTxFreeFifo(rorc_pci_dev_t *dev);
int rorcCheckTxNotFinished(rorc_pci_dev_t *dev);
int rorcCheckFreeFifo(rorc_pci_dev_t *dev);
int rorcPopFreeFifo (rorc_pci_dev_t *dev,
		      int             tx,
		      __u32           *blockAddress,
		      int             *blockLength,
		      int             *readyFifoIndex);
int pRorcWaitCmdProc(rorc_pci_dev_t *dev);
int pRorcWaitMbNotEmpty(rorc_pci_dev_t *dev, __u32 mbMask);
int pRorcEmptyMb(rorc_pci_dev_t *prorc, int mb_number, short print);
int dRorcEmptyRxStatus(rorc_pci_dev_t *prorc, short print);
int rorcReadRorcStatus(rorcHandle_t handle, rorcStatus_t *status);
char *rorcPrintStatus(rorcHandle_t handle, int prefix);
int rorcCheckDriver(rorcHandle_t dev, int crevision,
                                         int cmajor, int cminor, int crelease);
char* rorcInterpretBusMode(int bus_speed_mode);
char* rorcInterpretBusSpeed(int bus_speed_mode);
char *rorcInterpretReturnCode(int return_code);
void rorcInterpretStatus(rorcHandle_t dev, __u32 status, char* pref, char* suff);
void rorcInterpretErrors(__u32 errors, char* pref, char* suff);
__u32 rorcReadFw(rorcHandle_t handle);
void rorcInterpretVersion(__u32 x);
void rorcInterpretSerial(rorcHwSerial_t hw);
void rorcInterpretFw(__u32 fw);
int rorcReadCounters(rorcHandle_t dev, rorcCounter_t *counter, int inOrout);
int rorcReadRxDmaCount(rorcHandle_t dev);
int rorcReadTxDmaCount(rorcHandle_t dev);
int rorcParamOn(rorc_pci_dev_t *dev, int param);
int rorcParamOff(rorc_pci_dev_t *dev);
int rorcHltFlctlOn(rorc_pci_dev_t *dev);
int rorcHltFlctlOff(rorc_pci_dev_t *dev);
int rorcHltSplitOn(rorc_pci_dev_t *dev);
int rorcHltSplitOff(rorc_pci_dev_t *dev);
int rorcArmDataGenerator(rorc_pci_dev_t *dev,
                          __u32           initEventNumber,
                          __u32           initDataWord,
                          int             dataPattern,
                          int             eventLen,
                          int             seed,
                          int             *rounded_len);
int rorcStartDataGenerator(rorc_pci_dev_t *dev,
                            __u32           maxLoop);
int rorcStopDataGenerator(rorc_pci_dev_t *dev);
int rorcStartDataReceiver(rorc_pci_dev_t        *dev,
                           unsigned long          readyFifoBaseAdress);
int rorcStopDataReceiver(rorc_pci_dev_t *dev);
int rorcStartDownload(rorc_pci_dev_t *dev,
                       unsigned long bufferPhysAddress,
                       unsigned long bufferWordLength,
                       unsigned long returnPhysAddress);
int rorcStopDownload(rorc_pci_dev_t *dev);
int rorcStartJtag(rorc_pci_dev_t        *dev,
                   unsigned long        bufferPhysAddress,
                   unsigned long        bufferWordLength,
                   unsigned long        returnPhysAddress);
int rorcStopJtag(rorc_pci_dev_t        *dev);





#endif /* _RORC_LIB_H_ */
