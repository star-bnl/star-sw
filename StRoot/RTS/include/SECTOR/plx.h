#ifndef _PCI911_PLX_H_
#define _PCI911_PLX_H_

/*
modification history
--------------------
01c,23jul96,snc	 added BTERM definitions for local memory and DMA
01b,04mar96,snc  fixed PCI_INT_LINE macro definition
01a,17may95,snc  Written.
*/

#define VULONG  volatile unsigned long
#define VUSHORT volatile unsigned short
#define VUCHAR  volatile unsigned char

/***** PLX PCI 9060 Definitions *****/
#define PCI9060_UNIT0_BASE		0xA0000000
#define PCI9060_UNIT1_BASE		0x11100000

/* PLX memory base addresses */
#define PLX_UNIT0_MEMBASE		0x10000000
#define PLX_UNIT1_MEMBASE		0x11000000

#define PLX_UNIT0_IOBASE		0x20000000
#define PLX_UNIT1_IOBASE		0x22000000

/* There are two PCI buses installed. */
#define NUM_PCI_BUSES			2
#define MAX_PLX_UNITNUM			1
#define NUM_PCI_SLOTS			8
#define SLOTS_PER_BRIDGE		4
#define PCI_MAX_FUNCTIONS		8

/* Possible PCI interrupt lines */
#define INTA	0
#define INTB	1
#define INTC	2
#define INTD	3

/*
 * Macros to get pointers to word, short,or byte registers in PCI9060.
 * Registers are directly memory-mapped, starting at the base address for
 * the PCI9060 in question.  Some registers are 32 bits, some 16, some 8.
 * Since there are two pci chips, these macros take the base address of
 * the chip as their first parameter, and the offset of the register as
 * their second.
 */

#define PCI_WORD_REG(n,m)	((VULONG *)(n + m))
#define PCI_SHORT_REG(n,m)	((VUSHORT *)(n + m))
#define PCI_CHAR_REG(n,m)	((VUCHAR *)(n + m))

/*
 * PLX PCI9060 Configuration Registers
 */
#define PCI_VENDOR_ID(n)		PCI_SHORT_REG(n,0x00)
#define PCI_DEVICE_ID(n)		PCI_SHORT_REG(n,0x02)
#define PCI_COMMAND(n)			PCI_SHORT_REG(n,0x04)
#define PCI_STATUS(n)			PCI_SHORT_REG(n,0x06)
#define PCI_REV_ID(n)			PCI_CHAR_REG(n,0x08)
/* Note: Class Code Register occupies the upper 24 bits from the word at 0x08*/
#define PCI_CLASS_CODE(n)		PCI_WORD_REG(n,0x08)
#define PCI_CACHE_LINE_SZ(n) 	PCI_CHAR_REG(n,0x0c)
#define PCI_LATENCY_TIMER(n)	PCI_CHAR_REG(n,0x0d)
#define PCI_HEADER_TYPE(n)		PCI_CHAR_REG(n,0x0e)
#define PCI_BIST(n)				PCI_CHAR_REG(n,0x0f)
#define PCI_BASE_MMAP_REG(n) 	PCI_WORD_REG(n,0x10)
#define PCI_BASE_IOMAP_REG(n) 	PCI_WORD_REG(n,0x14)
#define PCI_BASE_LOCAL_MEM(n)	PCI_WORD_REG(n,0x18)
#define PCI_BASE_EXP_ROM(n)		PCI_WORD_REG(n,0x30)
#define PCI_INT_LINE(n)			PCI_CHAR_REG(n,0x3c)
#define PCI_MIN_GNT(n)			PCI_CHAR_REG(n,0x3e)
#define PCI_MAX_LAT(n)			PCI_CHAR_REG(n,0x3f)

/* 
 * Local Configuration Registers - PTOL:  PCI to Local
 *                                 LTOP:  Local to PCI
 *                                 MTOP:  Direct Master to PCI
 *                                 MTOPM: Direct Master to PCI Memory
 *                                 MTOPI: Direct Master to PCI I/O
 */	
#define RANGE_PTOL_MEM(n)      PCI_WORD_REG(n,0x80)
#define LOCAL_BASE_PTOL_MEM(n) PCI_WORD_REG(n,0x84)
#define RANGE_PTOL_ROM(n)      PCI_WORD_REG(n,0x90)
#define BREQO_CONTROL(n)       PCI_WORD_REG(n,0x94)
#define BUS_REGION_DESC_PTOL(n)    PCI_WORD_REG(n,0x98)
#define RANGE_MTOP(n)          PCI_WORD_REG(n,0x9c)
#define LOCAL_BASE_MTOPM(n)    PCI_WORD_REG(n,0xa0)
#define LOCAL_BASE_MTOPI(n)    PCI_WORD_REG(n,0xa4)
#define PCI_BASE_MTOP(n)       PCI_WORD_REG(n,0xa8)
#define PCI_CONFIG_ADDR_REG(n) PCI_WORD_REG(n,0xac)

/* Shared Run-Time Registers */
 
#define PTOL_MBOX0(n)       PCI_WORD_REG(n,0xc0)
#define PTOL_MBOX1(n)       PCI_WORD_REG(n,0xc4)
#define PTOL_MBOX2(n)       PCI_WORD_REG(n,0xc8)
#define PTOL_MBOX3(n)       PCI_WORD_REG(n,0xcc)
#define LTOP_MBOX4(n)       PCI_WORD_REG(n,0xd0)
#define LTOP_MBOX5(n)       PCI_WORD_REG(n,0xd4)
#define LTOP_MBOX6(n)       PCI_WORD_REG(n,0xd8)
#define LTOP_MBOX7(n)   	PCI_WORD_REG(n,0xdc)
#define PTOL_DOORBELL(n)    PCI_WORD_REG(n,0xe0)
#define LTOP_DOORBELL(n)    PCI_WORD_REG(n,0xe4)
#define PCI_INT_CSTAT(n)    PCI_WORD_REG(n,0xe8)
#define PCI_EEPROM_CTL(n)   PCI_WORD_REG(n,0xec)

#define PTOL_DOORBELL_PCI_OFFSET	0x60

/* Local DMA Registers */
 
#define DMA_CH0_MODE(n)     PCI_WORD_REG(n,0x100)
#define DMA_CH0_PADDR(n)    PCI_WORD_REG(n,0x104)
#define DMA_CH0_LADDR(n)    PCI_WORD_REG(n,0x108)
#define DMA_CH0_BCOUNT(n)   PCI_WORD_REG(n,0x10c)
#define DMA_CH0_DPTR(n)     PCI_WORD_REG(n,0x110)
#define DMA_CH1_MODE(n)     PCI_WORD_REG(n,0x114)
#define DMA_CH1_PADDR(n)    PCI_WORD_REG(n,0x118)
#define DMA_CH1_LADDR(n)    PCI_WORD_REG(n,0x11c)
#define DMA_CH1_BCOUNT(n)   PCI_WORD_REG(n,0x120)
#define DMA_CH1_DPTR(n)     PCI_WORD_REG(n,0x124)
#define DMA_CMD_STAT(n)     PCI_WORD_REG(n,0x128)
#define DMA_ARB_REG0(n)     PCI_WORD_REG(n,0x12c)
#define DMA_ARB_REG1(n)     PCI_WORD_REG(n,0x130)

/* PCI Command Register Bit Definitions */
#define PCI_CMD_IOSPACE     (1 << 0)
#define PCI_CMD_MEMSPACE    (1 << 1)
#define PCI_CMD_MASTER_ENAB (1 << 2)
#define PCI_CMD_PARITY_RESP (1 << 6)
#define PCI_CMD_SERR_ENAB   (1 << 8)
#define PCI_CMD_FASTBB_ENAB (1 << 9)
 
/* PCI Status Register Bit Definitions */
#define PCI_STAT_FASTBB_CAP (1 << 7)
#define PCI_DATA_PARITY_ERR (1 << 8)
#define PCI_TARGET_ABORT    (1 << 11)
#define PCI_RCV_TARGET_ABORT    (1 << 12)
#define PCI_RCV_MASTER_ABORT    (1 << 13)
#define PCI_SIGNALLED_SERR  (1 << 14)
#define PCI_BUS_PARITY_ERR  (1 << 15)

/* retrieve the DEVSEL assertion timing */
#define PCI_DEVSEL_TIMING(n) ((*PCI_STATUS(n) & 0x0600) >> 9)

/* PCI Class Code Register Bit Definitions - Read Only */
#define PCI_REG_LEVEL_PROG_IF(n)   ((*PCI_CLASS_CODE(n) & 0x0000ff00) >> 8)
#define PCI_SUB_CLASS_ENCODING(n)  ((*PCI_CLASS_CODE(n) & 0x00ff0000) >> 16)
#define PCI_BASE_CLASS_ENCODING(n) ((*PCI_CLASS_CODE(n) & 0xff000000) >> 24)

/* PCI Built-in Self Test Register Bit Definitions */
#define PCI_DEVICE_SUPPORTS_BIST    (1 << 7)
#define PCI_POST_BIST_RESULTS(n,x)  (*PCI_BIST(n) |= (x & 0x0f))
#define PCI_BIST_INTERRUPT          (1 << 6)

/* Various Interrupt bits and misc. */
#define SIGNALLED_SYSTEM_ERROR  (1 << 14)
#define RCVD_MASTER_ABORT       (1 << 13)
#define RCVD_TARGET_ABORT       (1 << 12)
#define SIGNALLED_TARGET_ABORT  (1 << 11)
#define LOCAL_INT_ACTIVE        (1 << 15)
#define LSERR_INT_ENABLE        (1 << 0)
#define LOCAL_INT_ENABLE        (1 << 16)
#define PCI_INT_ENABLE          (1 << 8)
#define LOCAL_DOORBELL_ENABLE   (1 << 17)
#define PCI_DOORBELL_ENABLE     (1 << 9)
#define DMA_CH0_INT_ENABLE      (1 << 18)
#define DMA_CH1_INT_ENABLE      (1 << 19)
#define LOCAL_DOORBELL_INT      (1 << 20)
#define DMA_CH0_INT             (1 << 21)
#define DMA_CH1_INT             (1 << 22)
#define BIST_INT                (1 << 23)
#define CPU_MASTER_ABORT        (1 << 24)
#define DMA0_MASTER_ABORT       (1 << 25)
#define DMA1_MASTER_ABORT       (1 << 26)
#define RETRIES_TARGET_ABORT    (1 << 27)
#define DMA0_DONE               (1 << 4)
#define DMA1_DONE               (1 << 12)
#define CLEAR_CH0_INTS          (1 << 3)
#define CLEAR_CH1_INTS          (1 << 11)
#define MAX_RETRIES_256         (1 << 12)
#define DMA_CH0_ENABLE          (1 << 0)
#define DMA_CH1_ENABLE          (1 << 8)
#define DMA_CH0_START           (1 << 1)
#define DMA_CH1_START           (1 << 9)
#define DMA_CH0_ABORT           (1 << 2)
#define DMA_CH1_ABORT           (1 << 10)
#define DMA_WRITE               (1 << 3)
#define BIST_NOT_SUPPORTED      (0 << 0)
#define PCI_BURST_TIME          (0xff)          /* 255 clocks, first cut */


/* some macros */
#define CLEAR_SIG_SERR(n)        (*PCI_STATUS(n) |= SIGNALLED_SYSTEM_ERROR)
#define CLEAR_SIG_ABORT(n)       (*PCI_STATUS(n) |= SIGNALLED_TARGET_ABORT)
#define CLEAR_MASTER_ABORT(n)    (*PCI_STATUS(n) |= RCVD_MASTER_ABORT)
#define CLEAR_TARGET_ABORT(n)    (*PCI_STATUS(n) |= RCVD_TARGET_ABORT)
#define CLEAR_BUS_PARITY_ERROR(n) (*PCI_STATUS(n) |= PCI_BUS_PARITY_ERR)
#define CLR_DMA_CH0(n)   (*DMA_CMD_STAT(n) |= CLEAR_CH0_INTS)
#define CLR_DMA_CH1(n)   (*DMA_CMD_STAT(n) |= CLEAR_CH1_INTS)
#define DISABLE_DMA_CH0(n) (*DMA_CMD_STAT(n) &= ~(DMA_CH0_ENABLE))
#define DISABLE_DMA_CH1(n) (*DMA_CMD_STAT(n) &= ~(DMA_CH1_ENABLE))
#define ENABLE_DMA_CH0(n) (*DMA_CMD_STAT(n) |= DMA_CH0_ENABLE)
#define ENABLE_DMA_CH1(n) (*DMA_CMD_STAT(n) |= DMA_CH1_ENABLE)
#define START_DMA_CH0(n)  (*DMA_CMD_STAT(n) |= DMA_CH0_START)
#define START_DMA_CH1(n)  (*DMA_CMD_STAT(n) |= DMA_CH1_START)
#define ABORT_DMA_CH0(n)  (*DMA_CMD_STAT(n) |= DMA_CH0_ABORT)
#define ABORT_DMA_CH1(n)  (*DMA_CMD_STAT(n) |= DMA_CH1_ABORT)
#define DMA_CH0_WRITE(n)  (*DMA_CH0_DPTR(n) |= DMA_WRITE)
#define DMA_CH0_READ(n)   (*DMA_CH0_DPTR(n) &= ~(DMA_WRITE))
#define DMA_CH1_WRITE(n)  (*DMA_CH1_DPTR(n) |= DMA_WRITE)
#define DMA_CH1_READ(n)    (*DMA_CH1_DPTR(n) &= ~(DMA_WRITE))
#define PCI_INIT_DONE(n) (*PCI_EEPROM_CTL(n) |= (1 << 31))
#define IS_DONE_SET(n)   (*PCI_EEPROM_CTL(n) & (1 << 31))
#define DISABLE_LSERR(n) (*PCI_INT_CSTAT(n) &= ~(LSERR_INT_ENABLE))
#define ENABLE_LSERR(n) (*PCI_INT_CSTAT(n) |= LSERR_INT_ENABLE)
#define PCI_IO_BASEADDR() 	(0x20000000)
#define PCI_MEM_BASEADDR() 	(0x00000000)

/* PCI Bus command encodings - really only 4 bits*/
#define PCI_IACK    (unsigned char)0x0
#define PCI_SPECIAL (unsigned char)0x1
#define PCI_IO_RD   (unsigned char)0x2
#define PCI_IO_WR   (unsigned char)0x3
#define PCI_MEM_RD  (unsigned char)0x6
#define PCI_MEM_WR  (unsigned char)0x7
#define PCI_CFG_RD  (unsigned char)0xa
#define PCI_CFG_WR  (unsigned char)0xb
#define PCI_MEM_RD_MULT (unsigned char)0xc
#define PCI_DUAL_ADDR   (unsigned char)0xd
#define PCI_MEM_RD_LINE (unsigned char)0xe
#define PCI_MEM_WR_INV  (unsigned char)0xf

#define DMA_READ_CODE       (PCI_MEM_RD << 0)
#define DMA_WRITE_CODE      (PCI_MEM_WR << 4)
#define MASTER_READ_CODE    (PCI_MEM_RD << 8)
#define MASTER_WRITE_CODE   (PCI_MEM_WR << 12)
 
#define RANGE_DRAM_32     0xfe000000    /* 32 bit mem. space, no prefetch */
#define RANGE_DRAM_8      0xff800000    /* 32 bit mem. space, no prefetch */
#define RANGE_DRAM_2      0xffe00000    /* 32 bit mem. space, no prefetch */
 
#define BASE_DRAM         0x10000001    /* local base and enable */
#define NO_IO_SPACE       0x00000000    /* no I/O CFG space in addr. map  */
#define PCI_MEMORY_SPACE  0x00000000
 
/* BREQo Control */
#define BREQO_ENABLE        (1 << 4)
#define DEADLOCK_TIMEOUT    0x4     /* deadlock after 32 clocks */
#define ROM_REMAP_ADDR      (0xf << 28) /* Region E is Flash ROM     */
 
/* Local Bus Region Descriptor */
#define MEM_BUS_32BIT       0x3
#define MEM_USE_RDY_INPUT   (1 << 6)
#define MEM_USE_BTERM_INPUT (1 << 7)
#define ROM_BUS_8BIT        (0x0 << 16)
#define ROM_USE_RDY_INPUT   (1 << 22)   /* not really necessary */
#define MEM_BURST_ENABLE    (1 << 24)
#define ROM_BURST_DISABLE   (0 << 26)
#define NO_TRDY_WHEN_TXFULL (1 << 27)   /* errata workaround */
#define RETRY_TIMEOUT       (8 << 28)  /* 64 CPU clocks */
 
/* PCI Region Descriptor */
#define PCI_MEM_MASTER_ENAB (1 << 0)
#define PCI_IO_MASTER_DISAB (0 << 1)
#define PCI_IO_MASTER_ENAB  (1 << 1)
#define PCI_LOCK_ENAB       (1 << 2)
#define PCI_PREFETCH_DISAB  (0 << 3)
#define PCI_RELEASE_FIFO_FULL   (0 << 4)
#define PCI_REMAP_ADDR      (0x0000 < 16)
 
#define PCI_CONFIG_DISAB    (0 << 31)
 
#define ROM_DECODE_ENABLE   (1 << 0)
#define ROM_DECODE_DISABLE  (0 << 0)

#define TEST_VAL                (unsigned long) 0x43564d45 /* "CVME" */
#define DRAM_2MEG               0x10200000
#define DRAM_8MEG               0x10800000
#define DRAM_32MEG              0x12000000
 
/* DMA Local Bus Region Descriptor */
#define LOCAL_BUS_32BIT     	0x3
#define USE_RDY_INPUT       	(1 << 6)
#define USE_BTERM_INPUT       	(1 << 7)
#define DMA_BURSTING_ENABLE 	(1 << 8)
#define DMA_BURSTING_DISABLE	(0 << 8)
#define DMA_CHAINING_DISABLED   (0 << 9)
#define DMA_INT_ENDOFTRANSFER   (1 << 10)
 
/* DMA operational stuff */
#define DMA_ARB0_VALUE      0x0         /* Latency and Pause Timers
                           disabled, BREQ input disabled,
                           rotational DMA priority */
#define DMA_ARB1_VALUE      0x0     /* DMAs can request both the
                           PCI and Local busses without
                           regard for # of FIFO entries
                           */
#define DMA_MODE_VALUE      LOCAL_BUS_32BIT | USE_RDY_INPUT

/*******************************************************************************
*
* PCI Configuration Space Header
*
*/
 
typedef struct
    {
    unsigned short  vendor_id;
    unsigned short  device_id;
    unsigned short  command;
    unsigned short  status;
    unsigned char   revision_id;
    unsigned char   prog_if;
    unsigned char   sub_class;
    unsigned char   base_class;
    unsigned char   cache_line_size;
    unsigned char   latency_timer;
    unsigned char   header_type;
    unsigned char   bist;
    unsigned long   pcibase_mm_regs;
    unsigned long   pcibase_im_regs;
    unsigned long   pcibase_local;
    unsigned long   reserved1[5];
    unsigned long   pcibase_exp_rom;
    unsigned long   reserved2[2];
    unsigned char   int_line;
    unsigned char   int_pin;
    unsigned char   min_gnt;
    unsigned char   max_lat;
    } PCI_CONFIG_SPACE;
 
 
/* Cyclone PCI-80960 Device Data */
#define VENDOR_STAR  0x1111     	/* Vendor Id */
#define SUNNY_P1	0x0001
#define SUNNY_P2	0x0002
#define SUNNY_98	0x0003
#define REVISION    	0       	/* Revision */
#define BASE_CLASS  0xff       		/* Base Class = other */
#define SUB_CLASS   0x00        	/* Sub Class = ? */
 
#define LOCAL_TO_PCI_OFFSET     0x40000000

/* configuration offsets */
#define VENDOR_ID_OFFSET	0x00
#define DEVICE_ID_OFFSET	0x02
#define COMMAND_OFFSET		0x04
#define STATUS_OFFSET		0x06
#define REVISION_OFFSET		0x08
#define PROG_IF_OFFSET		0x09
#define SUB_CLASS_OFFSET	0x0a
#define BASE_CLASS_OFFSET	0x0b
#define CACHE_LINE_OFFSET	0x0c
#define LATENCY_TIMER_OFFSET	0x0d
#define HEADER_TYPE_OFFSET	0x0e
#define BIST_OFFSET		0x0f
#define REGION0_BASE_OFFSET	0x10
#define REGION1_BASE_OFFSET	0x14
#define REGION2_BASE_OFFSET	0x18
#define REGION3_BASE_OFFSET	0x1c
#define REGION4_BASE_OFFSET	0x20
#define REGION5_BASE_OFFSET	0x24
#define EXP_ROM_OFFSET		0x30
#define INT_LINE_OFFSET		0x3c
#define INT_PIN_OFFSET		0x3d
#define MIN_GNT_OFFSET		0x3e
#define MAX_LAT_OFFSET		0x3f

#endif
