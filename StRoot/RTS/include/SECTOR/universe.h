/* universe.h - VMEbus Interface Controller */

/* Copyright, 1996-1997 Wind River Systems, Inc. */
/* Copyright 1996 Motorola, Inc. */

/*
modification history
--------------------
01c,18feb97,mas  changed VINT...SW_IACK to VINT...SW_INT per MR #39; changed
		 UNIVERSE_VME_IACK_INT to UNIVERSE_VME_SW_INT (SPR 7811).
01b,02jan97,dat  documentation
01a,10jul96,rhk  written.
*/

#ifndef INCuniverseh
#define INCuniverseh

/*
This file contains constants for the Universe PCI-VME interface chip.
The macro UNIVERSE_BASE_ADRS must be defined when including this header.

The registers are listed in ascending (numerical) order; the definitions
for each register are started with a header eg.
*/

#ifdef  _ASMLANGUAGE
#define CASTINT
#else
#define CASTINT (unsigned int *)
#endif  /* _ASMLANGUAGE */

/*
 * on-board access, register definitions
 * these registers MUST BE WRITTEN 4-BYTE WRITES ONLY
 * they can be read as byte, two-bytes or 4-bytes.
 */

#ifndef UNIVERSE_ADRS   /* to permit alternative board addressing */
#define UNIVERSE_ADRS(reg)   (CASTINT (UNIVERSE_BASE_ADRS + reg )) 
#endif  /* UNIVERSE_ADRS */

#ifndef UNIV_BUS_ADRS   /* to permit alternative board addressing */
#define UNIV_BUS_ADRS(reg)      (CASTINT (UNIV_BUS_BASE_ADRS + (reg)))
#endif  /* UNIV_BUS_ADRS */

#ifndef _ASMLANGUAGE

typedef struct
    {
    UINT32 *regAddr;                   /* Address of register */
    UINT32 regVal;                     /* Value of register */
    UINT32 regMask;                    /* Mask value of register */
    } UNIVERSE_REG_TYPE;

typedef struct
    {
    UINT32     pciBs;                  /* PCI_BS */
    UINT32     mastCtl;                /* MAST_CTL */
    UINT32     miscCtl;                /* MISC_CTL */
    UNIVERSE_REG_TYPE  pciLsi[4][4];   /* LSIx Registers */
    } UNIVERSE_CNFG_HDR;

#endif /*_ASMLANGUAGE*/

/* VME interrupt level definitions */

#define	LVL0			0x0001
#define LVL1			0x0002
#define LVL2			0x0004
#define LVL3			0x0008
#define LVL4			0x0010
#define LVL5			0x0020
#define LVL6			0x0040
#define LVL7			0x0080

/* additional VME interrupts supported by the UNIVERSE chip */

#define UNIVERSE_DMA_INT	0x0100  /* bit setting for DMA intr */
#define UNIVERSE_LERR_INT	0x0200  /* bit setting for PCI bus err intr */
#define UNIVERSE_VERR_INT	0x0400  /* bit setting for VMEbus err int */
#define UNIVERSE_VME_SW_IACK_INT 0x1000  /* bit setting for VME SW IACK intr */
#define UNIVERSE_PCI_SW_INT	0x2000  /* bit setting for SW intr */
#define UNIVERSE_SYSFAIL_INT	0x4000  /* bit setting for SYSFAIL intr */
#define UNIVERSE_ACFAIL_INT	0x8000  /* bit setting for ACFAIL intr */

#define UNIVERSE_INT_MASK	0x0000f700  /* mask for the interrupts
						defined above */

#define UNIVERSE_CNFG_OFFSET    0x100   /* offset VME specific part */

/* WRITES MUST BE 4-BYTE WRITES ONLY */

/*
 * Device ID - Newbridge allocated device ident    0x00    31-16
 * Vendor ID - PCI SIG allocated vendor identifier 0x00    15-00
 */

#define UNIVERSE_PCI_ID         UNIVERSE_ADRS(0x00)

/* PCI Configuration Space Control and Status Reg  0x00    31-00 */

#define UNIVERSE_PCI_CSR        UNIVERSE_ADRS(0x04)

/*
 * PCI Configuration Class Register
 * PCI Base Class Code - "PCI bridge device"       0x10    31-24
 * PCI Sub Class Code - "other bridge device"      0x10    23-16
 * PCI Programming Interface - (Not Applicable)    0x10    15-08
 * Revision ID                                     0x10    07-00
 */

#define UNIVERSE_PCI_CLASS      UNIVERSE_ADRS(0x08)

/* PCI Configuration Miscellaneous 0 Register      0x00    31-00 */

#define UNIVERSE_PCI_MISC0      UNIVERSE_ADRS(0x0c)

/*
 * PCI Base Address Register                       0x08    31-16
 * PCI Bus Address Space Register                  0x08    15-00
 */

#define UNIVERSE_PCI_BS         UNIVERSE_ADRS(0x10)

/* PCI Configuration Miscellaneous 1 Register      0x00    31-00 */

#define UNIVERSE_PCI_MISC1      UNIVERSE_ADRS(0x3c)

/* PCI Slave Image 0 Control Register              0x100   31-00 */

#define UNIVERSE_LSI0_CTL       UNIVERSE_ADRS(0x100)

/*
 * PCI Slave Image 0 Base Address Register         0x104   31-12
 * Universe Reserved                               0x104   11-00
 */

#define UNIVERSE_LSI0_BS        UNIVERSE_ADRS(0x104)

/*
 * PCI Slave Image 0 Bound Address Register        0x108   31-12
 * Universe Reserved                               0x108   11-00
 */

#define UNIVERSE_LSI0_BD        UNIVERSE_ADRS(0x108)

/*
 * PCI Slave Image 0 Translation Offset            0x10C   31-12
 * Universe Reserved                               0x10C   11-00
 */

#define UNIVERSE_LSI0_TO        UNIVERSE_ADRS(0x10c)

/* PCI Slave Image 1 Control Register              0x114   31-00 */

#define UNIVERSE_LSI1_CTL       UNIVERSE_ADRS(0x114)

/*
 * PCI Slave Image 1 Base Address Register         0x118   31-12
 * Universe Reserved                               0x118   11-00
 */

#define UNIVERSE_LSI1_BS        UNIVERSE_ADRS(0x118)

/*
 * PCI Slave Image 1 Bound Address Register        0x11C   31-12
 * Universe Reserved                               0x11C   11-00
 */

#define UNIVERSE_LSI1_BD        UNIVERSE_ADRS(0x11C)

/*
 * PCI Slave Image 1 Translation Offset            0x120   31-12
 * Universe Reserved                               0x120   11-00
 */

#define UNIVERSE_LSI1_TO        UNIVERSE_ADRS(0x120)

/* PCI Slave Image 2 Control Register              0x128   31-00 */

#define UNIVERSE_LSI2_CTL       UNIVERSE_ADRS(0x128)

/*
 * PCI Slave Image 2 Base Address Register         0x12C   31-12
 * Universe Reserved                               0x12C   11-00
 */

#define UNIVERSE_LSI2_BS        UNIVERSE_ADRS(0x12C)

/*
 * PCI Slave Image 2 Bound Address Register        0x130   31-12
 * Universe Reserved                               0x130   11-00
 */

#define UNIVERSE_LSI2_BD        UNIVERSE_ADRS(0x130)

/*
 * PCI Slave Image 2 Translation Offset            0x134   31-12
 * Universe Reserved                               0x134   11-00
 */

#define UNIVERSE_LSI2_TO        UNIVERSE_ADRS(0x134)

/* PCI Slave Image 3 Control Register              0x13C   31-00 */

#define UNIVERSE_LSI3_CTL       UNIVERSE_ADRS(0x13C)

/*
 * PCI Slave Image 3 Base Address Register         0x140   31-12
 * Universe Reserved                               0x140   11-00
 */

#define UNIVERSE_LSI3_BS        UNIVERSE_ADRS(0x140)

/*
 * PCI Slave Image 3 Bound Address Register        0x144   31-12
 * Universe Reserved                               0x144   11-00
 */

#define UNIVERSE_LSI3_BD        UNIVERSE_ADRS(0x144)

/*
 * PCI Slave Image 3 Translation Offset            0x148   31-12
 * Universe Reserved                               0x148   11-00
 */

#define UNIVERSE_LSI3_TO        UNIVERSE_ADRS(0x148)

/*
 * Universe Reserved                               0x170   31-02
 * Special Cycle                                   0x170   01-00
 */

#define UNIVERSE_SCYC_CTL       UNIVERSE_ADRS(0x170)

/*
 * Address for Special Cycle                       0x174   31-02
 * Universe Reserved                               0x174   01-00
 */

#define UNIVERSE_SCYC_ADDR      UNIVERSE_ADRS(0x174)

/* Special Cycle Bit Enable Mask                   0x178   31-00 */

#define UNIVERSE_SCYC_EN        UNIVERSE_ADRS(0x178)

/* Special Cycle Compare Register                  0x178   31-00 */

#define UNIVERSE_SCYC_CMP       UNIVERSE_ADRS(0x17c)

/* Special Cycle Swap Register                     0x178   31-00 */

#define UNIVERSE_SCYC_SWP       UNIVERSE_ADRS(0x180)

/* Other Registers */

#define UNIVERSE_LMISC          UNIVERSE_ADRS(0x184)
#define UNIVERSE_SLSI           UNIVERSE_ADRS(0x188)
#define UNIVERSE_L_CMDERR       UNIVERSE_ADRS(0x18c)
#define UNIVERSE_LAERR          UNIVERSE_ADRS(0x190)
#define UNIVERSE_DCTL           UNIVERSE_ADRS(0x200)
#define UNIVERSE_DTBC           UNIVERSE_ADRS(0x204)
#define UNIVERSE_DLA            UNIVERSE_ADRS(0x208)
#define UNIVERSE_DVA            UNIVERSE_ADRS(0x210)
#define UNIVERSE_DCPP           UNIVERSE_ADRS(0x218)
#define UNIVERSE_DGCS           UNIVERSE_ADRS(0x220)
#define UNIVERSE_D_LLUE         UNIVERSE_ADRS(0x224)
#define UNIVERSE_LINT_EN        UNIVERSE_ADRS(0x300)
#define UNIVERSE_LINT_STAT      UNIVERSE_ADRS(0x304)
#define UNIVERSE_LINT_MAP0      UNIVERSE_ADRS(0x308)
#define UNIVERSE_LINT_MAP1      UNIVERSE_ADRS(0x30C)
#define UNIVERSE_VINT_EN        UNIVERSE_ADRS(0x310)
#define UNIVERSE_VINT_STAT      UNIVERSE_ADRS(0x314)
#define UNIVERSE_VINT_MAP0      UNIVERSE_ADRS(0x318)
#define UNIVERSE_VINT_MAP1      UNIVERSE_ADRS(0x31C)
#define UNIVERSE_STATID         UNIVERSE_ADRS(0x320)
#define UNIVERSE_V1_STATID      UNIVERSE_ADRS(0x324)
#define UNIVERSE_V2_STATID      UNIVERSE_ADRS(0x328)
#define UNIVERSE_V3_STATID      UNIVERSE_ADRS(0x32C)
#define UNIVERSE_V4_STATID      UNIVERSE_ADRS(0x330)
#define UNIVERSE_V5_STATID      UNIVERSE_ADRS(0x334)
#define UNIVERSE_V6_STATID      UNIVERSE_ADRS(0x338)
#define UNIVERSE_V7_STATID      UNIVERSE_ADRS(0x33C)
#define UNIVERSE_MAST_CTL       UNIVERSE_ADRS(0x400)
#define UNIVERSE_MISC_CTL       UNIVERSE_ADRS(0x404)
#define UNIVERSE_MISC_STAT      UNIVERSE_ADRS(0x408)
#define UNIVERSE_USER_AM        UNIVERSE_ADRS(0x40C)
#define UNIVERSE_VSI0_CTL       UNIVERSE_ADRS(0xF00)
#define UNIVERSE_VSI0_BS        UNIVERSE_ADRS(0xF04)
#define UNIVERSE_VSI0_BD        UNIVERSE_ADRS(0xF08)
#define UNIVERSE_VSI0_TO        UNIVERSE_ADRS(0xF0C)
#define UNIVERSE_VSI1_CTL       UNIVERSE_ADRS(0xF14)
#define UNIVERSE_VSI1_BS        UNIVERSE_ADRS(0xF18)
#define UNIVERSE_VSI1_BD        UNIVERSE_ADRS(0xF1C)
#define UNIVERSE_VSI1_TO        UNIVERSE_ADRS(0xF20)
#define UNIVERSE_VSI2_CTL       UNIVERSE_ADRS(0xF28)
#define UNIVERSE_VSI2_BS        UNIVERSE_ADRS(0xF2C)
#define UNIVERSE_VSI2_BD        UNIVERSE_ADRS(0xF30)
#define UNIVERSE_VSI2_TO        UNIVERSE_ADRS(0xF34)
#define UNIVERSE_VSI3_CTL       UNIVERSE_ADRS(0xF3C)
#define UNIVERSE_VSI3_BS        UNIVERSE_ADRS(0xF40)
#define UNIVERSE_VSI3_BD        UNIVERSE_ADRS(0xF44)
#define UNIVERSE_VSI3_TO        UNIVERSE_ADRS(0xF48)
#define UNIVERSE_VRAI_CTL       UNIVERSE_ADRS(0xF70)
#define UNIVERSE_VRAI_BS        UNIVERSE_ADRS(0xF74)
#define UNIVERSE_VCSR_CTL       UNIVERSE_ADRS(0xF80)
#define UNIVERSE_VCSR_TO        UNIVERSE_ADRS(0xF84)
#define UNIVERSE_V_AMERR        UNIVERSE_ADRS(0xF88)
#define UNIVERSE_VAERR          UNIVERSE_ADRS(0xF8C)
#define UNIVERSE_VCSR_CLR       UNIVERSE_ADRS(0xFF4)
#define UNIVERSE_VCSR_SET       UNIVERSE_ADRS(0xFF8)
#define UNIVERSE_VCSR_BS        UNIVERSE_ADRS(0xFFC)

/* NOW LET'S DEFINE THE BITS FOR THESE REGISTERS */

/* PCI MISC0 Register */

#define PCI_MISC0_LATENCY_TIMER 0x0000f800      /* max. value for timer */

/* PCI Configuration Space Control and Status Register */

#define PCI_CSR_MASK            0x007ffc00      /* Reserved bits */
#define PCI_CSR_D_PE            (1 << 31)       /* Detected/Clear Parity Error*/
#define PCI_CSR_S_SERR          (1 << 30)       /* Signalled SERR#            */
#define PCI_CSR_R_MA            (1 << 29)       /* Received Master Abort      */
#define PCI_CSR_R_TA            (1 << 28)       /* Received Target Abort      */
#define PCI_CSR_S_TA            (1 << 27)       /* Signalled Target Abort     */
#define PCI_CSR_DEVSEL_MEDIUM   (1 << 25)       /* Universe is medium speed   */
#define PCI_CSR_DP_D            (1 << 24)       /* Master detected/generated  */
                                                /* a data parity error        */
#define PCI_CSR_TFBBC           (1 << 23)       /* Target Fast Back to Back   */
                                                /* Capable (must be ?)        */
#define PCI_CSR_MFBBC           (1 << 9)        /* Master Fast Back to Back   */
                                                /* Capable (must be 0)        */
#define PCI_CSR_SERR_EN         (1 << 8)        /* Enable SERR# drivers       */
#define PCI_CSR_WAIT            (1 << 7)        /* Wait Cycle Control         */
#define PCI_CSR_PERSP           (1 << 6)        /* Enable Parity Error Resp   */
#define PCI_CSR_VGAPS           (1 << 5)        /* VGA Palette Snp (must be 0)*/
#define PCI_CSR_MWI_EN          (1 << 4)        /* Enable Memory Write and    */
                                                /* Invalidate (must be 0)     */
#define PCI_CSR_SC              (1 << 3)        /* Respond to Special Cycles  */
                                                /* (must be 0)                */
#define PCI_CSR_BM              (1 << 2)        /* Master Enable              */
#define PCI_CSR_MS              (1 << 1)        /* Target Memory Enable       */
#define PCI_CSR_IOS             (1)             /* Target I/O Enable          */

/* PCI Slave Image Control Register 0 */

#define LSI0_CTL_MASK           0x3f380efc      /* Mask bits */
#define LSI0_CTL_EN             (1 << 31)       /* Enable PCI Slave Image     */
#define LSI0_CTL_WP             (1 << 30)       /* Enable Posted Writes       */
#define LSI0_CTL_D8             (0 << 22)       /* Max VME Data Width = 8     */
#define LSI0_CTL_D16            (1 << 22)       /* Max VME Data Width = 16    */
#define LSI0_CTL_D32            (2 << 22)       /* Max VME Data Width = 32    */
#define LSI0_CTL_D64            (3 << 22)       /* Max VME Data Width = 64    */
#define LSI0_CTL_A16            (0 << 16)       /* VME Address Space A16      */
#define LSI0_CTL_A24            (1 << 16)       /* VME Address Space A24      */
#define LSI0_CTL_A32            (2 << 16)       /* VME Address Space A32      */
#define LSI0_CTL_CSR            (5 << 16)       /* VME Address Space CSR      */
#define LSI0_CTL_USER1          (6 << 16)       /* VME Address Space USER 1   */
#define LSI0_CTL_USER2          (7 << 16)       /* VME Address Space USER 2   */
#define LSI0_CTL_PGM            (1 << 14)       /* Program AM Code            */
#define LSI0_CTL_DATA           (0 << 14)       /* Data AM Code               */
#define LSI0_CTL_SUP            (1 << 12)       /* Supervisor AM Code         */
#define LSI0_CTL_USR            (0 << 12)       /* User AM Code               */
#define LSI0_CTL_BLK            (1 << 8)        /* Supervisor AM Code         */
#define LSI0_CTL_SINGLE         (0 << 8)        /* User AM Code               */
#define LSI0_CTL_PCI_MEM        (0 << 0)        /* PCI Memory Space           */
#define LSI0_CTL_PCI_IO         (1 << 0)        /* PCI I/O Space              */
#define LSI0_CTL_PCI_CONFIG     (2 << 0)        /* PCI Type 1 Config Space    */

/* PCI Slave Image Base Address Register 0 */

#define LSI0_BS_MASK    0x00000fff

/* PCI Slave Image Bound Address Register 0 */

#define LSI0_BD_MASK    0x00000fff

/* PCI Slave Image Translation Offset Register 0 */

#define LSI0_TO_MASK    0x00000fff

/* PCI Slave Image Control Register 1 */

#define LSI1_CTL_MASK           0x3f380efc      /* Mask bits */
#define LSI1_CTL_EN             (1 << 31)       /* Enable PCI Slave Image     */
#define LSI1_CTL_WP             (1 << 30)       /* Enable Posted Writes       */
#define LSI1_CTL_D8             (0 << 22)       /* Max VME Data Width = 8     */
#define LSI1_CTL_D16            (1 << 22)       /* Max VME Data Width = 16    */
#define LSI1_CTL_D32            (2 << 22)       /* Max VME Data Width = 32    */
#define LSI1_CTL_D64            (3 << 22)       /* Max VME Data Width = 64    */
#define LSI1_CTL_A16            (0 << 16)       /* VME Address Space A16      */
#define LSI1_CTL_A24            (1 << 16)       /* VME Address Space A24      */
#define LSI1_CTL_A32            (2 << 16)       /* VME Address Space A32      */
#define LSI1_CTL_CSR            (5 << 16)       /* VME Address Space CSR      */
#define LSI1_CTL_USER1          (6 << 16)       /* VME Address Space USER 1   */
#define LSI1_CTL_USER2          (7 << 16)       /* VME Address Space USER 2   */
#define LSI1_CTL_PGM            (1 << 14)       /* Program AM Code            */
#define LSI1_CTL_DATA           (0 << 14)       /* Data AM Code               */
#define LSI1_CTL_SUP            (1 << 12)       /* Supervisor AM Code         */
#define LSI1_CTL_USR            (0 << 12)       /* User AM Code               */
#define LSI1_CTL_BLK            (1 << 8)        /* Supervisor AM Code         */
#define LSI1_CTL_SINGLE         (0 << 8)        /* User AM Code               */
#define LSI1_CTL_PCI_MEM        (0 << 0)        /* PCI Memory Space           */
#define LSI1_CTL_PCI_IO         (1 << 0)        /* PCI I/O Space              */
#define LSI1_CTL_PCI_CONFIG     (2 << 0)        /* PCI Type 1 Config Space    */

/* PCI Slave Image Base Address Register 1 */

#define LSI1_BS_MASK    0x0000ffff

/* PCI Slave Image Bound Address Register 1 */

#define LSI1_BD_MASK    0x0000ffff

/* PCI Slave Image Translation Offset Register 1 */

#define LSI1_TO_MASK    0x0000ffff

/* PCI Slave Image Control Register 2 */

#define LSI2_CTL_MASK           0x3f380efc      /* Mask bits */
#define LSI2_CTL_EN             (1 << 31)       /* Enable PCI Slave Image     */
#define LSI2_CTL_WP             (1 << 30)       /* Enable Posted Writes       */
#define LSI2_CTL_D8             (0 << 22)       /* Max VME Data Width = 8     */
#define LSI2_CTL_D16            (1 << 22)       /* Max VME Data Width = 16    */
#define LSI2_CTL_D32            (2 << 22)       /* Max VME Data Width = 32    */
#define LSI2_CTL_D64            (3 << 22)       /* Max VME Data Width = 64    */
#define LSI2_CTL_A16            (0 << 16)       /* VME Address Space A16      */
#define LSI2_CTL_A24            (1 << 16)       /* VME Address Space A24      */
#define LSI2_CTL_A32            (2 << 16)       /* VME Address Space A32      */
#define LSI2_CTL_CSR            (5 << 16)       /* VME Address Space CSR      */
#define LSI2_CTL_USER1          (6 << 16)       /* VME Address Space USER 1   */
#define LSI2_CTL_USER2          (7 << 16)       /* VME Address Space USER 2   */
#define LSI2_CTL_PGM            (1 << 14)       /* Program AM Code            */
#define LSI2_CTL_DATA           (0 << 14)       /* Data AM Code               */
#define LSI2_CTL_SUP            (1 << 12)       /* Supervisor AM Code         */
#define LSI2_CTL_USR            (0 << 12)       /* User AM Code               */
#define LSI2_CTL_BLK            (1 << 8)        /* Supervisor AM Code         */
#define LSI2_CTL_SINGLE         (0 << 8)        /* User AM Code               */
#define LSI2_CTL_PCI_MEM        (0 << 0)        /* PCI Memory Space           */
#define LSI2_CTL_PCI_IO         (1 << 0)        /* PCI I/O Space              */
#define LSI2_CTL_PCI_CONFIG     (2 << 0)        /* PCI Type 1 Config Space    */

/* PCI Slave Image Base Address Register 2 */

#define LSI2_BS_MASK    0x0000ffff

/* PCI Slave Image Bound Address Register 2 */

#define LSI2_BD_MASK    0x0000ffff

/* PCI Slave Image Translation Offset Register 2 */

#define LSI2_TO_MASK    0x0000ffff

/* PCI Slave Image Control Register 3 */

#define LSI3_CTL_MASK           0x3f380efc      /* Mask bits */
#define LSI3_CTL_EN             (1 << 31)       /* Enable PCI Slave Image     */
#define LSI3_CTL_WP             (1 << 30)       /* Enable Posted Writes       */
#define LSI3_CTL_D8             (0 << 22)       /* Max VME Data Width = 8     */
#define LSI3_CTL_D16            (1 << 22)       /* Max VME Data Width = 16    */
#define LSI3_CTL_D32            (2 << 22)       /* Max VME Data Width = 32    */
#define LSI3_CTL_D64            (3 << 22)       /* Max VME Data Width = 64    */
#define LSI3_CTL_A16            (0 << 16)       /* VME Address Space A16      */
#define LSI3_CTL_A24            (1 << 16)       /* VME Address Space A24      */
#define LSI3_CTL_A32            (2 << 16)       /* VME Address Space A32      */
#define LSI3_CTL_CSR            (5 << 16)       /* VME Address Space CSR      */
#define LSI3_CTL_USER1          (6 << 16)       /* VME Address Space USER 1   */
#define LSI3_CTL_USER2          (7 << 16)       /* VME Address Space USER 2   */
#define LSI3_CTL_PGM            (1 << 14)       /* Program AM Code            */
#define LSI3_CTL_DATA           (0 << 14)       /* Data AM Code               */
#define LSI3_CTL_SUP            (1 << 12)       /* Supervisor AM Code         */
#define LSI3_CTL_USR            (0 << 12)       /* User AM Code               */
#define LSI3_CTL_BLK            (1 << 8)        /* Supervisor AM Code         */
#define LSI3_CTL_SINGLE         (0 << 8)        /* User AM Code               */
#define LSI3_CTL_PCI_MEM        (0 << 0)        /* PCI Memory Space           */
#define LSI3_CTL_PCI_IO         (1 << 0)        /* PCI I/O Space              */
#define LSI3_CTL_PCI_CONFIG     (2 << 0)        /* PCI Type 1 Config Space    */

/* PCI Slave Image Base Address Register 3 */

#define LSI3_BS_MASK    0x0000ffff

/* PCI Slave Image Bound Address Register 3 */

#define LSI3_BD_MASK    0x0000ffff

/* PCI Slave Image Translation Offset Register 3 */

#define LSI3_TO_MASK    0x0000ffff

/* PCI Special Cycle Control Register */

#define SCYC_CTL_MASK   0xfffffffc
#define SCYC_CTL_DISABLE        (0)             /* Disable Special Cycle Gen  */
#define SCYC_CTL_RMW            (1)             /* RMW Special Cycle          */
#define SCYC_CTL_ADO            (2)             /* ADO Special Cycle          */

/* PCI Special Cycle Address Register */

#define SCYC_ADDR_MASK  0x3

/* PCI Miscellaneous Register */

#define LMISC_CRT_INFINITE      (0 << 28)       /* Coupled Request Timeout */
#define LMISC_CRT_128_USEC      (1 << 28)       /* Coupled Request Timeout */
#define LMISC_CRT_256_USEC      (2 << 28)       /* Coupled Request Timeout */
#define LMISC_CRT_512_USEC      (3 << 28)       /* Coupled Request Timeout */
#define LMISC_CRT_1024_USEC     (4 << 28)       /* Coupled Request Timeout */
#define LMISC_CRT_2048_USEC     (5 << 28)       /* Coupled Request Timeout */
#define LMISC_CRT_4096_USEC     (6 << 28)       /* Coupled Request Timeout */

#define LMISC_CWT_DISABLE       (0 << 24)       /* Coupled Window Timeout     */
                                /* Immediate Release after first transaction  */
#define LMISC_CWT_16_CLKS       (1 << 24)       /* Coupled Window Timeout     */
#define LMISC_CWT_32_CLKS       (2 << 24)       /* Coupled Window Timeout     */
#define LMISC_CWT_64_CLKS       (3 << 24)       /* Coupled Window Timeout     */
#define LMISC_CWT_128_CLKS      (4 << 24)       /* Coupled Window Timeout     */
#define LMISC_CWT_256_CLKS      (5 << 24)       /* Coupled Window Timeout     */
#define LMISC_CWT_512_CLKS      (6 << 24)       /* Coupled Window Timeout     */

/*
 * Special PCI Slave Image
 * - provides access to all of A16 and most of A24 VME Space
 */
#define SLSI_EN                 (1 << 31)       /* Enable PCI Slave Image     */
#define SLSI_WP                 (1 << 30)       /* Enable Posted Writes       */
#define SLSI_D16                (1 << 20)       /* Max VME Data Width = 16    */
#define SLSI_D32                (2 << 20)       /* Max VME Data Width = 32    */
#define SLSI_PGM                (1 << 12)       /* Program AM Code            */
#define SLSI_DATA               (0 << 12)       /* Data AM Code               */
#define SLSI_SUP                (1 << 8)        /* Supervisor AM Code         */
#define SLSI_USR                (0 << 8)        /* User AM Code               */
#define SLSI_PCI_MEM            (0 << 0)        /* PCI Memory Space           */
#define SLSI_PCI_IO             (1 << 0)        /* PCI I/O Space              */
#define SLSI_PCI_CONFIG         (2 << 0)        /* PCI Type 1 Config Space    */

/* PCI Command Error Log Register */

#define L_CMDERR_LOG            (0xf << 28)     /* Command Error Log          */
#define L_CMDERR_MASK           0x078fffff      /* Reserved bits */
#define L_CMDERR_M_ERR          (1 << 27)       /* Multiple Error Occurred    */
#define L_CMDERR_L_STAT         (1 << 23)       /* Logs are valid and halted  */
#define L_CMDERR_L_ENABLE       (1 << 23)       /* Clear and Enable Logging   */

/* DMA Transfer Control Register */

#define DCTL_MASK               0x7f380e7f      /* Reserved bits */
#define DCTL_L2V                (1 << 31)       /* PCI-to-VME transfer */
#define DCTL_VDW_8              (0)             /* Maximum data width 8 bits */
#define DCTL_VDW_16             (1 << 22)       /* Maximum data width 16 bits */
#define DCTL_VDW_32             (2 << 22)       /* Maximum data width 32 bits */
#define DCTL_VDW_64             (3 << 22)       /* Maximum data width 64 bits */
#define DCTL_VAS_A16            (0)             /* VME address space A16 */
#define DCTL_VAS_A24            (1 << 16)       /* VME address space A24 */
#define DCTL_VAS_A32            (2 << 16)       /* VME address space A32 */
#define DCTL_VAS_USER1          (6 << 16)       /* VME address space User1 */
#define DCTL_VAS_USER2          (7 << 16)       /* VME address space User2 */
#define DCTL_PGM_DATA           (0)             /* Data AM code */
#define DCTL_PGM_PRGM           (1 << 14)       /* Program AM code */
#define DCTL_SUPER_USER         (0)             /* Non-privileged AM code */
#define DCTL_SUPER_SUP          (1 << 12)       /* Supervisor AM code */
#define DCTL_VCT_EN             (1 << 8)        /* Block mode capable */
#define DCTL_LD64EN             (1 << 7)        /* 64-bit PCI transactions */

/* DMA Transfer Byte Count Register */

#define DTBC_MASK       0xff000000

/* DMA General Control/Status Register */

#define DGCS_MASK               0x00000000      /* Reserved bits */
#define DGCS_GO                 (1 << 31)       /* Start DMA */
#define DGCS_STOP_REQ           (1 << 30)       /* Stop Request */
#define DGCS_HALT_REQ           (1 << 29)       /* Halt Request */
#define DGCS_CHAIN              (1 << 27)       /* DMA chaining */
#define DGCS_VON_DONE           (0)             /* Transfer count until done*/
#define DGCS_VON_256            (1 << 20)       /* Transfer count 256 bytes */
#define DGCS_VON_512            (2 << 20)       /* Transfer count 512 bytes */
#define DGCS_VON_1024           (3 << 20)       /* Transfer count 1024 bytes */
#define DGCS_VON_2048           (4 << 20)       /* Transfer count 2048 bytes */
#define DGCS_VON_4096           (5 << 20)       /* Transfer count 4096 bytes */
#define DGCS_VON_8192           (6 << 20)       /* Transfer count 4096 bytes */
#define DGCS_VON_16384          (7 << 20)       /* Transfer count 16384 bytes */
#define DGCS_VOFF_0             (0)             /* Time off VME bus0 us */
#define DGCS_VOFF_16            (1 << 16)       /* Time off VME bus16 us */
#define DGCS_VOFF_32            (2 << 16)       /* Time off VME bus32 us */
#define DGCS_VOFF_64            (3 << 16)       /* Time off VME bus64 us */
#define DGCS_VOFF_128           (4 << 16)       /* Time off VME bus128 us */
#define DGCS_VOFF_256           (5 << 16)       /* Time off VME bus256 us */
#define DGCS_VOFF_512           (6 << 16)       /* Time off VME bus512 us */
#define DGCS_VOFF_1024          (7 << 16)       /* Time off VME bus1024 us */
#define DGCS_ACT                (1 << 15)       /* DMA active */
#define DGCS_STOP               (1 << 14)       /* DMA stopped */
#define DGCS_HALT               (1 << 13)       /* DMA halted */
#define DGCS_DONE               (1 << 11)       /* DMA done */
#define DGCS_LERR               (1 << 10)       /* PCI bus error */
#define DGCS_VERR               (1 << 9)        /* VME bus error */
#define DGCS_P_ERR              (1 << 8)        /* Protocol error */
#define DGCS_INT_STOP           (1 << 6)        /* Interrupt stop enable */
#define DGCS_INT_HALT           (1 << 5)        /* Interrupt halt enable */
#define DGCS_INT_DONE           (1 << 3)        /* Interrupt done enable */
#define DGCS_INT_LERR           (1 << 2)        /* Interrupt LERR enable */
#define DGCS_INT_VERR           (1 << 1)        /* Interrupt VERR enable */
#define DGCS_INT_P_ERR          (1)             /* Interrupt protocol enable */

/* DMA Linked List Update Enable Register */

/* PCI Configuration Base Address Register */

#define PCI_BS_SPACE            (1)             /* Memory or I/O power-up */

/* PCI Interrupt Enable Register */

#define LINT_EN_MASK            0xffff0000      /* Reserved bits */
#define LINT_EN_ACFAIL          (1 << 15)       /* Enable Interrupt ACFAIL   */
#define LINT_EN_SYSFAIL         (1 << 14)       /* Enable Interrupt SYSFAIL  */
#define LINT_EN_SW_INT          (1 << 13)       /* Enable Interrupt SW_INT   */
#define LINT_EN_SW_IACK         (1 << 12)       /* Enable Interrupt SW_ACK   */
#define LINT_EN_VERR            (1 << 10)       /* Enable Interrupt ACFAIL   */
#define LINT_EN_LERR            (1 << 9)        /* Enable Interrupt ACFAIL   */
#define LINT_EN_DMA             (1 << 8)        /* Enable Interrupt ACFAIL   */
#define LINT_EN_VIRQ7           (1 << 7)        /* Enable Interrupt VME IRQ7 */
#define LINT_EN_VIRQ6           (1 << 6)        /* Enable Interrupt VME IRQ6 */
#define LINT_EN_VIRQ5           (1 << 5)        /* Enable Interrupt VME IRQ5 */
#define LINT_EN_VIRQ4           (1 << 4)        /* Enable Interrupt VME IRQ4 */
#define LINT_EN_VIRQ3           (1 << 3)        /* Enable Interrupt VME IRQ3 */
#define LINT_EN_VIRQ2           (1 << 2)        /* Enable Interrupt VME IRQ2 */
#define LINT_EN_VIRQ1           (1 << 1)        /* Enable Interrupt VME IRQ1 */
#define LINT_EN_VOWN            (1 << 0)        /* Enable Interrupt VME OWN */

/* PCI Interrupt Status Register */

#define LINT_STAT_MASK          0xffff0800      /* Reserved bits */
#define LINT_STAT_ACFAIL        (1 << 15)       /* Status Interrupt ACFAIL   */
#define LINT_STAT_SYSFAIL       (1 << 14)       /* Status Interrupt SYSFAIL  */
#define LINT_STAT_SW_INT        (1 << 13)       /* Status Interrupt SW_INT   */
#define LINT_STAT_SW_IACK       (1 << 12)       /* Status Interrupt SW_ACK   */
#define LINT_STAT_VERR          (1 << 10)       /* Status Interrupt ACFAIL   */
#define LINT_STAT_LERR          (1 << 9)        /* Status Interrupt ACFAIL   */
#define LINT_STAT_DMA           (1 << 8)        /* Status Interrupt ACFAIL   */
#define LINT_STAT_VIRQ7         (1 << 7)        /* Status Interrupt VME IRQ7 */
#define LINT_STAT_VIRQ6         (1 << 6)        /* Status Interrupt VME IRQ6 */
#define LINT_STAT_VIRQ5         (1 << 5)        /* Status Interrupt VME IRQ5 */
#define LINT_STAT_VIRQ4         (1 << 4)        /* Status Interrupt VME IRQ4 */
#define LINT_STAT_VIRQ3         (1 << 3)        /* Status Interrupt VME IRQ3 */
#define LINT_STAT_VIRQ2         (1 << 2)        /* Status Interrupt VME IRQ2 */
#define LINT_STAT_VIRQ1         (1 << 1)        /* Status Interrupt VME IRQ1 */
#define LINT_STAT_VOWN          (1 << 0)        /* Status Interrupt VME OWN */

#define LINT_STAT_CLEAR		0xf7ff		/* clear all interrupts */
#define LINT_STAT_INT_MASK	0xd7ff		/* mask received interrupts */
#define LINT_STAT_FAIL_MASK     0x0000c000      /* mask for sysfail,acfail */

/* PCI Interrupt MAP Register 0 */

#define LINT_MAP0_MASK  0x88888888

/* PCI Interrupt MAP Register 1 */

#define LINT_MAP1_MASK  0x8888f888

/* VMEbus Interrupt Enable Register */

#define VINT_EN_MASK            0xffffe800      /* Reserved bits */
#define VINT_EN_SW_INT          (1 << 12)       /* SW_INT interrupt generate */
#define VINT_EN_VERR            (1 << 10)       /* VERR interrupt generate */
#define VINT_EN_LERR            (1 << 9)        /* LERR enable */
#define VINT_EN_DMA             (1 << 8)        /* DMA enable */
#define VINT_EN_LINT7           (1 << 7)        /* LINT7 enable */
#define VINT_EN_LINT6           (1 << 6)        /* LINT6 enable */
#define VINT_EN_LINT5           (1 << 5)        /* LINT5 enable */
#define VINT_EN_LINT4           (1 << 4)        /* LINT4 enable */
#define VINT_EN_LINT3           (1 << 3)        /* LINT3 enable */
#define VINT_EN_LINT2           (1 << 2)        /* LINT2 enable */
#define VINT_EN_LINT1           (1 << 1)        /* LINT1 enable */
#define VINT_EN_LINT0           (1)             /* LINT0 enable */

/* VMEbus Interrupt Status Register */

#define VINT_STAT_MASK          0xffffe800      /* Reserved bits */
#define VINT_STAT_SW_INT        (1 << 12)       /* SW_INT interrupt active */
#define VINT_STAT_VERR          (1 << 10)       /* VERR interrupt active */
#define VINT_STAT_LERR          (1 << 9)        /* LERR interrupt active */
#define VINT_STAT_DMA           (1 << 8)        /* DMA interrupt active */
#define VINT_STAT_LINT7         (1 << 7)        /* LINT7 interrupt active */
#define VINT_STAT_LINT6         (1 << 6)        /* LINT6 interrupt active */
#define VINT_STAT_LINT5         (1 << 5)        /* LINT5 interrupt active */
#define VINT_STAT_LINT4         (1 << 4)        /* LINT4 interrupt active */
#define VINT_STAT_LINT3         (1 << 3)        /* LINT3 interrupt active */
#define VINT_STAT_LINT2         (1 << 2)        /* LINT2 interrupt active */
#define VINT_STAT_LINT1         (1 << 1)        /* LINT1 interrupt active */
#define VINT_STAT_LINT0         (1)             /* LINT0 interrupt active */

#define VINT_STAT_CLEAR		0x17ff		/* clear outgoing VME intrs. */

/* VMEbus Interrupt Map Register 0 */

#define VINT_MAP0_MASK          0x88888888
#define VINT_MAP0_MAPPING	0x76543210

/* VMEbus Interrupt Map Register 1 */

#define VINT_MAP1_MASK          0xfffefeee
#define VINT_MAP1_DMA_MASK      0x07
#define VINT_MAP1_DMA_LVL_1     0x01
#define VINT_MAP1_DMA_LVL_2     0x02
#define VINT_MAP1_DMA_LVL_3     0x03
#define VINT_MAP1_DMA_LVL_4     0x04
#define VINT_MAP1_DMA_LVL_5     0x05
#define VINT_MAP1_DMA_LVL_6     0x06
#define VINT_MAP1_DMA_LVL_7     0x07

/* VMEbus Interrupt Status/ID Out Register */

#define STATID_MASK             0x1ffffff

/* VMEbus IRQ1 Status/ID Register */

#define V1_STATID_ERR           (1 << 8)        /* Bus Error during IACK     */

/* VMEbus IRQ2 Status/ID Register */

#define V2_STATID_ERR           (1 << 8)        /* Bus Error during IACK     */

/* VMEbus IRQ3 Status/ID Register */

#define V3_STATID_ERR           (1 << 8)        /* Bus Error during IACK     */

/* VMEbus IRQ4 Status/ID Register */

#define V4_STATID_ERR           (1 << 8)        /* Bus Error during IACK     */

/* VMEbus IRQ5 Status/ID Register */

#define V5_STATID_ERR           (1 << 8)        /* Bus Error during IACK     */

/* VMEbus IRQ6 Status/ID Register */

#define V6_STATID_ERR           (1 << 8)        /* Bus Error during IACK     */

/* VMEbus IRQ7 Status/ID Register */

#define V7_STATID_ERR           (1 << 8)        /* Bus Error during IACK     */

/* VMEbus Master Control Register */

#define MAST_CTL_MASK           0x0003ef00      /* Reserved bits */
#define MAST_CTL_RTRY_FOREVER   (0 << 28)       /* Max Retries before PCI err*/
#define MAST_CTL_PWON_128       (0 << 24)       /* Posted Write VME Xfer Cnt */
#define MAST_CTL_PWON_256       (1 << 24)       /* Posted Write VME Xfer Cnt */
#define MAST_CTL_PWON_512       (2 << 24)       /* Posted Write VME Xfer Cnt */
#define MAST_CTL_PWON_1024      (3 << 24)       /* Posted Write VME Xfer Cnt */
#define MAST_CTL_PWON_2048      (4 << 24)       /* Posted Write VME Xfer Cnt */
#define MAST_CTL_PWON_4096      (5 << 24)       /* Posted Write VME Xfer Cnt */
#define MAST_CTL_VRL0           (0 << 22)       /* VMEbus Request Level      */
#define MAST_CTL_VRL1           (1 << 22)       /* VMEbus Request Level      */
#define MAST_CTL_VRL2           (2 << 22)       /* VMEbus Request Level      */
#define MAST_CTL_VRL3           (3 << 22)       /* VMEbus Request Level      */
#define MAST_CTL_VRM_FAIR       (1 << 21)       /* FAIR Request Mode         */
#define MAST_CTL_VRM_DEMAND     (0 << 21)       /* Demand Request Mode       */
#define MAST_CTL_VREL_RWD       (0 << 20)       /* Release When Done         */
#define MAST_CTL_VREL_ROR       (1 << 20)       /* Release on Request        */
#define MAST_CTL_VOWN           (1 << 19)       /* Acquire and Hold VMEbus   */
#define MAST_CTL_VOWN_ACK       (1 << 18)       /* VMEbus bus held */
#define MAST_CTL_PABS_32        (0 << 12)       /* 32 Byte PCI Aligned Burst */
#define MAST_CTL_PABS_64        (1 << 12)       /* 64 Byte PCI Aligned Burst */
#define MAST_CTL_PABS_128	(2 << 12)	/* 128 Byte PCI Burst - Universe II */

/* Miscellaneous Control Register */

#define MISC_CTL_MASK           0x0820ffff      /* Reserved bits */
#define MISC_CTL_VBTO_DISABLE   (0 << 28)       /*   Forever VMEbus Timeout  */
#define MISC_CTL_VBTO_16USEC    (1 << 28)       /*   16 Usec VMEbus Timeout  */
#define MISC_CTL_VBTO_32USEC    (2 << 28)       /*   32 Usec VMEbus Timeout  */
#define MISC_CTL_VBTO_64USEC    (3 << 28)       /*   64 Usec VMEbus Timeout  */
#define MISC_CTL_VBTO_128USEC   (4 << 28)       /*  128 Usec VMEbus Timeout  */
#define MISC_CTL_VBTO_256USEC   (5 << 28)       /*  256 Usec VMEbus Timeout  */
#define MISC_CTL_VBTO_512USEC   (6 << 28)       /*  512 Usec VMEbus Timeout  */
#define MISC_CTL_VBTO_1024USEC  (7 << 28)       /* 1024 Usec VMEbus Timeout  */
#define MISC_CTL_VARB_PRIORITY  (1 << 26)       /* Priority Arbitration Mode */
#define MISC_CTL_VARB_RROBIN    (0 << 26)       /* Round Robin Arbitration   */
#define MISC_CTL_VARBTO_DISABLE (0 << 24)       /* Round Robin Arbitration   */
#define MISC_CTL_VARBTO_16USEC  (1 << 24)       /* Round Robin Arbitration   */
#define MISC_CTL_VARBTO_256USEC (2 << 24)       /* Round Robin Arbitration   */
#define MISC_CTL_SW_LRST        (1 << 23)       /* Software PCI Reset        */
#define MISC_CTL_SW_SRST        (1 << 22)       /* Software VMEbus Sysreset  */
#define MISC_CTL_BI_MODE        (1 << 20)       /*                           */
#define MISC_CTL_ENGBI          (1 << 19)       /*                           */
#define MISC_CTL_RESCIND        (1 << 18)       /* Rescinding DTACK Enable   */
#define MISC_CTL_NO_RESCIND     (0 << 18)       /* Rescinding DTACK Disable  */
#define MISC_CTL_SYSCON         (1 << 17)       /* Universe is SysController */
#define MISC_CTL_NOT_SYSCON     (0 << 17)       /* Universe not SysController */
#define MISC_CTL_V64AUTO        (1 << 16)       /* Initiate VME64 Auto ID    */

/* Miscellaneous Status Register */

#define MISC_STAT_LCL_SIZE_32   (0 << 30)       /* PCI Bus is 32 bits        */
#define MISC_STAT_LCL_SIZE_64   (1 << 30)       /* PCI Bus is 64 bits        */
#define MISC_STAT_DY4AUTO       (1 << 27)       /* DY4 Auto ID Enable        */
#define MISC_STAT_MYBBSY_NEGATED (1 << 21)      /* Universe Bus Busy Negated */
#define MISC_STAT_DY4DONE       (1 << 19)       /* DY4 Auto ID is Done       */
#define MISC_STAT_TXFE          (1 << 18)       /* Transmit FIFO Empty       */
#define MISC_STAT_RXFE          (1 << 17)       /* Receive FIFO Empty        */

/* VMEbus Slave Image 0 Control */

#define VSI0_CTL_MASK           0x1f08ff3c      /* Reserved Bits */
#define VSI0_CTL_EN             (1 << 31)       /* Image Enable               */
#define VSI0_CTL_PWEN           (1 << 30)       /* Posted Write Enable        */
#define VSI0_CTL_PREN           (1 << 29)       /* Prefetch Read Enable       */
#define VSI0_CTL_AM_DATA        (1 << 22)       /* Respond to Data AM Code    */
#define VSI0_CTL_AM_PGM         (2 << 22)       /* Respond to Prog AM Code    */
#define VSI0_CTL_AM_SUPER       (2 << 20)       /* Respond to Superv AM Code  */
#define VSI0_CTL_AM_USER        (1 << 20)       /* Respond to Non-Priv AM Code*/
#define VSI0_CTL_VAS_A16        (0 << 16)       /* Respond to VME A16         */
#define VSI0_CTL_VAS_A24        (1 << 16)       /* Respond to VME A24         */
#define VSI0_CTL_VAS_A32        (2 << 16)       /* Respond to VME A32         */
#define VSI0_CTL_VAS_USER1      (6 << 16)       /* Respond to VME Space User 1*/
#define VSI0_CTL_VAS_USER2      (7 << 16)       /* Respond to VME Space User 2*/
#define VSI0_CTL_LD64EN         (1 << 7)        /* Enable 64-bit PCI bus Xfer */
#define VSI0_CTL_LLRMW          (1 << 6)        /* Enable PCI lock of VME RMW */
#define VSI0_CTL_LAS_MEM        (0 << 0)        /* PCIbus Memory Space        */
#define VSI0_CTL_LAS_IO         (1 << 0)        /* PCIbus I/O Space           */
#define VSI0_CTL_LAS_CFG        (2 << 0)        /* PCIbus Config Space        */

/* VMEbus Slave Image 0 Base Address Register */

#define VSI0_BS_MASK    0x00000fff

/* VMEbus Slave Image 0 Bound Address Register */

#define VSI0_BD_MASK    0x00000fff

/* VMEbus Slave Image 0 Translation Offset Register */

#define VSI0_TO_MASK    0x00000fff

/* VMEbus Slave Image 1 Control */

#define VSI1_CTL_MASK           0x1f08ff3c      /* Reserved Bits */
#define VSI1_CTL_EN             (1 << 31)       /* Image Enable               */
#define VSI1_CTL_PWEN           (1 << 30)       /* Posted Write Enable        */
#define VSI1_CTL_PREN           (1 << 29)       /* Prefetch Read Enable       */
#define VSI1_CTL_AM_DATA        (1 << 22)       /* Respond to Data AM Code    */
#define VSI1_CTL_AM_PGM         (2 << 22)       /* Respond to Prog AM Code    */
#define VSI1_CTL_AM_SUPER       (2 << 20)       /* Respond to Superv AM Code  */
#define VSI1_CTL_AM_USER        (1 << 20)       /* Respond to Non-Priv AM Code*/
#define VSI1_CTL_VAS_A16        (0 << 16)       /* Respond to VME A16         */
#define VSI1_CTL_VAS_A24        (1 << 16)       /* Respond to VME A24         */
#define VSI1_CTL_VAS_A32        (2 << 16)       /* Respond to VME A32         */
#define VSI1_CTL_VAS_USER1      (6 << 16)       /* Respond to VME Space User 1*/
#define VSI1_CTL_VAS_USER2      (7 << 16)       /* Respond to VME Space User 2*/
#define VSI1_CTL_LD64EN         (1 << 7)        /* Enable 64-bit PCI bus Xfer */
#define VSI1_CTL_LLRMW          (1 << 6)        /* Enable PCI lock of VME RMW */
#define VSI1_CTL_LAS_MEM        (0 << 0)        /* PCIbus Memory Space        */
#define VSI1_CTL_LAS_IO         (1 << 0)        /* PCIbus I/O Space           */
#define VSI1_CTL_LAS_CFG        (2 << 0)        /* PCIbus Config Space        */

/* VMEbus Slave Image 1 Base Address Register */

#define VSI1_BS_MASK    0x0000ffff

/* VMEbus Slave Image 1 Bound Address Register */

#define VSI1_BD_MASK    0x0000ffff

/* VMEbus Slave Image 1 Translation Offset Register */

#define VSI1_TO_MASK    0x0000ffff

/* VMEbus Slave Image 2 Control */

#define VSI2_CTL_MASK           0x1f08ff3c      /* Reserved Bits */
#define VSI2_CTL_EN             (1 << 31)       /* Image Enable               */
#define VSI2_CTL_PWEN           (1 << 30)       /* Posted Write Enable        */
#define VSI2_CTL_PREN           (1 << 29)       /* Prefetch Read Enable       */
#define VSI2_CTL_AM_DATA        (1 << 22)       /* Respond to Data AM Code    */
#define VSI2_CTL_AM_PGM         (2 << 22)       /* Respond to Prog AM Code    */
#define VSI2_CTL_AM_SUPER       (2 << 20)       /* Respond to Superv AM Code  */
#define VSI2_CTL_AM_USER        (1 << 20)       /* Respond to Non-Priv AM Code*/
#define VSI2_CTL_VAS_A16        (0 << 16)       /* Respond to VME A16         */
#define VSI2_CTL_VAS_A24        (1 << 16)       /* Respond to VME A24         */
#define VSI2_CTL_VAS_A32        (2 << 16)       /* Respond to VME A32         */
#define VSI2_CTL_VAS_USER1      (6 << 16)       /* Respond to VME Space User 1*/
#define VSI2_CTL_VAS_USER2      (7 << 16)       /* Respond to VME Space User 2*/
#define VSI2_CTL_LD64EN         (1 << 7)        /* Enable 64-bit PCI bus Xfer */
#define VSI2_CTL_LLRMW          (1 << 6)        /* Enable PCI lock of VME RMW */
#define VSI2_CTL_LAS_MEM        (0 << 0)        /* PCIbus Memory Space        */
#define VSI2_CTL_LAS_IO         (1 << 0)        /* PCIbus I/O Space           */
#define VSI2_CTL_LAS_CFG        (2 << 0)        /* PCIbus Config Space        */

/* VMEbus Slave Image 2 Base Address Register */

#define VSI2_BS_MASK    0x0000ffff

/* VMEbus Slave Image 2 Bound Address Register */

#define VSI2_BD_MASK    0x0000ffff

/* VMEbus Slave Image 2 Translation Offset Register */

#define VSI2_TO_MASK    0x0000ffff

/* VMEbus Slave Image 3 Control */

#define VSI3_CTL_MASK           0x1f08ff3c      /* Reserved Bits */
#define VSI3_CTL_EN             (1 << 31)       /* Image Enable               */
#define VSI3_CTL_PWEN           (1 << 30)       /* Posted Write Enable        */
#define VSI3_CTL_PREN           (1 << 29)       /* Prefetch Read Enable       */
#define VSI3_CTL_AM_DATA        (1 << 22)       /* Respond to Data AM Code    */
#define VSI3_CTL_AM_PGM         (2 << 22)       /* Respond to Prog AM Code    */
#define VSI3_CTL_AM_SUPER       (2 << 20)       /* Respond to Superv AM Code  */
#define VSI3_CTL_AM_USER        (1 << 20)       /* Respond to Non-Priv AM Code*/
#define VSI3_CTL_VAS_A16        (0 << 16)       /* Respond to VME A16         */
#define VSI3_CTL_VAS_A24        (1 << 16)       /* Respond to VME A24         */
#define VSI3_CTL_VAS_A32        (2 << 16)       /* Respond to VME A32         */
#define VSI3_CTL_VAS_USER1      (6 << 16)       /* Respond to VME Space User 1*/
#define VSI3_CTL_VAS_USER2      (7 << 16)       /* Respond to VME Space User 2*/
#define VSI3_CTL_LD64EN         (1 << 7)        /* Enable 64-bit PCI bus Xfer */
#define VSI3_CTL_LLRMW          (1 << 6)        /* Enable PCI lock of VME RMW */
#define VSI3_CTL_LAS_MEM        (0 << 0)        /* PCIbus Memory Space        */
#define VSI3_CTL_LAS_IO         (1 << 0)        /* PCIbus I/O Space           */
#define VSI3_CTL_LAS_CFG        (2 << 0)        /* PCIbus Config Space        */

/* VMEbus Slave Image 3 Base Address Register */

#define VSI3_BS_MASK    0x0000ffff

/* VMEbus Slave Image 3 Bound Address Register */

#define VSI3_BD_MASK    0x0000ffff

/* VMEbus Slave Image 3 Translation Offset Register */

#define VSI3_TO_MASK    0x0000ffff

/* VMEbus Register Access Image Control Register */

#define VRAI_CTL_EN             (1 << 31)       /* Image Enable               */
#define VRAI_CTL_AM_DATA        (1 << 22)       /* Respond to Data AM Code    */
#define VRAI_CTL_AM_PGM         (2 << 22)       /* Respond to Prog AM Code    */
#define VRAI_CTL_AM_SUPER       (2 << 20)       /* Respond to Superv AM Code  */
#define VRAI_CTL_AM_USER        (1 << 20)       /* Respond to Non-Priv AM Code*/
#define VRAI_CTL_VAS_A16        (0 << 16)       /* Respond to VME A16         */
#define VRAI_CTL_VAS_A24        (1 << 16)       /* Respond to VME A24         */
#define VRAI_CTL_VAS_A32        (2 << 16)       /* Respond to VME A32         */
#define VRAI_CTL_VAS_USER1      (6 << 16)       /* Respond to VME Space User 1*/
#define VRAI_CTL_VAS_USER2      (7 << 16)       /* Respond to VME Space User 2*/

/* @prb */

/* VMEbus CSR Control Register */

#define VCSR_CTL_EN             (1 << 31)       /* Image Enable               */
#define VCSR_CTL_LAS_MEM        (0 << 0)        /* PCIbus Memory Space        */
#define VCSR_CTL_LAS_IO         (1 << 0)        /* PCIbus I/O Space           */
#define VCSR_CTL_LAS_CFG        (2 << 0)        /* PCIbus Config Space        */

/* VMEbus AM Code Error Log */

#define V_AMERR_MASK            0x07ffffff      /* Reserved bits */
#define V_AMERR_IACK            (1 << 25)       /*                            */
#define V_AMERR_M_ERR           (1 << 24)       /*                            */
#define V_AMERR_V_STAT          (1 << 23)       /*                            */

/* VMEbus CSR Bit Clear Register */

#define VCSR_CLR_MASK           0x1fffffff      /* Reserved bits */
#define VCSR_CLR_RESET          (1 << 31)       /* Negate PRST                */
#define VCSR_CLR_SYSFAIL        (1 << 30)       /* Negate SysFail             */
#define VCSR_CLR_FAIL           (1 << 29)       /* Board has Failed           */

/* VMEbus CSR Bit Set Register */

#define VCSR_SET_RESET          (1 << 31)       /* Assert PRST                */
#define VCSR_SET_SYSFAIL        (1 << 30)       /* Assert SysFail             */
#define VCSR_SET_FAIL           (1 << 29)       /* Board has Failed           */

/* VMEbus CSR Bit Clear Register */

#define VCSR_BS_MASK            0x3ffffff       /* Reserved bits */

#endif  /* INCuniverseh */
