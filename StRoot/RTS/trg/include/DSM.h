/*
 *  This file defines the DSM board address map offsets (in bytes)
 *
 */


/*#ifndef _DSM_H_INCLUDED_*/

#define CBLT_ADDRESS    0x20000000              /* Chain Block Transfer Address for all DSMs */

#define DSM_IBUF1       0x00000000		/* Input buffers */ 
#define DSM_IBUF2       0x00040000		/* Input buffers */ 
#define DSM_IBUF3       0x00080000		/* Input buffers */ 
#define DSM_IBUF4       0x000c0000		/* Input buffers */ 
#define DSM_LUT1	0x00100000		/* LUT */
#define DSM_LUT2	0x00140000		/* LUT */
#define DSM_LUT3	0x00180000		/* LUT */
#define DSM_LUT4	0x001c0000		/* LUT */
#define DSM_SIM1	0x00200000		/* Simulation mem. */
#define DSM_SIM2	0x00240000		/* Simulation mem. */
#define DSM_SIM3	0x00280000		/* Simulation mem. */
#define DSM_SIM4	0x002c0000		/* Simulation mem. */
#define DSM_OUT_MEM	0x00300000		/* Output mem. */
#define DSM_ENG_REG	0x00340000		/* Computational Engine Registers */
#define DSM_ENGINE_MEM	0x00400000		/* FPGA configuration mem. */
#define DSM_CSR1	0x00600000		/* Operation Control */
#define DSM_CSR2	0x00600004		/* First/Last Board */
#define DSM_CSR3	0x00600008		/* In/Out Memory Configuration */
#define DSM_CSR4	0x0060000c		/* Read Output FIFO */
#define DSM_CSR5	0x00600010		/* FIFO Reset */
#define DSM_CSR6	0x00600014		/* Load FIFO Blank */
#define DSM_CSR7	0x00600018		/* Local Address Control */
#define DSM_CSR8	0x0060001c		/* Read Local Address */
#define DSM_CSR9	0x00600020		/* Board Connected Status */
#define DSM_CSR10	0x00600024		/* FPGA Configured Status */
#define DSM_CSR11	0x00600028		/* Begin FPGA Configuration */
#define DSM_CSR12	0x0060002c		/* Read/write software LEDs */
#define DSM_CSR13       0x00600030              /* Reset clock strobes */
#define DSM_L1_REG      0x00000008              /* Address offset for CBLT */

//#define  CTB_Layer1   0
//#define  CTB_MWC_DSM  4
//#define  LAST_L1_DSM  5
//#define  ZDC_DSM      6

/*#define _DSM_H_INCLUDED_
#endif*/

/* end of file DSM.h */


