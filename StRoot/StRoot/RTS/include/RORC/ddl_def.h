#ifndef _ddl_def_
#define _ddl_def_

/************************************************
*  ddl_def.h                                    *
*  last update:        22/01/2007               *
*         common file for old and new ddl cards *
*  written by: Peter Csato and Ervin Denes      *
************************************************/

/* wait cycles for DDL respond */
#define DDL_TIMEOUT 100000
#define DDL_RESPONSE_TIME 1000
#define DDL_MAX_WAIT_CYCLE 0x7fffffffffffffffULL
#define DDL_MAX_REPLY 4
#define DDL_MAX_HW_ID 64
#define DDL_MAX_JTAG_LEN 100

/* maximum size of DDL events */
#define DDL_MAX_WORD 0x07FFFF 
#define DDL_MAX_BYTE 0x1FFFFC
#define DDL_HEADER_SIZE_BYTE 32
#define DDL_HEADER_SIZE_WORD 8 

/* maximum size of a simulated event sent by the DDG */
#define DDL_MAX_TX_WORD 0xFFFFFF

/*  destination field      */
#define RORC 0
#define DIU 1
#define SIU 2
#define DSI 3
#define FEE 4
#define JTAG 8

/*  command field */
/* FEE commands */
#define RDYRX	1	/* Ready to Receive */
#define EOBTR	11	/* End of Block Transfer */
#define STBWR	13	/* Start of Block Write */
#define STBRD	5	/* Start of Block Read */
#define FECTRL	12	/* Front-end control */
#define FESTRD	4	/* Front-end status readout */

/* interface commands */
#define LRST	10	/* Link Reset */
#define SUSPEND	10	/* Go to Power Off state */
#define LINIT	11	/* Link Initialisation */
#define WAKEUP	11	/* Wake up from Power Off state */
#define SRST	15	/* SIU Reset */
#define IFLOOP	9	/* DIU Transmitter or SIU Receiver Loop-back */
#define TSTOP	12	/* Self-test Stop */
#define TSTMODE 13	/* Self-test Start */
#define DTCC	8	/* Data Transmission Check Command */
#define RFWVER	4	/* Read Firmware Version of DIU or SIU */
#define RHWVER	6	/* Read Hardware Version of DIU or SIU */
#define RPMVAL  7       /* Read Power Monitor Value */
#define RandCIFST	0	/* Read & Clear Interface Status Word */

/* JTAG commands */
#define JSTART    13    /* Start JTAG block */
#define JTSTW     8     /* JTAG transmission check command */
#define EOJTR     11    /* End of JTAG Transfer */

/* PRBS Test Mode command parameters */
#define STRPRBS   1     /* Start PRBS test */
#define STPPRBS   0     /* Stop PRBS test */
#define CLRPRBS   3     /* Clear PRBS Counter */
#define RDPRBS  0x40000 /* Read PRBS Counter */

/* status codes */
#define CTSTW     0     /* CTSTW */
#define CTSTW_TO  1     /* CTSTW for Front-End TimeOut */
#define ILCMD     2     /* CTSTW for illegal command */
#define FESTW     4     /* FESTW */
#define HWSTW     6     /* HWSTW */
#define PMVAL     7     /* Power Monitor Value */
#define DTSTW     8     /* DTSTW */
#define DTSTW_TO  9     /* DTSTW with Front-End TimeOut*/
#define IFSTW    12     /* IFSTW */
#define TEVAL    13     /* Test Mode Error Value */
#define FWSTW    14     /* FWSTW */

/* data transmission status word */
#define DTSW    0x00000082
#define CONTINUATION_BIT 0x00000100
#define EOB     0x000000b4 /* End of block command */
#define CTSW    0x00000002 /* Command Transmission Status word */
#define JSTR    0x000000d2 /* Start of JTAG block */
#define JTSW    0x00000088 /* End of JTAG block */

/* masks for DIU/SIU status word */
#define STMASK     0xFFFFF0FF /* status word without ID# */
#define DIUMASK    0xBFFC7000 /* DIU port states (without loop-back bit) */
#define REMMASK    0x00038000 /* Remote SIU/DIU states */
#define DIUSTMASK  0x00007000 /* DIU port states */
#define SIUSTMASK  0x00007000 /* SIU port states */
#define DIUERMASK  0xBFFC0000 /* DIU error states */

/* DIU version */

#define NOT_DEF -1
#define NO_DIU   0
#define OLD      1
#define NEW      2
#define EMBEDDED 3

/* status/error bits for NEW (CMC connector) link cards */

/* DIU status/error bits */
#define ERROR_BIT  0x80000000 /* error bit */
#define DIU_LOOP   0x40000000 /* DIU loop-back mode */
#define LOSS_SYNC  0x20000000 /* loss of synchronization */
#define D_TXOF     0x10000000 /* transmit data/status overflow */
#define D_RES1     0x08000000 /* reserved */
#define D_OSINFR   0x04000000 /* ordered set in frame */
#define D_INVRX    0x02000000 /* invalide receive word */
#define D_CERR     0x01000000 /* CRC error */
#define D_RES2     0x00800000 /* reserved */
#define D_DOUT     0x00400000 /* data word out of frame */
#define D_IFDL     0x00200000 /* illegal frame delimiter */
#define D_LONG     0x00100000 /* too long frame */
#define D_RXOF     0x00080000 /* received data/status overflow */
#define D_FRERR    0x00040000 /* error in receive frame */

/* masks for DIU port states */
#define DIU_TSTM   0x00007000 /* DIU in PRBS Test Mode state */
#define DIU_POFF   0x00006000 /* DIU in Power Off state */
#define DIU_LOS    0x00005000 /* DIU in Offline and Loss of Synchro state */
#define DIU_NOSIG  0x00004000 /* DIU in Offline and No Signal state */
#define DIU_WAIT   0x00003000 /* DIU in Waiting for Power Off state */
#define DIU_ONL    0x00002000 /* DIU in Online state */
#define DIU_OFFL   0x00001000 /* DIU in Offline state */
#define DIU_POR    0x00000000 /* DIU in Power On Reset state */

/* SIU status/error bits */
#define S_LONGE    0x40000000 /* too long event or read data block */
#define S_IFEDS    0x20000000 /* illegal FEE data/status */
#define S_TXOF     0x10000000 /* transmit FIFO overflow */
#define S_IWDAT    0x08000000 /* illegal write data word */
#define S_OSINFR   0x04000000 /* ordered set inside a frame */
#define S_INVRX    0x02000000 /* invalid character inside a frame */
#define S_CERR     0x01000000 /* CRC error */
#define S_DJLERR   0x00800000 /* DTCC / JTCC error */
#define S_DOUT     0x00400000 /* data out of receive frame */
#define S_IFDL     0x00200000 /* illegal frame delimiter (SOF) */
#define S_LONG     0x00100000 /* too long receive frame */
#define S_RXOF     0x00080000 /* receive FIFO overflow */
#define S_FRERR    0x00040000 /* error in receive frame */
#define S_LPERR    0x00020000 /* link protocol error */
#define S_LBMOD    0x00010000 /* SIU in Loop Back Mode */
#define S_OPTRAN   0x00008000 /* open FEE transaction */

/* masks for SIU port states */
#define SIU_RESERV 0x00007000 /* reserved */
#define SIU_POFF   0x00006000 /* SIU in Power Off state */
#define SIU_LOS    0x00005000 /* SIU in Offline and Loss of Synchro state */
#define SIU_NOSIG  0x00004000 /* SIU in Offline and No Signal state */
#define SIU_WAIT   0x00003000 /* SIU in Waiting for Power Off state */
#define SIU_ONL    0x00002000 /* SIU in Online state */
#define SIU_OFFL   0x00001000 /* SIU in Offline state */
#define SIU_POR    0x00000000 /* SIU in Power On Reset state */


/* status/error bits for OLD link cards */

/* DIU status/error bits */
#define oDIU_LOOP   0x40000000 /* DIU loop-back mode */
#define oLOSS_SIGN  0x20000000 /* loss of signal */
#define oD_RTOUT    0x10000000 /* receiver synchronisation time-out */
#define oD_LOSY     0x08000000 /* loss or word synchronisation */
#define oD_RDERR    0x04000000 /* running disparity error */
#define oD_INVRX    0x02000000 /* invalide receive word */
#define oD_CERR     0x01000000 /* CRC error */
#define oD_UNREC    0x00800000 /* unrecognised ordered set received */
#define oD_DOUT     0x00400000 /* data word out of frame */
#define oD_IFDL     0x00200000 /* illegal frame delimiter */
#define oD_LONG     0x00100000 /* too long frame */
#define oD_RXOV     0x00080000 /* received data/status overflow */
#define oD_LTOUT    0x00040000 /* link initialisation time-out */

/* masks for received ordered sets */
#define oSIU_SRST   0x00038000 /* Remote DIU in Reset state, sends SRST */
#define oSIU_FAIL   0x00030000 /* Remote SIU/DIU in fail state, sends Not_Op */
#define oSIU_OFFL   0x00028000 /* Remote SIU/DIU in offl. state, sends Oper */
#define oSIU_LINIT  0x00020000 /* Remote DIU is sending L_Init */
#define oSIU_ACT    0x00018000 /* Remote SIU/DIU in active state, sends Idle */
#define oSIU_XOFF   0x00010000 /* Remote SIU/DIU sends Xoff */
#define oSIU_XON    0x00008000 /* Remote SIU/DIU sends Xon*/
#define oSIU_ELSE   0x00000000 /* Remote SIU/DIU is sending data or delimiter */

/* masks for DIU port states */
#define oDIU_NOSYNC 0x00007000 /* receiver not synchronised state */
#define oDIU_RSTSIU 0x00006000 /* DIU idle (reset SIU) state */
#define oDIU_FAIL   0x00005000 /* DIU fail state */
#define oDIU_OFFL   0x00004000 /* DIU offline state */
#define oDIU_LRES   0x00003000 /* DIU LRES metastable state */
#define oDIU_START  0x00002000 /* DIU START metastable state */
#define oDIU_ACCED  0x00001000 /* DIU ACCED metastable state */
#define oLINK_ACT   0x00000000 /* link active state */

/* SIU status/error bits */
#define oS_LONGE    0x40000000 /* too long event or read data block */
#define oS_IFEDS    0x20000000 /* illegal FEE data/status */
#define oS_TXOF     0x10000000 /* transmit FIFO overflow */
#define oS_IWDAT    0x08000000 /* illegal write data word */
#define oS_WBLER    0x04000000 /* write data block length error */
#define oS_RXOV     0x02000000 /* receive FIFO overflow */
#define oS_LONGD    0x01000000 /* too long data frame */
#define oS_LONGC    0x00800000 /* too long command frame */
#define oS_OSIN     0x00400000 /* ordered set inside a frame */
#define oS_DOUT     0x00200000 /* data out of receive frame */
#define oS_LPERR    0x00100000 /* link protocol error */
#define oS_CHERR    0x00080000 /* check summ error in receive frame */
#define oS_UNREC    0x00040000 /* unrecognised ordered set */
#define oS_INVRX    0x00020000 /* invalid receive word */
#define oS_WALER    0x00010000 /* word alignment error */
#define oS_ISPCH    0x00008000 /* illegal special character */
#define oS_RDERR    0x00004000 /* running disparity error */
#define oS_IRXCD    0x00002000 /* illegal receive code */
#define oS_BUFER    0x00001000 /* elastic buffer over/under run */

#endif
