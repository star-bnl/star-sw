#ifndef _PLX_LIB_H
#define _PLX_LIB_H

#ifdef __cplusplus
extern "C" {
#endif

#include <vxWorks.h>

/* PLX 9080 registers */
/* 
	they are divided in 2 structures due to the peculiarities of the
	PLX decode depending on the master : local CPU or PCI
*/

struct plxStructCfg {

volatile UINT32   PCI_ID ;
volatile UINT32   PCI_CSR  ;
volatile UINT32   PCI_CLASS  ;
volatile UINT32   PCI_CLSR ;
volatile UINT32   PCIBAR0        ;
volatile UINT32   PCIBAR1   ;
volatile UINT32   PCIBAR2   ;
volatile UINT32   PCIBAR3   ;
volatile UINT32   PCIBAR4   ;
volatile UINT32   PCIBAR5   ;
volatile UINT32   PCICIS   ;
volatile UINT32   PCISVID   ;
volatile UINT32   PCIERBAR   ;
int r1[2] ;
volatile UINT32   PCI_MISC1        ;
} ;

struct plxStructReg {

volatile UINT32 	LAS0RR ;
volatile UINT32 	LAS0BA ;
volatile UINT32 	MARBR;
volatile UINT32 	BIGEND;
volatile UINT32 	EROMRR;
volatile UINT32 	EROMBA;
volatile UINT32 	LBRD0;
volatile UINT32 	DMRR;
volatile UINT32 	DMLBAM;
volatile UINT32 	DMLBAI;
volatile UINT32 	DMPBAM;
volatile UINT32 	DMCFGA;

volatile UINT32 	OPLFIS;
volatile UINT32 	OPLFIM;
volatile UINT32 	R1[2] ;
volatile UINT32 	MBOX0;
volatile UINT32 	MBOX1;

volatile UINT32 	MBOX2;
volatile UINT32 	MBOX3;
volatile UINT32 	MBOX4;
volatile UINT32 	MBOX5;
volatile UINT32 	MBOX6;
volatile UINT32 	MBOX7;
volatile UINT32 	P2LDBELL;
volatile UINT32 	L2PDBELL;
volatile UINT32 	INTCSR;
volatile UINT32 	CNTRL;
volatile UINT32 	PCIHIDR;
volatile UINT32 	PCIHREV;
volatile UINT32 	MBOX0_N;
volatile UINT32 	MBOX1_N;


volatile UINT32 	DMAMODE0;
volatile UINT32 	DMAPADR0;
volatile UINT32 	DMALADR0;
volatile UINT32 	DMASIZ0;
volatile UINT32 	DMADPR0;
volatile UINT32 	DMAMODE1;
volatile UINT32 	DMAPADR1;
volatile UINT32 	DMALADR1;
volatile UINT32 	DMASIZ1;
volatile UINT32 	DMADPR1;
volatile UINT32 	DMACSR;
volatile UINT32 	DMAARB;
volatile UINT32 	DMATHR;

volatile UINT32		R3[3] ;

volatile UINT32 	MQCR;
volatile UINT32 	QBAR;
volatile UINT32 	IFHPR;
volatile UINT32 	IFTPR;
volatile UINT32 	IPHPR;
volatile UINT32 	IPTPR;
volatile UINT32 	OFHPR;
volatile UINT32 	OFTPR;
volatile UINT32 	OPHPR;
volatile UINT32 	OPTPR;
volatile UINT32 	QSR;
volatile UINT32 	R2;

volatile UINT32 	LAS1RR;
volatile UINT32 	LAS1BA;
volatile UINT32 	LBRD1;

} ;




#ifdef __cplusplus
}
#endif
#endif	/* _PLX_LIB_H */
