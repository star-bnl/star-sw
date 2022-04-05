#ifndef _MVME_PCI_LIB_H_
#define _MVME_PCI_LIB_H_

#include <vxWorks.h>	// for UINT32 etc...

#define SPAN_DEV	20


#define PMC1_DEV	16
#define PMC1_IRQ	PMC_INT_LVL1

#define PMC2_DEV	17
#define PMC2_IRQ	PMC_INT_LVL2

#define SPAN1_PMC1_DEV	2
#define SPAN1_PMC1_IRQ	PMC_INT_LVL3

#define SPAN1_PMC2_DEV	3
#define SPAN1_PMC2_IRQ	PMC_INT_LVL4

#define SPAN2_PMC1_DEV	4
#define SPAN2_PMC1_IRQ	PMC_INT_LVL1

#define SPAN2_PMC2_DEV	5
#define SPAN2_PMC2_IRQ	PMC_INT_LVL2

#define SPAN_DEV_ID	0x00221011
#define SCI_D_DEV_ID	0x065811c8
#define MM6140_DEV_ID	0x61401332
#define MM6155_DEV_ID	0x61551332
#define NCR725_DEV_ID	0x000F1000
#define DEC21140_DEV_ID	0x00091011
#define MYRI_X_DEV_ID	0x804310E8
#define MYRI_64_DEV_ID  0x804314C1
#define ACRO_470_DEV_ID	0x545616D5	
#define RAMIX551_DEV_ID	0x020011B0

// this routine sets all the relevant devices on the bus
// that haven't been configured by the kernel

// number of appropriate devices found during mvmePciSetBus()
extern volatile int mvmeSciNum ;
extern volatile int mvmeScsiNum ;
extern volatile int mvmeEthNum ;
extern volatile int mvmeMemNum ;
extern volatile int mvmeMyriNum ;
extern volatile int mvmeIONum ;

struct mvmePMCBoard {
	UINT32 id ;		// dev_id, vendor_id field as an UINT32 (CFG address 0x00)
	UINT32 base[3] ;	// PCI CFG space addresses 0x10, 0x14, 0x18
	UINT8 vector ;		// IRQ vector - directly usable by intConnect() & intEnable()
	UINT8 bus ;		// PCI bus - for use by pciConfigXXX routines
	UINT8 dev ;		// PCI device - for use by pciConfigXXX routines
} ;

// the appropriate structures
extern volatile struct mvmePMCBoard mvmePMCScsi[] ;
extern volatile struct mvmePMCBoard mvmePMCEth[] ;
extern volatile struct mvmePMCBoard mvmePMCMem[] ;
extern volatile struct mvmePMCBoard mvmePMCSci[] ;
extern struct mvmePMCBoard mvmePMCMyri[] ;
extern volatile struct mvmePMCBoard mvmePMCIO[] ;

#endif // _MVME_PCI_LIB_H_
