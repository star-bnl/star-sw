#ifndef _UNI_LIB_H_
#define _UNI_LIB_H_


#include <vxWorks.h>

#include <MVME/universe.h>	/* from the BSP */

/*
	the main Universe structure is meant to be used as
	a pointer only and is carefully constructed so that
	the offsets match the actual registers!
	DO NOT CHANGE!
	The various "rx" registers are "Universe reserved".
*/

/* for Universe version I */
struct uniStruct {

volatile UINT32    PCI_ID ;
volatile UINT32    PCI_CSR	;
volatile UINT32    PCI_CLASS	;
volatile UINT32    PCI_MISC0	;
volatile UINT32    PCI_BS	;
volatile UINT32    PCI_BS1	;
int r1[9] ;
volatile UINT32    PCI_MISC1	;
int r2[48] ;
volatile UINT32    LSI0_CTL	;
volatile UINT32    LSI0_BS	;
volatile UINT32    LSI0_BD	;
volatile UINT32    LSI0_TO	;
int r3 ;
volatile UINT32    LSI1_CTL	;
volatile UINT32    LSI1_BS	;
volatile UINT32    LSI1_BD	;
volatile UINT32    LSI1_TO	;
int r4 ;
volatile UINT32    LSI2_CTL	;
volatile UINT32    LSI2_BS	;
volatile UINT32    LSI2_BD	;
volatile UINT32    LSI2_TO	;
int r5 ;
volatile UINT32    LSI3_CTL	;
volatile UINT32    LSI3_BS	;
volatile UINT32    LSI3_BD	;
volatile UINT32    LSI3_TO	;
int r6[9] ;
volatile UINT32    SCYC_CTL	;
volatile UINT32    SCYC_ADDR	;
volatile UINT32    SCYC_EN	;
volatile UINT32    SCYC_CMP	;
volatile UINT32    SCYC_SWP	;
volatile UINT32    LMISC		;
volatile UINT32    SLSI		;
volatile UINT32    L_CMDERR	;
volatile UINT32    LAERR	;
int r7[3] ;
volatile UINT32    LSI4_CTL	;
volatile UINT32    LSI4_BS	;
volatile UINT32    LSI4_BD	;
volatile UINT32    LSI4_TO	;
int r33 ;
volatile UINT32    LSI5_CTL	;
volatile UINT32    LSI5_BS	;
volatile UINT32    LSI5_BD	;
volatile UINT32    LSI5_TO	;
int r43 ;
volatile UINT32    LSI6_CTL	;
volatile UINT32    LSI6_BS	;
volatile UINT32    LSI6_BD	;
volatile UINT32    LSI6_TO	;
int r53 ;
volatile UINT32    LSI7_CTL	;
volatile UINT32    LSI7_BS	;
volatile UINT32    LSI7_BD	;
volatile UINT32    LSI7_TO	;
int r77[5] ;
volatile UINT32    DCTL	;
volatile UINT32    DTBC	;
volatile UINT32    DLA		;
int r8 ;
volatile UINT32    DVA		;
int r9 ;
volatile UINT32    DCPP	;
int r10 ;
volatile UINT32    DGCS	;
volatile UINT32    D_LLUE	;
int r11[54] ;
volatile UINT32    LINT_EN	;
volatile UINT32    LINT_STAT	;
volatile UINT32    LINT_MAP0	;
volatile UINT32    LINT_MAP1	;
volatile UINT32    VINT_EN	;
volatile UINT32    VINT_STAT	;
volatile UINT32    VINT_MAP0	;
volatile UINT32    VINT_MAP1	;
volatile UINT32    STATID	;
volatile UINT32    V1_STATID	;
volatile UINT32    V2_STATID	;
volatile UINT32    V3_STATID	;
volatile UINT32    V4_STATID	;
volatile UINT32    V5_STATID	;
volatile UINT32    V6_STATID	;
volatile UINT32    V7_STATID	;
volatile UINT32	   LINT_MAP2 ;
volatile UINT32	   VINT_MAP2 ;
volatile UINT32	   MBOX0 ;
volatile UINT32	   MBOX1 ;
volatile UINT32	   MBOX2 ;
volatile UINT32	   MBOX3 ;
volatile UINT32	   SEMA0 ;
volatile UINT32	   SEMA1 ;

int r12[40] ;
volatile UINT32    MAST_CTL	;
volatile UINT32    MISC_CTL	;
volatile UINT32    MISC_STAT	;
volatile UINT32    USER_AM	;
int r13[700] ;
volatile UINT32    VSI0_CTL	;
volatile UINT32    VSI0_BS	;
volatile UINT32    VSI0_BD	;
volatile UINT32    VSI0_TO	;
int r14 ;
volatile UINT32    VSI1_CTL	;
volatile UINT32    VSI1_BS;
volatile UINT32    VSI1_BD;
volatile UINT32    VSI1_TO;
int r15 ;
volatile UINT32    VSI2_CTL;
volatile UINT32    VSI2_BS;
volatile UINT32    VSI2_BD;
volatile UINT32    VSI2_TO;
int r16 ;
volatile UINT32    VSI3_CTL;
volatile UINT32    VSI3_BS;
volatile UINT32    VSI3_BD;
volatile UINT32    VSI3_TO;
int r17[6] ;
volatile UINT32    LM_CTL ;
volatile UINT32    LM_BS ;
int r177 ;
volatile UINT32    VRAI_CTL;
volatile UINT32    VRAI_BS;
int r18[2] ;
volatile UINT32    VCSR_CTL;
volatile UINT32    VCSR_TO;
volatile UINT32    V_AMERR;
volatile UINT32    VAERR;
volatile UINT32    VSI4_CTL	;
volatile UINT32    VSI4_BS	;
volatile UINT32    VSI4_BD	;
volatile UINT32    VSI4_TO	;
int r144 ;
volatile UINT32    VSI5_CTL	;
volatile UINT32    VSI5_BS;
volatile UINT32    VSI5_BD;
volatile UINT32    VSI5_TO;
int r154 ;
volatile UINT32    VSI6_CTL;
volatile UINT32    VSI6_BS;
volatile UINT32    VSI6_BD;
volatile UINT32    VSI6_TO;
int r164 ;
volatile UINT32    VSI7_CTL;
volatile UINT32    VSI7_BS;
volatile UINT32    VSI7_BD;
volatile UINT32    VSI7_TO;

int r19[5] ;
volatile UINT32    VCSR_RES ;
volatile UINT32    VCSR_CLR;
volatile UINT32    VCSR_SET;
volatile UINT32    VCSR_BS ;

} ;



#endif	/*  _UNI_LIB_H */
