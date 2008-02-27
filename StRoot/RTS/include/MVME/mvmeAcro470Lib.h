#ifndef _MVME_ACRO_470_LIB_H_
#define _MVME_ACRO_470_LIB_H_


extern int acro470Init(void)	 ;

extern UINT32 acro470Read(void) ;	// edge pulses
extern int acro470Clear(UINT32 what)  ;	// clear edge...

extern UINT8 acro470ReadLvl(void) ;

extern UINT8 acro470SetLvl(UINT8 what) ;
extern UINT8 acro470GetLvl(void) ;

extern void acro470SetBit(UINT32 bit) ;
extern void acro470ClrBit(UINT32 bit) ;
extern void acro470PulseBit(UINT32 bit) ;

extern void acro470IrqEnable(int on) ;
extern int acro470IrqGet(void) ;

extern int acro470Vector ;
#endif
