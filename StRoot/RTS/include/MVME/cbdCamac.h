#ifndef _CBD_CAMAC_H_
#define _CBD_CAMAC_H_

#define CBD_BASE	0xFB800000

#define CBD_LAM_VME_IRQ	3
#define CBD_IT2_VME_IRQ	2
#define CBD_IT4_VME_IRQ	4



#define BCNAF16(b,c,n,a,f) (CBD_BASE | ((b)<<19) | ((c)<<16) | ((n)<<11) | ((a)<<7) | ((f)<<2) | 2) 
#define BCNAF24(b,c,n,a,f) (CBD_BASE | ((b)<<19) | ((c)<<16) | ((n)<<11) | ((a)<<7) | ((f)<<2)) 

extern inline UINT32 cam24r(int b, int c, int n, int a, int f)
{
	UINT32 ret ;

	// hi first
	ret = (*(UINT16 *)BCNAF24(b,c,n,a,f))<<16 ;
	// lo second
	ret |= *(UINT16 *)BCNAF16(b,c,n,a,f) ;

	return ret ;
}

extern inline void cam24w(int b, int c, int n, int a, int f, UINT32 data)
{
	// hi first
	*(UINT16 *)BCNAF24(b,c,n,a,f) = data >> 16 ;
	// lo second
	*(UINT16 *)BCNAF16(b,c,n,a,f) = data & 0xFFFF ;

	return  ;
}


extern int cbdCrate[8] ;
extern int cbdCrateCou ;


#endif
