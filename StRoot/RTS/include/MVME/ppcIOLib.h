/*
	these routines _MUST_ be declared as they are because we want to
	inline them. 

*/

#ifndef _PPC_IO_LIB_H
#define _PPC_IO_LIB_H

#ifdef __cplusplus
extern "C" {
#endif


/*
	eieio ensures that all the instructions preceeding it will finish
	before new ones are started.
	It effectively flushes the PPC's pipeline
*/
extern __inline void eieio(void)
{
	__asm__ volatile ("eieio ") ;

	return ;
}


/*
	w32 writes a 32bit quantity "data" to the address in
	"addr" followed by the sync primitive (eieio) thus
	ensuring that the write finishes before any other operation
	after it
*/
extern __inline void w32(volatile unsigned int *addr, unsigned int data)
{
	*addr = data ;
	eieio() ;
	return ;
}

/*
	see above
*/
extern __inline void w16(volatile unsigned short *addr, unsigned short data)
{
	*addr = data ;
	eieio() ;
	return ;
}

/*
	see above
*/
extern __inline void w8(volatile unsigned char *addr, unsigned char data)
{
	*addr = data ;
	eieio() ;
	return ;
}


/*
	routines returns the byte-swapped 32bit integer at address "addr".
	Used for endian conversion
*/	
extern __inline unsigned int rs32(volatile unsigned int *addr)
{
	unsigned int tmp ;

	__asm__ volatile ("lwbrx %0,0,%1 " : "=rI" (tmp) : "rI" (addr)) ;


	return tmp ;
}

/*
	see above except for 16bit datum
*/
extern __inline unsigned short rs16(volatile unsigned short *addr)
{
	unsigned int tmp ;

	__asm__ volatile ("lhbrx %0,0,%1 " : "=rI" (tmp) : "rI" (addr)) ;


	return (unsigned short)tmp ;
}


/*
	writes the data byte-spawed to address addr followed by the
	syncing primitive (eieio)
	Used for endian conversion
*/
extern __inline void ws32(volatile unsigned int *addr, unsigned int data)
{
	__asm__ volatile ("stwbrx %0,0,%1; eieio " : : "r" (data) , "rI" (addr)) ;

	return ;
}

/*
	writes the data byte-spawed to address addr WIHTOUT eieio
	Used for endian conversion
*/
extern __inline void ws32_no_eieio(volatile unsigned int *addr, unsigned int data)
{
	__asm__ volatile ("stwbrx %0,0,%1 " : : "r" (data) , "rI" (addr)) ;

	return ;
}

/*
	see above except for 16bit data
*/
extern __inline void ws16(volatile unsigned short *addr, unsigned short data)
{
	__asm__ volatile ("sthbrx %0,0,%1 \n eieio " : : "r" (data) , "rI" (addr)) ;

	return ;
}



#ifdef __cplusplus
}
#endif

#endif
