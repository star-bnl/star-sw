typedef unsigned int   UINT32 ;
typedef unsigned short UINT16 ;

UINT32 swap32(UINT32 in) 
{
	register UINT32 x ;
	x = in ;

	return (x&0xff000000) >> 24 | \
        (x&0x00ff0000) >> 8  | \
        (x&0x0000ff00) << 8  | \
        (x&0x000000ff) << 24;
}


UINT16 swap16(UINT16 in)
{
	register UINT16 x ;

	x = in ;

	return (x&0xFF00) >> 8 | (x&0xFF) << 8 ;
}
