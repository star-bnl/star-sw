#ifndef IDL_LONG

#ifndef __alpha
	#define IDL_CHAR char
	#define IDL_OCTET unsigned char
	#define IDL_UCHAR unsigned char
	#define IDL_UNSIGNED_CHAR unsigned char
	#define IDL_SHORT short
	#define IDL_USHORT unsigned short
	#define IDL_UNSIGNED_SHORT unsigned short
	#define IDL_LONG long
	#define IDL_ULONG unsigned long
	#define IDL_UNSIGNED_LONG unsigned long
	#define IDL_FLOAT float
	#define IDL_DOUBLE double
#else
	#define IDL_CHAR char
	#define IDL_OCTET unsigned char
	#define IDL_UCHAR unsigned char
	#define IDL_UNSIGNED_CHAR unsigned char
	#define IDL_SHORT short
	#define IDL_USHORT unsigned short
	#define IDL_UNSIGNED_SHORT unsigned short
	#define IDL_LONG int
	#define IDL_ULONG unsigned int
	#define IDL_UNSIGNED_LONG unsigned int
	#define IDL_FLOAT float
	#define IDL_DOUBLE double
#endif

#endif
