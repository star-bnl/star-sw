/* program to find gcc work around */

#ifndef WIN32
#include <stdio.h>

#define DS_NAME_DIM	32	/* size of name plus zero byte */ 
typedef unsigned char octet;

typedef enum ds_code_t {
	DS_TYPE_CHAR = 0,	/* ascii character [0, 128) */
	DS_TYPE_OCTET,		/* unsigned 8-bit integer [0, 256) */
	DS_TYPE_SHORT,		/* signed 16-bit integer (-2^15, 2^15) */
	DS_TYPE_U_SHORT,	/* unsigned 16-bit integer [0, 2^16) */
	DS_TYPE_LONG,		/* signed 32-bit integer (-2^31, 2^31) */
	DS_TYPE_U_LONG,		/* unsigned 32-bit integer [0, 2^32) */
	DS_TYPE_FLOAT,		/* IEEE 32-bit floating point */
	DS_TYPE_DOUBLE,		/* IEEE 64-bit floating point */
	DS_TYPE_STRUCT		/* only constructed type */
}DS_CODE_T;

typedef struct ds_type_t {
	char name[DS_NAME_DIM];	/* type name */
	enum ds_code_t code;	/* type code */
	unsigned flags;			/* type flags */
	size_t size;			/* in memory sizeof(type) */
	size_t modulus;			/* alignment modulus */
	size_t stdsize;			/* size in standard encoding */
	size_t stdmodulus;		/* modulus in standard encoding */
	size_t nField;			/* number of fields that follow */
}DS_TYPE_T;

#define DS_MODULUS_STRUCT(s, c, t) struct c ## _MOD{char x; t y;}c ## _MOD_T

#define DS_TYPE_INIT(s, c, t) {#t, c, 0, sizeof(t),\
	sizeof(c ## _MOD_T) - sizeof(t), s, s, 0}

DS_MODULUS_STRUCT(1, DS_TYPE_CHAR,    char);
DS_MODULUS_STRUCT(1, DS_TYPE_OCTET,   octet);
DS_MODULUS_STRUCT(2, DS_TYPE_SHORT,   short);
DS_MODULUS_STRUCT(2, DS_TYPE_U_SHORT, unsigned short);
DS_MODULUS_STRUCT(4, DS_TYPE_LONG,    long);
DS_MODULUS_STRUCT(4, DS_TYPE_U_LONG,  unsigned long);
DS_MODULUS_STRUCT(4, DS_TYPE_FLOAT,   float);
DS_MODULUS_STRUCT(8, DS_TYPE_DOUBLE,  double);

static DS_TYPE_T basicType[] = {
	DS_TYPE_INIT(1, DS_TYPE_CHAR,    char),
	DS_TYPE_INIT(1, DS_TYPE_OCTET,   octet),
	DS_TYPE_INIT(2, DS_TYPE_SHORT,   short),
	DS_TYPE_INIT(2, DS_TYPE_U_SHORT, unsigned short),
	DS_TYPE_INIT(4, DS_TYPE_LONG,    long),
	DS_TYPE_INIT(4, DS_TYPE_U_LONG,  unsigned long),
	DS_TYPE_INIT(4, DS_TYPE_FLOAT,   float),
	DS_TYPE_INIT(8, DS_TYPE_DOUBLE,  double)};
	
static size_t nBasic = 0;

void main()
{
	size_t i, n = sizeof(basicType)/sizeof(basicType[0]);
	DS_TYPE_T *t;
	
	for (i = 0; i < n; i++) {
		t = &basicType[i];
		printf("%d\t%d\t%d\t%d\t%d\t%d\t%d\t%s\n",
			t->code, t->flags, t->size, t->modulus, t->stdsize,
			t->stdmodulus, t->nField, t->name);
			
	}

}
#endif /* WIN32 */
