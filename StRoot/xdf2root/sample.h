/* Copyright 1993, Lawrence Berkeley Laboratory */

/* sample.h - definitions for sample I/O */

/*
modification history
--------------------
01a,31jul93,whg  written.
*/

/*
DESCRIPTION
TBS ...
*/
#define NROW_TABLE1 10
#define NROW_TABLE2 20
#define NROW_TABLE3 30
#define NROW_TABLE4 10
#define NROW_TABLE5 15
#define DSET_DIM 10

#define byte signed char
#define u_byte unsigned char
#define u_short unsigned short
#define u_int unsigned
#define u_long unsigned long

#define TYPE_A_S "struct type_a {byte b; short s; int i; long l;}"
typedef struct type_a {byte b; short s; int i; long l;}TYPE_A_T;

#define TYPE_B_S "struct type_b {u_byte ub; u_short us; u_int ui; u_long ul;}"
typedef struct type_b {u_byte ub; u_short us; u_int ui; u_long ul;}TYPE_B_T;

#define TYPE_C_S "struct type_c {char c, str[21]; float f[3]; double d;}"
typedef struct type_c {char c, str[21]; float f[3]; double d;}TYPE_C_T;

#define TYPE_D_S "struct type_d {int i; struct T {int a[3][5]; float p[3];}v;}"
typedef struct type_d {int i; struct T {int a[3][5]; float p[3];}v;}TYPE_D_T;

int checkTable(void *pData, char *decl, size_t nRow, size_t checkNRow);
int setDataset(DS_DATASET_T *pSet);
int setTable(void *pData, char *decl, size_t elcount);
