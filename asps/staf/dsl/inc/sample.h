/* Copyright 1993, Lawrence Berkeley Laboratory */

/* sample.h - definitions for sample I/O */

/*
modification history
--------------------
31jul93,whg  written.
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

#define SAMPLE_FILE "sample.bin"

#define TYPE_A_S "struct type_a {octet b; short s; long l;}"
typedef struct type_a {octet b; short s; long l;}TYPE_A_T;

#define TYPE_B_S "struct type_b {octet b; unsigned short us; unsigned long ul;}"
typedef struct type_b {octet b; unsigned short us; unsigned long ul;}TYPE_B_T;

#define TYPE_C_S "struct type_c {char c, str[21]; float f[3]; double d;}"
typedef struct type_c {char c, str[21]; float f[3]; double d;}TYPE_C_T;

#define TYPE_D_S "struct type_d {struct T {long a[3][5]; float p[3];}v;}"
typedef struct type_d {struct T {long a[3][5]; float p[3];}v;}TYPE_D_T;

int readDynamic(void);
int readProject(void);
int readVars(void);
int sample(void);
int writeDynamic(void);
int writeVars(void);
