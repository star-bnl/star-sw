/*
	ltproto.h.in
		provides all low-level prototypes for linking a C++ program
		that uses clooptools.h with the (Fortran) LoopTools library
		this file is part of LoopTools
		last modified 28 Jun 04 th
*/


#define A0 a0sub_
#define CA0 ca0sub_

#define B0 b0sub_
#define CB0 cb0sub_
#define DB0 db0sub_
#define CDB0 cdb0sub_
#define B1 b1sub_
#define CB1 cb1sub_
#define DB1 db1sub_
#define CDB1 cdb1sub_
#define B00 b00sub_
#define CB00 cb00sub_
#define DB00 db00sub_
#define CDB00 cdb00sub_
#define B11 b11sub_
#define CB11 cb11sub_
#define DB11 db11sub_
#define CDB11 cdb11sub_

#define C0 c0sub_
#define CC0 cc0sub_
#define C0i c0isub_
#define CC0i cc0isub_
#define Cget cget_
#define CCget ccget_

#define D0 d0sub_
#define CD0 cd0sub_
#define D0i d0isub_
#define CD0i cd0isub_
#define Dget dget_
#define CDget cdget_

#define ffini ffini_
#define ffexi ffexi_

#define setmudim setmudim_
#define getmudim getmudim_
#define setdelta setdelta_
#define getdelta getdelta_
#define setlambda setlambda_
#define getlambda getlambda_

#define cachelookup cachelookup_
#define setcachelast setcachelast_
#define getcachelast getcachelast_

#define Ccache cbase_
#define cbase_(pos) double_complex(cbase_[pos - 1].r, cbase_[pos - 1].i)
#define CCcache ccbase_
#define ccbase_(pos) double_complex(ccbase_[pos - 1].r, ccbase_[pos - 1].i)
#define Dcache dbase_
#define dbase_(pos) double_complex(dbase_[pos - 1].r, dbase_[pos - 1].i)
#define CDcache cdbase_
#define cdbase_(pos) double_complex(cdbase_[pos - 1].r, cdbase_[pos - 1].i)

typedef struct { double re, im; } dcomplex;


#ifdef __cplusplus
extern "C" {
#endif

void A0(dcomplex *, const double *);
void CA0(dcomplex *, const dcomplex *);

void B0(dcomplex *, const double *, const double *, const double *);
void CB0(dcomplex *, const dcomplex *, const dcomplex *, const dcomplex *);
void DB0(dcomplex *, const double *, const double *, const double *);
void CDB0(dcomplex *, const dcomplex *, const dcomplex *, const dcomplex *);
void B1(dcomplex *, const double *, const double *, const double *);
void CB1(dcomplex *, const dcomplex *, const dcomplex *, const dcomplex *);
void DB1(dcomplex *, const double *, const double *, const double *);
void CDB1(dcomplex *, const dcomplex *, const dcomplex *, const dcomplex *);
void B00(dcomplex *, const double *, const double *, const double *);
void CB00(dcomplex *, const dcomplex *, const dcomplex *, const dcomplex *);
void DB00(dcomplex *, const double *, const double *, const double *);
void CDB00(dcomplex *, const dcomplex *, const dcomplex *, const dcomplex *);
void B11(dcomplex *, const double *, const double *, const double *);
void CB11(dcomplex *, const dcomplex *, const dcomplex *, const dcomplex *);
void DB11(dcomplex *, const double *, const double *, const double *);
void CDB11(dcomplex *, const dcomplex *, const dcomplex *, const dcomplex *);

void C0(dcomplex *,
  const double *, const double *, const double *,
  const double *, const double *, const double *);
void CC0(dcomplex *,
  const dcomplex *, const dcomplex *, const dcomplex *,
  const dcomplex *, const dcomplex *, const dcomplex *);
void C0i(dcomplex *, const int *,
  const double *, const double *, const double *,
  const double *, const double *, const double *);
void CC0i(dcomplex *, const int *,
  const dcomplex *, const dcomplex *, const dcomplex *,
  const dcomplex *, const dcomplex *, const dcomplex *);
int Cget(
  const double *, const double *, const double *,
  const double *, const double *, const double *);
int CCget(
  const dcomplex *, const dcomplex *, const dcomplex *,
  const dcomplex *, const dcomplex *, const dcomplex *);

void D0(dcomplex *,
  const double *, const double *, const double *, const double *,
  const double *, const double *,
  const double *, const double *, const double *, const double *);
void CD0(dcomplex *,
  const dcomplex *, const dcomplex *, const dcomplex *, const dcomplex *,
  const dcomplex *, const dcomplex *,
  const dcomplex *, const dcomplex *, const dcomplex *, const dcomplex *);
void D0i(dcomplex *, const int *,
  const double *, const double *, const double *, const double *,
  const double *, const double *,
  const double *, const double *, const double *, const double *);
void CD0i(dcomplex *, const int *,
  const dcomplex *, const dcomplex *, const dcomplex *, const dcomplex *,
  const dcomplex *, const dcomplex *,
  const dcomplex *, const dcomplex *, const dcomplex *, const dcomplex *);
int Dget(
  const double *, const double *, const double *, const double *,
  const double *, const double *,
  const double *, const double *, const double *, const double *);
int CDget(
  const dcomplex *, const dcomplex *, const dcomplex *, const dcomplex *,
  const dcomplex *, const dcomplex *,
  const dcomplex *, const dcomplex *, const dcomplex *, const dcomplex *);

void ffini();

void ffexi();

void setmudim(const double *);
double getmudim();
void setdelta(const double *);
double getdelta();
void setlambda(const double *);
double getlambda();

void setcachelast(const dcomplex *, const int *);
int getcachelast(const dcomplex *);

extern dcomplex Ccache[], CCcache[], Dcache[], CDcache[];

#ifdef __cplusplus
}
#endif

