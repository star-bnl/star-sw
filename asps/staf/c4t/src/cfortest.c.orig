/* cfortest.c */
/* Burkhard Burow, burow@vxdesy.cern.ch, U. of Toronto, 1993. */

#include <stdio.h>
#include "cfortran.h"

#define EASY_SELECT     /* To see the various examples select one of: 
        EASY_SELECT,SUBT_SELECT,  SZ_SELECT,  FT_SELECT,  FZ_SELECT, SS1_SELECT,
         ABC_SELECT,  RR_SELECT, REV_SELECT, FCB_SELECT,  EQ_SELECT,  F0_SELECT,
          FA_SELECT,  FB_SELECT,  FC_SELECT,  FD_SELECT,  FE_SELECT,  FF_SELECT,
          FG_SELECT,  FH_SELECT,  FI_SELECT,  FJ_SELECT,  FK_SELECT,  FL_SELECT,
          FM_SELECT,  FN_SELECT,  VV_SELECT,  V7_SELECT,FAND_SELECT,FORR_SELECT,
      STRTOK_SELECT,USER_SELECT, FUN_SELECT, SUB_SELECT.   Q_SELECT,  E2_SELECT,
        FSTR_SELECT.
*/

#ifdef NAGf90Fortran
/* NAG f90 library hijacks main() and the user's program starts with a call to
   void f90_main(void);                   
   No real problem here, but woe is the C appliation which uses command line
   arguments for which NAG f90 provides no support. */
#define main f90_main
#endif

#ifdef EASY_SELECT
#define EASY(A,B)      CCALLSFSUB2(EASY,easy,PINT,INT, A,B)

main() {
int a;
printf("\nEASY EXAMPLE\n");
EASY(a,7);
printf("The FORTRAN routine EASY(a,7) returns a = %d\n", a);
}
#endif

#ifdef SUBT_SELECT
#define SUBT(A,B,C) CCALLSFSUB3(SUBT,subt,PSTRINGV,STRINGV,FLOAT,A,B,C)

int main() {
static char v[][5] = {"000 ", "1", "22", " "};
static char w[][9]  = {" ", "bb","ccc ","dddd"};
SUBT(v, w, 10.);
printf("main:v=%s,%s,%s,%s. PSTRINGV => Has had trailing blanks stripped.\n",
       v[0],v[1],v[2],v[3]);
printf("main:w=%s,%s,%s,%s. STRINGV => malloc'd copy for FORTRAN=> C intact.\n"
       ,w[0],w[1],w[2],w[3]);
}
#endif

#ifdef SZ_SELECT
#define sz_ELEMS_1   ZTRINGV_ARGS(3)
#define sz_ELEMLEN_1 ZTRINGV_NUM(6)
#define sz_ELEMS_2   ZTRINGV_NUM(4)
#define sz_ELEMLEN_2 ZTRINGV_NUM(8)
#define SZ(A,B,C) CCALLSFSUB3(SZ,sz,PZTRINGV,ZTRINGV,INT,A,B,C)

int main() {
static char v[][7] = {"000 ", "1", "22", " "};
static char w[][9]  = {" ", "bb","ccc ","dddd"};
SZ(v, w, 4);
printf("main:v=%s,%s,%s,%s. PSTRINGV => Has had trailing blanks stripped.\n",
       v[0],v[1],v[2],v[3]);
printf("main:w=%s,%s,%s,%s. STRINGV => malloc'd copy for FORTRAN=> C intact.\n"
       ,w[0],w[1],w[2],w[3]);
}
#endif

#ifdef FT_SELECT
PROTOCCALLSFFUN3(STRING,FT,ft,PSTRINGV,STRINGV,FLOAT)
#define FT(A,B,C) CCALLSFFUN3(FT,ft,PSTRINGV,STRINGV,FLOAT,A,B,C)

main() {
static char v[][5] = {"000 ", "1", "22", " "};
static char w[][9]  = {" ", "bb","ccc ","dddd"};
float a = 10.0;
printf("FT(v, w, a); returns:%s.\n",FT(v, w, a));
printf("main:v=%s,%s,%s,%s. PSTRINGV => Has had trailing blanks stripped.\n",
       v[0],v[1],v[2],v[3]);
printf("main:w=%s,%s,%s,%s. STRINGV => malloc'd copy for FORTRAN=> C intact.\n"
       ,w[0],w[1],w[2],w[3]);
}
#endif

#ifdef FZ_SELECT
#define fz_ELEMS_1   ZTRINGV_ARGF(3)
#define fz_ELEMLEN_1 ZTRINGV_NUM(6)
#define fz_ELEMS_2   ZTRINGV_NUM(4)
#define fz_ELEMLEN_2 ZTRINGV_NUM(8)
PROTOCCALLSFFUN3(STRING,FZ,fz,PZTRINGV,ZTRINGV,INT)
#define FZ(A,B,C) CCALLSFFUN3(FZ,fz,PZTRINGV,ZTRINGV,INT,A,B,C)

main() {
static char v[][7] = {"000 ", "1", "22", " "};
static char w[][9]  = {" ", "bb","ccc ","dddd"};
printf("FZ(v, w, a); returns:%s.\n",FZ(v, w, 4));
printf("main:v=%s,%s,%s,%s. PSTRINGV => Has had trailing blanks stripped.\n",
       v[0],v[1],v[2],v[3]);
printf("main:w=%s,%s,%s,%s. STRINGV => malloc'd copy for FORTRAN=> C intact.\n"
       ,w[0],w[1],w[2],w[3]);
}
#endif

#ifdef SS1_SELECT
#define SS1(A1)             CCALLSFSUB1(SS1,ss1,PSTRING,A1)
#define FORSTR1(A1)         CCALLSFSUB1(FORSTR1,forstr1,PSTRING,A1)

main() {
static char b[] = "abcdefghij", forb[13] = "abcdefghijkl";
SS1(b); FORSTR1(forb);
printf("SS1(b) returns b = %s; FORSTR1(forb) = returns forb = %s;\n", b, forb);
}
#endif

#ifdef ABC_SELECT
#define ABC(A1,A2,A3)       CCALLSFSUB3(ABC,abc,STRING,PSTRING,PSTRING,A1,A2,A3)

main() {
static char aa[] = "one  ", bb[] = "two  ", cc[] = "three"; int i; 
for (i=0; i<10; i++) {printf("%s;%s;%s;\n",aa,bb,cc); ABC(aa,bb,cc);}
}
#endif

#ifdef RR_SELECT
PROTOCCALLSFFUN1(FLOAT,RR,rr,INT)
#define RR(A1)               CCALLSFFUN1(RR,rr,INT,A1)
PROTOCCALLSFFUN0(STRING,FORSTR2,forstr2)
#define FORSTR2()           CCALLSFFUN0(FORSTR2,forstr2)
PROTOCCALLSFFUN1(STRING,FORSTR,forstr,STRING)
#define FORSTR(A1)          CCALLSFFUN1(FORSTR,forstr,STRING,A1)

main() {
static char aa[] = "one";
int rrr = 333;
printf("RR(rrr=%d) returns int arg. as float:%f\n",rrr,RR(rrr));
printf("FORSTR(aa=%s) returns the string arg. as:%s<-end here\n",aa,FORSTR(aa));
printf("FORSTR2() returns the string constant:%s<-end here\n",FORSTR2());
}
#endif

#ifdef REV_SELECT
PROTOCCALLSFFUN1(INT,FREV,frev,INTV)
#define FREV(A1)               CCALLSFFUN1(FREV,frev,INTV,A1)
#define REV(A1)                CCALLSFSUB1(REV,rev,INTV,A1)

main() {
static int a[] = {1,2};
printf("REV(a[0,1]=%d,%d) receives:",a[0],a[1]);
REV(a); printf("a[0,1]=%d,%d\n",a[0],a[1]);
printf("FREV(a[0,1]=%d,%d) receives:",a[0],a[1]);
printf("%d",FREV(a)); printf(" with a[0,1]=%d,%d\n",a[0],a[1]);
}
#endif

#ifdef FCB_SELECT
#define FFCB()                 CCALLSFSUB0(FFCB,ffcb)

typedef struct { char v[13],w[4][13],x[2][3][13]; } FCB_DEF;
#define Fcb COMMON_BLOCK(FCB,fcb)
COMMON_BLOCK_DEF(FCB_DEF,Fcb);
FCB_DEF Fcb;

main() {
char cv[14];
static char cw[4][14]    = {"C's w[0]", "C's w[1]", "C's w[2]", "C's w[3]"};
static char cx[2][3][14] = {"C's x[0][0]", "C's x[0][1]", "C's x[0][2]", 
                            "C's x[1][0]", "C's x[1][1]", "C's x[1][2]"};
C2FCBSTR("C's V" ,Fcb.v,0);
C2FCBSTR(cw      ,Fcb.w,1);
C2FCBSTR(cx      ,Fcb.x,2);
FFCB();
FCB2CSTR(Fcb.v   ,cv   ,0);
FCB2CSTR(Fcb.w   ,cw   ,1);
FCB2CSTR(Fcb.x   ,cx   ,2);
printf("FFCB returns v = %s.\n",cv);
printf("FFCB returns w[1,2,3,4] = %s,%s,%s,%s.\n",cw[0],cw[1],cw[2],cw[3]);
printf("FFCB returns x[0,(1,2,3)] = %s,%s,%s.\n",cx[0][0],cx[0][1],cx[0][2]);
printf("FFCB returns x[1,(1,2,3)] = %s,%s,%s.\n",cx[1][0],cx[1][1],cx[1][2]);
}
#endif

#ifdef EQ_SELECT
#define FEQ()                 CCALLSFSUB0(FEQ,feq)

#define KWBANK 690
typedef struct {
  int nzebra; float gversn,zversn; int ixstor,ixdiv,ixcons; float fendq[16];
  union {
    struct {
      int Lmain,Lr1; 
      union {float Ws[KWBANK]; int Iws[2];}u;
    }s;
    union {
      int Lq[80];
      struct {
        int dummy[8];
        union {float Q[2]; int Iq[2];}u;
      }s;
    }u;
  }u;
} GCBANK_DEF;
#define lmain u.s.Lmain
#define lr1   u.s.Lr1
#define ws    u.s.u.Ws
#define iws   u.s.u.Iws
#define lq    u.u.Lq
#define q     u.u.s.u.Q
#define iq    u.u.s.u.Iq
#define GCbank COMMON_BLOCK(GCBANK,gcbank)
COMMON_BLOCK_DEF(GCBANK_DEF,GCbank);
GCBANK_DEF GCbank;

main() {
FEQ();
printf("GCbank.nzebra       = %d.\n", GCbank.nzebra);
printf("GCbank.gversn       = %f.\n", GCbank.gversn);
printf("GCbank.zversn       = %f.\n", GCbank.zversn);
printf("GCbank.ixstor       = %d.\n", GCbank.ixstor);
printf("GCbank.ixcons       = %d.\n", GCbank.ixcons);
printf("GCbank.fendq[15]    = %f.\n", GCbank.fendq[15]);
printf("GCbank.lmain        = %d.\n", GCbank.lmain);
printf("GCbank.lr1          = %d.\n", GCbank.lr1);
printf("GCbank.ws[KWBANK-1] = %f.\n", GCbank.ws[KWBANK-1]);
printf("GCbank.iq[0]        = %d.\n", GCbank.iq[0]);
}
#endif

/* The following functions, exist through cor, are called by FORTRAN functions,
   as shown by the remaining examples. */

#ifdef CF_SAME_NAMESPACE
/* 
   VAX/VMS
   HP-UX (without the f77 +ppu      option. Ignore the undesirable -U option.)
   IBMR2 (without the xlf -qextname option.)
have C and FORTRAN sharing the same name space. The name space is
case-insensitive for VAX/VMS. There are several ways, some are described in
cfortran.doc, to meet this constraint, which is only a difficulty for C
routines to be called by FORTRAN.

The conflict is explicitly avoided, as shown, for the routines: ca, cb, cc, cd.

For VAX/VMS we need to change the name, (changing the case is not enough since
VAX/VMS is case insensitive. This is done implicitly via the defines given 
below: 

For the IBM and the HP, we have chosen to name the C routines using a Proper 
Case notation, i.e:
                   Exist, Ce, Ccff, Ccg, Cch, Ci, Cj, Ck, Cl, Cm, Cn, Cand, Cor.
instead of the usual C convention:
                   exist, ce, ccff, ccg, cch, ci, cj, ck, cl, cm, cn, cand, cor.

NOTE THAT THIS DEMO WILL RUN ON ALL MACHINES,
EXCEPT THE HP9000      (when not using f77 +ppu) 
AND    THE IBM RS/6000 (when not using f77 -qextname),
IF 'Exist', ETC. ARE CHANGED TO LOWER CASE.
i.e. Only these two machines, when their Fortran compilers aren't forced to
     append underscores, can require code to go against C naming norms.
*/

#ifdef vmsFortran
#define Exist EXIST_
/*#define ca    CA_*/    /* We don't do this since we've decided to call the
                            routine ca from FORTRAN  as CFORTRANCA.           */
/*#define cb    CB_*/    /* Similarly we call cb as CFCB.                     */
/*#define cc    CC_*/    /*               and cc as CFCC.                     */
/*#define cd    CD_*/    /*               and cd as   CDCFORT.                */
#define Ce    CE_
#define Ccff  CCFF_
#define Ccg   CCG_
#define Cch   CCH_
#define Ci    CI_
#define Cj    CJ_
#define Ck    CK_
#define Cl    CL_
#define Cm    CM_
#define Cn    CN_
#define Cvv   CVV_
#define Cv7   CV7_
#define Cand  CAND_
#define Cor   COR_
#define Cadd  CADD_
#define Cfun  CFUN_
#define Pstru PSTRU_
#define Pstr  PSTR_
#endif                      /* vmsFortran        */
#endif                      /* CF_SAME_NAMESPACE */

void Exist() {printf("exist: was called.\n");}
FCALLSCSUB0(Exist,EXIST,exist)

void ca(i) int i; {printf("ca: had integer argument:%d.\n",i);}
FCALLSCSUB1(ca,CFORTRANCA,cfortranca,INT)
/*           ^      ^-----------^---------FORTRAN name. 
             |__ C name.                                                      */


/* With the next 2 lines we tell cfortran.h that for the subsequent FCALLSCSUBn
   and FCALLSCSUBn declarations, FORTRAN entry points to C routines have the 
   C name prefaced with the characters 'CF', i.e. whereas the
   C name of the routine is 'cb', the routine is called from FORTRAN as 'CFCB'. 
   Similarly C's cc, is CFCC for FORTRAN. */
#undef  fcallsc
#define fcallsc(UN,LN) preface_fcallsc(CF,cf,UN,LN)

void cb(i) int *i; 
{printf("cb: had pointer argument to integer:%d.\n",*i); *i*=2;}
FCALLSCSUB1(cb,CB,cb,PINT)

void cc(s) char *s; {printf("cc: had string argument:%s.\n",s);}
FCALLSCSUB1(cc,CC,cc,STRING)

/* With the next 2 lines we tell cfortran.h that for the subsequent FCALLSCSUBn
   and FCALLSCSUBn declarations, FORTRAN entry points to C routines have the 
   C name appended with the characters 'CFORT', i.e. whereas the C name of the
   routine is 'cd', the routine is called from FORTRAN as 'CDCFORT'.          */
#undef  fcallsc
#define fcallsc(UN,LN) append_fcallsc(CFORT,cfort,UN,LN)

void cd(s) char *s;
{printf("cd: had string argument:%s.\n",s); strcpy(s,"to you 12345678");}
FCALLSCSUB1(cd,CD,cd,PSTRING)

#undef  fcallsc
#define fcallsc        orig_fcallsc
/* The preceeding line returns FORTRAN names to being the original C names.   */

void Ce(v) char v[][5];
{printf("ce: had string vector argument:%s,%s,%s.\n",v[0],v[1],v[2]);}
#define ce_STRV_A1 TERM_CHARS(' ',1)
FCALLSCSUB1(Ce,CE,ce,STRINGV)

void Ccff(v, n) char v[][5]; int n;
{int i;
printf("ccff: had %d string vector argument:",n);
for (i=0; i<n-1; i++) printf("%s,",v[i]);
printf("%s.\n",v[i]);
}
#define ccff_STRV_A1 NUM_ELEM_ARG(2)
FCALLSCSUB2(Ccff,CCFF,ccff,STRINGV,INT)


int Ccg() {return 111;}
FCALLSCFUN0(INT,Ccg,CCG,ccg)

char *Cch() {return "hello";}
FCALLSCFUN0(STRING,Cch,CCH,cch)

char *Ci(v) char v[][5]; {return v[3];}
#define ci_STRV_A1 NUM_ELEMS(6)
FCALLSCFUN1(STRING,Ci,CI,ci,STRINGV)

char *Cj(v) int v; {printf("cj:v=%d\n",v);return "hello";}
FCALLSCFUN1(STRING,Cj,CJ,cj,INT)

float Ck() {return 111.;}
FCALLSCFUN0(FLOAT,Ck,CK,ck)

DOUBLE_PRECISION Cl() {return 111.;}
FCALLSCFUN0(DOUBLE,Cl,CL,cl)

float Cm(a) float a; {return a;}
FCALLSCFUN1(FLOAT,Cm,CM,cm,FLOAT)

DOUBLE_PRECISION Cn(a,b) DOUBLE_PRECISION a; DOUBLE_PRECISION b; {return a+b;}
FCALLSCFUN2(DOUBLE,Cn,CN,cn,DOUBLE,DOUBLE)

void Cvv(d,f,i) DOUBLE_PRECISION d[2][2]; float f[2][2]; int i[2][2];
{
int j,k; double t[2][2];
for (j=0; j<2; j++) for (k=0; k<2; k++) {
  t[j][k] = d[j][k];
  d[j][k] = f[j][k];
  f[j][k] = i[j][k];
  i[j][k] = t[j][k];
}
return;
}
FCALLSCSUB3(Cvv,CVV,cvv,DOUBLEVV,FLOATVV,INTVV)

DOUBLE_PRECISION Cv7(d) DOUBLE_PRECISION d[2][3][5][7][11][13][1];
{
DOUBLE_PRECISION t=0;
int i,j,k,l,m,n,o;
for (            i=0; i< 2; i++) 
  for (          j=0; j< 3; j++)
    for (        k=0; k< 5; k++) 
      for (      l=0; l< 7; l++) 
        for (    m=0; m<11; m++) 
          for (  n=0; n<13; n++) 
            for (o=0; o< 1; o++) t += d[i][j][k][l][m][n][o];
return t;
}
FCALLSCFUN1(DOUBLE,Cv7,CV7,cv7,DOUBLEVVVVVVV)

int Cand(a,b) int a; int b; {return a && b;}
FCALLSCFUN2(LOGICAL,Cand,CAND,cand,LOGICAL,LOGICAL)

int Cor(a,b) int *a; int *b; {int t; t= *a;*a= *b;*b=t; return *a || *b;}
FCALLSCFUN2(LOGICAL,Cor,COR,cor,PLOGICAL,PLOGICAL)

void Pstru(s) char *s; { strcpy(s,"new pstring"); return;}
FCALLSCSUB1(Pstru,PSTRU,pstru,PSTRING)

void Pstr(s) char *s;
{
static char *save=NULL;
char *temp;
int ls, lsave;
/* If required, reset, or prepare to reset the saved location. */
if (!s || !save) { save=s; return; }
ls    = strlen(s   );
lsave = strlen(save);
temp = malloc(ls>lsave?ls:lsave);
/* Switch contents of argument with contents of saved string. */
strcpy(temp,save);
strcpy(save,s   );
strcpy(s   ,temp);
free(temp);
return;
}
/* Provide 3 interfaces using the the 3 types of PSTRING. */
FCALLSCSUB1(Pstr,PSTR,pstr,PSTRING)
FCALLSCSUB1(Pstr,PNSTR,pnstr,PNSTRING)
FCALLSCSUB1(Pstr,PPSTR,ppstr,PPSTRING)


#ifdef F0_SELECT
#define FEXIST()               CCALLSFSUB0(FEXIST,fexist)

main() {FEXIST();}
#endif

#ifdef FA_SELECT
#define FA(A1)               CCALLSFSUB1(FA,fa,INT,A1)

main() {FA(1234);}
#endif

#ifdef FB_SELECT
#define FB(A1)               CCALLSFSUB1(FB,fb,PINT,A1)

main() 
{int i,ii; i=ii=1234; 
 FB(ii); printf("MAIN: FB(i=%d) returns with i=%d.\n",i,ii);}
#endif

#ifdef FC_SELECT
#define FC(A1)               CCALLSFSUB1(FC,fc,STRING,A1)

main() {FC("hello");}
#endif

#ifdef FD_SELECT
#define FD(A1)               CCALLSFSUB1(FD,fd,PSTRING,A1)

main() 
{static char i[] = "happy     "; static char ii[] = "happy      "; 
 FD(ii); printf("MAIN: FD(i=%s) returns with i=%s.\n",i,ii);}
#endif

#ifdef FE_SELECT
#define FE(A1)               CCALLSFSUB1(FE,fe,STRINGV,A1)

main() 
{static char v[][5] = {"0000", "1", "22", ""}; FE(v);}
#endif

#ifdef FF_SELECT
#define FF(A1,A2)               CCALLSFSUB2(FF,ff,STRINGV,INT, A1,A2)

main() 
{static char v[][5] = {"0000", "1", "22", ""}; 
 FF(v,sizeof(v)/sizeof v[0]);}
#endif

#ifdef FG_SELECT
PROTOCCALLSFFUN0(INT,FG,fg)
#define FG()               CCALLSFFUN0(FG,fg)

main() 
{printf("FG() returns %d.\n",FG());}
#endif

#ifdef FH_SELECT
PROTOCCALLSFFUN0(STRING,FH,fh)
#define FH()               CCALLSFFUN0(FH,fh)

main() 
{printf("FH() returns %s.\n",FH());}
#endif

#ifdef FI_SELECT
PROTOCCALLSFFUN1(STRING,FI,fi,STRINGV)
#define FI(A1)               CCALLSFFUN1(FI,fi,STRINGV,A1)

main() 
{static char v[][5] = {"0000", "1", "22", "333", "8", "9"}; 
 printf("FI(v) returns %s.\n",FI(v));}
#endif

#ifdef FJ_SELECT
PROTOCCALLSFFUN1(STRING,FJ,fj,INT)
#define FJ(A1)               CCALLSFFUN1(FJ,fj,INT,A1)

main() 
{ printf("FJ(2) returns %s.\n",FJ(2));}
#endif

#ifdef FK_SELECT
PROTOCCALLSFFUN0(FLOAT,FK,fk)
#define FK()               CCALLSFFUN0(FK,fk)

main() 
{printf("FK() returns %f.\n",FK());}
#endif

#ifdef FL_SELECT
PROTOCCALLSFFUN0(DOUBLE,FL,fl)
#define FL()               CCALLSFFUN0(FL,fl)

main() 
{printf("FL() returns %f.\n",(double)FL());}
#endif                       /* ^- cast req.d for CRAY. */

#ifdef FM_SELECT
PROTOCCALLSFFUN1(FLOAT,FM,fm,FLOAT) 
#define FM(A)               CCALLSFFUN1(FM,fm,FLOAT, A)

main() 
{printf("FM(111.) returns %f.\n",FM(111.));}
#endif

#ifdef FN_SELECT
PROTOCCALLSFFUN2(DOUBLE,FN,fn,DOUBLE,DOUBLE)
#define FN(A,B)             CCALLSFFUN2(FN,fn,DOUBLE,DOUBLE, A,B)

main() 
{printf("FN(1./3, 2./3) returns %f.\n",(double)FN(1./3, 2./3));}
#endif                                /* ^- cast req.d for CRAY. */

#ifdef VV_SELECT
#define VV(D,F,I)             CCALLSFSUB3(VV,vv,DOUBLEVV,FLOATVV,INTVV,D,F,I)

main()
{
DOUBLE_PRECISION d[2][2]; 
float            f[2][2];
int              i[2][2];
int j,k;
for (j=0; j<2; j++) for (k=0; k<2; k++) {
  d[j][k] = 100+10*j+k;
  f[j][k] = 200+10*j+k;
  i[j][k] = 300+10*j+k;
}
VV(d,f,i);
                               /*  \/- cast req.d for CRAY. */
printf("%4.0f%4.0f%4.0f%4.0f\n",(double)d[0][0],(double)d[0][1],
                                (double)d[1][0],(double)d[1][1]);
printf("%4.0f%4.0f%4.0f%4.0f\n",f[0][0],f[0][1],f[1][0],f[1][1]);
printf("%4d%4d%4d%4d\n"        ,i[0][0],i[0][1],i[1][0],i[1][1]);
}
#endif

#ifdef V7_SELECT
PROTOCCALLSFFUN1(DOUBLE,V7,v7,DOUBLEVVVVVVV)
#define V7(D)               CCALLSFFUN1(V7,v7,DOUBLEVVVVVVV,D)

main()
{
/* Original d[2][3][5][7][11][13][17] died a SEGV on DECstation MIPS cc 2.10, 
   just like e.g.             main() {double d[2][3][5][7][11][13][17], t=0;} */

DOUBLE_PRECISION d[2][3][5][7][11][13][1], t=0, r=1, tf;
int i,j,k,l,m,n,o;
for (            i=0; i< 2; i++) 
  for (          j=0; j< 3; j++)
    for (        k=0; k< 5; k++) 
      for (      l=0; l< 7; l++) 
        for (    m=0; m<11; m++) 
          for (  n=0; n<13; n++) 
            for (o=0; o< 1; o++) {
              r /= 2;
              t += r;
              d[i][j][k][l][m][n][o] = r;
            }
tf=V7(d);                         
printf("main() filled array d with a total: %10.9f\n", (double)t );
printf("V7()   returned the value:          %10.9f\n", (double)tf);
}                                 /* cast req.d for CRAY -^ */
#endif

#ifdef FAND_SELECT
PROTOCCALLSFFUN2(LOGICAL,FAND,fand,LOGICAL,LOGICAL)
#define FAND(A,B)             CCALLSFFUN2(FAND,fand,LOGICAL,LOGICAL, A,B)

main() 
{printf("FAND(0, 1) returns %d.\n",FAND(0, 1));}
#endif

#ifdef FORR_SELECT
PROTOCCALLSFFUN2(LOGICAL,FORR,forr,PLOGICAL,PLOGICAL)
#define FORR(A,B)             CCALLSFFUN2(FORR,forr,PLOGICAL,PLOGICAL, A,B)

main() 
{int a=2, b=0; printf("Calling FORR(a=%d, b=%d).\n", a,b);
               printf("FORR() returned %d.\n", FORR(a, b));
               printf("With a=%d, b=%d.\n", a,b);}
#endif


#include <string.h>
FCALLSCFUN2(STRING,strtok,CSTRTOK,cstrtok,STRING,STRING)

#ifdef STRTOK_SELECT
#define FSTRTOK()      CCALLSFSUB0(FSTRTOK,fstrtok)

main() {FSTRTOK();}
#endif

#ifdef USER_SELECT
/* We define a new type USERINT. [Same functionality as PINT actually.] */

#ifdef OLD_VAXC        /* To avoid %CC-I-PARAMNOTUSED. */
#pragma nostandard
#endif

#define VUSERINT              VSIMPLE
#define SEP_USERINT        SEP_SIMPLE
#define INT_USERINT        INT_SIMPLE
#define ZUSERINT              ZSIMPLE
#define STR_USERINT        STR_SIMPLE
#define CCUSERINT            CCSIMPLE
#define AAUSERINT( T,A,B)     BUSERINT(T,A)
#define UUSERINT(  T,A)       NUSERINT(T,A)

#define NUSERINT(  T,A)  int *A
#define BUSERINT(  T,A)     &(A)

#ifdef OLD_VAXC        /* Have avoided %CC-I-PARAMNOTUSED. */
#pragma standard
#endif

#define EASY(A,B)      CCALLSFSUB2(EASY,easy,USERINT,INT, A,B)

main() {
int a;
printf("\nUsing user defined USERINT argument type.\n");
EASY(a,7);
printf("The FORTRAN routine EASY(a,7) returns a = %d\n", a);
}
#endif

#ifdef FUN_SELECT /* Passing C or Fortran Functions to Fortran routines. */
PROTOCCALLSFFUN3(INT,FUNADD,funadd,ROUTINE,INT,INT)
#define FUNADD(F,A,B)      CCALLSFFUN3(FUNADD,funadd,ROUTINE,INT,INT, F,A,B)

int Cadd(a,b) int a; int b; {return a+b;}
FCALLSCFUN2(INT,Cadd,CADD,cadd,INT,INT)

PROTOCCALLSFFUN2(INT,FADD,fadd,INT,INT)

main() {

printf("\nFUNADD(CADD,1,2) returns %d\n", 
       FUNADD(      C_FUNCTION(CADD,cadd),1,2) );
printf("\nFUNADD(FADD,3,4) returns %d\n", 
       FUNADD(FORTRAN_FUNCTION(FADD,fadd),3,4) );
}
#endif

#ifdef SUB_SELECT /* Fortran passes routines to C. */
#define FUNARG(F,A,B,C) CCALLSFSUB4(FUNARG,funarg,ROUTINE,INT,INT,PINT, F,A,B,C)

int Cfun(f,a,b) int (*f)(); int a; int b; {int c; f(&a,&b,&c); return c;}
#ifndef apollo
#undef  ROUTINE_1
#define ROUTINE_1  (int (*)())
#endif
FCALLSCFUN3(INT,Cfun,CFUN,cfun,ROUTINE,INT,INT)

main() {
int c;
FUNARG(C_FUNCTION(CFUN,cfun),1,2,c);
printf("\nFUNARG(CFUN,1,2,c) returns with c=%d\n",c);
}
#endif

#if !(defined(apollo)||defined(sun))
       /* Silly apollo suffers multiple declaration of realloc,malloc,calloc. */
       /* Sun doesn't have prototype for qsort nor stdlib.h.                  */
#include <stdlib.h>
#endif
#ifndef apollo
#undef  ROUTINE_4
#define ROUTINE_4 (int (*)())
#endif
FCALLSCSUB4(qsort,FQSORT,fqsort,PVOID,INT,INT,ROUTINE)
/* Note that we've assumed in the above that size_t == int */

#ifdef Q_SELECT
#define FQSORTEX(SIZEOF_INT) CCALLSFSUB1(FQSORTEX,fqsortex,INT,SIZEOF_INT)

main() {FQSORTEX(sizeof(int));}
#endif

#ifdef E2_SELECT
/* Only to demo. that we can force a wrapper to be used for subroutines. */
PROTOCCALLSFFUN2(VOID,EASY,easy,PINT,INT)
#define EASY(A,B)      CCALLSFFUN2(EASY,easy,PINT,INT, A,B)

main() {
int a;
printf("\nEASY (2) EXAMPLE\n");
EASY(a,7);
printf("The FORTRAN routine EASY(a,7) returns a = %d\n", a);
}
#endif

#ifdef FSTR_SELECT
#define FSTR()                 CCALLSFSUB0(FSTR,fstr)

main() { FSTR(); }
#endif
