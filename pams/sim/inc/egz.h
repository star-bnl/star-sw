/* cfortran header file for EGZ
*/

PROTOCCALLSFFUN2 (INT,EGZINIT,egzinit,INT,STRING)
#define EGZINIT(A1,A2)  CCALLSFFUN2(EGZINIT,egzinit,INT,STRING,A1,A2)

PROTOCCALLSFFUN0 (INT,EGZIN,egzin)
#define EGZIN()  CCALLSFFUN0(EGZIN,egzin)

PROTOCCALLSFFUN1 (INT,EGZEND,egzend,INT)
#define EGZEND(A1)  CCALLSFFUN1(EGZEND,egzend,INT,A1)

#define HIJING_TO_GEANT(A1,A2)  CCALLSFSUB2(HIJING_TO_GEANT,hijing_to_geant,LONG,PLONG,A1,A2)

#define ISA_TO_GEANT(A1,A2)  CCALLSFSUB2(ISA_TO_GEANT,isa_to_geant,LONG,PLONG,A1,A2)

#define LUND_TO_GEANT(A1,A2)  CCALLSFSUB2(LUND_TO_GEANT,lund_to_geant,LONG,PLONG,A1,A2)

/* EGZ event common blocks (event.inc) */
typedef struct
     {
     int     nptls;
     float   bimevt;
     int     nntarg, nptarg, natarg, nnproj, npproj, naproj, ntry;
     float   px[30000], py[30000], pz[30000], energy[30000];
     int     idptl[30000], ioptl[30000];
     float   mass[30000], theta[30000], phi[30000], y[30000];
     int     gtype[30000];
     float   pt[30000];
     } EGEVNT_DEF;
#define EGevnt COMMON_BLOCK(EGEVNT,egevnt)
COMMON_BLOCK_DEF(EGEVNT_DEF,EGevnt);
EGEVNT_DEF EGevnt;

typedef struct
     {
     int     atarg, ztarg, aproj, zproj;
     float   sqrts, bmin, bmax;
     int     nbeam;
     } EGRXN_DEF;
#define EGrxn COMMON_BLOCK(EGRXN,egrxn)
COMMON_BLOCK_DEF(EGRXN_DEF,EGrxn);
EGRXN_DEF EGrxn;

typedef struct
{
int     evtcod;
float   evtver, rextra[50];
int     iextra[50];
} EGCTRL_DEF;
#define EGctrl COMMON_BLOCK(EGCTRL,egctrl)
COMMON_BLOCK_DEF(EGCTRL_DEF,EGctrl);
EGCTRL_DEF EGctrl;

typedef struct
     {
     int        z_input, z_lun_out, z_lun_in, part_head_size, part_data_size,
                z_double_out, z_blocksize;
     char       z_fz_option_out[4], z_fz_option_in[4];
     int        z_fzlogl;
     } EGZEBRA_DEF;
#define EGzebra COMMON_BLOCK(EGZEBRA,zebra_io)
COMMON_BLOCK_DEF(EGZEBRA_DEF,EGzebra);
EGZEBRA_DEF EGzebra;
