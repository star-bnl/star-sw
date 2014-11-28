*CMZ :  2.00/02 19/01/2000  23.56.38  by  Pavel Nevski
*-- Author :
      SUBROUTINE RZMAKE(LUNIN,CHDIR,NWKEY,CHFORM,CHTAG,NRECPin,CHOPT)
*
************************************************************************
*
*           Routine to create a new RZ file
*           To use an already existing file CALL RZFILE
* Input:
*   LUNIN   Logical unit number associated with  the RZ file.   A FORTRAN
*           OPEN statement must precede the call to RZFILE.
*           Starting address of the memory area which will contain the RZ
*           information ('M' option)
*   CHDIR   Character variable specifying  the name of the  top directory
*           to be associated with unit LUN.
*   NWKEY   Number of words associated to a key (maximum 5)
*   CHFORM  Character variable describing each element  of the key vector
*           'B' Bit string but not zero
*           'H' Hollerith (4 characters)
*           'I' Integer (nonzero)
*           Ex: CHFORM='IIH' for NWKEY=3 and the 2 first keys are integer
*               and the third one is Hollerith
*   CHTAG   Character array defined as CHARACTER*8 CHTAG(NWKEY).
*           Each  element of  the  array allows  the  description of  the
*           corresponding element in the key vector with a tag of up to 8
*           characters.
*   NRECP   Number of physical records for primary allocation
*   CHOPT   Character variable specifying the selected options.
*           medium
*             default
*                   Disk
*             'M'   Memory
*                   In this  case the user  must have allocated  at least
*                   NRecp*LRecp words of memory starting at address LUNIN.
*                   LRecp is passes as the first word in this area
*           mode
*             default
*                   Native mode
*             'X'   Exchange mode
*           other
*             'F'   Format NRECP records (unless 'M')
*             'C'   C I/O (unless 'M')
*                   LRECL (words) taken from IQUEST(10)
*             'O'   OLD format for Cycle information (default is NEW)
*
* Called by <USER>
*
*  Author  : R.Brun DD/US/PD
*  Written : 01.04.86
*  Last mod: 14.09.93 No longer force exchange mode for LINUX
*          : 09.03.94 S.Banerjee (Change in cycle structure)
*          : 30.01.95 J.Shiers. Permit nrecp>65000 for new format
*          : 10.12.97 P.Nevski  Default is NEW
*          : 01.03.99 Perevozchikov  default NRECP for NEW is 1M
************************************************************************
*
*KEEP,ZUNIT.
      COMMON /ZUNIT/ IQREAD,IQPRNT,IQPR2,IQLOG,IQPNCH,IQTTIN,IQTYPE
      COMMON /ZUNITZ/IQDLUN,IQFLUN,IQHLUN,  NQUSED
*KEEP,ZSTATE.
      COMMON /ZSTATE/QVERSN,NQPHAS,IQDBUG,NQDCUT,NQWCUT,NQERR
     +,              NQLOGD,NQLOGM,NQLOCK,NQDEVZ,NQOPTS(6)
*KEEP,RZCL.
      PARAMETER      (IQDROP=25, IQMARK=26, IQCRIT=27, IQSYSX=28)
      INTEGER      IQUEST
      COMMON/QUEST/IQUEST(100)
      COMMON /ZVFAUT/IQVID(2),IQVSTA,IQVLOG,IQVTHR(2),IQVREM(2,6)
      COMMON /ZEBQ/  IQFENC(4), LQ(100)
                              DIMENSION    IQ(92),        Q(92)
                              EQUIVALENCE (IQ(1),LQ(9)), (Q(1),IQ(1))
      COMMON /MZCA/  NQSTOR,NQOFFT(16),NQOFFS(16),NQALLO(16), NQIAM
     +,              LQATAB,LQASTO,LQBTIS, LQWKTB,NQWKTB,LQWKFZ
     +,              MQKEYS(3),NQINIT,NQTSYS,NQM99,NQPERM,NQFATA,NQCASE
     +,              NQTRAC,MQTRAC(48)
                                       EQUIVALENCE (KQSP,NQOFFS(1))
      COMMON /MZCB/  JQSTOR,KQT,KQS,  JQDIVI,JQDIVR
     +,              JQKIND,JQMODE,JQDIVN,JQSHAR,JQSHR1,JQSHR2,NQRESV
     +,              LQSTOR,NQFEND,NQSTRU,NQREF,NQLINK,NQMINR,LQ2END
     +,              JQDVLL,JQDVSY,NQLOGL,NQSNAM(6)
                                       DIMENSION    IQCUR(16)
                                       EQUIVALENCE (IQCUR(1),LQSTOR)
      COMMON /MZCC/  LQPSTO,NQPFEN,NQPSTR,NQPREF,NQPLK,NQPMIN,LQP2E
     +,              JQPDVL,JQPDVS,NQPLOG,NQPNAM(6)
     +,              LQSYSS(10), LQSYSR(10), IQTDUM(22)
     +,              LQSTA(21), LQEND(20), NQDMAX(20),IQMODE(20)
     +,              IQKIND(20),IQRCU(20), IQRTO(20), IQRNO(20)
     +,              NQDINI(20),NQDWIP(20),NQDGAU(20),NQDGAF(20)
     +,              NQDPSH(20),NQDRED(20),NQDSIZ(20)
     +,              IQDN1(20), IQDN2(20),      KQFT, LQFSTA(21)
                                       DIMENSION    IQTABV(16)
                                       EQUIVALENCE (IQTABV(1),LQPSTO)
C
      COMMON /RZCL/  LTOP,LRZ0,LCDIR,LRIN,LROUT,LFREE,LUSED,LPURG
     +,              LTEMP,LCORD,LFROM
                   EQUIVALENCE (LQRS,LQSYSS(7))
C
*KEEP,RZDIR.
      PARAMETER (NLPATM=100)
      COMMON /RZDIRN/NLCDIR,NLNDIR,NLPAT
      COMMON /RZDIRC/CHCDIR(NLPATM),CHNDIR(NLPATM),CHPAT(NLPATM)
      CHARACTER*16   CHNDIR,    CHCDIR,    CHPAT
C
*KEEP,RZCLUN.
      COMMON /RZCLUN/LUN,LREC,ISAVE,IMODEX,IRELAT,NHPWD,IHPWD(2)
     +,              IZRECL,IMODEC,IMODEH
C
*KEEP,RZK.
      PARAMETER (KUP=5,KPW1=7,KNCH=9,KDATEC=10,KDATEM=11,KQUOTA=12,
     +           KRUSED=13,KWUSED=14,KMEGA=15,KRZVER=16,KIRIN=17,
     +           KIROUT=18,KRLOUT=19,KIP1=20,KNFREE=22,KNSD=23,KLD=24,
     +           KLB=25,KLS=26,KLK=27,KLF=28,KLC=29,KLE=30,KNKEYS=31,
     +           KNWKEY=32,KKDES=33,KNSIZE=253,KEX=6,KNMAX=100)
C
*KEEP,RZCYCLE.
*
*     Pointers to cycle content
*
*     KLCYCL : length of cycle block (4,7)
*     KPPCYC : pointer to previous cycle
*     KFRCYC : first record number
*     KSRCYC : secord record number
*     KFLCYC : creation date/time and other stuff
*     KORCYC : offset in first record to data
*     KCNCYC : cycle number
*     KNWCYC : number of words in d/s
*     KKYCYC : key number to which this cycle belongs (only for version 1)
*     KVSCYC : version of RZ cycles structure (0, 1)
*
      INTEGER        KLCYCL, KPPCYC, KFRCYC, KSRCYC, KFLCYC, KORCYC,
     +               KCNCYC, KNWCYC, KKYCYC, KVSCYC
      COMMON/RZCYCLE/KLCYCL, KPPCYC, KFRCYC, KSRCYC, KFLCYC, KORCYC,
     +               KCNCYC, KNWCYC, KKYCYC, KVSCYC
*KEND.
* Nrecp: If non zero, overwrites default NHrecp in AGI version of RZMAKE.
*        Otherwise it is 32k in standard cernlib and 1M in AGI.
*        In principal it may be passed in IQUEST(10) by calling HROPEN
*        with Copt='Q', but HRFILE anyway resets it to 100<NQUOTA<65000.
      integer            NHrecp
      common /rzNHRECP/  NHrecp
      CHARACTER    CHOPT*(*),CHDIR*(*),CHFORM*(*)
      CHARACTER*16 CHTOP
      CHARACTER*(*)  CHTAG(*)
      DIMENSION    IOPTV(6),IHDIR(2)
      EQUIVALENCE (IOPTM,IOPTV(1)), (IOPTX,IOPTV(2))
     +,           (IOPTF,IOPTV(3)), (IOPTC,IOPTV(4))
     +,           (IOPTN,IOPTV(5)), (IOPTO,IOPTV(6))
      INTEGER      HMEM/0/
*
*-----------------------------------------------------------------------
*
*KEEP,Q$JBIT.
      JBIT(IZW,IZP)     = AND(ISHFTR(IZW,IZP-1),1)
      JBYT(IZW,IZP,NZB) = ISHFTR(LSHIFT(IZW,33-IZP-NZB),32-NZB)
*KEND.
      IQUEST(1)=0
      LOGLV = MIN(NQLOGD,4)
      LOGLV = MAX(LOGLV,-3)
*
      CALL UOPTC(CHOPT,'MXFCNO',IOPTV)


      IOPTN  = 1-IOPTO
      IMODEX = IOPTX
      IMODEC = IOPTC
      IRELAT = 0
      LUNP   = LUNIN
      IF(IOPTC.NE.0) LUNP = IQUEST(11)

*
**	         VP correction on NRECP default is 1M for new format
*
      NRECP = NRECPin
      if (ioptn.ne.0) then                  ! NEW RZ format
         M = 1 000 000
         if (NHRecp.gt.0) M=NHRecp*1024
         NRECP = max(NRECPin,M)
      endif
*
*                Check NWKEY and NRECP
*
      IF(NWKEY.LE.0.OR.NWKEY.GT.KNMAX)THEN
         IF(LOGLV.GE.-2) WRITE(IQLOG,9010)
 9010    FORMAT(' RZMAKE. NWKEY input value is invalid')
         IQUEST(1) =1
         IQUEST(11)=NWKEY
         GO TO 99
      ENDIF

      IF(NRECP.LT.2.OR.(NRECP.GT.65000.AND.IOPTN.EQ.0))THEN
         IF(LOGLV.GE.-2) WRITE(IQLOG,9011)
 9011    FORMAT(' RZMAKE. NRECP input value is invalid')
         IQUEST(1) =1
         IQUEST(11)=NRECP
         GO TO 99
      ENDIF
*
*     Find record length (as specified in the OPEN statement)
*
      IF(IOPTM.NE.0)THEN
*
*          A, Memory option. LUNIN itself IS the buffer and contains the
*                            value of LUNP as the block length (what a mess)
         LUNP  = HMEM+1
         LRECP = LUNIN
         IF(LRECP.LT.100.OR.LRECP.GT.10000)LRECP=1024

      ELSE
*
*          B, Standard option DISK. Use information as specified
*             in the Fortran OPEN statement
*
         IF (IOPTC.EQ.0) THEN
            INQUIRE(UNIT=LUNP,RECL=LRECB)
*
            LRECP=LRECB/4
         ELSE
*
*     Take LRECL from IQUEST(10) in case of C I/O option
*
            LRECP = IQUEST(10)
         ENDIF
      ENDIF
*
      LUN    = LUNP
      IZRECL = LRECP
      IF(LUN.LE.0.AND.IOPTM.EQ.0)THEN
         IF(LOGLV.GE.-2) WRITE(IQLOG,9012)
 9012    FORMAT(' RZMAKE. LUN input value is invalid')
         IQUEST(1) =1
         IQUEST(11)=LUN
         GO TO 99
      ENDIF
      IF(LRECP.LT.50)THEN
         IF(LOGLV.GE.-2) WRITE(IQLOG,9013)
 9013    FORMAT(' RZMAKE. LRECP input value less than 50')
         IQUEST(1) =1
         IQUEST(11)=LRECP
         GO TO 99
      ENDIF
*     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
*          Save existing material (if any)
*
      CALL RZSAVE
*
      IF(LOGLV.GE.0) WRITE(IQLOG,9014) LUNP,NRECP,LRECP,CHOPT
 9014 FORMAT(' RZMAKE. Unit ',I6,' Initializing Nrec=',i6,
     >' with LREC=',I6,', OPT= ',A)
      CALL MZSDIV (0,-7)
*
*           Check if LUN not already defined
*
      LRZ=LQRS
  10  IF(LRZ.NE.0)THEN
         IF(IQ(KQSP+LRZ-5).EQ.LUN)THEN
            IF(LOGLV.GE.-2) WRITE(IQLOG,9015)
 9015       FORMAT(' RZMAKE. Logical unit number already in use')
            IQUEST(1) =1
            IQUEST(11)=LUN
            GO TO 99
         ELSE
            LRZ=LQ(KQSP+LRZ)
            GO TO 10
         ENDIF
      ENDIF
*
*            First call to RZMAKE, create link area
*
      IF(LQRS.EQ.0)THEN
         CALL MZLINK(JQPDVS,'RZCL',LTOP,LTOP,LFROM)
         CALL MZBOOK(JQPDVS,LRZ0,LQRS,1,'RZ0 ',2,2,36,2,0)
         IQ(KQSP+LRZ0-5)=0
         ISAVE = 1
         NHPWD = 0
         CALL VBLANK(IHPWD,2)
      ENDIF
      NCHD  = LEN(CHDIR)
      IF(NCHD.GT.16)NCHD=16
      CHTOP = CHDIR(1:NCHD)
*
*            Create control bank
*
      IDTIME=0
      CALL RZDATE(IDTIME,IDATE,ITIME,2)
      KTAGS = KKDES+(NWKEY-1)/10+1
      NREC  = NRECP
      LREC  = LRECP
      NWREC = (NREC-1)/32 +1
      NW    = 50+NWREC
      NRD   = (NW-1)/LREC +1
      NWL   = NRD*LREC
      LD    = KTAGS+2*NWKEY
      LB    = LD+NRD+1
      LS    = LB+3+NWREC
      LK    = LS
      LF    = LS
*
      CALL MZBOOK (JQPDVS,LTOP,LQRS,1,'RZ  ',10,9,NWL,2,0)
*
*            Disk or memory
*
      IF(IOPTM.EQ.0)THEN
         IQ(KQSP+LTOP-5) = LUN
*
*            C I/O?
         IF(IOPTC.NE.0) CALL SBIT1(IQ(KQSP+LTOP),5)
      ELSE
         NMEM=IQ(KQSP+LRZ0)+1
         IQ(KQSP+LRZ0)=NMEM
         IQ(KQSP+LTOP-5)=-NMEM
         IF(2*NMEM.GT.IQ(KQSP+LRZ0-1))THEN
            CALL MZPUSH(JQPDVS,LRZ0,0,10,'I')
         ENDIF
         IQ(KQSP+LRZ0+2*NMEM-1)=LOCF(LUNIN)-LOCF(IQ(1))+1
         IQ(KQSP+LRZ0+2*NMEM  )=LRECP
         LUN=-NMEM
      ENDIF
*
*            Pre-format file
*
      IF((IOPTF.NE.0).AND.(IOPTM.EQ.0))THEN
         DO 100 I=2,NRECP
  100    CALL RZIODO(LUN,LREC,I,IQ(KQSP+LTOP+1),2)
         IF(IQUEST(1).NE.0)THEN
            IF(LOGLV.GE.-1) WRITE(IQLOG,1000) I-1
 1000       FORMAT(' RZMAKE. Could only pre-format',I6,' records')
            IQUEST(1)=0
         ENDIF
      ENDIF
*
*            Write empty record for locks
*
      CALL RZIODO(LUN,LREC,1,IQ(KQSP+LTOP+1),2)
      IF(IQUEST(1).NE.0) GO TO 99
*
*            Build top-directory parameters
*
      CALL SBIT1(IQ(KQSP+LTOP),2)
      CALL VBLANK(IQ(KQSP+LTOP+1),4)
      CALL UCTOH(CHDIR,IQ(KQSP+LTOP+1),4,NCHD)
      CALL ZHTOI(IQ(KQSP+LTOP+1),IQ(KQSP+LTOP+1),4)
*
      NHPWD = 0
      CALL VBLANK(IHPWD,2)
      CALL UCOPY(IHPWD,IQ(KQSP+LTOP+KPW1),2)
      IQ(KQSP+LTOP+KPW1+2) = NCHD
      IF(IMODEX.GT.0)THEN
         CALL SBIT1(IQ(KQSP+LTOP+KPW1+2),12)
      ENDIF
      IQ(KQSP+LTOP+KDATEC) = IDTIME
      IQ(KQSP+LTOP+KDATEM) = IDTIME
      IQ(KQSP+LTOP+KQUOTA) = NREC
      IQ(KQSP+LTOP+KRUSED) = NRD
      IQ(KQSP+LTOP+KWUSED) = NWL
      IF (IOPTN.NE.0) THEN
        WRITE(IQLOG,7001) CHDIR
 7001   FORMAT(' RZMAKE. new RZ format selected for ',a)
*     +        ' This file will not be readable with versions',
*     +            ' of RZ prior to release 94B')
        IQ(KQSP+LTOP+KRZVER) = 1
      ELSE
        WRITE(IQLOG,7007) CHDIR
 7007   FORMAT(' *****  RZMAKE. OLD RZ format selected for ',a)
*     +        ' This file will have the limit on the number of',
*     +           ' blocks < 64 K')
        IQ(KQSP+LTOP+KRZVER) = 0
      ENDIF
      IQ(KQSP+LTOP+KIP1)   = 2
      IQ(KQSP+LTOP+KNFREE) = NWL-LF
      IQ(KQSP+LTOP+KLD)    = LD
      IQ(KQSP+LTOP+KLB)    = LB
      IQ(KQSP+LTOP+KLS)    = LS
      IQ(KQSP+LTOP+KLK)    = LK
      IQ(KQSP+LTOP+KLF)    = LF
      IQ(KQSP+LTOP+KLC)    = NWL+1
      IQ(KQSP+LTOP+KLE)    = NWL
      IQ(KQSP+LTOP+KNWKEY) = NWKEY
      IQ(KQSP+LTOP+LD)     = NRD
      IQ(KQSP+LTOP+LB)     = NWREC
      IQ(KQSP+LTOP+LB+1)   = LREC
      IQ(KQSP+LTOP+LB+2)   = IDTIME
*
      NCHF=LEN(CHFORM)
      NCH =LEN(CHTAG(1))
      IF(NCH.GT.8)NCH=8
      DO 20 I=1,NWKEY
         IF(NCH.LT.8)CALL VBLANK(IHDIR,2)
         CALL UCTOH(CHTAG(I),IHDIR,4,NCH)
         CALL UCOPY(IHDIR,IQ(KQSP+LTOP+KTAGS+2*(I-1)),2)
         IFORM=2
         IF(I.LE.NCHF)THEN
            IF(CHFORM(I:I).EQ.'B')IFORM=1
            IF(CHFORM(I:I).EQ.'H')IFORM=3
            IF(CHFORM(I:I).EQ.'A')IFORM=4
         ENDIF
         IKDES=(I-1)/10
         IKBIT1=3*I-30*IKDES-2
         CALL SBYT(IFORM,IQ(KQSP+LTOP+KKDES+IKDES),IKBIT1,3)
  20  CONTINUE
      CALL ZHTOI(IQ(KQSP+LTOP+KTAGS),IQ(KQSP+LTOP+KTAGS),2*NWKEY)
      DO 30 I=1,NRD
         IQ(KQSP+LTOP+LD+I)=I+1
         CALL SBIT1(IQ(KQSP+LTOP+LB+3),I+1)
  30  CONTINUE
*
*            Store default LOG level
*
      LOGL = LOGLV + 3
      CALL SBYT(LOGL,IQ(KQSP+LTOP),15,3)
      CALL RZVCYC(LTOP)
*
*            Allocate free records
*
      CALL MZBOOK(JQPDVS,LFREE,LTOP,-2,'RZFR',0,0,3,2,0)
      IQ(KQSP+LFREE-5)=LUN
      IQ(KQSP+LFREE+1)=1
      IQ(KQSP+LFREE+2)=NRD+2
      IQ(KQSP+LFREE+3)=NREC
*
*            Allocate space for used records
*
      CALL MZBOOK(JQPDVS,LUSED,LTOP,-3,'RZUS',0,0,21,2,0)
*
      IQ(KQSP+LUSED-5)=LUN
      LRIN  = 0
      LPURG = 0
      LROUT = 0
      LCDIR = LTOP
      NLCDIR= 1
      NLNDIR= 1
      NLPAT = 1
      CHCDIR(1)=CHTOP
      CHNDIR(1)=CHTOP
      IQUEST(1)=0
*
  99  RETURN
C     prevent "never used" warning
99999 I=JBIT(1,2)+JBYT(1,2,3)
      END


