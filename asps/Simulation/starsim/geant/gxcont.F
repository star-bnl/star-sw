*
* $Id: gxcont.F,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
* $Log: gxcont.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:41  potekhin
*
* Revision 1.7  2003/11/08 17:47:28  nevski
* event offset=0
*
* Revision 1.5  2002/01/24 22:53:20  nevski
* maxtime control added
*
* Revision 1.4  2001/10/28 15:02:41  nevski
* support automatic run
*
* Revision 1.3  2001/06/20 00:18:32  nevski
* rzin instability hopefully gone
*
* Revision 1.2  2001/03/02 21:17:22  nevski
* commons changed as in atlas version
*
* Revision 1.1  2001/02/27 10:15:45  nevski
*  first working release
*
* Revision 1.5  2000/01/07 00:48:49  nevski
* unlimited track/vertex version
*
#include "geant321/pilot.h"
*CMZ :  1.30/00 16/04/97  20.12.19  by  Pavel Nevski
*CMZ :  3.21/02 07/07/94  19.47.38  by  S.Giani
*-- Author :
      SUBROUTINE GXCONT
**
**    ******************************************************************
**    *                                                                *
**    *      General control commands                                  *
**    *                                                                *
**    *       Authors:   R.Brun      **********                        *
**    *                  P.Zanarini  **********                        *
**    *                  S.Giani     **********                        *
**    *                                                                *
**    * Modifications:                                 PN, 06 July 96  *
**    *  - strong typing introduced to find undefined variables        *
**    *  - GCTIME common was missing                                   *
**    *  - SPART command corrected ( a la NA49 and Star): Ubuf,        *
**    *    branching ratios and decay modes are KUIP vectors!          *
**    *  - RNDM command with single parameter ISEQ (1-215) recals      *
**    *    standard precomputed sequences.                             *
**    *    If ISEQ=0, sequence is elected according to IGTIME.         *
**    *  - Time control introduced - upon the limit IQUEST(1) is set   *
**    *  - Multi- processor support in TRIG command (Nmax Ltok Itok)   *
**    *  - Read and Save text run header in user run bank              *
**    ******************************************************************
**
#include "commons/quest.inc"
#include "geant321/pawc.inc"
#include "geant321/gcbank.inc"
#include "geant321/gctmed.inc"
#include "geant321/gcunit.inc"
#include "geant321/gcflag.inc"
#include "geant321/gckine.inc"
#include "geant321/gcphys.inc"
#include "geant321/gcmulo.inc"
#include "geant321/gclist.inc"
#include "geant321/gcstak.inc"
#include "geant321/gcdraw.inc"
#include "geant321/gctime.inc"
#include "zebra/zstate.inc"
#include "slug/sclink.inc"
#include "atlsim/agctoken.inc"
*
      Integer      IGIWTY,IwkSty,i,N,Npar,Npk,Nch,Level,Numb,Imat,
     >             NumHit,Llow,Lhigh,Llo1,Lhig1,Llo2,Lhig2,NW,Len,
     >             Idm,Nlmat,Nmec,Kmec,ItMed,Is1,Is2,IA/0/,
     >             NST/0/,IDSTACK(1000)/1000*0/
      Real         A,Z,Dens,RadL,AbsL,ParVal,T,TL
      CHARACTER*4  CHMECA,CHDEB,CHSET,CHDET,CHMECS(20)
      CHARACTER*32 CHPATL,CHPAR,Cmate,Cpart,Cvec
      CHARACTER    CHNAME*20,crun*8/' '/	
      External     IGIWTY
      Logical      First_QNEXT/.true./,First/.true./
* the following is a part of the GCDRAW keeps, forgotten in the cernlib-95a
#if defined(CERNLIB_95A)
      Integer      MyISel
      real         zzfu,zzfv
#endif
      Real               TIMMAX
      COMMON /AGCTMAX/   TIMMAX
**
**    ------------------------------------------------------------------
**
      If (First) then
          First  = .false.
          CtTitle = '     '
      endif
*
      IWKSTY = IGIWTY(1)
      CALL KUPATL(CHPATL,NPAR)
*
      IF (CHPATL.EQ.'KINE') THEN
         CALL KUGETI(IKINE)
         NPK=NPAR-1
         DO 10 I=1,NPK
            CALL KUGETR(PKINE(I))
   10    CONTINUE
*
      ELSEIF (CHPATL.EQ.'SORD') THEN
         CALL KUGETI(ISTORD)
*
*     ELSEIF (CHPATL.EQ.'PATR') THEN
*        CALL KUGETI(NJTMAX)
*        CALL KUGETI(NJTMIN)
*        WRITE(CHMAIL,'('' *** GXCONT: Parallel tracking disabled, '',
*    +                  ''see documentation'')')
*        CALL GMAIL(1,1)
*
      ELSEIF (CHPATL.EQ.'RUNG') THEN
         Call KUGETC(Crun,len)
         Read (Crun,*,err=21) IDRUN
         Go to 22
  21     Call AgRUNNUM (Crun)
         Read (Crun,*,err=22) IDRUN
  22     Write(Lout,*) ' Run number set to ',IDRUN

*        allow short form:
         if (NPAR.gt.1) then
           CALL KUGETI(IDEVT)
         else
           IDEVT=0
         endif
         IF (JRUNG.GT.0) then
           IQ(JRUNG+1) = IDRUN
         ENDIF

*        CtTitle is used in QNEXT for job control :
         If (NPAR.GT.2) CALL KUGETE(CtTITLE,LEN)
*
      ELSEIF (CHPATL.EQ.'GTIME') THEN
         CALL KUGETR(TIMMAX)
         CALL KUGETR(TIMEND)
         CALL KUGETI(ITIME)
         if (IDEBUG.GT.0) then
            call TIMEL(TL)
            WRITE(CHMAIL,'('' *** GXCONT: Time left is '',F12.1)') TL
            CALL  GMAIL(0,0)
         endif
*
      ELSEIF (CHPATL.EQ.'TRACK') THEN
         IF(JXYZ.NE.0)CALL MZDROP(IXDIV,JXYZ,' ')
         IF(JHITS.NE.0)CALL MZDROP(IXDIV,JHITS,' ')
         CALL MZGARB(IXDIV,0)
         CALL GTREVE
*
      ELSEIF (CHPATL.EQ.'TRIGGER') THEN
         CALL KUGETI(N)
         Neven0=0
         Itoken=0
         if (Npar.gt.1) Call KUGETI(Neven0)
         if (Npar.gt.2) Call KUGETI(Itoken)
         NEVENT=IEVENT+N
         If (First_QNEXT) then
             First_QNEXT=.false.
             CALL QNEXTE
         else
             CALL QNEXT
         endif
*
      ELSEIF (CHPATL.EQ.'SWITCH') THEN
         CALL KUGETI(I)
         IF(I.GE.1.AND.I.LE.10) THEN
            IF(NPAR.LT.2)THEN
               WRITE(CHMAIL,'('' ISWIT('',I2,'')= '',I3)')I,ISWIT(I)
               CALL GMAIL(0,0)
            ENDIF
            CALL KUGETI(ISWIT(I))
         ENDIF
*
      ELSEIF (CHPATL.EQ.'PRINT') THEN
         CALL KUGETC(CHNAME,NCH)
         N=0
         CALL KUGETI(N)
         CALL GPRINT(CHNAME,N)
*
      ELSEIF (CHPATL.EQ.'DEBUG') THEN
         CALL KUGETC(CHDEB,NCH)
         NS=min(max(1,NS),1000-1)
         IDSTACK(NS+1)=IDEBUG

         IF (CHDEB.EQ.'RESTORE') THEN
            IDEBUG= IDSTACK(NS)
            NS = NS - 1
         ELSE IF (CHDEB.EQ.'ON') THEN
            IDEBUG=max(IDEBUG,1)
            NS    = NS+1
         ELSE
            IDEBUG= 0
            NS    = NS+1
         ENDIF
*
         If (IDEBUG.gt.0) then
            ITEST = 1
            ITIME = 1
            IDEMIN= 1
            IDEMAX= 1000000
         else
            ITEST = 0
            ITIME = 0
            IDEMIN= 0
            IDEMAX= 0
         endif
*
      ELSEIF (CHPATL.EQ.'OUTPUT_LP') THEN
         CALL KUGETI(LOUT)
*
      ELSEIF (CHPATL.EQ.'PHITS') THEN
         CALL KUGETC(CHSET,NCH)
         CALL KUGETC(CHDET,NCH)
         CALL KUGETI(NUMHIT)
         IPKHIT=NUMHIT
         CALL GPHITS(CHSET,CHDET)
         IPKHIT=0
*
      ELSEIF (CHPATL.EQ.'PDIGI') THEN
         CALL KUGETC(CHSET,NCH)
         CALL KUGETC(CHDET,NCH)
         CALL GPDIGI(CHSET,CHDET)
*
      ELSEIF (CHPATL.EQ.'MZLOGL') THEN
         IXSTOR=0
         LEVEL=0
         CALL KUGETI(LEVEL)
         CALL MZLOGL(IXSTOR,LEVEL)
*
      ELSEIF (CHPATL.EQ.'PMATE') THEN
         CALL KUGETI(NUMB)
         IF(IWKSTY.GE.1.AND.IWKSTY.LE.10) THEN
            CALL GPMATX(NUMB)
         ELSE
            CALL GPMATE(NUMB)
         ENDIF
*
      ELSEIF (CHPATL.EQ.'SMATE') THEN
         CALL KUGETI(IMAT)
         CALL KUGETC(Cmate,NCH)
         CALL KUGETR(A)
         CALL KUGETR(Z)
         CALL KUGETR(DENS)
         CALL KUGETR(RADL)
         CALL KUGETR(ABSL)
         CALL KUGETV(Cvec,LLOW,LHIGH)
         CALL KUGETI(NW)
         CALL GSMATE(IMAT,Cmate,A,Z,DENS,RADL,ABSL,QQ(LLOW),NW)
*
      ELSEIF (CHPATL.EQ.'SMIXT') THEN
         CALL KUGETI(IMAT)
         CALL KUGETC(Cmate,NCH)
         CALL KUGETV(Cvec,LLOW,LHIGH)
         CALL KUGETV(Cvec,LLO1,LHIG1)
         CALL KUGETR(DENS)
         CALL KUGETI(NLMAT)
         CALL KUGETV(Cvec,LLO2,LHIG2)
         CALL GSMIXT(IMAT,Cmate,QQ(LLOW),QQ(LLO1),DENS,NLMAT,QQ(LLO2))
*
      ELSEIF (CHPATL.EQ.'PRMAT') THEN
         CALL KUGETI(Imat)
         CALL KUGETI(IPART)
         CALL KUGETC(CHMECA,NCH)
         CALL GPRMAT(Imat,IPART,CHMECA,NEKBIN,ELOW)
*
      ELSEIF (CHPATL.EQ.'DRMAT') THEN
         CALL KUGETI(Imat)
         CALL KUGETI(IPART)
         IF(NPAR.GT.2) THEN
            NMEC = NPAR-2
            DO 11 KMEC=1,NMEC
               CALL KUGETC(CHMECS(KMEC),NCH)
  11        CONTINUE
         ELSE
            NMEC = 1
            CHMECS(1)='ALL'
         ENDIF
         CALL GDRMAT(Imat,IPART,CHMECS,NMEC)
*
      ELSEIF (CHPATL.EQ.'PLMAT') THEN
         CALL KUGETI(Imat)
         CALL KUGETI(IPART)
         CALL KUGETC(CHMECA,NCH)
         IDM=0
         CALL KUGETI(IDM)
         CALL GPLMAT(Imat,IPART,CHMECA,NEKBIN,ELOW,IDM)
*
      ELSEIF (CHPATL.EQ.'SPART') THEN
         CALL KUGETI(IPART)
         CALL KUGETC(Cpart,NCH)
         CALL KUGETI(ITRTYP)
         CALL KUGETR(AMASS)
         CALL KUGETR(CHARGE)
         CALL KUGETR(TLIFE)
         CALL KUGETV(Cvec,LLOW,LHIGH)
         CALL KUGETI(NW)
         CALL GSPART(IPART,Cpart,ITRTYP,AMASS,CHARGE,TLIFE,QQ(LLOW),NW)
         CALL KUGETV(CVEC,LLO1,LHIG1)
         CALL KUGETV(CVEC,LLO2,LHIG2)
         CALL GSDK  (IPART,QQ(LLO1),IQQ(LLO2))
*
      ELSEIF (CHPATL.EQ.'PPART') THEN
         CALL KUGETI(NUMB)
         IF(IWKSTY.GE.1.AND.IWKSTY.LE.10) THEN
            CALL GPPARX(NUMB)
         ELSE
            CALL GPPART(NUMB)
         ENDIF
*
      ELSEIF (CHPATL.EQ.'PRKINE') THEN
         CALL KUGETI(NUMB)
         IF(IWKSTY.GE.1.AND.IWKSTY.LE.10) THEN
            CALL GPKINX(NUMB)
         ELSE
            CALL GPKINE(NUMB)
         ENDIF
*
      ELSEIF (CHPATL.EQ.'STPAR') THEN
         CALL KUGETI(ITMED)
         CALL KUGETC(CHPAR,NCH)
         CALL KUGETR(PARVAL)
         CALL GSTPAR(ITMED,CHPAR,PARVAL)
*
      ELSEIF (CHPATL.EQ.'RNDM') THEN
         IF (NPAR.EQ.0) THEN
            CALL GRNDMQ(IS1,IS2,0,'G')
            WRITE(CHMAIL, 10000) IS1, IS2
10000       FORMAT('  RANDOM NUMBER = ',2I12)
            CALL GMAIL(0,0)
         ELSEIF (NPAR.EQ.1) THEN
            CALL KUGETI(IS1)
            If (IS1.LE.0) IS1=IGTIME
            IS2 = mod(IS1, 215 ) + 1
            CALL GRNDMQ(0, 0, IS2, ' ')
         ELSEIF (NPAR.EQ.2) THEN
            CALL KUGETI(NRNDM(1))
            CALL KUGETI(NRNDM(2))
            If (NRNDM(2).ge.0) THEN
               CALL GRNDMQ(NRNDM(1),NRNDM(2),0,'S')
            else
               IS1 = mod (NRNDM(1),215) + 1
               CALL GRNDMQ( 0, 0, IS1, ' ')
            endif
         ELSE
            WRITE(CHMAIL, 10100)
            CALL GMAIL(0,0)
10100       FORMAT(' Please give up to 2 seeds to initialize',
     +             ' the random number generator')
         ENDIF
         CALL GRNDMQ(NRNDM(1),NRNDM(2),0,'G')
      ENDIF
*
   30 END

