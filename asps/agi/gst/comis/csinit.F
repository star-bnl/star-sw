*
* $Id: csinit.F,v 1.2 1998/04/16 17:02:19 fisyak Exp $
*
* $Log: csinit.F,v $
* Revision 1.2  1998/04/16 17:02:19  fisyak
* 2nd pass with gstar
*
*
* -----------------------------------------------------------------
#include "comis/pilot.h"
*CMZ :          10/11/97  12.13.09  by  Pavel Nevski
*CMZ :  1.30/00 22/04/97  15.56.15  by  Pavel Nevski
*-- Author : V.Berezhnoi
         SUBROUTINE CSINIT(NW)
***---------------------------
#include "comis/cslun.inc"
         COMMON/CSCONT/LCONT
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/comis.inc"
#include "comis/csbuf.inc"
#include "comis/csrec.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
#include "comis/cssysd.inc"
#include "comis/csdpvs.inc"
#include "comis/cskeys.inc"
#include "comis/cscbwl.inc"
#include "comis/csichv.inc"
#include "comis/cskucs.inc"
#if defined(CERNLIB_SHL)
#include "comis/cshlnm.inc"
#endif
#if defined(CERNLIB_PAW)
#include "comis/cshfill.inc"
#endif
      COMMON /MPLIST/ INDP
         EXTERNAL MDEXP, CSMBIT
         EXTERNAL CSOFIL,CSOLOG,CSOMAP,CSLIB
         EXTERNAL CGCONT,CSCOMX,CSEXTX,CSPTFS
         EXTERNAL CSNPAR,CSKPAR,CSIPAR,CSRPAR,CSLPAR,
     +            CSTPAR,CSCPAR,CSSPAR
         CHARACTER KWORDS*22
      DATA KWORDS/'ENDIFITHENELSEIFENDDOD'/
      COMMON /CSDEBUG/ ICSDEBUG
 
      INDP      = 0
      CALL MDINIT(LHP,MDEXP)
      CALL MHDEF (LHP-6,MDEXP)
      LAST      = MAX0(NW,LASTK)-2
      IBASE     = 0
      IPC       = 0
      ITA       = 0
      IDP       = 0
      IFORS     = 1
      NCBARR    = 0
      NTRACE    = 1
      NPARAM    = 1
      IBSEM     = MHLOC(LSSTK)
      ITS       = IBSEM
      ILSEM     = LSSTK+IBSEM
      LIMPL     = 17
      IBIMPL    = MHLOC(LIMPL)
      IQ(IBIMPL)= 0
      LBB       = 10
      NBB       = 0
      NBG       = 0
      IBB       = MHLOC(LBB)-1
      IPMCAD    = 0
      IPGB      = 0
      IPGP      = 0
      IPGI      = 0
      IPLI      = 0
      IPLL      = 0
      LDATA     = 0
      IEXTGB    = 0
      LCONT     = 0
      NUMST     = 0
      JTCH      = MJCHAR(IBUF(1))
      NCHS      = LCSTK*NBYTPW
      JID       = MJCHAR(IDEN(1))
      JTOPA     = LOCF(IA(1))-1
      JPMB      = 0
      JPMC      = 0
      JMB       = JTCH
      JMC       = JMB
      NMC       = 0
      NMB       = NCHS
      JFI1      = MJSCHA(KWORDS)
      JFI2      = JFI1+4
      JTHEN     = JFI2+2
      JELSE     = JTHEN+4
      JOD1      = JELSE+6
      JOD2      = JOD1+4
      JSR       = MJSCHA(REC)
      JSR1      = MJSCHA(REC1)
      JSMAIN    = MJSCHA(MAINPR)
      JB4       = MJCHAR(IB(4))
      PROMPT(1) = 'CS>'
      PROMPT(2) = 'FSD>'
      PROMPT(3) = 'MND>'
      PROMPT(4) = 'PAU>'
      PROMPT(5) = '???>'
      KEYRD     = 0
      LIBRD     = 0
      KEYC      = 0
      IRECU     = 1-LRECU
      LUNINP    = 5
      ISTPM     = 0
      ICBWL     = 0
      LISTCL    = 0
      ICHLA     = ICHAR('A')
      ICHLZ     = ICHAR('Z')
      ICHD0     = ICHAR('0')
      ICHD9     = ICHAR('9')
      ICHSUSC   = ICHAR('_')
      ICHSX     = ICHAR('#')
      ICHSE     = ICHAR('!')
      ICHBLN    = ICHAR(' ')
      CALL UCTOH('    ',KBLN,4,4)
      ICHQUO    = ICHAR('''')
      ICHSSC    = ICHAR(';')
      ICHSEQ    = ICHAR('=')
      ICHBRA    = ICHAR('(')
      ICHKET    = ICHAR(')')
      ICHMINU   = ICHAR('-')
      ICHPLUS   = ICHAR('+')
      ICHCOMM   = ICHAR(',')
#if defined(CERNLIB_PAW)
      CALL CSPAWI
      MODHFI    = 0
#endif
#if !defined(CERNLIB_PAW)
      JKUVBS    =-1
#endif
      CHPATH='/tmp/'
#if (defined(CERNLIB_HPUX))&&(defined(CERNLIB_SHL))
      CHF77 ='f77 -c +z +ppu -K -O'
      CHCC  ='cc -c +z -O'
#endif
#if (defined(CERNLIB_AIX))&&(defined(CERNLIB_SHL))
      CHF77 ='xlf -qextname -qrndsngl -qcharlen=32767 -c'
      CHCC  ='cc -c'
#endif
#if (defined(CERNLIB_SUN)||defined(CERNLIB_MSDOS))&&(defined(CERNLIB_SOLARIS))&&(defined(CERNLIB_SHL))
* increase some internal tables: up to 100 nested control statements,
* 200 continuation lines, 10000 identifier names per module
      CHF77 ='/opt/SUNWspro/bin/f77 -Nc100 -Nl200 -Nn10000 -c -pic'
      CHCC  ='/opt/SUNWspro/bin/cc -c -K pic'
#endif
#if (defined(CERNLIB_SUN))&&(!defined(CERNLIB_SOLARIS))&&(defined(CERNLIB_SHL))
      CHF77 ='f77 -c -pic'
      CHCC  ='cc -c -pic'
#endif
#if (defined(CERNLIB_SGI))&&(defined(CERNLIB_SHL))
      CHF77 ='f77 -c'
      CHCC  ='cc -cckr -c'
#endif
#if (defined(CERNLIB_ALPHA_OSF))&&(defined(CERNLIB_SHL))
      CHF77 ='f77 -c'
      CHCC  ='cc -c'
#endif
#if (defined(CERNLIB_LINUX))&&(defined(CERNLIB_SHL))
* pn, 29.10.97: not clear yet, may be '-pic' not needed
      CHF77 ='f77 -c -PIC'
      CHCC  ='cc -c -PIC'
#endif
      RETURN
      END
 
