*
* $Id: rzin.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
*
* $Log: rzin.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.1  2001/02/27 10:15:27  nevski
*  first working release
*
* Revision 1.1  2000/01/06 02:42:36  nevski
* elastic zebra updated
*
* Revision 1.2  1996/04/24 17:26:54  mclareni
* Extend the include file cleanup to dzebra, rz and tq, and also add
* dependencies in some cases.
*
* Revision 1.1.1.1  1996/03/06 10:47:24  mclareni
* Zebra
*
*
#include "zebra/pilot.h"
      SUBROUTINE RZIN(IXDIV,LSUP,JBIAS,KEYU,ICYCLE,CHOPT)
*
************************************************************************
*
*        To read one record and create the corresponding data structure
*           which will be pointed by (LSUP,JBIAS) in division IXDIV
* Input:
*   IXDIV   Index of the division to receive the data structure
*           IXDIV = 0 means division 2 of the primary store
*   *LSUP*
*   JBIAS   JBIAS < 1 : LSUP is the supporting bank and JBIAS is the link
*           bias specifying where the data structure has to be introduced
*           into this bank, i.e.  the data structure will be connected to
*           LQ(LSUP+JBIAS).
*           JBIAS =  1 :  LSUP  is the  supporting link,  i.e.   the data
*           structure is connected to LSUP (top level data structure)
*           JBIAS = 2 : Stand alone data structure, no connection.
*   KEYU    Keyword vector of the information to be read
*   ICYCLE  Cycle number of the key to be read
*           ICYCLE > highest cycle number means read the highest cycle
*           ICYCLE = 0 means read the lowest cycle
*   CHOPT   Character variable specifying the options selected.
*           data structure
*             default
*                   Same as 'D' below
*             'A'   Read continuation of the previously read data structure
*                   with identifier KEYU,ICYCLE
*                   Given that option implies that the record was written with
*                   the same option by a call to RZOUT.
*             'C'   Provide   information   about   the   cycle   numbers
*                   associated with KEY.
*                   The  total number  of  cycles  and the  cycle  number
*                   identifiers of the 19 highest  cycles are returned in
*                   IQUEST(50) and IQUEST(51..89) respectively
*             'D'   Read the  Data structure  with the  (key,cycle)  pair
*                   specified.
*             'N'   Read the neighbouring. keys (i.e. those preceding and
*                   following KEY).
*                   The  key-vectors of  the previous  and  next key  are
*                   available   respectively   as   IQUEST(31..35)    and
*                   IQUEST(41..45), see below.
*             'R'   Read data into existing bank at LSUP,JBIAS
*             'S'   KEYU(1) contains the key serial number
* Output:
*   *LSUP*  For JBIAS = 1  or 2,  LSUP contains the entry  address to the
*           data structure
*           In any case IQUEST(11) returns the entry address
*
* Called by <USER>
*
*  Author  : R.Brun DD/US/PD
*  Written : 12.04.86
*  Last mod: 20.12.90
*          : 12.07.94 Return bank address when the input address is 0
*                     and the data was written with RZVOUT.
*                     Return IQUEST(11) correctly.
*
* PN, 12.09.99 : check error flag after RZINS too ( bad input material )
*                if error, try once more
*
************************************************************************
#include "zebra/rzcl.inc"
#include "zebra/rzclun.inc"
#include "zebra/rzk.inc"
#include "zebra/zunit.inc"
#include "zebra/zstate.inc"
      CHARACTER*(*) CHOPT
      DIMENSION KEYU(*)
      DIMENSION LSUP(1),JBIAS(1),IQK(10),IQKS(10)
      EQUIVALENCE (IOPTA,IQUEST(91)), (IOPTC,IQUEST(92))
     +,      (IOPTD,IQUEST(93)), (IOPTN,IQUEST(94)), (IOPTR,IQUEST(95))
     +,      (IOPTS,IQUEST(96))
*     integer LTOPO/0/,IHDIR(4)
*
*-----------------------------------------------------------------------
*
#include "zebra/q_jbit.inc"
*
*           Make sure input buffer exists
*
*      IF (LTOP.ne.LTOPO) then
*         LTOPO=LTOP
*         CALL ZITOH(IQ(KQSP+LTOP+1),IHDIR,4)
*         write (*,1007) IQ(KQSP+LTOP-4),IQ(KQSP+LTOP-5),IHDIR
* 1007    format(' RZIN: input from ',A4,i12,2x,4A4)
*      ENDIF
*
      LRIN=LQ(KQSP+LTOP-7)
      IF(LRIN.EQ.0)THEN
         CALL MZBOOK(JQPDVS,LRIN,LTOP,-7,'RZIN',0,0,LREC+1,2,-1)
         IQ(KQSP+LRIN-5)=IQ(KQSP+LTOP-5)
         IQ(KQSP+LTOP+KIRIN)=0
      ENDIF
*
*          Find KEY,CYCLE
*
      CALL RZINK(KEYU,ICYCLE,CHOPT)
      IF(IQUEST(1).NE.0)GO TO 99
      IF(IOPTC.NE.0.AND.IOPTD.EQ.0)GO TO 99
      IDTIME=IQUEST(14)
      IDNW  =IQUEST(12)
      IF(IOPTS.NE.0)CALL UCOPY(IQUEST(20),IQKS,10)
      IF(IOPTN.NE.0)THEN
         IF(IOPTD.EQ.0)GO TO 99
         CALL UCOPY(IQUEST(41),IQK,10)
      ENDIF
*
      LBANK=0
      IF(LSUP(1).NE.0)THEN
         CALL MZSDIV(IXDIV,1)
         IF(JBIAS(1).LE.0)LBANK=LQ(KQS+LSUP(1)+JBIAS(1))
         IF(JBIAS(1).GT.0)LBANK=LSUP(1)
      ENDIF
*
      IFORM=JBYT(IQUEST(14),1,3)
      IF(IFORM.EQ.0)THEN
*
*           Read data structure into LBANK
*
         do itry=1,2
            CALL RZINS(IXDIV,LSUP,JBIAS,LBANK)
            IF(IQUEST(1).EQ.0) GO TO 10
            IQ(KQSP+LTOP+KIRIN)=0
            IQUEST(1)=0
         enddo
         IF (NQLOGD.GE.-1) WRITE(IQLOG,1001) IQUEST(11)
 1001    FORMAT(' RZINS input error',i6)
         GO TO 99
 10      CONTINUE
*
      ELSE
*
*          Case when record has been written with RZVOUT
*
         NDATA=IQUEST(12)
         IF(LBANK.NE.0)THEN
            IF(NDATA.LE.IQ(KQS+LBANK-1))THEN
               CALL RZREAD(IQ(KQS+LBANK+1),NDATA,1,IFORM)
               IQUEST(11) = LBANK
            ELSE
               IQUEST(1)=3
            ENDIF
         ELSE
            CALL MZBOOK(IXDIV,LFROM,LSUP,JBIAS,'RZIN',0,0,NDATA,
     +                  IFORM,-1)
            CALL RZREAD(IQ(KQS+LFROM+1),NDATA,1,IFORM)
            IQUEST(11) = LFROM
         ENDIF
      ENDIF
      IQUEST(14)=IDTIME
      IQUEST(12)=IDNW
      IF(IOPTN.NE.0)CALL UCOPY(IQK ,IQUEST(41),10)
      IF(IOPTS.NE.0)CALL UCOPY(IQKS,IQUEST(20),10)
*
  99  RETURN
C     prevent "never used" warning
99999 N=JBIT(1,2)+JBYT(1,2,3)
      END
