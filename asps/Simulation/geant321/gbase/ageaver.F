*
* $Id: ageaver.F,v 1.1.1.1 2004/01/15 00:12:03 potekhin Exp $
*
* $Log: ageaver.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:03  potekhin
*
* Revision 1.1  1998/02/10 13:04:43  atgeant3
* *** empty log message ***
*
*


      SUBROUTINE AGEAVER(CHTITL,IDATQQ,ITIMQQ,VERSQQ)
*                                                                      *
*----------------------------------------------------------------------*
*                                                                      *
* Name : AGEAVER                                                       *
*                                                                      *
* Description :                                                        *
*        Return code specific sequences for SLUG identification        *
*                                                                      *
* Arguments   :                                                        *
*        [name]  ([in | out | in/out])      [description]              *
*        CHTITL   OUT    CMZ Title                                     *
*        IDATQQ   OUT    Date of extraction for library compilation    *
*        ITIMQQ   OUT    Time of extraction for library compilation    *
*        VERSQQ   OUT    CMZ version number "vv.rr/ll"                 *
*                                                                      *
*----------------------------------------------------------------------*
*                                                                      *
* Author : Steve ONeale                       Date : 27/02/96          *
*                                                                      *
* Keywords :                                                           *
*        SLUG CMZ CODE MANAGEMENT GEANT DATE TIME STAMP VERSION TITLE  *
*----------------------------------------------------------------------*
*
       IMPLICIT NONE
C ----------- end CDE --------------------------------------------------
      CHARACTER*(*)   CHTITL,VERSQQ
      INTEGER         IDATQQ,ITIMQQ

*   Inserts   IDATQQ=yymmdd
      IDATQQ = CPP_DATE
*   Inserts   ITIMQQ=hhmm
      ITIMQQ = CPP_TIME
*   Inserts   VERSQQ='vv.rr/ll' and IVERSQ=vvrrll
      VERSQQ = CPP_VERS
      CHTITL=
* Continuation line with characters from the CMZ title
     + CPP_TITLE_CH
      END          ! AGEAVER
