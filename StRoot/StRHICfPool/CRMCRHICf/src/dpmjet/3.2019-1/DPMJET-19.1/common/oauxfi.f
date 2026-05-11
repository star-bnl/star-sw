      
      SUBROUTINE OAUXFI ( FILE, IONUMB, CHSTTS, IERR )

*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1997-2013      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     Open AUXiliary FIle:                                             *
*                                                                      *
*     Created  on  30 January 1997  by   Alfredo Ferrari & Paola Sala  *
*                                              INFN - Milan            *
*                                                                      *
*     Last change  on   03-Feb-13   by     Alfredo Ferrari, INFN-Milan *
*                                                                      *
*          file   = file name                                          *
*          ionumb = logical unit number                                *
*          chstts = status word (optional, def. old)                   *
*          ierr   = error flag (output)                                *
*                                                                      *
*----------------------------------------------------------------------*
*

#ifdef FOR_FLUKA
      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
      INCLUDE '(COMPUT)'
#else
      INCLUDE 'DBLPRC'
      INCLUDE 'DIMPAR'
      INCLUDE 'IOUNIT'
      INCLUDE 'COMPUT'
#endif
      
*
      CHARACTER FILE*(*), CHSTTS*(*), CARD*132, FSTATS*10, FFORM*12,
     &          FACCSS*10
      LOGICAL LSCRAT, LFABRT
*
      LFABRT = IERR .NE. -1000000
      LSCRAT = .FALSE.
      IERR   = 0
*  +-------------------------------------------------------------------*
*  |  Status New:
      IF ( INDEX ( CHSTTS, 'NEW' ) .GT. 0 .OR.
     &     INDEX ( CHSTTS, 'new' ) .GT. 0 ) THEN
         FSTATS = 'NEW'
*  |
*  +-------------------------------------------------------------------*
*  |  Status Unknown:
      ELSE IF ( INDEX ( CHSTTS, 'UNKNOWN' ) .GT. 0 .OR.
     &          INDEX ( CHSTTS, 'unknown' ) .GT. 0 ) THEN
         FSTATS = 'UNKNOWN'
*  |
*  +-------------------------------------------------------------------*
*  |  Status Scratch:
      ELSE IF ( INDEX ( CHSTTS, 'SCRATCH' ) .GT. 0 .OR.
     &          INDEX ( CHSTTS, 'scratch' ) .GT. 0 ) THEN
         FSTATS = 'SCRATCH'
         LSCRAT = .TRUE.
*  |
*  +-------------------------------------------------------------------*
*  |  Status Old (default):
      ELSE
         FSTATS = 'OLD'
      END IF
*  |
*  +-------------------------------------------------------------------*
*  +-------------------------------------------------------------------*
*  |  Form Unformatted:
      IF ( INDEX ( CHSTTS, 'UNFORMATTED' ) .GT. 0 .OR.
     &     INDEX ( CHSTTS, 'unformatted' ) .GT. 0 ) THEN
         FFORM  = 'UNFORMATTED'
*  |
*  +-------------------------------------------------------------------*
*  |  Form Formatted:
      ELSE
         FFORM  = 'FORMATTED'
      END IF
*  |
*  +-------------------------------------------------------------------*
*  +-------------------------------------------------------------------*
*  |  Access direct:
      IF ( INDEX ( CHSTTS, 'DIRECT' ) .GT. 0 .OR.
     &     INDEX ( CHSTTS, 'direct' ) .GT. 0 ) THEN
         FACCSS = 'DIRECT'
*  |
*  +-------------------------------------------------------------------*
*  |  Access append:
      ELSE IF ( INDEX ( CHSTTS, 'APPEND' ) .GT. 0 .OR.
     &     INDEX ( CHSTTS, 'append' ) .GT. 0 ) THEN
         FACCSS = 'APPEND'
*  |
*  +-------------------------------------------------------------------*
*  |  Access sequential:
      ELSE
         FACCSS = 'SEQUENTIAL'
      END IF
*  |
*  +-------------------------------------------------------------------*
      IF ( .NOT. LSCRAT ) THEN
!          LQ   = MIN ( LNNBLN (FILE), 132 )
         CARD (1:LQ) = FILE (1:LQ)
      END IF
*  First of all: try to open the file in the current directory:
      IF ( LSCRAT ) THEN
         OPEN ( UNIT   = IONUMB,
     &          STATUS = FSTATS, FORM = FFORM , ACCESS = FACCSS,
     &          ERR    = 4000 )
      ELSE
         OPEN ( UNIT   = IONUMB, FILE = CARD (1:LQ),
     &          STATUS = FSTATS, FORM = FFORM , ACCESS = FACCSS,
     &          ERR    = 1000 )
      END IF
      RETURN
 1000 CONTINUE
*  Second attempt: try to open the file in the original work directory:
      OPEN ( UNIT   = IONUMB, FILE = PWDDIR (1:KPWDIR) // CARD (1:LQ),
     &       STATUS = FSTATS, FORM = FFORM , ACCESS = FACCSS,
     &       ERR    = 2000 )
      RETURN
 2000 CONTINUE
*  Third attempt: try to open the file in the FLUKA directory:
      OPEN ( UNIT   = IONUMB, FILE = HFLDIR (1:KFLDIR) // CARD (1:LQ),
     &       STATUS = FSTATS, FORM = FFORM , ACCESS = FACCSS,
     &       ERR    = 3000 )
      RETURN
 3000 CONTINUE
*  Last attempt: try to open the file in the user home directory:
      OPEN ( UNIT   = IONUMB, FILE = HOMDIR (1:KHMDIR) // CARD (1:LQ),
     &       STATUS = FSTATS, FORM = FFORM , ACCESS = FACCSS,
     &       ERR    = 4000 )
      RETURN
 4000 CONTINUE
*  +-------------------------------------------------------------------*
*  |  File opening was supposed to succeed (Lfabrt=.False. means the
*  |  file coud be or could be not exist, both acceptable)
      IF ( LFABRT ) THEN
         WRITE (LUNOUT,5000) CARD(1:LQ), IONUMB
 5000    FORMAT (' *** Impossible to open file ***',/,1X,A,/,
     &           ' *** on unit ',I4,' ***' )
! D        CALL FLABRT ( 'OAUXFI', 'IMPOSSIBLE TO OPEN FILE' )
      END IF
*  |
*  +-------------------------------------------------------------------*
      IERR = 1
      RETURN
*== End of subroutine Oauxfi ==========================================*
      END
