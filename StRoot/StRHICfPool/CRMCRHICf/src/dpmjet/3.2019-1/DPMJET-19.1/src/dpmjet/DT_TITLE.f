
      SUBROUTINE DT_TITLE
 
      IMPLICIT NONE
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      CHARACTER*6 cversi
      CHARACTER*11 cchang
      DATA cversi , cchang/'19.1.0' , '09 Jul 2019'/
 
      CALL DT_XTIME
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,99010) cversi , cchang
99010 FORMAT (1X,'+-------------------------------------------------',
     &        '----------------------+',/,1X,'|',71X,'|',/,1X,'|',23X,
     &        'DPMJET-III version ',A6,23X,'|',/,1X,'|',71X,'|',/,1X,
     &        '|',22X,'(Last change:  ',A11,')',22X,'|',/,1X,'|',71X,
     &        '|',/,1X,'|',12X,'Authors:',51X,'|',/,1X,'|',21X,
     &        'Stefan Roesler     (CERN)',25X,'|',/,1X,'|',21X,
     &        'Anatoli Fedynitch  (ICRR)',25X,'|',/,1X,'|',21X,
     &        'Ralph Engel        (KIT) ',25X,'|',/,1X,'|',21X,
     &        'Johannes Ranft     (Siegen Univ.)',17X,'|',/,1X,'|',71X,
     &        '|',/,1X,'|',8X,'https://github.com/afedynitch/dpmjet',27X,
     &        '|',/,1X,'|',71X,'|',/,1X,
     &        '+-------------------------------------------------',
     &        '----------------------+',/,1X,
     &        '| Contact: @github ',39X,/,1X,
     &        '+-------------------------------------------------',
     &        '----------------------+',/)
 
      END SUBROUTINE
