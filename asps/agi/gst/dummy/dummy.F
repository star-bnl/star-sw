 
*CMZ :  1.30/00 29/03/97  18.01.53  by  Pavel Nevski
*-- Author :    Pavel Nevski   19/03/97
*      SUBROUTINE TRACEQC
*      WRITE(*,'('' Interrupt trace routine not available '')')
*      END

#ifdef CERNLIB_LINUX
*CMZ :          17/11/97  18.33.05  by  Pavel Nevski
*-- Author :    Pavel Nevski   17/11/97
      subroutine PMINIT
      print *,'  PMINIT: motif interface not linked '
      end
      subroutine KUINIM(p)
      character*(*) p
      print *,'  KUINIM: motif interface not linked '
      end
      subroutine GBROS
      call       ZBRDEF
      end
      subroutine KUWHAM(p)
      character*(*) p
      call       KUWHAG
      end
#endif 
