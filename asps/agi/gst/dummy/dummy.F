#if !(defined(CERNLIB_LINUX) || defined(CERNLIB_WINNT))
# ifndef MOTIF
#   define MOTIF
# endif
#endif

#ifndef MOTIF
*CMZ :          17/11/97  18.33.05  by  Pavel Nevski
*-- Author :    Pavel Nevski   17/11/97
      subroutine PMINIT
      print *,'  PMINIT: motif interface not linked '
      end
      subroutine KUINIM(p)
      character*(*) p
      print *,'  KUINIM: motif interface not linked '
      end
      subroutine GBROWS
*      call       ZBRDEF
      end
      subroutine KUWHAM(p)
      character*(*) p
      call       KUWHAG
      end
#endif
