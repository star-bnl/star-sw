* $Id: agmotif.F,v 1.1.1.1 2004/01/12 23:49:38 potekhin Exp $
* $Log: agmotif.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:38  potekhin
*
* Revision 1.1  2001/02/27 10:14:47  nevski
*  first working release
*
#if !defined(CERNLIB_MOTIF)
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
