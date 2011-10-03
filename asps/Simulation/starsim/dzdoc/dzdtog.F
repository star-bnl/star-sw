* $Id: dzdtog.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
* $Log: dzdtog.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.1  2001/02/27 10:15:23  nevski
*  first working release
*
* Revision 1.2  2000/01/06 23:57:24  nevski
* cvs header record corrected
*
* Revision 1.1  2000/01/06 01:01:10  nevski
* dzdoc for agi
*
      SUBROUTINE DZDTOG(IWKID)
#if defined(CERNLIB_HIGZ)
*      CALL IGSG(IWKID)
#endif
#if defined(CERNLIB_GKS)
      CALL GCATOG(IWKID)
#endif
      END
***********************************************************************
