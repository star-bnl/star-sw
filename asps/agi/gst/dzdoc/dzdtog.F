$Id: dzdtog.F,v 1.1 2000/01/06 01:01:10 nevski Exp $
$Log: dzdtog.F,v $
Revision 1.1  2000/01/06 01:01:10  nevski
dzdoc for agi

      SUBROUTINE DZDTOG(IWKID)
#if defined(CERNLIB_HIGZ)
*      CALL IGSG(IWKID)
#endif
#if defined(CERNLIB_GKS)
      CALL GCATOG(IWKID)
#endif
      END
***********************************************************************
