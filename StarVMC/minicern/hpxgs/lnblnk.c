/*
 * $Id: lnblnk.c,v 1.2 2021/05/09 03:01:03 perev Exp $
 *
 * $Log: lnblnk.c,v $
 * Revision 1.2  2021/05/09 03:01:03  perev
 * ClearUp
 *
 * Revision 1.1.1.1  2005/05/25 20:36:46  fisyak
 *
 *
 * Revision 1.1.1.1  2002/07/24 15:56:28  rdm
 * initial import into CVS
 *
 * Revision 1.1.1.1  1999/05/18 15:55:28  fca
 * AliRoot sources
 *
 * Revision 1.1.1.1  1996/02/15 17:49:32  mclareni
 * Kernlib
 *
 */
/*>    ROUTINE LNBLNK
  CERN PROGLIB# M507    LNBLNK          .VERSION KERNHPX  1.02  920511
  ORIG. 30/04/92, RDM + JZ

  N = LNBLNK (CHLINE)   find last non-blank character in CHLINE
*/
      int lnblnk_(chline, len)
      char  *chline;
      int   len;
{
      char  *chcur;

      chcur = chline + len;
      while (chcur > chline)
        {  if (*--chcur != ' ')      goto exit; }
      return 0;

exit: return chcur+1 - chline;
}
/*> END <----------------------------------------------------------*/
