*
* $Id: agclink.inc,v 1.1 1998/04/10 15:30:45 fisyak Exp $
*
* $Log: agclink.inc,v $
* Revision 1.1  1998/04/10 15:30:45  fisyak
* Move gstar into CVS
*
*
*
* agclink.inc
*
*CMZ :  1.00/00 08/11/95  00.06.17  by  Pavel Nevski
*-- Author :    Pavel Nevski   12/08/95
*   links for dynamic banks access
       INTEGER    NALINKMAX,AG_NLINK,AG_LINK
       PARAMETER  (NALINKMAX=100)
       COMMON /AGCLINK/ AG_NLINK,AG_LINK(0:NALINKMAX)
*      - - - - - - - - - - - - - - - - - - - - - -
 
