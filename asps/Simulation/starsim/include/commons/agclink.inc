* $Id: agclink.inc,v 1.1.1.1 2004/01/12 23:49:49 potekhin Exp $
* $Name:  $
       INTEGER    NALINKMAX,AG_NLINK,AG_LINK
       PARAMETER  (NALINKMAX=100)
       COMMON /AGCLINK/ AG_NLINK,AG_LINK(0:NALINKMAX)
*      - - - - - - - - - - - - - - - - - - - - - -

