*
* $Id: agcbuff.inc,v 1.1 1998/04/10 15:30:42 fisyak Exp $
*
* $Log: agcbuff.inc,v $
* Revision 1.1  1998/04/10 15:30:42  fisyak
* Move gstar into CVS
*
*
*
* agcbuff.inc
*
*CMZ :  1.00/00 06/02/95  14.55.02  by  Pavel Nevski
*-- Author :    Pavel Nevski   06/02/95
*     buffer for hit/digi maps
      Integer          Ibuf,Lbuf
      Parameter        (Lbuf=500 000)
      Common /AGCBUFF/ Ibuf(Lbuf)
*     - - - - - - - - - - - - - -
