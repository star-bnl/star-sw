*
* $Id: agcclus.inc,v 1.1 1998/04/10 15:30:43 fisyak Exp $
*
* $Log: agcclus.inc,v $
* Revision 1.1  1998/04/10 15:30:43  fisyak
* Move gstar into CVS
*
*
* -----------------------------------------------------------------
*
* agcclus.inc
*
*CMZ :  1.30/00 30/07/96  19.09.55  by  Pavel Nevski
*CMZ :  1.00/00 12/03/95  20.40.54  by  Pavel Nevski
*-- Author :    Pavel Nevski, Sasha Rozanov
*                                            clustering bank
      Integer          Ndig,Ktyp,Lh,Nclu,Kdig,Ltra,Ktypdig,Nodig
      Real                          Fdig,Zdig,Edig
      Parameter        (Lh=1024)
      Common /ARCdigt/ Ndig,Ktyp,Fdig(Lh),Zdig(Lh),Edig(Lh),
     +                           Nclu(Lh),Kdig(lh),Ltra(Lh)
      COMMON /AGCCLUS/ Ktypdig(Lh),NoDig(Lh)
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
