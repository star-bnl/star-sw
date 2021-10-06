* $Id: agcclus.inc,v 1.1.1.1 2004/01/12 23:49:49 potekhin Exp $
* $Name:  $
      Integer          Ndig,Ktyp,Lh,Nclu,Kdig,Ltra,Ktypdig,Nodig,Fenc
      Real                          Fdig,Zdig,Edig,Ldig,Tdig
      Parameter        (Lh=1024)
      Common /ARCdigt / Ndig,Ktyp,Fdig(Lh),Zdig(Lh),Edig(Lh),
     +                            Nclu(Lh),Kdig(lh),Ltra(Lh)
      Common /ARCdigtT/  Ldig(Lh),Tdig(Lh),Fenc(Lh)
      Common /AGCCLUS /  Ktypdig(Lh),NoDig(Lh)
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
