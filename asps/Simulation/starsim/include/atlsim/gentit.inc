* $Id: gentit.inc,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
* $Log: gentit.inc,v $
* Revision 1.1.1.1  2004/01/12 23:49:41  potekhin
*
* Revision 1.1  2001/12/14 23:11:39  nevski
* upgrade atlsim library to atlsim main level
*
* Revision 1.1  2001/02/27 10:15:56  nevski
*  first working release
*
* description for a generic hit common
*     - - - - - - - - -
      Integer         Lnmax,Lnam0,Lnams,Nmh
      Parameter       (Lnmax=50,Lnam0=35,Lnams=Lnmax-Lnam0,Nmh=15)
      character*4     Gname(Lnmax)  /'id','trac','next','volume',
     >  'X','Y','Z','XX','YY','ZZ','CX','CY','CZ','PX','PY','PZ',
     >  'PTOT','R','RR','PHI','THE','ETA','TDR','TOF','SLEN','STEP',
     >  'ETOT','LGAM','LPTO','ELOS','USER','BIRK','DE','NONE','UNKN',
     >   Lnams*'    '/
*     aliases
      character*4     Tname(Lnmax) /'id','trac','next','volume',
     >  '_', '_', '_', 'x', '_', '_', '_', '_', '_', 'p', '_','_',
     >  '_', '_', '_', '_', '_', '_', '_','tof','s_tr','ds',
     >  'e', '_', '_','de','de','de','de','comp','res',
     >   Lnams*'    '/
*     - - - - - - - - -
      integer         Lnam,jX,jP,iX,iC,iPT,iLP,iET,iE,xD2M,pD2M,iflg,iadr
      common /gentit/ Lnam,jX,jP,iX,iC,iPT,iLP,iET,iE,xD2M,pD2M,
     >                iflg(Lnmax), iadr(Nmh)
      character*4     Cset,Cdet,Chit
      Real            FHmin,FHmax,FHbin,hits
      Integer         nvl,numbv
      common /gensit/ Cset,Cdet, nvl(Nmh),numbv(Nmh),hits(Nmh),
                      FHmin(Nmh),FHmax(Nmh),FHbin(Nmh),chit(Nmh)
*     - - - - - - - - -
*
