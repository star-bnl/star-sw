* $Id: gnccon.inc,v 1.1.1.1 2004/01/12 23:49:37 potekhin Exp $
* $Log: gnccon.inc,v $
* Revision 1.1.1.1  2004/01/12 23:49:37  potekhin
*
* Revision 1.2  2001/03/02 21:03:12  nevski
* agmer splitted, gnz restored
*
* Revision 1.1  2001/02/27 10:14:16  nevski
*  first working release
*
* genz standard
      INTEGER       IGNVER,IGNZBV,IGNDAT,IGNTIM,IGNMAC,IGNJOB,
     >              MMGENR,MMGENE,MMRUNT,MMEVNT,MMPASS,MMGENP,IOCHED
      REAL          GNCELE,GNFACM
      COMMON/GNCCON/IGNVER,IGNZBV,IGNDAT,IGNTIM,IGNJOB(4),IGNMAC,
     >              MMGENR,MMGENE,MMEVNT,MMRUNT,MMPASS,MMGENP,
     >              IOCHED(2),GNCELE,GNFACM
C
