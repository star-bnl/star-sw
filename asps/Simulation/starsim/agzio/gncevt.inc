* $Id: gncevt.inc,v 1.1.1.1 2004/01/12 23:49:37 potekhin Exp $
* $Log: gncevt.inc,v $
* Revision 1.1.1.1  2004/01/12 23:49:37  potekhin
*
* Revision 1.1  2001/02/27 10:14:16  nevski
*  first working release
*
      REAL           GNVERT,GNWEIG
      INTEGER        IGNWFL,IGNGAT
      LOGICAL        LGNBKR,LGNPRR,LGNWTR,LGNUPR,LGNMAG
      COMMON/GNCEVT/ GNVERT(4),IGNWFL,GNWEIG,LGNBKR,LGNPRR,LGNWTR,
     +               LGNUPR,LGNMAG,IGNGAT
C
C GNVERT(4) -current default vertex: x,y,z,time (metres,seconds),
C            relative to coordinate origin and gate reference time.
C IGNWFL    -weight flag.
C GNWEIG    -weight of event. Meaning dependent on IGNWFL
C LGNBKR    -flag  .TRUE.= BooK Run is pending
C LGNPRR    -flag  .TRUE.= Print Run information is pending
C LGNWTR    -flag  .TRUE.= Write Run tree is pending
C LGNUPR    -flag  .TRUE.= UnPack Run information to HEPEVT is pending
C LGNMAG    -flag  .TRUE.= There is non-zero magnetic field.
C IGNGAT    -current internal gate number (used when necessary)

