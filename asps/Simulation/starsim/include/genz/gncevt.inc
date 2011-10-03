* $Id: gncevt.inc,v 1.1.1.1 2004/01/12 23:49:49 potekhin Exp $
* $Name:  $
      COMMON/GNCEVT/GNVERT(4),IGNWFL,GNWEIG,LGNBKR,LGNPRR,LGNWTR,
     +LGNUPR,LGNMAG,IGNGAT
      REAL GNVERT,GNWEIG
      LOGICAL LGNBKR,LGNPRR,LGNWTR,LGNUPR,LGNMAG
      INTEGER IGNWFL,IGNGAT
C
C GNVERT(4) -current default vertex: x,y,z,time (metres,seconds), relative
C            to coordinate origin and gate reference time.
C
C IGNWFL    -weight flag.
C GNWEIG    -weight of event. Meaning dependent on IGNWFL
C
C LGNBKR    -flag  .TRUE.= BooK Run is pending
C LGNPRR    -flag  .TRUE.= Print Run information is pending
C LGNWTR    -flag  .TRUE.= Write Run tree is pending
C LGNUPR    -flag  .TRUE.= UnPack Run information to HEPEVT is pending
C
C LGNMAG    -flag  .TRUE.= There is non-zero magnetic field.
C
C IGNGAT    -current internal gate number (used when necessary)
