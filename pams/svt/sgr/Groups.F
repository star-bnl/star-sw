C
C These functions create pixel maps of the SVT (Zscaled versus Phi)
C and then forms groups of adjacent pixels.
C 
C
C===============================================================================
      INTEGER FUNCTION PIX_Reset( PIX_Map, PIX_info, NP, NZ, NN )
C
      IMPLICIT NONE
C
#include "sgr_pixmap.inc"
C
C PURPOSE OF FUNCTION:
C     Reset Pixel map for next event.
C
C INPUT  VARIABLES:
      INTEGER NP, NZ, NN
      INTEGER PIX_Map( NP, NZ, NN )
      RECORD/ sgr_pixmap_st / PIX_info
C
C OUTPUT  VARIABLES:
C     NONE
C
C
C MEANING OF RETURN ERROR CODES:
C  Err_Ok      : Normal
C  Err_Mild    : -
C  Err_Warning : -
C  Err_Fatal   : -
C
C ROUTINES THIS FUNCTION IS CALLED FROM:
C
C ROUTINES THIS FUNCTION CALLS:
C
C FUNCTION WRITTEN BY:
C     Duncan Prindle,    8-8-92
C
C INCLUDE FILES:
C
      INTEGER I, J, K
C
      DO K = 1,PIX_info.NHitsMax
         DO J = 1,PIX_info.NZ
            DO I = 1,PIX_info.NPhi
               PIX_Map(I,J,K) = 0
               END DO
            END DO
         END DO
      PIX_Reset = 1
      RETURN
      END
C
C===============================================================================
      INTEGER FUNCTION PIX_Add_Point( IH, Phi, ZS,
     +                                PIX_Map, PIX_info, NP, NZ, NN )
C
      IMPLICIT NONE
C
#include "sgr_pixmap.inc"
C
C PURPOSE OF FUNCTION:
C     Add SVT hit to pixel map.
C
C INPUT  VARIABLES:
      INTEGER IH    ! SVT hit index.
      REAL    Phi   ! Phi of SVT hit.
      REAL    ZS    ! Scaled Z of SVT hit.
      INTEGER NP, NZ, NN
      INTEGER PIX_Map( NP, NZ, NN )
      RECORD/ sgr_pixmap_st/ PIX_info
C
C OUTPUT  VARIABLES:
C     NONE
C
C
C MEANING OF RETURN ERROR CODES:
C  Err_Ok      : Normal.
C  Err_Mild    : Phi or ZS out of range.
C  Err_Warning : Too many hits in pixel.
C  Err_Fatal   : -
C
C ROUTINES THIS FUNCTION IS CALLED FROM:
C
C ROUTINES THIS FUNCTION CALLS:
C
C FUNCTION WRITTEN BY:
C     Duncan Prindle,    8-8-92
C
      INTEGER I, J, K
C
      I = 1 + PIX_info.NPhi *
     +         (Phi-PIX_info.PhiMin)/(PIX_info.PhiMax-PIX_info.PhiMin)
      IF ((I.LT.1) .OR. (I.GT.PIX_info.NPhi)) THEN
          PIX_Add_Point = 0
          RETURN
          END IF
      J = 1 + PIX_info.NZ *
     +         (ZS-PIX_info.ZMin)/(PIX_info.ZMax-PIX_info.ZMin)
      IF ((J.LT.1) .OR. (J.GT.PIX_info.NZ)) THEN
          PIX_Add_Point = 0
          RETURN
          END IF
      DO K = 1,PIX_info.NHitsMax
         IF (PIX_Map(I,J,K).EQ.0) THEN
             PIX_Map(I,J,K) = IH
             PIX_Add_Point = 1
             RETURN
             END IF
         END DO
      WRITE(6,*)' PIX_Map not deep enough. '
      PIX_Add_Point = -1
      RETURN
      END
C
C===============================================================================
      INTEGER FUNCTION PIX_Next_Pixel( IPhi, IZ,
     +                                 PIX_Map, PIX_info, NP, NZ, NN )
C
      IMPLICIT NONE
C
#include "sgr_pixmap.inc"
C
C PURPOSE OF FUNCTION:
C     Find next pixel with a hit. Star at min Z and Phi, scanning Phi first.
C
C INPUT  VARIABLES:
      INTEGER IPhi    ! Starting value of Phi index.
      INTEGER IZ      ! Starting value of Z index.
      INTEGER NP, NZ, NN
      INTEGER PIX_Map( NP, NZ, NN )
      RECORD/ sgr_pixmap_st/ PIX_info
C
C
C OUTPUT  VARIABLES:
C     INTEGER IPhi    ! Index pointing to next pixel with data.
C     INTEGER IZ      ! Index pointing to next pixel with data.
C     NONE
C
C
C MEANING OF RETURN ERROR CODES:
C  Err_Ok      : Normal.
C  Err_Mild    : No more hits.
C  Err_Warning : -
C  Err_Fatal   : -
C
C ROUTINES THIS FUNCTION IS CALLED FROM:
C
C ROUTINES THIS FUNCTION CALLS:
C
C FUNCTION WRITTEN BY:
C     Duncan Prindle,    8-8-92
C
C
    1 CONTINUE
         IPhi = IPhi + 1
         IF (IPhi.GT.PIX_info.NPhi) THEN
             IPhi = 1
             IZ   = IZ + 1
             IF (IZ.GT.PIX_info.NZ) THEN
                 PIX_Next_Pixel = 0
                 RETURN
                 END IF
             END IF
         IF (PIX_Map(IPhi,IZ,1).LE.0) GO TO 1
      PIX_Next_Pixel = 1
      RETURN
      END
C
C===============================================================================
      INTEGER FUNCTION PIX_Right_Pixel( IPhi, IZ,
     +                                  PIX_Map, PIX_info, NP, NZ, NN,
     +                                  There, IPhi2, IZ2 )
C
      IMPLICIT NONE
C
#include "sgr_pixmap.inc"
C
C PURPOSE OF FUNCTION:
C     Return pixel on +Phi side of (IPhi,IZ).
C
C INPUT  VARIABLES:
      INTEGER IPhi    ! Starting value of Phi index.
      INTEGER IZ      ! Starting value of Z index.
      INTEGER NP, NZ, NN
      INTEGER PIX_Map( NP, NZ, NN )
      RECORD/ sgr_pixmap_st/ PIX_info
C
C OUTPUT  VARIABLES:
      LOGICAL There ! True if pixel on +Phi side of (IPhi,IZ) is occupied.
      INTEGER IPhi2 ! Pointer to pixel on +Phi side of (IPhi,IZ).
      INTEGER IZ2   ! Pointer to pixel on +Phi side of (IPhi,IZ).
C
C
C MEANING OF RETURN ERROR CODES:
C  Err_Ok      : Normal.
C  Err_Mild    : No pixel on +Phi side of (IPhi,IZ).
C  Err_Warning : -
C  Err_Fatal   : -
C
C ROUTINES THIS FUNCTION IS CALLED FROM:
C
C ROUTINES THIS FUNCTION CALLS:
C
C FUNCTION WRITTEN BY:
C     Duncan Prindle,    8-8-92
C
      IPhi2 = IPhi
      IZ2   = IZ
      IF (IPhi2 .EQ. PIX_info.NPhi) THEN
          IF (PIX_info.PhiMin .NE. (PIX_info.PhiMax-360.)) THEN
              There = .FALSE.
              PIX_Right_Pixel = 0
              RETURN
              END IF
          IPhi2 = 1
        ELSE
          IPhi2 = IPhi2 + 1
        END IF
      There = PIX_Map(IPhi2,IZ2,1).GT.0
      PIX_Right_Pixel = 1
      RETURN
      END
C
C===============================================================================
      INTEGER FUNCTION PIX_Left_Pixel( IPhi, IZ,
     +                                 PIX_Map, PIX_info, NP, NZ, NN,
     +                                 There, IPhi2, IZ2 )
C
      IMPLICIT NONE
C
#include "sgr_pixmap.inc"
C
C PURPOSE OF FUNCTION:
C     Return pixel on -Phi side of (IPhi,IZ).
C
C INPUT  VARIABLES:
      INTEGER IPhi    ! Starting value of Phi index.
      INTEGER IZ      ! Starting value of Z index.
      INTEGER NP, NZ, NN
      INTEGER PIX_Map( NP, NZ, NN )
      RECORD/ sgr_pixmap_st/ PIX_info
C
C OUTPUT  VARIABLES:
      LOGICAL There ! True if pixel on -Phi side of (IPhi,IZ) is occupied.
      INTEGER IPhi2 ! Pointer to pixel on -Phi side of (IPhi,IZ).
      INTEGER IZ2   ! Pointer to pixel on -Phi side of (IPhi,IZ).
C
C
C MEANING OF RETURN ERROR CODES:
C  Err_Ok      : Normal.
C  Err_Mild    : No pixel on -Phi side of (IPhi,IZ).
C  Err_Warning : -
C  Err_Fatal   : -
C
C ROUTINES THIS FUNCTION IS CALLED FROM:
C
C ROUTINES THIS FUNCTION CALLS:
C
C FUNCTION WRITTEN BY:
C     Duncan Prindle,    8-8-92
C
      IPhi2 = IPhi
      IZ2   = IZ
      IF (IPhi2 .EQ. 1) THEN
          IF (PIX_info.PhiMin .NE. (PIX_info.PhiMax-360.)) THEN
              There = .FALSE.
              PIX_Left_Pixel = 0
              RETURN
              END IF
          IPhi2 = PIX_info.NPhi
        ELSE
          IPhi2 = IPhi2 - 1
        END IF
      There = PIX_Map(IPhi2,IZ2,1).GT.0
      PIX_Left_Pixel = 1
      RETURN
      END
C
C===============================================================================
      INTEGER FUNCTION PIX_Up_Pixel( IPhi, IZ,
     +                               PIX_Map, PIX_info, NP, NZ, NN,
     +                               There, IPhi2, IZ2 )
C
      IMPLICIT NONE
C
#include "sgr_pixmap.inc"
C
C PURPOSE OF FUNCTION:
C     Return pixel on +Z side of (IPhi,IZ).
C
C INPUT  VARIABLES:
      INTEGER IPhi    ! Starting value of Phi index.
      INTEGER IZ      ! Starting value of Z index.
      INTEGER NP, NZ, NN
      INTEGER PIX_Map( NP, NZ, NN )
      RECORD/ sgr_pixmap_st/ PIX_info
C
C OUTPUT  VARIABLES:
      LOGICAL There ! True if pixel on +Z side of (IPhi,IZ) is occupied.
      INTEGER IPhi2 ! Pointer to pixel on -Z side of (IPhi,IZ).
      INTEGER IZ2   ! Pointer to pixel on -Z side of (IPhi,IZ).
C
C
C MEANING OF RETURN ERROR CODES:
C  Err_Ok      : Normal.
C  Err_Mild    : No pixel on +Z side of (IPhi,IZ).
C  Err_Warning : -
C  Err_Fatal   : -
C
C ROUTINES THIS FUNCTION IS CALLED FROM:
C
C ROUTINES THIS FUNCTION CALLS:
C
C FUNCTION WRITTEN BY:
C     Duncan Prindle,    8-8-92
C
      IPhi2 = IPhi
      IZ2 = IZ
      IF (IZ.EQ.PIX_info.NZ) THEN
          There = .FALSE.
          PIX_Up_Pixel = 0
        ELSE
          IZ2 = IZ2 + 1
          There = PIX_Map(IPhi,IZ2,1).GT.0
          PIX_Up_Pixel = 1
        END IF
      RETURN
      END
C
C===============================================================================
      INTEGER FUNCTION PIX_Down_Pixel( IPhi, IZ,
     +                                 PIX_Map, PIX_info, NP, NZ, NN,
     +                                 There, IPhi2, IZ2 )
C
      IMPLICIT NONE
C
#include "sgr_pixmap.inc"
C
C PURPOSE OF FUNCTION:
C     Return pixel on -Z side of (IPhi,IZ).
C
C INPUT  VARIABLES:
      INTEGER IPhi    ! Starting value of Phi index.
      INTEGER IZ      ! Starting value of Z index.
      INTEGER NP, NZ, NN
      INTEGER PIX_Map( NP, NZ, NN )
      RECORD/ sgr_pixmap_st/ PIX_info
C
C OUTPUT  VARIABLES:
      LOGICAL There ! True if pixel on -Z side of (IPhi,IZ) is occupied.
      INTEGER IPhi2 ! Pointer to pixel on -Z side of (IPhi,IZ).
      INTEGER IZ2   ! Pointer to pixel on -Z side of (IPhi,IZ).
C
C
C MEANING OF RETURN ERROR CODES:
C  Err_Ok      : Normal.
C  Err_Mild    : No pixel on -Z side of (IPhi,IZ).
C  Err_Warning : -
C  Err_Fatal   : -
C
C ROUTINES THIS FUNCTION IS CALLED FROM:
C
C ROUTINES THIS FUNCTION CALLS:
C
C FUNCTION WRITTEN BY:
C     Duncan Prindle,    8-8-92
C
      IPhi2 = IPhi
      IZ2   = IZ
      IF (IZ2.EQ.1) THEN
          There = .FALSE.
          PIX_Down_Pixel = 0
        ELSE
          IZ2 = IZ2 - 1
          There = PIX_map(IPhi2,IZ2,1).GT.0
          PIX_Down_Pixel = 1
        END IF
      RETURN
      END
C
C===============================================================================
      INTEGER FUNCTION PIX_UpRight_Pixel( IPhi, IZ,
     +                                    PIX_Map, PIX_info, NP, NZ, NN,
     +                                    There, IPhi2, IZ2 )
C
      IMPLICIT NONE
C
#include "sgr_pixmap.inc"
C
C PURPOSE OF FUNCTION:
C     Return pixel on +Phi,+Z side of (IPhi,IZ).
C
C INPUT  VARIABLES:
      INTEGER IPhi    ! Starting value of Phi index.
      INTEGER IZ      ! Starting value of Z index.
      INTEGER NP, NZ, NN
      INTEGER PIX_Map( NP, NZ, NN )
      RECORD/ sgr_pixmap_st/ PIX_info
C
C OUTPUT  VARIABLES:
      LOGICAL There ! True if pixel on +Phi,+Z side of (IPhi,IZ) is occupied.
      INTEGER IPhi2 ! Pointer to pixel on +Phi,+Z side of (IPhi,IZ).
      INTEGER IZ2   ! Pointer to pixel on +Phi,+Z side of (IPhi,IZ).
C
C
C MEANING OF RETURN ERROR CODES:
C  Err_Ok      : Normal.
C  Err_Mild    : No pixel on +Phi,+Z side of (IPhi,IZ).
C  Err_Warning : -
C  Err_Fatal   : -
C
C ROUTINES THIS FUNCTION IS CALLED FROM:
C
C ROUTINES THIS FUNCTION CALLS:
C
C FUNCTION WRITTEN BY:
C     Duncan Prindle,    8-8-92
C
      IPhi2 = IPhi
      IZ2   = IZ
      IF (IPhi2 .EQ. PIX_info.NPhi) THEN
          IF (PIX_info.PhiMin .NE. (PIX_info.PhiMax-360.)) THEN
              There = .FALSE.
              PIX_UpRight_Pixel = 0
              RETURN
              END IF
          IPhi2 = 1
        ELSE
          IPhi2 = IPhi2 + 1
        END IF
      IF (IZ2.EQ.PIX_info.NZ) THEN
          There = .FALSE.
          PIX_UpRight_Pixel = 0
        ELSE
          IZ2 = IZ2 + 1
          There = PIX_map(IPhi2,IZ2,1).GT.0
          PIX_UpRight_Pixel = 1
        END IF
      RETURN
      END
C
C===============================================================================
      INTEGER FUNCTION PIX_DownRight_Pixel( IPhi, IZ,
     +                                      PIX_Map,PIX_info,NP,NZ,NN,
     +                                      There, IPhi2, IZ2 )
C
      IMPLICIT NONE
C
#include "sgr_pixmap.inc"
C
C PURPOSE OF FUNCTION:
C     Return pixel on +Phi,-Z side of (IPhi,IZ).
C
C INPUT  VARIABLES:
      INTEGER IPhi    ! Starting value of Phi index.
      INTEGER IZ      ! Starting value of Z index.
      INTEGER NP, NZ, NN
      INTEGER PIX_Map( NP, NZ, NN )
      RECORD/ sgr_pixmap_st/ PIX_info
C
C OUTPUT  VARIABLES:
      LOGICAL There ! True if pixel on +Phi,-Z side of (IPhi,IZ) is occupied.
      INTEGER IPhi2 ! Pointer to pixel on +Phi,-Z side of (IPhi,IZ).
      INTEGER IZ2   ! Pointer to pixel on +Phi,-Z side of (IPhi,IZ).
C
C
C MEANING OF RETURN ERROR CODES:
C  Err_Ok      : Normal.
C  Err_Mild    : No pixel on +Phi,-Z side of (IPhi,IZ).
C  Err_Warning : -
C  Err_Fatal   : -
C
C ROUTINES THIS FUNCTION IS CALLED FROM:
C
C ROUTINES THIS FUNCTION CALLS:
C
C FUNCTION WRITTEN BY:
C     Duncan Prindle,    8-8-92
C
      IPhi2 = IPhi
      IZ2   = IZ
      IF (IPhi2 .EQ. PIX_info.NPhi) THEN
          IF (PIX_info.PhiMin .NE. (PIX_info.PhiMax-360.)) THEN
              There = .FALSE.
              PIX_DownRight_Pixel = 0
              RETURN
              END IF
          IPhi2 = 1
        ELSE
          IPhi2 = IPhi2 + 1
        END IF
      IF (IZ2.EQ.1) THEN
          There = .FALSE.
          PIX_DownRight_Pixel = 0
        ELSE
          IZ2 = IZ2 - 1
          There = PIX_map(IPhi2,IZ2,1).GT.0
          PIX_DownRight_Pixel = 1
        END IF
      RETURN
      END
C
C===============================================================================
      INTEGER FUNCTION PIX_UpLeft_Pixel( IPhi, IZ,
     +                                   PIX_Map, PIX_info, NP, NZ, NN,
     +                                   There, IPhi2, IZ2 )
C
      IMPLICIT NONE
C
#include "sgr_pixmap.inc"
C
C PURPOSE OF FUNCTION:
C     Return pixel on -Phi,+Z side of (IPhi,IZ).
C
C INPUT  VARIABLES:
      INTEGER IPhi    ! Starting value of Phi index.
      INTEGER IZ      ! Starting value of Z index.
      INTEGER NP, NZ, NN
      INTEGER PIX_Map( NP, NZ, NN )
      RECORD/ sgr_pixmap_st/ PIX_info
C
C OUTPUT  VARIABLES:
      LOGICAL There ! True if pixel on -Phi,+Z side of (IPhi,IZ) is occupied.
      INTEGER IPhi2 ! Pointer to pixel on -Phi,+Z side of (IPhi,IZ).
      INTEGER IZ2   ! Pointer to pixel on -Phi,+Z side of (IPhi,IZ).
C
C
C MEANING OF RETURN ERROR CODES:
C  Err_Ok      : Normal.
C  Err_Mild    : No pixel on -Phi,+Z side of (IPhi,IZ).
C  Err_Warning : -
C  Err_Fatal   : -
C
C ROUTINES THIS FUNCTION IS CALLED FROM:
C
C ROUTINES THIS FUNCTION CALLS:
C
C FUNCTION WRITTEN BY:
C     Duncan Prindle,    8-8-92
C
      IPhi2 = IPhi
      IZ2   = IZ
      IF (IPhi2 .EQ. 1) THEN
          IF (PIX_info.PhiMin .NE. (PIX_info.PhiMax-360.)) THEN
              There = .FALSE.
              PIX_UpLeft_Pixel = 0
              RETURN
              END IF
          IPhi2 = PIX_info.NPhi
        ELSE
          IPhi2 = IPhi2 - 1
        END IF
      IF (IZ2.EQ.PIX_info.NZ) THEN
          There = .FALSE.
          PIX_UpLeft_Pixel = 0
        ELSE
          IZ2 = IZ2 + 1
          There = PIX_map(IPhi2,IZ2,1).GT.0
          PIX_UpLeft_Pixel = 1
        END IF
      RETURN
      END
C
C===============================================================================
      INTEGER FUNCTION PIX_DownLeft_Pixel( IPhi, IZ,
     +                                     PIX_Map, PIX_info,NP,NZ,NN,
     +                                     There, IPhi2, IZ2 )
C
      IMPLICIT NONE
C
#include "sgr_pixmap.inc"
C
C PURPOSE OF FUNCTION:
C     Return pixel on -Phi,-Z side of (IPhi,IZ).
C
C INPUT  VARIABLES:
      INTEGER IPhi    ! Starting value of Phi index.
      INTEGER IZ      ! Starting value of Z index.
      INTEGER NP, NZ, NN
      INTEGER PIX_Map( NP, NZ, NN )
      RECORD/ sgr_pixmap_st/ PIX_info
C
C OUTPUT  VARIABLES:
      LOGICAL There ! True if pixel on -Phi,-Z side of (IPhi,IZ) is occupied.
      INTEGER IPhi2 ! Pointer to pixel on -Phi,-Z side of (IPhi,IZ).
      INTEGER IZ2   ! Pointer to pixel on -Phi,-Z side of (IPhi,IZ).
C
C
C MEANING OF RETURN ERROR CODES:
C  Err_Ok      : Normal.
C  Err_Mild    : No pixel on -Phi,-Z side of (IPhi,IZ).
C  Err_Warning : -
C  Err_Fatal   : -
C
C ROUTINES THIS FUNCTION IS CALLED FROM:
C
C ROUTINES THIS FUNCTION CALLS:
C
C FUNCTION WRITTEN BY:
C     Duncan Prindle,    8-8-92
C
      IPhi2 = IPhi
      IZ2   = IZ
      IF (IPhi2 .EQ. 1) THEN
          IF (PIX_info.PhiMin .NE. (PIX_info.PhiMax-360.)) THEN
              There = .FALSE.
              PIX_DownLeft_Pixel = 0
              RETURN
              END IF
          IPhi2 = PIX_info.NPhi
        ELSE
          IPhi2 = IPhi2 - 1
        END IF
      IF (IZ2.EQ.1) THEN
          There = .FALSE.
          PIX_DownLeft_Pixel = 0
        ELSE
          IZ2 = IZ2 - 1
          There = PIX_map(IPhi2,IZ2,1).GT.0
          PIX_DownLeft_Pixel = 1
        END IF
      RETURN
      END
C
C===============================================================================
      INTEGER FUNCTION PIX_Hit_List( IPhi,IZ,
     +                               PIX_Map, PIX_info, NP, NZ, NN,
     +                               NHits, LHits )
C
      IMPLICIT NONE
C
#include "sgr_pixmap.inc"
C
C PURPOSE OF FUNCTION:
C     Return list of hits in pixel LP
C
C INPUT  VARIABLES:
      INTEGER IPhi    ! Phi index.
      INTEGER IZ      ! Z index.
      INTEGER NP, NZ, NN
      INTEGER PIX_Map( NP, NZ, NN )
      RECORD/ sgr_pixmap_st/ PIX_info
C
C
C OUTPUT  VARIABLES:
      INTEGER NHits                     ! Number of hits in (IPhi,IZ)
      INTEGER LHits(NN)                 ! List of hits in (IPhi,IZ)
C
C
C MEANING OF RETURN ERROR CODES:
C  Err_Ok      : Normal.
C  Err_Mild    : No hit in LP. (This includes if hits are marked as used.)
C  Err_Warning : -
C  Err_Fatal   : -
C
C ROUTINES THIS FUNCTION IS CALLED FROM:
C
C ROUTINES THIS FUNCTION CALLS:
C
C FUNCTION WRITTEN BY:
C     Duncan Prindle,    8-8-92
C
C
      INTEGER K
C
      K     = 1
      NHits = 0
    1 CONTINUE
         IF (PIX_Map(IPhi,IZ,K).GT.0) THEN
             NHits = NHits + 1
             LHits(NHits) = PIX_Map(IPhi,IZ,K)
             K = K + 1
             IF (K.LE.PIX_info.NHitsMax) THEN
                 GO TO 1
               ELSE
                 PIX_Hit_List = 0
                 RETURN
               END IF
             END IF
      IF (NHits.EQ.0) THEN
          PIX_Hit_List = 0
        ELSE
          PIX_Hit_List = 1
        END IF
      RETURN
      END
C
C===============================================================================
      INTEGER FUNCTION PIX_Mark( IPhi,IZ,
     +                           PIX_Map, PIX_info, NP, NZ, NN )
C
      IMPLICIT NONE
C
#include "sgr_pixmap.inc"
C
C PURPOSE OF FUNCTION:
C     Mark pixel as used.
C
C INPUT  VARIABLES:
      INTEGER IPhi    ! Phi index.
      INTEGER IZ      ! Z index.
      INTEGER NP, NZ, NN
      INTEGER PIX_Map( NP, NZ, NN )
      RECORD/ sgr_pixmap_st/ PIX_info
C
C
C MEANING OF RETURN ERROR CODES:
C  Err_Ok      : Normal.
C  Err_Mild    : No hits in LP
C  Err_Warning : -
C  Err_Fatal   : -
C
C ROUTINES THIS FUNCTION IS CALLED FROM:
C
C ROUTINES THIS FUNCTION CALLS:
C
C FUNCTION WRITTEN BY:
C     Duncan Prindle,    8-8-92
C
C
      INTEGER K
C
      IF (PIX_Map(IPhi,IZ,1).EQ.0) THEN
          PIX_Mark = 0
          RETURN
          END IF
      K = 1
    1 CONTINUE
         PIX_Map(IPhi,IZ,K) = - ABS(PIX_Map(IPhi,IZ,K))
         K = K + 1
         IF (K.LE.PIX_info.NHitsMax) THEN
             IF (PIX_Map(IPhi,IZ,K).NE.0)THEN
                 GO TO 1
                 END IF
             END IF
      PIX_Mark = 1
      RETURN
      END
C
C
C These functions use pixel maps of the SVT (Zscaled versus Phi)
C to form groups of adjacent pixels.
C 
C
C===============================================================================
      INTEGER FUNCTION GRP_Reset( Group, n_group )
C
      IMPLICIT NONE
#include "sgr_groups.inc"
C

C
C PURPOSE OF FUNCTION:
C     Reset Group to empty.
C
C INPUT  VARIABLES:
      INTEGER n_group
      RECORD/ sgr_groups_st / Group(n_group)

C
C OUTPUT  VARIABLES:
C     NONE
C
C
C MEANING OF RETURN ERROR CODES:
C  Err_Ok      : Normal
C  Err_Mild    : -
C  Err_Warning : -
C  Err_Fatal   : -
C
C ROUTINES THIS FUNCTION IS CALLED FROM:
C
C ROUTINES THIS FUNCTION CALLS:
C
C FUNCTION WRITTEN BY:
C     Duncan Prindle,    8-8-92
C
C
      INTEGER I
C
      DO I = 1,n_group
         Group(I).id1 = 0
         Group(I).id2 = 0
         Group(I).ident = 0
         END DO
      GRP_Reset = 1
      RETURN
      END
C
C
C===============================================================================
      INTEGER FUNCTION GRP_Add_Pixel( IPhi, IZ,
     +                                PIX_Map, PIX_info, NP, NZ, NN,
     +                                i_group, Group, l_group, n_group )
C
      IMPLICIT NONE
C
#include "sgr_groups.inc"
#include "sgr_pixmap.inc"

C
C PURPOSE OF FUNCTION:
C     Add all hits in (IPhi,IZ) to Group.
C
C INPUT  VARIABLES:
      INTEGER IPhi    ! Phi index.
      INTEGER IZ      ! Z index.
      INTEGER NP, NZ, NN
      INTEGER PIX_Map( NP, NZ, NN )
      RECORD/ sgr_pixmap_st / PIX_info
      INTEGER i_group ! Current Group number.
      INTEGER l_group ! last entry used in Group
      INTEGER n_group ! total length of Group
      RECORD/ sgr_groups_st/ Group(n_group)
C
C OUTPUT  VARIABLES:
C     NONE
C
C
C MEANING OF RETURN ERROR CODES:
C  Err_Ok      : Normal.
C  Err_Mild    : -
C  Err_Warning : Too many hits in Group.
C  Err_Fatal   : -
C
C ROUTINES THIS FUNCTION IS CALLED FROM:
C
C ROUTINES THIS FUNCTION CALLS:
      INTEGER PIX_Hit_List
      INTEGER PIX_Mark
C
C FUNCTION WRITTEN BY:
C     Duncan Prindle,    8-8-92
C
      INTEGER Ok, NHits, LHits(100), I, J
C
      Ok = PIX_Hit_List( IPhi, IZ, PIX_Map,
     +                   PIX_info, PIX_info.NPhi, PIX_info.NZ,
     +                   PIX_info.NHitsMax, NHits, LHits )
      Ok = PIX_Mark( IPhi, IZ, PIX_Map,
     +               PIX_info, PIX_info.NPhi, PIX_info.NZ,
     +               PIX_info.NHitsMax )
      DO I = 1,NHits
         J = l_group + I
         IF (l_group+I.GT.n_group) THEN
             GRP_Add_Pixel = -1
             RETURN
             END IF
         l_group = l_group + 1
*         Group(l_group).id1     = i_group
         Group(l_group).id2 = LHits(I)
*         Group(l_group).Layer  = 0    ! Don't have access to layer information.
         END DO
      GRP_Add_Pixel = 1
      RETURN
      END
C
C===============================================================================
      INTEGER FUNCTION GRP_Check_Neighbors( IPhi, IZ,
     +                                      PIX_Map, PIX_info,NP,NZ,NN,
     +                                   i_group,Group,l_group,n_group)
C
      IMPLICIT NONE
C
#include "sgr_pixmap.inc"
#include "sgr_groups.inc"
C
C PURPOSE OF FUNCTION:
C     Check pixel neighbors of LP. Add them to group if there.
C     For each pixel found we check pixels around them.
C     In principle this routine is recursive.
C     It will be easier to read when it is explicitly recursive.
C
C INPUT  VARIABLES:
      INTEGER IPhi    ! Phi index.
      INTEGER IZ      ! Z index.
      INTEGER NP, NZ, NN
      INTEGER PIX_Map( NP, NZ, NN )
      RECORD/ sgr_pixmap_st / PIX_info
      INTEGER i_group ! Current group Number.
      INTEGER l_group ! last entry used in Group
      INTEGER n_group ! total length of Group
      RECORD/ sgr_groups_st / Group(n_group)
C
C PURPOSE OF FUNCTION:
C     Check pixel neighbors of LP. Add them to group if there.
C     For each pixel found we check pixels around them.
C     In principle this routine is recursive.
C     It will be easier to read when it is explicitly recursive.
C
C
C OUTPUT  VARIABLES:
C     l_group
C     Group
C
C
C MEANING OF RETURN ERROR CODES:
C  Err_Ok      : Normal.
C  Err_Mild    : -
C  Err_Warning : -
C  Err_Fatal   : -
C
C ROUTINES THIS FUNCTION IS CALLED FROM:
C
C ROUTINES THIS FUNCTION CALLS:
      INTEGER PIX_Right_Pixel
      INTEGER PIX_UpRight_Pixel
      INTEGER PIX_Up_Pixel
      INTEGER PIX_UpLeft_Pixel
      INTEGER PIX_Left_Pixel
      INTEGER PIX_DownLeft_Pixel
      INTEGER PIX_Down_Pixel
      INTEGER PIX_DownRight_Pixel
      INTEGER GRP_Add_Pixel
C
C FUNCTION WRITTEN BY:
C     Duncan Prindle,    8-8-92
C
C
      INTEGER MaxLevel
      PARAMETER (MaxLevel=200)
      INTEGER IPhi2(MaxLevel+1), IZ2(MaxLevel+1)
      INTEGER CDir(MaxLevel), LDir(MaxLevel), SDir(MaxLevel)
      INTEGER Level, Ok
      LOGICAL There
C
      Level = 1
      IPhi2(Level) = IPhi  ! Starting at current position
      IZ2(Level)   = IZ    ! Starting at current position
      CDir(Level)  = 1     ! Current Direction
      SDir(Level)  = 0     ! Direction to Skip
      LDir(Level)  = 8     ! Last Direction to check.
    1    CONTINUE
         IF (CDir(Level).EQ.1) THEN
             Ok = PIX_Right_Pixel( IPhi2(Level), IZ2(Level),
     +                             PIX_map, PIX_info,
     +                             PIX_info.NPhi, PIX_info.NZ,
     +                             PIX_info.NHitsMax,
     +                             There, IPhi2(Level+1), IZ2(Level+1) )
           ELSE IF (CDir(Level).EQ.2) THEN
             Ok = PIX_UpRight_Pixel( IPhi2(Level), IZ2(Level),
     +                               PIX_map, PIX_info,
     +                               PIX_info.NPhi, PIX_info.NZ,
     +                               PIX_info.NHitsMax,
     +                               There,IPhi2(Level+1),IZ2(Level+1))
           ELSE IF (CDir(Level).EQ.3) THEN
             Ok = PIX_Up_Pixel( IPhi2(Level), IZ2(Level),
     +                          PIX_map, PIX_info,
     +                          PIX_info.NPhi, PIX_info.NZ,
     +                          PIX_info.NHitsMax,
     +                          There, IPhi2(Level+1), IZ2(Level+1) )
           ELSE IF (CDir(Level).EQ.4) THEN
             Ok = PIX_UpLeft_Pixel( IPhi2(Level), IZ2(Level),
     +                              PIX_map, PIX_info,
     +                              PIX_info.NPhi, PIX_info.NZ,
     +                              PIX_info.NHitsMax,
     +                              There,IPhi2(Level+1),IZ2(Level+1))
           ELSE IF (CDir(Level).EQ.5) THEN
             Ok = PIX_Left_Pixel( IPhi2(Level), IZ2(Level),
     +                            PIX_map, PIX_info,
     +                            PIX_info.NPhi, PIX_info.NZ,
     +                            PIX_info.NHitsMax,
     +                            There, IPhi2(Level+1), IZ2(Level+1) )
           ELSE IF (CDir(Level).EQ.6) THEN
             Ok = PIX_DownLeft_Pixel( IPhi2(Level), IZ2(Level),
     +                                PIX_map, PIX_info,
     +                                PIX_info.NPhi, PIX_info.NZ,
     +                                PIX_info.NHitsMax,
     +                              There,IPhi2(Level+1),IZ2(Level+1))
           ELSE IF (CDir(Level).EQ.7) THEN
             Ok = PIX_Down_Pixel( IPhi2(Level), IZ2(Level),
     +                            PIX_map, PIX_info,
     +                            PIX_info.NPhi, PIX_info.NZ,
     +                            PIX_info.NHitsMax,
     +                            There, IPhi2(Level+1), IZ2(Level+1) )
           ELSE IF (CDir(Level).EQ.8) THEN
             Ok = PIX_DownRight_Pixel( IPhi2(Level), IZ2(Level),
     +                                 PIX_map, PIX_info,
     +                                 PIX_info.NPhi, PIX_info.NZ,
     +                                 PIX_info.NHitsMax,
     +                               There,IPhi2(Level+1),IZ2(Level+1))
           END IF
         IF (There) THEN
             Ok = GRP_Add_Pixel( IPhi2(Level+1), IZ2(Level+1),
     +                           PIX_Map, PIX_info,
     +                           PIX_info.NPhi, PIX_info.NZ,
     +                           PIX_info.NHitsMax,
     +                           i_group, Group, l_group, n_group )
             IF (Level.EQ.MaxLevel) THEN
                 WRITE(6,*)' GRP_Check_Neighbors is too deep '
                 GO TO 2
                 END IF
             Level = Level + 1
             CDir(Level) = CDir(Level-1)
             IF (CDir(Level).EQ.1) THEN
                 SDir(Level) = 5
                 LDir(Level) = 8
               ELSE IF (CDir(Level).EQ.2) THEN
                 SDir(Level) = 6
                 LDir(Level) = 1
               ELSE IF (CDir(Level).EQ.3) THEN
                 SDir(Level) = 7
                 LDir(Level) = 2
               ELSE IF (CDir(Level).EQ.4) THEN
                 SDir(Level) = 8
                 LDir(Level) = 3
               ELSE IF (CDir(Level).EQ.5) THEN
                 SDir(Level) = 1
                 LDir(Level) = 4
               ELSE IF (CDir(Level).EQ.6) THEN
                 SDir(Level) = 2
                 LDir(Level) = 5 
               ELSE IF (CDir(Level).EQ.7) THEN
                 SDir(Level) = 3
                 LDir(Level) = 6
               ELSE IF (CDir(Level).EQ.8) THEN
                 SDir(Level) = 4
                 LDir(Level) = 7
               END IF
             SDir(Level) = 0   ! Remove when we optimize search.
             GO TO 1
             END IF
    2    CONTINUE
         IF (CDir(Level).EQ.LDir(Level)) THEN
             Level = Level - 1
             IF (Level.EQ.0) GO TO 3
             GO TO 1
             END IF
         CDir(Level) = CDir(Level) + 1
         IF (CDir(Level).EQ.9) CDir(Level) = 1
         IF (CDir(Level).EQ.SDir(Level)) CDir(Level) = CDir(Level) + 1
         IF (CDir(Level).EQ.9) CDir(Level) = 1
         GO TO 1
C
    3 CONTINUE
      GRP_Check_Neighbors = 1
      RETURN
      END
C
