* $Id: tfs_hit_smear.F,v 1.2 1998/03/03 23:03:14 hardtke Exp $
* $Log: tfs_hit_smear.F,v $
* Revision 1.2  1998/03/03 23:03:14  hardtke
* use constants files
*
* Revision 1.1  1998/01/27 01:47:23  fisyak
* Split sources
*
	SUBROUTINE TFS_HIT_SMEAR( SECT, XI, YI, ZI, XO, YO, ZO,
     >                          ST_DEV_XY, ST_DEV_Z, ST_DEV_X, ST_DEV_Y)
        IMPLICIT NONE
C-----------------------------------------------------------------------
C     Input Arguments:
C       sect      - sector number
C       xi        - GEANT X coordinate
C       yi        - GEANT Y coordinate
C       zi        - GEANT Z coordinate
C       st_dev_xy - width of the signal in the transverse direction
C       st_dev_z  - width of the signal in the longitudinal direction
C
C     Output Arguments:
C       x0       - smeared XI
C       y0       - smeared YI
C       z0       - smeared ZI
C       st_dev_x - width of the signal in the x direction
C       st_dev_y - width of the signal in the y direction
C
C     Functional Description:
C	This subroutine will take as input a sector number for the tpc
C	(sect) and three coordinates for a space point (xi,yi,zi) in units
C	of centimeters and  the resolution in the transverse and drift
C       direction. It will then add a normally distributed error to
C	the coordinates for the space point, along the pad row direction
C	for the particular sector and in the electron drift direction(z).
C
C     Author:
C       Bill Christie   LBL
C
C     Modified by:
C       Peter G. Jones  LBL  (510)-486-5436
C
C     Creation Date:
C       09-Apr-1991
C
C     History:
C     Apr 09, 1991           Original by Peter Jones.
C     Apr 10, 1997           Modefied by Bill Love to set correct errors
C                            in hit smearing.
C     Jun 18, 1997           Modified by Dhammika W. to fix a bug in 
C                            z-coordinate  smearing.
C     Mar 2, 1998    DH      Remove list of unused constants
C
C-----------------------------------------------------------------------

      INTEGER	SECT

      REAL	XI,YI,ZI,XO,YO,ZO
      REAL	ERR_PAD		!ERROR ALONG PAD ROW FOR SPACE POINT
      REAL	ERR_DRFT	!ERROR ALONG DRIFT DIRECTION FOR SPACE POINT
      REAL	ST_DEV_XY	!SIGMA FOR X_Y ERROR (cm)
      REAL      ST_DEV_X
      REAL      ST_DEV_Y
      REAL	ST_DEV_Z	!SIGMA FOR Z ERROR   (cm)
      REAL      SMEAR           !Random no. gaussian distrib. unit sigma
      REAL      RDUMMY
      REAL      PHI
      real      xlocal(3),xglobal(3)
      integer   iret
      integer   tgc_local_to_global, tgc_global_to_local

C=========================== Begin Executable Code =======================

        CALL RANNOR(SMEAR,RDUMMY)
        ERR_PAD = ST_DEV_XY*SMEAR
        CALL RANNOR(SMEAR,RDUMMY)
        ERR_DRFT = ST_DEV_Z*SMEAR
        xglobal(1)=xi
        xglobal(2)=yi
        xglobal(3)=zi
        iret=tgc_global_to_local(sect,xglobal,xlocal)
        xlocal(1)=xlocal(1)+err_pad
        iret=tgc_local_to_global(sect,xlocal,xglobal)
        xo=xglobal(1)
        yo=xglobal(2)
        zo=zi+err_drft
C     Make sure that z coordinate remains in the same hemisphere after the 
C     smearing to maintain the correct sector number corresponding this hit.
C     --- DSW Jun 18, 1997  --- 
        if(zi.gt.0.0.and.zo.lt.0.0)then
           zo = zi + abs(err_drft)
        elseif(zi.lt.0.0.and.zo.gt.0.0)then
           zo = zi - abs(err_drft)
        endif
        xlocal(1)=st_dev_xy
        xlocal(2)=0
        xlocal(3)=0
        iret=tgc_local_to_global(sect,xlocal,xglobal)
        ST_DEV_X = abs(xglobal(1))
        ST_DEV_Y = abs(xglobal(2))
	RETURN
	END
