      SUBROUTINE ev0_track_mom_r(trk,iflag,p)
c:>------------------------------------------------------------------
C:ROUTINE:      ev0_track_mom_r
C:DESCRIPTION:  returns track momentum components from track parameters
C:ARGUMENTS:    see code
C:ARGUMENTS:    see code
C:RETURN VALUE: 
c:>------------------------------------------------------------------
      IMPLICIT NONE
C   Input arguments  :	trk(6)	!helix parameters of the track
C			iflag	!if =5 it is a neutral particle and
C				!trk(6)=1./momentum of particle
C   Output arguments : 	p(3)	!momentum vector of track
C   Functional Description :  	returns the momentum components
C				of a track
C   Created  DEC-1991  S.Margetis
C   Error conditions	:	None
C   Status 		:	Tested
C----------------------------------------------------------------------

      INTEGER iflag
      REAL    trk(6),p(3),phi
      REAL    const,beta,lamda,cl
      REAL    sph,cph,pt,ptot
      PARAMETER (const=0.299792458,beta=5.)	!beta(field) in KGauss

C-----Extract momentum components from helix --------------------------

	lamda=atan(trk(5))
	cl   =cos(lamda)
	phi  =trk(4)
	cph  =cos(phi)
	sph  =sin(phi)

C-------check if it is a neutral particle------------------------------

       IF(iflag.eq.5) THEN
	ptot =1./trk(6)
	pt   =ptot*cl
	p(3) =pt*trk(5)
       ELSE
	pt   =(beta*const)/(1000.*abs(trk(6)))
	p(3) =pt*trk(5)
	ptot =sqrt(pt*pt+p(3)*p(3))
       ENDIF	!iflag.eq.5

	p(1) =ptot*cl*cph
	p(2) =ptot*cl*sph

  999 CONTINUE
      RETURN
      END
