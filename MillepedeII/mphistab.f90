
! Code converted using TO_F90 by Alan Miller
! Date: 2012-03-16  Time: 11:07:45

!> \file
!! Histogramming package.
!!
!! \author Volker Blobel, University Hamburg, 2005-2009 (initial Fortran77 version)
!! \author Claus Kleinwort, DESY (maintenance and developement)
!!
!! \copyright
!! Copyright (c) 2009 - 2022 Deutsches Elektronen-Synchroton,
!! Member of the Helmholtz Association, (DESY), HAMBURG, GERMANY \n\n
!! This library is free software; you can redistribute it and/or modify
!! it under the terms of the GNU Library General Public License as
!! published by the Free Software Foundation; either version 2 of the
!! License, or (at your option) any later version. \n\n
!! This library is distributed in the hope that it will be useful,
!! but WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!! GNU Library General Public License for more details. \n\n
!! You should have received a copy of the GNU Library General Public
!! License along with this program (see the file COPYING.LIB for more
!! details); if not, write to the Free Software Foundation, Inc.,
!! 675 Mass Ave, Cambridge, MA 02139, USA.
!!
!!\verbatim
!!                          HMP... and GMP...
!!                    Histogram and XY data in text files
!!
!!     Booking:
!!
!!     CALL HMPDEF(IH,XA,XB,TEXT)               CALL GMPDEF(IG,ITYP,TEXT)
!!     where                                    where
!!        IH    = 1 ... 10                         IG    = 1 ... 10
!!        XA,XB = left, right limit                ITYP  = 1 dots
!!        TEXT  = explanation                            = 2 line
!!                                                       = 3 dots and line
!!                                                       = 4 symbols
!!                                                       = 5 mean/sigma
!!                                                 TEXT  = explanation
!!
!!     CALL HMPLUN(LUNW)                        CALL GMPLUN(LUNW)
!!     unit for output                          unit for output
!!
!!     CALL HMPENT(IH,X)                        CALL GMPXY(IG,X,Y)
!!     entry flt.pt. X                          add (X,Y) pair
!!
!!                                              CALL GMPXYD(IG,X,Y,DX,DY)
!!                                              add (X,Y,DX,DY) ITYP=4
!!
!!        new                                   CALL GMPMS(IG,X,Y)
!!                                              mean/sigma from x,y
!!
!!     Booking log integer histogram:
!!
!!     CALL HMPLDF(IH,TEXT)
!!     book and reset log integer histogram
!!
!!     CALL HMPLNT(IH,IX)
!!     entry integer IX
!!
!!     Printing and writing:
!!
!!     CALL HMPRNT(IH)                          CALL GMPRNT(IG)
!!     print histogram IH or all, if 0          print data Ig or all, if 0
!!
!!     CALL HMPWRT(IH)                          CALL GMPWRT(IG)
!!     write histogram IH or all to file        write data IG or all to file
!!
!!
!!     Storage manager for GMP...
!!
!!      CALL STMARS(NDIM)           !! init/reset  storage manager, partially dynamic
!!
!!      CALL STMAPR(JFLC,X,Y)       !! store pair (X,Y)
!!
!!      CALL STMADP(JFLC,FOUR)      !! store double pair
!!
!!      CALL STMACP(JFLC,ARRAY,N)   !! copy (cp) all pairs to array
!!
!!      CALL STMARM(JFLC)           !! remove (rm) stored paiirs
!!
!!\endverbatim
!!
!! The number of histograms is limited to NUMHIS (=15), the number of XY data plots
!! to NUMGXY (=10) and the storage of XY points to NDIM (=5000). As each XY plot can
!! contain up to NLIMIT (=500) points (before averaging) NDIM should be NLIMIT*NUMGXY.

!     *************************** Histograms ******************************

!> Histogram constants.
MODULE hmpcons
    USE mpdef
    IMPLICIT NONE
    
    INTEGER(mpi), PARAMETER :: numhis=15  !< number of histograms
    INTEGER(mpi), PARAMETER :: nbin=120   !< number of bins
    INTEGER(mpi), PARAMETER :: nsampl=120 !< number of samples for auto scaling
    
END MODULE hmpcons
    
!> Histogram data.
MODULE hmpdata
    USE hmpcons
    IMPLICIT NONE

    INTEGER(mpi) :: lun=7 !< unit for output
    INTEGER(mpi) :: inhist(nbin,numhis)    !< histogram (bin) data
    INTEGER(mpi) :: jnhist(5,numhis)      !< histogram statistics
    INTEGER(mpi) :: khist(numhis)=0       !< histgram type
    INTEGER(mpi) :: kvers(numhis)         !< histogram version
    REAL(mps) :: fnhist(nsampl,numhis)    !< initial data for auto scaling
    REAL(mps) :: xl(6,numhis)             !< histogram binning
    REAL(mpd) :: dl(2,numhis)             !< histogram moments
    CHARACTER (LEN=60):: htext(numhis)    !< histogram text

END MODULE hmpdata    

!> book, reset histogram
SUBROUTINE hmpdef(ih,xa,xb,text)
    USE hmpdata

    IMPLICIT NONE

    !     book millepede histogram, 'nbin' bins

    INTEGER(mpi), INTENT(IN)                      :: ih
    REAL(mps), INTENT(IN)                         :: xa
    REAL(mps), INTENT(IN)                         :: xb
    CHARACTER (LEN=*), INTENT(IN)            :: text

    SAVE
    !     ...
    IF(ih <= 0.OR.ih > numhis) RETURN
    !      IF(XA.EQ.XB)                RETURN
    inhist(:,ih)=0
    jnhist(:,ih)=0
    xl(1,ih)=xa
    xl(2,ih)=xb
    xl(3,ih)=0.0
    IF(xa /= xb) xl(3,ih)=REAL(nbin,mps)/(xb-xa)
    xl(6,ih)=0.5*(xa+xb) ! center
    IF(khist(ih) == 0) THEN
        kvers(ih)=0
    ELSE
        kvers(ih)=kvers(ih)+1
    END IF
    khist(ih)=1    ! flt.pt. (lin)
    htext(ih)=text
    dl(1,ih)=0.0_mpd
    dl(2,ih)=0.0_mpd 
    RETURN
END SUBROUTINE hmpdef

!> book, reset log histogram
SUBROUTINE hmpldf(ih,text)
    USE hmpdata

    IMPLICIT NONE
    
    INTEGER(mpi), INTENT(IN)                      :: ih
    CHARACTER (LEN=*), INTENT(IN)            :: text

    SAVE
    IF(ih <= 0.OR.ih > numhis) RETURN
    inhist(:,ih)=0
    jnhist(:,ih)=0
    IF(khist(ih) == 0) THEN
        kvers(ih)=0
    ELSE
        kvers(ih)=kvers(ih)+1
    END IF
    khist(ih)=2    ! integer log
    htext(ih)=text
    xl(1,ih)=0.0
    xl(2,ih)=6.0    
    RETURN
END SUBROUTINE hmpldf

!> entry flt.pt.
SUBROUTINE hmpent(ih,x)
    USE hmpdata

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpi) :: j
    
    INTEGER(mpi), INTENT(IN)                      :: ih
    REAL(mps), INTENT(IN)                         :: x
        
    IF(ih <= 0.OR.ih > numhis) RETURN
    IF(khist(ih) /= 1)          RETURN
    IF(jnhist(4,ih) >= 2147483647) RETURN
    jnhist(4,ih)=jnhist(4,ih)+1      ! count
    IF(jnhist(4,ih) <= nsampl) THEN
        fnhist(jnhist(4,ih),ih)=x     ! store value
        IF(jnhist(4,ih) == nsampl) THEN
            CALL hmpmak(inhist(1,ih),fnhist(1,ih),jnhist(1,ih), xl(1,ih),dl(1,ih))
        END IF
        RETURN
    END IF
    !      IF(JNHIST(1,IH)+JNHIST(2,IH)+JNHIST(3,IH).EQ.0) THEN
    !         XL(4,IH)=X
    !         XL(5,IH)=X
    !      END IF
    i=INT(1.0+xl(3,ih)*(x-xl(1,ih)),mpi)   ! X - Xmin
    j=2
    IF(i <  1) j=1
    IF(i > nbin) j=3
    jnhist(j,ih)=jnhist(j,ih)+1
    xl(4,ih)=MIN(xl(4,ih),x)
    xl(5,ih)=MAX(xl(5,ih),x)
    IF(j /= 2) RETURN
    inhist(i,ih)=inhist(i,ih)+1
    dl(1,ih)=dl(1,ih)+ x-xl(6,ih)
    dl(2,ih)=dl(2,ih)+(x-xl(6,ih))**2
    RETURN
END SUBROUTINE hmpent

!> entry integer
SUBROUTINE hmplnt(ih,ix)
    USE hmpdata

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpi) :: j

    INTEGER(mpi), INTENT(IN)                      :: ih
    INTEGER(mpi), INTENT(IN)                      :: ix
    
    IF(ih <= 0.OR.ih > numhis) RETURN
    IF(khist(ih) /= 2)          RETURN
    IF(jnhist(1,ih) >= 2147483647) RETURN
    IF(ix <= 0) THEN
        jnhist(1,ih)=jnhist(1,ih)+1
    ELSE
        IF(jnhist(4,ih) == 0) jnhist(4,ih)=ix
        IF(jnhist(5,ih) == 0) jnhist(5,ih)=ix
        jnhist(4,ih)=MIN(jnhist(4,ih),ix)
        jnhist(5,ih)=MAX(jnhist(5,ih),ix)
        i=INT(1.0+20.0*LOG10(REAL(ix,mps)),mpi)
        j=2
        IF(i <  1) j=1
        IF(i > nbin) j=3
        IF(j == 2)  inhist(i,ih)=inhist(i,ih)+1
        jnhist(j,ih)=jnhist(j,ih)+1
    END IF
    RETURN
END SUBROUTINE hmplnt

!> print, content vert
SUBROUTINE hmprnt(ih)
    USE hmpdata

    IMPLICIT NONE
    INTEGER(mpi) :: iha
    INTEGER(mpi) :: ihb
    INTEGER(mpi) :: ihc
    INTEGER(mpi) :: j
    INTEGER(mpi) :: nn
    REAL(mps) :: xcent
    REAL(mps) :: xmean
    REAL(mps) :: xsigm
        
    INTEGER(mpi), INTENT(IN)                      :: ih
    
    IF(ih == 0) THEN
        iha=1
        ihb=numhis
    ELSE
        IF(ih <= 0.OR.ih > numhis) RETURN
        iha=ih
        ihb=ih
    END IF
    DO ihc=iha,ihb
        IF(khist(ihc) /= 0) THEN
            IF(khist(ihc) == 1) THEN
                CALL hmpmak(inhist(1,ihc),fnhist(1,ihc),jnhist(1,ihc),  &
                    xl(1,ihc),dl(1,ihc))
            END IF
            nn=jnhist(1,ihc)+jnhist(2,ihc)+jnhist(3,ihc)
            IF(nn /= 0.OR.khist(ihc) == 3) THEN
                WRITE(*,111)
111             FORMAT(' ______',2('______________________________'))
                IF(kvers(ihc) == 1) THEN
                    WRITE(*,*) 'Histogram',ihc,': ',htext(ihc)
                ELSE
                    WRITE(*,*) 'Histogram',ihc,'/',kvers(ihc),': ',htext(ihc)
                END IF
                IF(khist(ihc) == 1) THEN
                    WRITE(*,*) '   Out_low  inside  out_high = ', (jnhist(j,ihc),j=1,3)
                ELSE IF(khist(ihc) == 2) THEN
                    WRITE(*,*) '   0_or_negative  inside  above_10^6 = ',  &
                        (jnhist(j,ihc),j=1,3)
                END IF
                IF(khist(ihc) == 3) THEN
                    CALL pfvert(nbin,fnhist(1,ihc))
                END IF
                IF(jnhist(2,ihc) /= 0) THEN        ! integer content
                    CALL pivert(nbin,inhist(1,ihc))
                    IF(khist(ihc) == 1) THEN
                        CALL psvert(xl(1,ihc),xl(2,ihc))
                    ELSE IF(khist(ihc) == 2) THEN
                        CALL psvert(0.0,6.0)
                    END IF
                END IF
                IF(khist(ihc) == 1) THEN
                    WRITE(*,*) '   Min and Max are',xl(4,ihc),xl(5,ihc)
                    IF(jnhist(2,ihc) > 1) THEN
                        xmean=REAL(xl(6,ihc)+dl(1,ihc)/REAL(jnhist(2,ihc),mps),mps)
                        xcent=0.5*(xl(1,ihc)+xl(2,ihc))
                        xsigm=REAL((dl(2,ihc)-dl(1,ihc)**2/REAL(jnhist(2,ihc),mps)),mps)
                        xsigm=SQRT(xsigm/REAL(jnhist(2,ihc)-1,mps))
                        WRITE(*,*) '   Mean and sigma are', xmean,' +-',xsigm
                    END IF
                ELSE IF(khist(ihc) == 2) THEN
                    WRITE(*,*) '   Plot of log10 of entries. Min and Max are',  &
                        jnhist(4,ihc),jnhist(5,ihc)
                END IF
            END IF
        END IF
    END DO
    RETURN
END SUBROUTINE hmprnt

!> unit for output
SUBROUTINE hmplun(lunw)
    USE hmpdata

    IMPLICIT NONE
    
    INTEGER(mpi), INTENT(IN)                      :: lunw
    
    lun=lunw
    RETURN
END SUBROUTINE hmplun

!> write histogram text file
SUBROUTINE hmpwrt(ih)
    USE hmpdata

    IMPLICIT NONE
    INTEGER(mpi) :: iha
    INTEGER(mpi) :: ihb
    INTEGER(mpi) :: ihc
    INTEGER(mpi) :: i
    INTEGER(mpi) :: j
    REAL(mps) :: xcent
    REAL(mps) :: xmean
    REAL(mps) :: xsigm
        
    INTEGER(mpi), INTENT(IN)                      :: ih

    IF(lun <= 0) RETURN
    IF(ih == 0) THEN
        iha=1
        ihb=numhis
    ELSE
        IF(ih <= 0.OR.ih > numhis) RETURN
        iha=ih
        ihb=ih
    END IF

    DO ihc=iha,ihb ! histogram loop
        IF(khist(ihc) /= 0) THEN
            IF(khist(ihc) == 1) THEN
                CALL hmpmak(inhist(1,ihc),fnhist(1,ihc),jnhist(1,ihc),  &
                    xl(1,ihc),dl(1,ihc))
            END IF
            WRITE(lun,204) ' '
            WRITE(lun,201) ihc,kvers(ihc),khist(ihc)
            WRITE(lun,204) htext(ihc)
            IF (jnhist(1,ihc)+jnhist(2,ihc)+jnhist(3,ihc) == 0  &
                .AND.xl(1,ihc) == xl(2,ihc)) THEN
                !     hist is empty and hist range makes no sense
                !     - cause: hist with 'variable edges' was never filled
                !     - workaround: make lower and upper edge of hist differ in output
                WRITE(lun,202) nbin,xl(1,ihc)-0.001,xl(2,ihc)+0.001
            ELSE
                WRITE(lun,202) nbin,xl(1,ihc),xl(2,ihc)
            END IF
            WRITE(lun,203) (jnhist(j,ihc),j=1,3)
            WRITE(lun,204) 'bincontent'
            IF(khist(ihc) == 1.OR.khist(ihc) == 2) THEN
                CALL kprint(lun,inhist(1,ihc),nbin)
            ELSE
                WRITE(lun,219) (fnhist(i,ihc),i=1,nbin)
            END IF
    
            IF(khist(ihc) == 1) THEN
                WRITE(lun,205) xl(4,ihc),xl(5,ihc)
            ELSE IF(khist(ihc) == 2) THEN
                WRITE(lun,205) REAL(jnhist(4,ihc),mps),REAL(jnhist(5,ihc),mps)
            END IF
            IF(khist(ihc) == 1) THEN
                IF(jnhist(2,ihc) > 1) THEN
                    xmean=REAL(xl(6,ihc)+dl(1,ihc)/REAL(jnhist(2,ihc),mps),mps)
                    xcent=0.5*(xl(1,ihc)+xl(2,ihc))
                    xsigm=REAL((dl(2,ihc)-dl(1,ihc)**2/REAL(jnhist(2,ihc),mps)),mps)
                    xsigm=SQRT(xsigm/REAL(jnhist(2,ihc)-1,mps))
                    WRITE(lun,206) xmean,xsigm
                END IF
            END IF
            WRITE(lun,204) 'end of histogram'
        END IF
    END DO
    RETURN

201 FORMAT('Histogram ',i4,10X,'version ',i4,10X,'type',i2)
202 FORMAT(10X,' bins, limits ',i4,2G15.5)
203 FORMAT(10X,'out-low inside out-high ',3I10)
204 FORMAT(a)
205 FORMAT('minmax',2E15.7)
206 FORMAT('meansigma',2E15.7)

219 FORMAT(4E15.7)
END SUBROUTINE hmpwrt

!> hist scale from data
SUBROUTINE hmpmak(inhist,fnhist,jnhist,xl,dl)
    USE hmpcons

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpi) :: j
    INTEGER(mpi) :: k
    INTEGER(mpi) :: nn
    REAL(mps) :: x
    REAL(mps) :: xa
    REAL(mps) :: xb

    INTEGER(mpi), INTENT(OUT)                     :: inhist(nbin)
    REAL(mps), INTENT(IN)                         :: fnhist(nsampl)
    INTEGER(mpi), INTENT(IN OUT)                  :: jnhist(5)
    REAL(mps), INTENT(IN OUT)                     :: xl(6)
    REAL(mpd), INTENT(OUT)            :: dl(2)
    REAL(mps) :: cphist(nsampl)

    SAVE
    !     ...
    nn=jnhist(4)
    !      WRITE(*,*) 'HMPMAK: NN,JNHIST(5)',NN,JNHIST(5)
    IF(nn == 0.OR.jnhist(5) /= 0) RETURN
    jnhist(5)=1
    cphist(:)=fnhist(:)
    CALL heapf(cphist,nn)
    IF(xl(3) == 0.0) THEN
        CALL bintab(cphist,nn,xa,xb)
        xl(1)=xa
        xl(2)=xb
        xl(3)=0.0
        IF(xa /= xb) xl(3)=REAL(nbin,mps)/(xb-xa)
        xl(6)=0.5*(xa+xb) ! center
    END IF
    xl(4)=cphist( 1)
    xl(5)=cphist(nn)
    !      WRITE(*,*) 'XL ',XL
    DO i=1,nn
        inhist(i)=0
    END DO
    DO k=1,nn
        x=cphist(k)
        i=INT(1.0+xl(3)*(x-xl(1)),mpi)   ! X - Xmin
        !       WRITE(*,*) 'K,I,X ',K,I,X
        j=2
        IF(i <  1) j=1
        IF(i > nbin) j=3
        jnhist(j)=jnhist(j)+1
        IF(j == 2) THEN
            inhist(i)=inhist(i)+1
            dl(1)=dl(1)+ x-xl(6)
            dl(2)=dl(2)+(x-xl(6))**2
        END IF
    END DO
END SUBROUTINE hmpmak

!> hist scale from data
SUBROUTINE bintab(tab,n,xa,xb)
    USE hmpcons

    IMPLICIT NONE
    REAL(mps) :: dd
    REAL(mps) :: dx
    INTEGER(mpi) :: i
    INTEGER(mpi) :: iexp
    INTEGER(mpi) :: ii
    INTEGER(mpi) :: j
    INTEGER(mpi) :: m1
    INTEGER(mpi) :: m2
    INTEGER(mpi) :: n1
    INTEGER(mpi) :: n2
    INTEGER(mpi) :: nch
    REAL(mps) :: rat
    REAL(mps) :: x1
    REAL(mps) :: x2
    REAL(mps) :: xx
    !     Bin limits XA and XB from TAB(N)

    INTEGER(mpi), INTENT(IN)                      :: n
    REAL(mps), INTENT(IN)                         :: tab(n)
    REAL(mps), INTENT(OUT)                        :: xa
    REAL(mps), INTENT(OUT)                        :: xb

    REAL(mps) :: bin(10)
    DATA bin/1.0,1.5,2.0,3.0,4.0,5.0,8.0,10.0,15.0,20.0/
    SAVE
    !     ...
    
    CALL heapf(tab,n) ! reduced statistic
    !      WRITE(*,*) ' '
    !      WRITE(*,*) 'Sorted ',N,(TAB(I),I=1,N)
    IF(n < nsampl) THEN
        x1=tab(1)
        x2=tab(n)
    !         WRITE(*,*) 'reduced statistic X1 X2 ',X1,X2
    ELSE              ! large statistic
        m1=INT(1.0+0.05*REAL(n),mpi)
        m2=INT(1.0+0.16*REAL(n),mpi)
        x1=tab(m1)-4.0*(tab(m2)-tab(m1))
        IF(x1 < 0.0.AND.tab(1) >= 0.0) x1=tab(1)
        x2=tab(n+1-m1)+4.0*(tab(n+1-m1)-tab(n+1-m2))
        IF(x2 > 0.0.AND.tab(n) <= 0.0) x2=tab(n)
        !         WRITE(*,*) 'large statistic ',X1,X2
        !         WRITE(*,*) 'min und max ',TAB(1),TAB(N)
        IF(x1*tab(1) <= 0.0) x1=0.0
        IF(x2*tab(n) <= 0.0) x2=0.0
        !         WRITE(*,*) 'large statistic zero ',X1,X2
        IF(x1*x2 < 0.0.AND.MIN(-x1,x2) > 0.6*MAX(-x1,x2)) THEN
            xx=MAX(-x1,x2) ! symmetry
            x1=-xx
            x2=+xx
        ELSE IF(x1*x2 > 0.0.AND. & ! include zero ?
            ABS(MIN(x1,x2)) < 0.4*ABS(MAX(x1,x2))) THEN
            IF(x1 < 0.0) THEN
                x2=0.0
            ELSE
                x1=0.0
            END IF
        END IF
    !         WRITE(*,*) 'large statistic ',X1,X2
    END IF
    IF(x1 == x2) THEN
        x1=x1-1.0
        x2=x2+1.0
    END IF
    dx=x2-x1
    !      WRITE(*,*) 'X1,X2,DX ',N,X1,X2,DX
    rat=0.0
    ii=1
    DO j=1,11
        i=j
        IF(j == 11) i=ii
        iexp=INT(101.0+LOG10(dx)-LOG10(6.0*bin(i)),mpi)
        iexp=iexp-100
        dd=bin(i)*10.0**iexp
        
        n1=INT(ABS(x1)/dd,mpi)
        IF(x1 < 0.0) n1=-n1
        IF(REAL(n1,mps)*dd > x1) n1=n1-1
        !       WRITE(*,*) 'Bin ',I,N1,N1*DD,X1
  
        n2=INT(ABS(x2)/dd,mpi)
        IF(x2 < 0.0) n2=-n2
        IF(REAL(n2,mps)*dd < x2) n2=n2+1
        !       WRITE(*,*) 'Bin ',I,N1,N2,N2*DD,X2
10      IF(n2-n1 < 6) THEN
            nch=0 ! number of changes
            IF(n1 /= 0) THEN
                n1=n1-1
                nch=nch+1
            END IF    
            IF(n2-n1 < 6.AND.n2 /= 0) THEN
                n2=n2+1
                nch=nch+1
            ENDIF    
            IF (nch > 0) GO TO 10 
            ! avoid infinite loop
            print *, ' BINTAB: break infinite loop ', n1, n2, n, x1, x2, dd
            n1=-3
            n2=3
        END IF
        !       WRITE(*,*) 'corrected N1 N2 ',N1,N2
        xa=SIGN(REAL(n1,mps)*dd,x1)
        xb=SIGN(REAL(n2,mps)*dd,x2)
        !       WRITE(*,*) J,' resulting limits XA XB ',XA,XB
        IF((x2-x1)/(xb-xa) > rat) THEN
            ii=i
            rat=(x2-x1)/(xb-xa)
        END IF
    END DO
!      WRITE(*,*) J,' resulting limits XA XB ',XA,XB
END SUBROUTINE bintab

!> print integer array
SUBROUTINE kprint(lun,list,n)
    USE mpdef

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpi) :: ia
    INTEGER(mpi) :: ib
    INTEGER(mpi) :: k
    INTEGER(mpi) :: ln
    INTEGER(mpi) :: lp
    INTEGER(mpi) :: np
    !     print integer array LIST(N)

    INTEGER(mpi), INTENT(IN OUT)                  :: lun
    INTEGER(mpi), INTENT(IN)                      :: n
    INTEGER(mpi), INTENT(IN)                      :: list(n)
    INTEGER(mpi) :: li(7)
    DATA li/2,3,4,6,8,9,12/ ! number of characters
    SAVE
    !     ...
    ib=0
10  ia=ib+1
    IF(ia > n) RETURN
    DO k=1,7
        np=72/li(k)
        ib=MIN(ia-1+np,n)
        IF(k <= 6) THEN
            lp=10**(li(k)-1)-1 ! maximum positive
            ln=-lp/10       ! minimum negative
            DO i=ia,ib
                IF(list(i) > lp.OR.list(i) < ln) GO TO 20
            END DO
        END IF
        IF(k == 1) THEN
            WRITE(lun,101) (list(i),i=ia,ib)
        ELSE IF(k == 2) THEN
            WRITE(lun,102) (list(i),i=ia,ib)
        ELSE IF(k == 3) THEN
            WRITE(lun,103) (list(i),i=ia,ib)
        ELSE IF(k == 4) THEN
            WRITE(lun,104) (list(i),i=ia,ib)
        ELSE IF(k == 5) THEN
            WRITE(lun,105) (list(i),i=ia,ib)
        ELSE IF(k == 6) THEN
            WRITE(lun,106) (list(i),i=ia,ib)
        ELSE IF(k == 7) THEN
            WRITE(lun,107) (list(i),i=ia,ib)
        END IF
        GO TO 10
20  CONTINUE
    END DO
101 FORMAT(36I2)
102 FORMAT(24I3)
103 FORMAT(18I4)
104 FORMAT(12I6)
105 FORMAT( 9I8)
106 FORMAT( 8I9)
107 FORMAT( 6I12)
END SUBROUTINE kprint

!     ***************************** XY data ****************************

!> XY constants.
MODULE gmpcons
    USE mpdef
    IMPLICIT NONE
    
    INTEGER(mpi), PARAMETER :: numgxy=10  !< number of XY data plots
    
END MODULE gmpcons

!> Histogram data.
MODULE gmpdata
    USE gmpcons
    IMPLICIT NONE
    
    INTEGER(mpi) :: lun=7           !< unit for output
    INTEGER(mpi) :: nlimit=500      !< max. number of XY pairs
    INTEGER(mpi) :: nstr(numgxy)=0  !< initialization flag 
    INTEGER(mpi) :: igtp(numgxy)    !< type of XY data
    !     ITYP = 1  X,Y     as dots
    !          = 2  X,Y     as line
    !          = 3  X,Y     as line and dots
    !          = 4  X,Y, DX,DY symbols
    !          = 5  mean, sigma of Y vs X (GMPMS)
    INTEGER(mpi) :: lvers(numgxy)   !< version
    INTEGER(mpi) :: nst(3,numgxy)   !< counters for GMPMS

    INTEGER(mpi) :: jflc(5,numgxy)  !< meta data
    INTEGER(mpi) :: kflc(5,numgxy)  !< meta data 
    !     JFLC(1,.) = first used index
    !     JFLC(2,.) = last used index
    !     JFLC(3,.) = counter of used places
    !     JFLC(4,.) = counter of ignored
    !     JFLC(5,.) = limit for JFLC(3)
    REAL(mps) :: xyplws(10,numgxy)  !< additional data for GMPMS
    REAL(mps) :: y1(numgxy)         !< first Y (as X) for GMPMS 
    REAL(mps), DIMENSION(:,:), ALLOCATABLE :: array  !< X,Y
    REAL(mps), DIMENSION(:,:), ALLOCATABLE :: array4 !< X,Y,DX,DY
    REAL(mps), DIMENSION(:), ALLOCATABLE :: array1   !< Y(X)
    CHARACTER (LEN=60):: gtext(numgxy)               !< text 
    
END MODULE gmpdata

!> book, reset XY storage
SUBROUTINE gmpdef(ig,ityp,text)
    USE gmpdata

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpi) :: j

    INTEGER(mpi), INTENT(IN)                      :: ig
    INTEGER(mpi), INTENT(IN)                      :: ityp
    CHARACTER (LEN=*), INTENT(IN)            :: text

    LOGICAL:: start
    SAVE
    DATA start/.TRUE./
    !     ...
    IF(start) THEN
        start=.FALSE.
        ! dummy call to increase nlimit ?
        if(ig == 0) nlimit = max(nlimit, ityp)
        CALL stmars(nlimit*numgxy)    ! initialize storage
        DO i=1,numgxy
            DO j=1,5
                jflc(j,i)=0
                kflc(j,i)=0
            END DO
        END DO
        ALLOCATE (array(2,nlimit))
        ALLOCATE (array4(4,(nlimit+1)/2))
        ALLOCATE (array1(nlimit*2))
    END IF

    IF(ig < 1.OR.ig > numgxy) RETURN
    IF(ityp < 1.OR.ityp > 5) RETURN
    IF(nstr(ig) == 0) THEN
        lvers(ig)=0
    ELSE
        lvers(ig)=lvers(ig)+1
    END IF
    nstr(ig)=1 ! by GF
    !        remove stored elements
    IF(jflc(1,ig) /= 0) CALL stmarm(jflc(1,ig))
    IF(kflc(1,ig) /= 0) CALL stmarm(kflc(1,ig))
    igtp(ig)=ityp
    gtext(ig)=text
    DO j=1,5
        jflc(j,ig)=0
    END DO
    jflc(5,ig)=nlimit
    IF(ityp == 5) THEN
        DO j=1,5
            kflc(j,ig)=0
        END DO
        jflc(5,ig)=128 ! maximum of 128 values
        kflc(5,ig)=nlimit
        nst(1,ig)=0
        nst(2,ig)=0
        nst(3,ig)=1
        DO j=1,10
            xyplws(j,ig)=0.0
        END DO
    END IF
    RETURN
END SUBROUTINE gmpdef

!> add (X,Y) pair
SUBROUTINE gmpxy(ig,x,y)
    USE gmpdata

    IMPLICIT NONE
    INTEGER(mpi), INTENT(IN)                      :: ig
    REAL(mps), INTENT(IN)                         :: x
    REAL(mps), INTENT(IN)                         :: y
    
    IF(ig  < 1.OR.ig > numgxy) RETURN        ! check argument IG
    IF(igtp(ig) < 1.OR.igtp(ig) > 3) RETURN   ! check type
    CALL stmapr(jflc(1,ig),x,y)
    RETURN
END SUBROUTINE gmpxy

!> add (X,Y,DX,DY)
SUBROUTINE gmpxyd(ig,x,y,dx,dy)
    USE gmpdata

    IMPLICIT NONE
    REAL(mps) :: four(4)            !< X,Y,DX,DY

    INTEGER(mpi), INTENT(IN)                      :: ig
    REAL(mps), INTENT(IN)                         :: x
    REAL(mps), INTENT(IN)                         :: y
    REAL(mps), INTENT(IN)                         :: dx
    REAL(mps), INTENT(IN)                         :: dy
    
    IF(ig  < 1.OR.ig > numgxy) RETURN        ! check argument IG
    IF(igtp(ig) /= 4) RETURN
    four(1)=x
    four(2)=y
    four(3)=dx
    four(4)=dy
    CALL stmadp(jflc(1,ig),four)
    RETURN
END SUBROUTINE gmpxyd

!> mean sigma(X) from Y
SUBROUTINE gmpms(ig,x,y)
    USE gmpdata

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpi) :: n

    INTEGER(mpi), INTENT(IN)                      :: ig
    REAL(mps), INTENT(IN)                         :: x
    REAL(mps), INTENT(IN)                         :: y
    
    !     mean sigma from Y, as a function of X
    !      WRITE(*,*) 'GMPMS ',IG,X,Y

    IF(ig  < 1.OR.ig > numgxy) RETURN        ! check argument IG
    IF(igtp(ig) /= 5) RETURN

    xyplws(10,ig)=x  ! last X  coordinate
    IF(nst(1,ig) == 0) THEN
        y1(ig)=y
        nst(1,ig)=1
        IF(kflc(3,ig) == 0) xyplws(9,ig)=x        ! start coordinate
    ELSE
        nst(1,ig)=0
        CALL stmapr(kflc(1,ig),y1(ig),y) ! store pair
        IF(kflc(3,ig) >= kflc(5,ig)) THEN
            CALL stmacp1(kflc(1,ig),array1,n) ! get data
            IF(kflc(1,ig) /= 0) CALL stmarm(kflc(1,ig))         ! remove data
            CALL rmesig(array1,n,xyplws(2,ig),xyplws(4,ig))
            nst(2,ig)=nst(2,ig)+1
            IF(nst(2,ig) == 1) xyplws(7,ig)=xyplws(9,ig)
            xyplws(8,ig)=x                   ! end coordinate
            xyplws(5,ig)=xyplws(5,ig)+xyplws(2,ig)
            xyplws(6,ig)=xyplws(6,ig)+xyplws(4,ig)
            IF(nst(2,ig) == nst(3,ig)) THEN
                xyplws(1,ig)=0.5*(xyplws(7,ig)+xyplws(8,ig))
                xyplws(2,ig)=xyplws(5,ig)/REAL(nst(3,ig),mps)
                xyplws(3,ig)=0.5*(xyplws(8,ig)-xyplws(7,ig))
                xyplws(4,ig)=xyplws(6,ig)/REAL(nst(3,ig),mps)
                xyplws(5,ig)=0.0
                xyplws(6,ig)=0.0
                nst(2,ig)=0
                CALL stmadp(jflc(1,ig),xyplws(1,ig))
                IF(jflc(3,ig) >= jflc(5,ig)) THEN
                    CALL stmacp4(jflc(1,ig),array4,n)   ! get data
                    IF(jflc(1,ig) /= 0) CALL stmarm(jflc(1,ig))            ! remove data
                    DO i=1,n,2                   ! average
                        xyplws(7,ig)=array4(1,i  )-array4(3,  i)
                        xyplws(8,ig)=array4(1,i+1)+array4(3,i+1)
                        xyplws(1,ig)=0.5*(xyplws(7,ig)+xyplws(8,ig))
                        xyplws(2,ig)=0.5*(array4(2,i)+array4(2,i+1))
                        xyplws(3,ig)=0.5*(xyplws(8,ig)-xyplws(7,ig))
                        xyplws(4,ig)=0.5*(array4(4,i)+array4(4,i+1))
                        CALL stmadp(jflc(1,ig),xyplws(1,ig))
                    END DO
                    nst(3,ig)=nst(3,ig)+nst(3,ig)
                END IF
            END IF
        END IF
    END IF
    RETURN
END SUBROUTINE gmpms

!> print XY data
SUBROUTINE gmprnt(ig)
    USE gmpdata

    IMPLICIT NONE
    INTEGER(mpi) :: iga
    INTEGER(mpi) :: igb
    INTEGER(mpi) :: igc
    INTEGER(mpi) :: j
    INTEGER(mpi) :: n
    INTEGER(mpi) :: na
    REAL(mps) :: wght
    
    INTEGER(mpi), INTENT(IN)                      :: ig
    
    IF(ig == 0) THEN
        iga=1
        igb=numgxy
    ELSE
        IF(ig <= 0.OR.ig > numgxy) RETURN
        iga=ig
        igb=ig
    END IF
    DO igc=iga,igb

        IF(igtp(igc) >= 1.AND.igtp(igc) <= 3) THEN
            WRITE(*,*) ' '
            WRITE(*,*) 'Store ',igc,': ',gtext(igc)
            IF(jflc(4,igc) == 0) THEN
                WRITE(*,*) '      stored n-tuples: ',jflc(3,igc)
            ELSE
                WRITE(*,*) '   stored n-tuples,  not-stored n-tuples: ',  &
                    jflc(3,igc),', ',jflc(4,igc)
            END IF
    
            CALL stmacp(jflc(1,igc),array,na) ! get all data
    
            DO n=1,na
                WRITE(*,102) n, array(1,n),array(2,n)
            END DO
    
        ELSE IF(igtp(igc) == 4) THEN
    
            WRITE(*,*) ' '
            WRITE(*,*) 'Store ',igc,': ',gtext(igc)
            IF(jflc(4,igc) == 0) THEN
                WRITE(*,*) '      stored n-tuples: ',jflc(3,igc)
            ELSE
                WRITE(*,*) '   stored n-tuples,  not-stored n-tuples: ',  &
                    jflc(3,igc),', ',jflc(4,igc)
            END IF
    
            CALL stmacp4(jflc(1,igc),array4,na) ! get all data
    
            DO n=1,na
                WRITE(*,102) n,(array4(j,n),j=1,4)
            END DO
    
        ELSE IF(igtp(igc) == 5) THEN
    
            CALL stmacp1(kflc(1,igc),array1,n) ! get data
            IF(kflc(1,igc) /= 0) CALL stmarm(kflc(1,igc))         ! remove data
            IF(nst(1,igc) == 1) THEN
                n=n+1
                array1(n)=y1(igc)
                nst(1,igc)=0 ! reset
            END IF
            IF(n /= 0) THEN
                xyplws(7,igc)=xyplws( 9,igc)
                xyplws(8,igc)=xyplws(10,igc)
                CALL rmesig(array1,n,xyplws(2,igc),xyplws(4,igc))
                wght=REAL(n,mps)/REAL(nst(3,igc)*kflc(5,igc),mps)
                xyplws(5,igc)=xyplws(5,igc)+xyplws(2,igc)*wght
                xyplws(6,igc)=xyplws(6,igc)+xyplws(4,igc)*wght
                xyplws(2,igc)=xyplws(5,igc)/(REAL(nst(2,igc),mps)+wght)
                xyplws(4,igc)=xyplws(6,igc)/(REAL(nst(2,igc),mps)+wght)
                xyplws(1,igc)=0.5*(xyplws(7,igc)+xyplws(8,igc))
                xyplws(3,igc)=0.5*(xyplws(8,igc)-xyplws(7,igc))
                CALL stmadp(jflc(1,igc),xyplws(1,igc))
            END IF
    
            WRITE(*,*) ' '
            WRITE(*,*) 'Store ',igc,': ',gtext(igc)
            IF(jflc(4,igc) == 0) THEN
                WRITE(*,*) '      stored n-tuples: ',jflc(3,igc)
            ELSE
                WRITE(*,*) '   stored n-tuples,  not-stored n-tuples: ',  &
                    jflc(3,igc),', ',jflc(4,igc)
            END IF
    
            CALL stmacp4(jflc(1,igc),array4,na) ! get all data
            DO n=1,na
                WRITE(*,102) n,(array4(j,n),j=1,4)
            END DO
        END IF
    END DO
    RETURN
102 FORMAT(i12,4G15.7)
    ! 103  FORMAT('       Index    ___X___       ___Y___     '/
    !     +       '       ----- -------------- --------------')
    ! 104  FORMAT('       Index    ___X___       ___Y___     ',
    !     +                   '    ___DX__       ___DY__     '/
    !     +       '       ----- -------------- --------------',
    !     +                   ' -------------- --------------')
END SUBROUTINE gmprnt

!> unit for output
SUBROUTINE gmplun(lunw)
    USE gmpdata

    IMPLICIT NONE

    INTEGER(mpi), INTENT(IN)                      :: lunw

    lun=lunw
    RETURN
END SUBROUTINE gmplun

!> write XY text file
SUBROUTINE gmpwrt(ig)
    USE gmpdata

    IMPLICIT NONE
    INTEGER(mpi) :: iga
    INTEGER(mpi) :: igb
    INTEGER(mpi) :: igc
    INTEGER(mpi) :: j
    INTEGER(mpi) :: n
    INTEGER(mpi) :: na
    REAL(mps) :: wght
    
    INTEGER(mpi), INTENT(IN)                      :: ig

    IF(lun <= 0) RETURN
    IF(ig == 0) THEN
        iga=1
        igb=numgxy
    ELSE
        IF(ig <= 0.OR.ig > numgxy) RETURN
        iga=ig
        igb=ig
    END IF
    DO igc=iga,igb
        IF(igtp(igc) == 5) THEN
    
            CALL stmacp1(kflc(1,igc),array1,n) ! get data
            IF(kflc(1,igc) /= 0) CALL stmarm(kflc(1,igc))         ! remove data
            IF(nst(1,igc) == 1) THEN
                n=n+1
                array1(n)=y1(igc)
                nst(1,igc)=0 ! reset
            END IF
            IF(n /= 0) THEN
                xyplws(7,igc)=xyplws( 9,igc)
                xyplws(8,igc)=xyplws(10,igc)
                CALL rmesig(array1,n,xyplws(2,igc),xyplws(4,igc))
                wght=REAL(n,mps)/REAL(nst(3,igc)*kflc(5,igc),mps)
                xyplws(5,igc)=xyplws(5,igc)+xyplws(2,igc)*wght
                xyplws(6,igc)=xyplws(6,igc)+xyplws(4,igc)*wght
                xyplws(2,igc)=xyplws(5,igc)/(REAL(nst(2,igc),mps)+wght)
                xyplws(4,igc)=xyplws(6,igc)/(REAL(nst(2,igc),mps)+wght)
                xyplws(1,igc)=0.5*(xyplws(7,igc)+xyplws(8,igc))
                xyplws(3,igc)=0.5*(xyplws(8,igc)-xyplws(7,igc))
                CALL stmadp(jflc(1,igc),xyplws(1,igc))
            END IF
    
        END IF
        IF(jflc(3,igc)+jflc(4,igc) /= 0) THEN
            WRITE(lun,204) ' '
            WRITE(lun,201) igc,lvers(igc),igtp(igc)
            WRITE(lun,204) gtext(igc)
            WRITE(lun,203) jflc(3,igc)+jflc(4,igc)
            IF(igtp(igc) >= 1.AND.igtp(igc) <= 3) THEN
                CALL stmacp(jflc(1,igc),array,na) ! get all data
                WRITE(lun,204) 'x-y'
                DO n=1,na
                    WRITE(lun,205) array(1,n),array(2,n)
                END DO
            ELSE IF(igtp(igc) == 4.OR.igtp(igc) == 5) THEN
                CALL stmacp4(jflc(1,igc),array4,na) ! get all data
                WRITE(lun,204) 'x-y-dx-dy'
                DO n=1,na
                    WRITE(lun,205) (array4(j,n),j=1,4)
                END DO
            END IF
            WRITE(lun,204) 'end of xy-data'
        END IF
    END DO
    RETURN
201 FORMAT('XY-Data ',i4,10X,'version ',i4,10X,'type',i2)
203 FORMAT(10X,'stored  not-stored ',2I10)
204 FORMAT(a)
205 FORMAT(3X,4G15.7)
END SUBROUTINE gmpwrt

!> storage manager data.
MODULE stmamod
    USE mpdef
    IMPLICIT NONE
    
    REAL(mps), DIMENSION(:,:), ALLOCATABLE :: tk    ! pair storage for data pairs
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: next    ! pointer
    INTEGER(mpi) :: iflc1     ! first and last index of free pairs
    INTEGER(mpi) :: iflc2     ! first and last index of free pairs
        
END MODULE stmamod

!> init/reset  storage
SUBROUTINE stmars(ndim)
    USE stmamod

    IMPLICIT NONE
    INTEGER(mpi) :: i

    INTEGER(mpi), INTENT(IN)                      :: ndim

    SAVE

    !INTEGER(mpi) :: jflc(5)         ! user array
    !     JFLC(1) = first used index
    !     JFLC(2) = last used index
    !     JFLC(3) = counter of used places
    !     JFLC(4) = counter of ignored
    !     JFLC(5) = limit for JFLC(3)
    !     ...
    !print *, ' stmars ndim ', ndim
    ALLOCATE (tk(2,ndim))
    ALLOCATE (next(ndim))
    
    DO i=1,ndim
        next(i)=i+1   ! pointer to next free location
        tk(1,i)=0.0   ! reset
        tk(2,i)=0.0
    END DO
    next(ndim)=0   ! ... and end pointer
    iflc1=1        ! index first free pair
    iflc2=ndim     ! index last free pair
    RETURN
END SUBROUTINE stmars                          ! init/

!> store pair (X,Y)
SUBROUTINE stmapr(jflc,x,y)
    USE stmamod

    IMPLICIT NONE
    INTEGER(mpi) :: ifre

    INTEGER(mpi), INTENT(INOUT)                   :: jflc(5)
    REAL(mps), INTENT(IN)                         :: x
    REAL(mps), INTENT(IN)                         :: y
        
    ifre=iflc1                   ! index of free place
    IF(ifre == 0.OR.jflc(3) >= jflc(5)) THEN ! overflow
        jflc(4)=jflc(4)+1
    ELSE
        iflc1=next(ifre)          ! pointer to new free location
        IF(jflc(1) == 0) THEN     ! first item
            jflc(1)=ifre
        ELSE
            next(jflc(2))=ifre
        END IF
        next(ifre)=0
        jflc(2)=ifre              ! last index
        jflc(3)=jflc(3)+1         ! counter
        tk(1,ifre)=x
        tk(2,ifre)=y
    END IF
    RETURN
END SUBROUTINE stmapr

!> store double pair
SUBROUTINE stmadp(jflc,four)
    USE stmamod

    IMPLICIT NONE
    INTEGER(mpi) :: ifrea
    INTEGER(mpi) :: ifreb

    INTEGER(mpi), INTENT(INOUT)                   :: jflc(5)
    REAL(mps), INTENT(IN)                         :: four(4)
    
    ifrea=iflc1                  ! index of 1. free place
    IF(ifrea == 0) THEN ! overflow
        jflc(4)=jflc(4)+1
    ELSE
        ifreb=next(iflc1)         ! index of 2. free place
        IF(ifreb == 0.OR.jflc(3) >= 2*jflc(5)) THEN ! overflow
            jflc(4)=jflc(4)+1
        ELSE
            iflc1=next(ifreb)      ! pointer to new free location
            IF(jflc(1) == 0) THEN  ! first item
                jflc(1)=ifrea
            ELSE
                next(jflc(2))=ifrea
            END IF
            next(ifreb)=0
            jflc(2)=ifreb          ! last index
            jflc(3)=jflc(3)+1      ! counter
            tk(1,ifrea)=four(1)
            tk(2,ifrea)=four(2)
            tk(1,ifreb)=four(3)
            tk(2,ifreb)=four(4)
        END IF
    END IF
    RETURN
END SUBROUTINE stmadp

!> copy (cp) all pairs to array
SUBROUTINE stmacp(jflc,array,n)
    USE stmamod

    IMPLICIT NONE
    INTEGER(mpi) :: ind

    INTEGER(mpi), INTENT(IN)                      :: jflc(5)
    INTEGER(mpi), INTENT(OUT)                     :: n
    REAL(mps), INTENT(OUT)                        :: array(2,*)
    
    n=0
    ind=jflc(1)
10  IF(ind == 0) RETURN
    n=n+1
    array(1,n)=tk(1,ind)
    array(2,n)=tk(2,ind)
    ind=next(ind)
    GO TO 10
END SUBROUTINE stmacp

!> copy (cp) all pairs to array4
SUBROUTINE stmacp4(jflc,array,n)
    USE stmamod

    IMPLICIT NONE
    INTEGER(mpi) :: ind1
    INTEGER(mpi) :: ind2

    INTEGER(mpi), INTENT(IN)                      :: jflc(5)
    INTEGER(mpi), INTENT(OUT)                     :: n
    REAL(mps), INTENT(OUT)                        :: array(4,*)
    
    n=0
    ind1=jflc(1)
10  IF(ind1 == 0) RETURN
    ind2=next(ind1) ! 2nd pair
    IF(ind2 == 0) RETURN
    n=n+1
    array(1,n)=tk(1,ind1)
    array(2,n)=tk(2,ind1)
    array(3,n)=tk(1,ind2)
    array(4,n)=tk(2,ind2)
    ind1=next(ind2)
    GO TO 10
END SUBROUTINE stmacp4

!> copy (cp) all pairs to array1
SUBROUTINE stmacp1(jflc,array,n)
    USE stmamod

    IMPLICIT NONE
    INTEGER(mpi) :: ind

    INTEGER(mpi), INTENT(IN)                      :: jflc(5)
    INTEGER(mpi), INTENT(OUT)                     :: n
    REAL(mps), INTENT(OUT)                        :: array(*)
    
    n=0
    ind=jflc(1)
10  IF(ind == 0) RETURN
    n=n+1
    array(n)=tk(1,ind)
    n=n+1
    array(n)=tk(2,ind)
    ind=next(ind)
    GO TO 10
END SUBROUTINE stmacp1

!> remove (rm) stored pairs
SUBROUTINE stmarm(jflc)
    USE stmamod

    IMPLICIT NONE
    INTEGER(mpi) :: j

    INTEGER(mpi), INTENT(INOUT)                   :: jflc(5)
    
    next(iflc2)=jflc(1)          ! connect to free space
    iflc2=jflc(2)                ! new last free index
    DO j=1,4
        jflc(j)=0
    END DO
END SUBROUTINE stmarm                          ! init/

!> robust mean and sigma
SUBROUTINE rmesig(x,n,xloc,xsca)
    USE mpdef

    IMPLICIT NONE
    INTEGER(mpi) :: i
    !     robust determination of location and scale parameter,
    !        for Gaussian data: location=mean and scale=standard deviation
    !     XLOC = median of X_i            (N values in array X(N))
    !     XCSA = median of | X_i - XLOC |, times 1.4826

    INTEGER(mpi), INTENT(IN)                      :: n
    REAL(mps), INTENT(IN OUT)                     :: x(n) ! input array, modified
    REAL(mps), INTENT(OUT)                        :: xloc
    REAL(mps), INTENT(OUT)                        :: xsca
    SAVE
    !     ...
    xloc=0.0
    xsca=0.0
    IF(n <= 0) RETURN
    CALL heapf(x,n)  ! sort
    xloc=0.5*(x((n+1)/2)+x((n+2)/2))         ! location
    DO i=1,n
        x(i)=ABS(x(i)-xloc)
    END DO
    CALL heapf(x,n) ! sort
    xsca=1.4826*0.5*(x((n+1)/2)+x((n+2)/2))  ! dispersion
END SUBROUTINE rmesig



