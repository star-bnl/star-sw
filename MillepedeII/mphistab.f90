
! Code converted using TO_F90 by Alan Miller
! Date: 2012-03-16  Time: 11:07:45

!> \file
!! Histogramming package.
!!
!! \author Volker Blobel, University Hamburg, 2005-2009 (initial Fortran77 version)
!! \author Claus Kleinwort, DESY (maintenance and developement)
!!
!! \copyright
!! Copyright (c) 2009 - 2015 Deutsches Elektronen-Synchroton,
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
!!      CALL STMARS                 !! init/reset  storage manager
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

SUBROUTINE hmpdef(ih,xa,xb,text)           ! book, reset histogram
    USE mpdef

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpi) :: iha
    INTEGER(mpi) :: ihb
    INTEGER(mpi) :: ihc
    INTEGER(mpi) :: ix
    INTEGER(mpi) :: j
    INTEGER(mpi) :: lun
    INTEGER(mpi) :: lunw
    INTEGER(mpi) :: nbin
    INTEGER(mpi) :: nn
    REAL(mps) :: x
    REAL(mps) :: xcent
    REAL(mps) :: xmean
    REAL(mps) :: xsigm
    !     book millepede histogram, 120 bins

    INTEGER(mpi), INTENT(IN)                      :: ih
    REAL(mps), INTENT(IN)                         :: xa
    REAL(mps), INTENT(IN)                         :: xb
    CHARACTER (LEN=*), INTENT(IN)            :: text
    INTEGER(mpi), PARAMETER :: numhis=15
    INTEGER(mpi) :: inhist(120,numhis)
    INTEGER(mpi) ::jnhist(5,numhis)
    INTEGER(mpi) ::khist(numhis)
    REAL(mps) :: fnhist(120,numhis)
    EQUIVALENCE (inhist(1,1),fnhist(1,1))
    INTEGER(mpi) :: kvers(numhis)
    REAL(mps) :: xl(6,numhis)
    REAL(mpd):: dl(2,numhis)
    CHARACTER (LEN=60):: htext(numhis)
    SAVE
    DATA khist/numhis*0/,lun/7/
    !     ...
    IF(ih <= 0.OR.ih > numhis) RETURN
    !      IF(XA.EQ.XB)                RETURN
    DO i=1,120
        inhist(i,ih)=0
    END DO
    DO j=1,5
        jnhist(j,ih)=0
    END DO
    xl(1,ih)=xa
    xl(2,ih)=xb
    xl(3,ih)=0.0
    IF(xa /= xb) xl(3,ih)=120.0/(xb-xa)
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

    ENTRY hmpldf(ih,text)                   ! book, reset log histogram
    IF(ih <= 0.OR.ih > numhis) RETURN
    DO i=1,120
        inhist(i,ih)=0
    END DO
    DO j=1,5
        jnhist(j,ih)=0
    END DO
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

    ENTRY hmpent(ih,x)                      ! entry flt.pt.
    IF(ih <= 0.OR.ih > numhis) RETURN
    IF(khist(ih) /= 1)          RETURN
    IF(jnhist(4,ih) >= 2147483647) RETURN
    jnhist(4,ih)=jnhist(4,ih)+1      ! count
    IF(jnhist(4,ih) <= 120) THEN
        fnhist(jnhist(4,ih),ih)=x     ! store value
        IF(jnhist(4,ih) == 120) THEN
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
    IF(i > 120) j=3
    jnhist(j,ih)=jnhist(j,ih)+1
    xl(4,ih)=MIN(xl(4,ih),x)
    xl(5,ih)=MAX(xl(5,ih),x)
    IF(j /= 2) RETURN
    inhist(i,ih)=inhist(i,ih)+1
    dl(1,ih)=dl(1,ih)+ x-xl(6,ih)
    dl(2,ih)=dl(2,ih)+(x-xl(6,ih))**2
    RETURN

    ENTRY hmplnt(ih,ix)                     ! entry integer
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
        IF(i > 120) j=3
        IF(j == 2)  inhist(i,ih)=inhist(i,ih)+1
        jnhist(j,ih)=jnhist(j,ih)+1
    END IF
    RETURN

    ENTRY hmprnt(ih)                        ! print, content vert
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
                    CALL pfvert(120,fnhist(1,ihc))
                END IF
                IF(jnhist(2,ihc) /= 0) THEN        ! integer content
                    CALL pivert(120,inhist(1,ihc))
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

    ENTRY hmplun(lunw)                      ! unit for output
    lun=lunw
    RETURN

    ENTRY hmpwrt(ih)                        ! write histogram text file
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
            nbin=120
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

201 FORMAT('Histogram ',i4,10X,'version ',i4,10X,'type',i2)
202 FORMAT(10X,' bins, limits ',i4,2G15.5)
203 FORMAT(10X,'out-low inside out-high ',3I10)
204 FORMAT(a)
205 FORMAT('minmax',2E15.7)
206 FORMAT('meansigma',2E15.7)

219 FORMAT(4E15.7)
END SUBROUTINE hmpdef

SUBROUTINE hmpmak(inhist,fnhist,jnhist,xl,dl) ! hist scale from data
    USE mpdef

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpi) :: j
    INTEGER(mpi) :: k
    INTEGER(mpi) :: nn
    REAL(mps) :: x
    REAL(mps) :: xa
    REAL(mps) :: xb

    INTEGER(mpi), INTENT(OUT)                     :: inhist(120)
    REAL(mps), INTENT(IN)                         :: fnhist(120)
    INTEGER(mpi), INTENT(IN OUT)                  :: jnhist(5)
    REAL(mps), INTENT(IN OUT)                     :: xl(6)
    REAL(mpd), INTENT(OUT)            :: dl(2)
    REAL(mps) :: cphist(120)



    SAVE
    !     ...
    nn=jnhist(4)
    !      WRITE(*,*) 'HMPMAK: NN,JNHIST(5)',NN,JNHIST(5)
    IF(nn == 0.OR.jnhist(5) /= 0) RETURN
    jnhist(5)=1
    DO i=1,nn
        !       WRITE(*,*) 'copy ',I,FNHIST(I)
        cphist(i)=fnhist(i)
    END DO
    CALL heapf(cphist,nn)
    IF(xl(3) == 0.0) THEN
        CALL bintab(cphist,nn,xa,xb)
        xl(1)=xa
        xl(2)=xb
        xl(3)=0.0
        IF(xa /= xb) xl(3)=120.0/(xb-xa)
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
        IF(i > 120) j=3
        jnhist(j)=jnhist(j)+1
        IF(j == 2) THEN
            inhist(i)=inhist(i)+1
            dl(1)=dl(1)+ x-xl(6)
            dl(2)=dl(2)+(x-xl(6))**2
        END IF
    END DO
END SUBROUTINE hmpmak




SUBROUTINE bintab(tab,n,xa,xb)             ! hist scale from data
    USE mpdef

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
    REAL(mps) :: rat
    REAL(mps) :: x1
    REAL(mps) :: x2
    REAL(mps) :: xx
    !     Bin limits XA and XB from TAB(N)

    REAL(mps), INTENT(IN)                         :: tab(n)
    INTEGER(mpi), INTENT(IN)                      :: n
    REAL(mps), INTENT(OUT)                        :: xa
    REAL(mps), INTENT(OUT)                        :: xb

    REAL(mps) :: bin(10)
    DATA bin/1.0,1.5,2.0,3.0,4.0,5.0,8.0,10.0,15.0,20.0/
    SAVE
    !     ...

    CALL heapf(tab,n) ! reduced statistic
    !      WRITE(*,*) ' '
    !      WRITE(*,*) 'Sorted ',(TAB(I),I=1,N)
    IF(n < 100) THEN
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
    !      WRITE(*,*) 'X1,X2,DX ',X1,X2,DX
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
        !       WRITE(*,*) 'Bin ',I,N2,N2*DD,X2
10      IF(n2-n1 < 6) THEN
            IF(n1 /= 0) n1=n1-1
            IF(n2-n1 < 6.AND.n2 /= 0) n2=n2+1
            GO TO 10
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

SUBROUTINE kprint(lun,list,n)              ! print integer array
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
    INTEGER(mpi), INTENT(IN)                      :: list(n)
    INTEGER(mpi), INTENT(IN)                      :: n
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

SUBROUTINE gmpdef(ig,ityp,text)            ! book, reset XY storage
    USE mpdef

    IMPLICIT NONE
    REAL(mps) :: dx
    REAL(mps) :: dy
    INTEGER(mpi) :: i
    INTEGER(mpi) :: iga
    INTEGER(mpi) :: igb
    INTEGER(mpi) :: igc
    INTEGER(mpi) :: j
    INTEGER(mpi) :: lun
    INTEGER(mpi) :: lunw
    INTEGER(mpi) :: n
    INTEGER(mpi) :: na
    REAL(mps) :: wght
    REAL(mps) :: x
    REAL(mps) :: y
    REAL(mps) :: y1
    !     ITYP = 1  X,Y     as dots
    !          = 2  X,Y     as line
    !          = 3  X,Y     as line and dots
    !          = 4  X,Y, DX,DY symbols

    INTEGER(mpi), INTENT(IN)                      :: ig
    INTEGER(mpi), INTENT(IN)                      :: ityp
    CHARACTER (LEN=*), INTENT(IN)            :: text
    INTEGER(mpi), PARAMETER :: narr=1000
    REAL(mps) :: array(2,narr)
    REAL(mps) ::array4(4,narr/2)
    REAL(mps) ::array1(narr+narr)
    REAL(mps) ::four(4)
    EQUIVALENCE (array(1,1),array4(1,1),array1(1))
    INTEGER(mpi), PARAMETER :: numgxy=10
    INTEGER(mpi), PARAMETER :: nlimit=500
    INTEGER(mpi) :: nstr(numgxy)
    INTEGER(mpi) ::igtp(numgxy)
    INTEGER(mpi) ::lvers(numgxy)
    INTEGER(mpi) ::nst(3,numgxy)
    REAL(mps) :: xyplws(10,numgxy)
    INTEGER(mpi) :: jflc(5,numgxy)
    INTEGER(mpi) ::kflc(5,numgxy)
    !     JFLC(1,.) = first used index
    !     JFLC(2,.) = last used index
    !     JFLC(3,.) = counter of used places
    !     JFLC(4,.) = counter of ignored
    !     JFLC(5,.) = limit for JFLC(3)
    CHARACTER (LEN=60):: gtext(numgxy)

    LOGICAL:: start
    SAVE
    DATA start/.TRUE./,lun/7/
    DATA nstr/numgxy*0/   ! by GF
    !     ...
    IF(start) THEN
        start=.FALSE.
        CALL stmars    ! initialize storage
        DO i=1,numgxy
            DO j=1,5
                jflc(j,i)=0
                kflc(j,i)=0
            END DO
        END DO
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
        kflc(5,ig)=narr
        nst(1,ig)=0
        nst(2,ig)=0
        nst(3,ig)=1
        DO j=1,10
            xyplws(j,ig)=0.0
        END DO
    END IF
    RETURN

    ENTRY gmpxy(ig,x,y)                     ! add (X,Y) pair
    IF(ig  < 1.OR.ig > numgxy) RETURN        ! check argument IG
    IF(igtp(ig) < 1.OR.igtp(ig) > 3) RETURN   ! check type
    CALL stmapr(jflc(1,ig),x,y)
    RETURN

    ENTRY gmpxyd(ig,x,y,dx,dy)              ! add (X,Y,DX,DY)
    IF(ig  < 1.OR.ig > numgxy) RETURN        ! check argument IG
    IF(igtp(ig) /= 4) RETURN
    four(1)=x
    four(2)=y
    four(3)=dx
    four(4)=dy
    CALL stmadp(jflc(1,ig),four)
    RETURN

    ENTRY gmpms(ig,x,y)                     ! mean sigma(X) from Y
    !     mean sigma from Y, as a function of X
    !      WRITE(*,*) 'GMPMS ',IG,X,Y

    IF(ig  < 1.OR.ig > numgxy) RETURN        ! check argument IG
    IF(igtp(ig) /= 5) RETURN

    xyplws(10,ig)=x  ! last X  coordinate
    IF(nst(1,ig) == 0) THEN
        y1=y
        nst(1,ig)=1
        IF(kflc(3,ig) == 0) xyplws(9,ig)=x        ! start coordinate
    ELSE
        nst(1,ig)=0
        CALL stmapr(kflc(1,ig),y1,y) ! store pair
        IF(kflc(3,ig) >= kflc(5,ig)) THEN
            CALL stmacp(kflc(1,ig),array,n) ! get data
            CALL stmarm(kflc(1,ig))         ! remove data
            n=n+n
            CALL rmesig(array,n,xyplws(2,ig),xyplws(4,ig))
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
                    CALL stmacp(jflc(1,ig),array4,n)   ! get data
                    n=n/2
                    CALL stmarm(jflc(1,ig))            ! remove data
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

    ENTRY gmprnt(ig)                        ! print XY data
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
    
            CALL stmacp(jflc(1,igc),array,na) ! get all data
            na=na/2
    
            DO n=1,na
                WRITE(*,102) n,(array4(j,n),j=1,4)
            END DO
    
        ELSE IF(igtp(igc) == 5) THEN
    
            CALL stmacp(kflc(1,igc),array,n) ! get data
            CALL stmarm(kflc(1,igc))         ! remove data
            n=n+n
            IF(nst(1,igc) == 1) THEN
                n=n+1
                array1(n)=y1
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
    
            CALL stmacp(jflc(1,igc),array,na) ! get all data
            na=na/2
            DO n=1,na
                WRITE(*,102) n,(array4(j,n),j=1,4)
            END DO
        END IF
    END DO
    RETURN

    ENTRY gmplun(lunw)                      ! unit for output
    lun=lunw
    RETURN

    ENTRY gmpwrt(ig)                        ! write XY text file
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
    
            CALL stmacp(kflc(1,igc),array,n) ! get data
            CALL stmarm(kflc(1,igc))         ! remove data
            n=n+n
            IF(nst(1,igc) == 1) THEN
                n=n+1
                array1(n)=y1
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
            CALL stmacp(jflc(1,igc),array,na) ! get all data
            IF(igtp(igc) >= 1.AND.igtp(igc) <= 3) THEN
                WRITE(lun,204) 'x-y'
                DO n=1,na
                    WRITE(lun,205) array(1,n),array(2,n)
                END DO
            ELSE IF(igtp(igc) == 4.OR.igtp(igc) == 5) THEN
                WRITE(lun,204) 'x-y-dx-dy'
                na=na/2
                DO n=1,na
                    WRITE(lun,205) (array4(j,n),j=1,4)
                END DO
            END IF
            WRITE(lun,204) 'end of xy-data'
        END IF
    END DO
102 FORMAT(i12,4G15.7)
    ! 103  FORMAT('       Index    ___X___       ___Y___     '/
    !     +       '       ----- -------------- --------------')
    ! 104  FORMAT('       Index    ___X___       ___Y___     ',
    !     +                   '    ___DX__       ___DY__     '/
    !     +       '       ----- -------------- --------------',
    !     +                   ' -------------- --------------')
201 FORMAT('XY-Data ',i4,10X,'version ',i4,10X,'type',i2)
203 FORMAT(10X,'stored  not-stored ',2I10)
204 FORMAT(a)
205 FORMAT(3X,4G15.7)
END SUBROUTINE gmpdef


SUBROUTINE stmars                          ! init/reset  storage
    USE mpdef

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpi) :: ifre
    INTEGER(mpi) :: ifrea
    INTEGER(mpi) :: ifreb
    INTEGER(mpi) :: ind
    INTEGER(mpi) :: j
    INTEGER(mpi) :: n
    REAL(mps) :: x
    REAL(mps) :: y
    INTEGER(mpi), PARAMETER :: ndim=5000   ! storage dimension, should be NUMGXY*NLIMIT
    REAL(mps) :: tk(2,ndim)    ! pair storage for data pairs
    INTEGER(mpi) :: next(ndim)    ! pointer
    INTEGER(mpi) :: iflc1     ! first and last index of free pairs
    INTEGER(mpi) ::iflc2     ! first and last index of free pairs
    SAVE

    REAL(mps) :: four(4) ! double_pair, copy array
    REAL(mps) ::array(2,*) ! double_pair, copy array
    INTEGER(mpi) :: jflc(5)         ! user array
    !     JFLC(1) = first used index
    !     JFLC(2) = last used index
    !     JFLC(3) = counter of used places
    !     JFLC(4) = counter of ignored
    !     JFLC(5) = limit for JFLC(3)
    !     ...
    DO i=1,ndim
        next(i)=i+1   ! pointer to next free location
        tk(1,i)=0.0   ! reset
        tk(2,i)=0.0
    END DO
    next(ndim)=0   ! ... and end pointer
    iflc1=1        ! index first free pair
    iflc2=ndim     ! index last free pair
    RETURN

    ENTRY stmapr(jflc,x,y)                  ! store pair (X,Y)
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

    ENTRY stmadp(jflc,four)                 ! store double pair
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

    ENTRY stmacp(jflc,array,n)              ! copy (cp) all pairs to array
    n=0
    ind=jflc(1)
10  IF(ind == 0) RETURN
    n=n+1
    array(1,n)=tk(1,ind)
    array(2,n)=tk(2,ind)
    ind=next(ind)
    GO TO 10

    ENTRY stmarm(jflc)                      ! remove (rm) stored paiirs
    next(iflc2)=jflc(1)          ! connect to free space
    iflc2=jflc(2)                ! new last free index
    DO j=1,4
        jflc(j)=0
    END DO
END SUBROUTINE stmars                          ! init/

SUBROUTINE rmesig(x,n,xloc,xsca)           ! robust mean and sigma
    USE mpdef

    IMPLICIT NONE
    INTEGER(mpi) :: i
    !     robust determination of location and scale parameter,
    !        for Gaussian data: location=mean and scale=standard deviation
    !     XLOC = median of X_i            (N values in array X(N))
    !     XCSA = median of | X_i - XLOC |, times 1.4826

    REAL(mps), INTENT(IN OUT)                     :: x(n) ! input array, modified
    INTEGER(mpi), INTENT(IN)                      :: n
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



