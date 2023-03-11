
! Code converted using TO_F90 by Alan Miller
! Date: 2012-03-16  Time: 11:09:16

!> \file
!! Analyse text string.
!!
!! \author Volker Blobel, University Hamburg, 2005-2009 (initial Fortran77 version)
!! \author Claus Kleinwort, DESY (maintenance and developement)
!!
!! \copyright
!! Copyright (c) 2009 - 2019 Deutsches Elektronen-Synchroton,
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

!> Keyword position.
MODULE mptext
    USE mpdef

    IMPLICIT NONE
    SAVE
    INTEGER(mpi) :: keya   !< start (position) of first keyword
    INTEGER(mpi) :: keyb   !< end (position) of first keyword
    INTEGER(mpi) :: keyc   !< end (position) of last keyword

END MODULE mptext

!> Translate text.
!!
!! Translate TEXT into arrays of double precision numbers DNUMS(NUMS).
!! Text preceeding numbers is TEXT(KEYA:KEYB), if KEYB >= KEYA.
!!
!! \param[in]   text  text
!! \param[out]  nums  number of numbers found
!! \param[out]  dnum  array of numbers found

SUBROUTINE ratext(text,nums,dnum)
    USE mptext

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpi) :: ia
    INTEGER(mpi) :: ib
    INTEGER(mpi) :: ic
    INTEGER(mpi) :: ich
    INTEGER(mpi) :: icl
    INTEGER(mpi) :: icode
    INTEGER(mpi) :: j
    INTEGER(mpi) :: k

    INTEGER(mpi) :: lent
    INTEGER(mpi) :: num

    CHARACTER (LEN=*), INTENT(IN)            :: text
    INTEGER(mpi), INTENT(OUT)                     :: nums
    REAL(mpd), INTENT(OUT)            :: dnum(*)

    INTEGER(mpi) :: last ! last non-blank character
    INTEGER(mpi), PARAMETER :: ndim=1000
    INTEGER(mpi), DIMENSION(2,ndim):: icd
    CHARACTER (LEN=1) :: ch
    REAL(mpd) :: dic(ndim)
    REAL(mpd) :: dumber
    INTEGER(mpi) :: icdt(ndim)
    SAVE
    !     ...
    nums=0
    last=0
    keya=0
    keyb=0
    keyc=0
    IF(text(1:1) == '*') RETURN
    num=ICHAR('0')
    lent=0
    last=0
    DO i=1,LEN(text)        ! find comment and end
        IF(lent == 0.AND.(text(i:i) == '!'.OR.text(i:i) == '%')) lent=i
        IF(text(i:i) /= ' ') last=i
    END DO
    IF(lent == 0) lent=last+1
    icd(1,1)=lent

    j=1
    icdt(1)=0
    icl=0
    DO i=1,lent-1
        ch =text(i:i)
        ich=ICHAR(ch)
        ic=0
        IF(ch == '.') ic=1
        IF(ch == '+') ic=2
        IF(ch == '-') ic=3
        IF(ch == 'E') ic=4
        IF(ch == 'D') ic=4
        IF(ch == 'e') ic=4
        IF(ch == 'd') ic=4
        IF(ic > 0) THEN
            j=j+1
            icd(1,j)=i
            icd(2,j)=i
            icdt(j)=ic
        ELSE
            ic=6
            IF(ich >= num.AND.ich <= num+9) ic=5  ! digit
            IF(ic /= icl) THEN
                j=j+1
                icd(1,j)=i
                icdt(j)=ic
            END IF
            icd(2,j)=i
        END IF
        icl=ic ! previous IC
    END DO
    icdt(j+1)=0

    DO i=1,j                  ! define number
        IF(icdt(i) == 5) THEN
            dumber=0.0D0
            DO k=icd(1,i),icd(2,i)
                dumber=10.0_mpd*dumber+REAL(ICHAR(text(k:k))-num,mpd)
            END DO
            dic(i)=dumber
        END IF
    END DO
    icdt(j+1)=0

    DO i=2,j                  ! get dots
        IF(icdt(i) == 1) THEN
            icode=0
            IF(icdt(i-1) == 5.AND.icd(2,i-1)+1 == icd(1,i)) icode=1
            IF(icdt(i+1) == 5.AND.icd(1,i+1)-1 == icd(2,i)) icode=icode+2
            IF(icode == 1) THEN            ! 123.
                icd(2,i-1)=icd(2,i)
                icdt(i)=0
            ELSE IF(icode == 2) THEN       ! .456
                dic(i)=10.0D0**(icd(1,i+1)-icd(2,i+1)-1)*dic(i+1)
                icdt(i)=5
                icd(2,i)=icd(2,i+1)
                icdt(i+1)=0
            ELSE IF(icode == 3) THEN       ! 123.456
                dic(i-1)=dic(i-1)+ 10.0D0**(icd(1,i+1)-icd(2,i+1)-1)*dic(i+1)
                icd(2,i-1)=icd(2,i+1)
                icdt(i)=0
                icdt(i+1)=0
            END IF
        END IF
    END DO

    k=1                         ! remove blanks, compress
    DO i=2,j
        IF(icdt(i) == 6.AND.text(icd(1,i):icd(2,i)) == ' ') icdt(i)=0
        IF(icdt(i) /= 0) THEN
            k=k+1
            icd(1,k)=icd(1,i)
            icd(2,k)=icd(2,i)
            icdt(k)=icdt(i)
            dic(k)=dic(i)
        END IF
    END DO
    j=k

    DO i=2,j-1
        IF(icdt(i) == 2.OR.icdt(i) == 3) THEN   !  +-
            IF(icdt(i+1) == 5) THEN
                icd(1,i+1)=icd(1,i)
                IF(icdt(i) == 3) dic(i+1)=-dic(i+1)
                icdt(i)=0
            END IF
        END IF
    END DO

    k=1                         ! compress
    DO i=2,j
        IF(icdt(i) == 6.AND.text(icd(1,i):icd(2,i)) == ' ') icdt(i)=0
        IF(icdt(i) /= 0) THEN
            k=k+1
            icd(1,k)=icd(1,i)
            icd(2,k)=icd(2,i)
            icdt(k)=icdt(i)
            dic(k)=dic(i)
        END IF
    END DO
    j=k

    DO i=2,j-1
        IF(icdt(i) == 4) THEN        ! E or D
            IF(icdt(i-1) == 5.AND.icdt(i+1) == 5) THEN
                icd(2,i-1)=icd(2,i+1)
                dic(i-1)=dic(i-1)*10.0D0**dic(i+1)
                icdt(i)=0
                icdt(i+1)=0
            END IF
        END IF
    END DO

    nums=0                         ! compress
    DO i=1,j
        IF(icdt(i) == 5) THEN
            nums=nums+1
            icd(1,nums)=icd(1,i)
            icd(2,nums)=icd(2,i)
            dnum(nums)=dic(i)
        END IF
    END DO

    ! range of keyword (and optional text argument)
    ia=0
    ib=0
    ic=0
    k=0
    DO i=1,icd(1,1)-1
        ! (still) leading blanks ?
        IF(ia == 0) THEN
            IF(text(i:i) /= ' ') THEN
                ia=i ! first non blank char
            ELSE
                CYCLE ! skip
            END IF
        END IF
        ! non blank char ?
        IF(text(i:i) /= ' ')  THEN
            ic=i ! last non blank char
        ELSE
            k=i ! new blank
        END IF
        IF(k == 0) ib=i ! last non blank char in keyword
    END DO
    keya=ia
    keyb=ib
    keyc=ic
END SUBROUTINE ratext

!> Analyse text range.
!!
!! \param[in]  text  text
!! \param[out] ia    index of first non-blank character, or =1
!! \param[out] ib    index of last non-blank character,  or =0 - comment excluded
!! \param[out] nab   index of last non-blank character (=0 for blank text)

SUBROUTINE rltext(text,ia,ib,nab)
    USE mpdef

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpi) :: lim

    CHARACTER (LEN=*), INTENT(IN)            :: text
    INTEGER(mpi), INTENT(OUT)                     :: ia
    INTEGER(mpi), INTENT(OUT)                     :: ib
    INTEGER(mpi), INTENT(OUT)                     :: nab

    SAVE
    !     ...
    ia=0
    ib=0
    nab=0
    lim=0
    DO i=1,LEN(text)
        IF(text(i:i) /= ' ') nab=i
        IF((i == 1.AND.text(1:1) == '*').OR.text(i:i) == '!') THEN
            IF(lim == 0) lim=i
        END IF
    END DO
    IF(lim == 0) THEN
        lim=nab
    ELSE
        lim=lim-1
    END IF
    DO i=1,lim
        IF(ia == 0.AND.text(i:i) /= ' ') ia=i
        IF(text(i:i) /= ' ') ib=i
    END DO
END SUBROUTINE rltext

!> Approximate string matching.
!!
!! Approximate (parallel) string matching - case insensitive.
!! Return number of matching characters (in same order) in strings PAT and TEXT,
!! and number NPAT, NTEXT of characters of string PAT and string TEXT.
!! Strings are considered from first to last non-blank character.
!!
!! Example:
!!
!!      MATCH = MATINT(' keYs ','keyWO RD',NPAT,NTEXT)
!!      returns MATCH=3, NPAT=4, NTEXT=8
!!
!! \param[in]   pat    pattern
!! \param[in]   text   text
!! \param[out]  npat   number of characters in pattern
!! \param[out]  ntext  number of characters in text
!! \return      number of matching characters in pattern and text

INTEGER(mpi) FUNCTION matint(pat,text,npat,ntext)
    USE mpdef

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpi) :: ic
    INTEGER(mpi) :: ip
    INTEGER(mpi) :: ipa
    INTEGER(mpi) :: ipb
    INTEGER(mpi) :: ita
    INTEGER(mpi) :: itb
    INTEGER(mpi) :: j
    INTEGER(mpi) :: jc
    INTEGER(mpi) :: jt
    INTEGER(mpi) :: last

    CHARACTER (LEN=*), INTENT(IN) :: pat
    CHARACTER (LEN=*), INTENT(IN) :: text
    INTEGER(mpi), INTENT(OUT) :: npat
    INTEGER(mpi), INTENT(OUT) :: ntext

    LOGICAL :: start                        ! for case conversion
    CHARACTER (LEN=26) :: chu
    CHARACTER (LEN=26) :: chl
    INTEGER(mpi) :: nj(0:255)
    SAVE
    DATA  chu/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
    DATA  chl/'abcdefghijklmnopqrstuvwxyz'/
    DATA  start/.TRUE./
    DATA nj/256*0/
    !     ...
    IF(start) THEN
        start=.FALSE.
        DO j=0,255
            nj(j)=j
        END DO
        DO i=1,26
            nj(ICHAR(chl(i:i)))=ICHAR(chu(i:i))
        END DO
    END IF
    !     ...
    matint=0
    ntext=0
    DO i=1,LEN(text)               ! find indices ITA...ITB
        IF(text(i:i) /= ' ') GO TO 10
    END DO
    GO TO 15
10  ita=i
    DO i=ita,LEN(text)
        IF(text(i:i) /= ' ') itb=i
    END DO
    ntext=itb-ita+1               ! number of charcaters in TEXT

15  npat=0
    DO i=1,LEN(pat)               ! find indices IPA...IPB
        IF(pat(i:i) /= ' ') GO TO 20
    END DO
    RETURN
20  ipa=i
    DO i=ipa,LEN(pat)
        IF(pat(i:i) /= ' ') ipb=i
    END DO
    npat=ipb-ipa+1
 
    ! parallel matching
    ip=ipa
    jt=ita
    last=0
    DO WHILE (ip <= ipb.AND.jt <= itb)
        jc=nj(ICHAR(text(jt:jt)))
        ic=nj(ICHAR(pat(ip:ip)))
        IF (ic == jc) THEN ! match, increment both
            matint=matint+1
            ip=ip+1
            jt=jt+1
        ELSE ! check remaining length
            IF (ipb-ip == itb-jt) THEN ! equal, increment other than last
                ip=ip+last
                last=1-last ! 'invert' last
                jt=jt+last
            ELSE IF (ipb-ip > itb-jt) THEN ! increment ip (remaing pattern is larger)
                ip=ip+1
                last=0 ! ip was incremented last
            ELSE ! increment jt (remaing text is larger)
                jt=jt+1
                last=1 ! jt was incremented last
            ENDIF  
        END IF
    END DO
END FUNCTION matint


