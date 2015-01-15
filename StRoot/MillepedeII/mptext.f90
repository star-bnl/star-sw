
! Code converted using TO_F90 by Alan Miller
! Date: 2012-03-16  Time: 11:09:16

!> \file
!! Analyse text string.

!> Keyword position.
MODULE mptext
    USE mpdef

    IMPLICIT NONE
    SAVE
    INTEGER(mpi) :: keya   !< start (position) of keyword
    INTEGER(mpi) :: keyb   !< end (position) of keyword

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
    CHARACTER (LEN=16) :: keywrd
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

    keywrd=' '                     ! assemble keyword
    ia=0
    ib=-1
    DO i=1,icd(1,1)-1
        IF(ia == 0.AND.text(i:i) /= ' ') ia=i
        IF(text(i:i) /= ' ') ib=i
    END DO
    IF(ib >= 0) keywrd=text(ia:ib)
    keya=ia
    keyb=MAX(0,ib)
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
!! Approximate string matching - case insensitive.
!! Return number of matches of string PAT in string TEXT,
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
!! \return      number of matching characters of pattern in text

INTEGER(mpi) FUNCTION matint(pat,text,npat,ntext)
    USE mpdef

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpi) :: ic
    INTEGER(mpi) :: ideq
    INTEGER(mpi) :: ip
    INTEGER(mpi) :: ipa
    INTEGER(mpi) :: ipb
    INTEGER(mpi) :: ita
    INTEGER(mpi) :: itb
    INTEGER(mpi) :: j
    INTEGER(mpi) :: jc
    INTEGER(mpi) :: jot
    INTEGER(mpi) :: jt
    INTEGER(mpi) :: npatma

    CHARACTER (LEN=*), INTENT(IN) :: pat
    CHARACTER (LEN=*), INTENT(IN) :: text
    INTEGER(mpi), INTENT(OUT) :: npat
    INTEGER(mpi), INTENT(OUT) :: ntext

    !GF
    !      INTEGER ID(0:100,2)
    PARAMETER (npatma=512)
    INTEGER(mpi) :: id(0:npatma,2)
    ! end GF
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
    !GF      IF(NPAT.GT.100) STOP 'MATINT: string PAT too long!   '
    IF(npat > npatma) THEN
        WRITE(*,*) 'too long PAT (', pat,'):', npat, ' >', npatma
        CALL peend(34,'Aborted, pattern string too long')
        STOP 'MATINT: string PAT too long!   '
    END IF
    !GF end
    id(0,1)=0
    DO i=0,npat
        id(i,2)=i
    END DO
    jot=2

    DO j=1,ntext
        jot=3-jot
        jt=j+ita-1
        jc=nj(ICHAR(text(jt:jt)))
        DO i=1,npat
            ip=i+ipa-1
            ideq=id(i-1,3-jot)
            ic=nj(ICHAR(pat(ip:ip)))
            IF(ic /= jc) ideq=ideq+1
            id(i,jot)=MIN(ideq,id(i,3-jot)+1,id(i-1,jot)+1)
        END DO
        matint=MAX(matint,npat-id(npat,jot))
    END DO
END FUNCTION matint


