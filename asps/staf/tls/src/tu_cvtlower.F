        SUBROUTINE TU_CVTLOWER (strout,strin)
         
C 
C FUNCTIONAL DESCRIPTION:       
C 
c       converts upper case in strin to lower case in strout
C 
C DUMMY ARGUMENTS:
C 
C 
C IMPLICIT INPUTS:
C 
C    character*(*) strin input
C 
C IMPLICIT OUTPUTS:
C 
C    strout = output string
C 
C 
C SIDE EFFECTS:
C 
C 
C 
        IMPLICIT NONE
        CHARACTER*(*) strin,strout
        INTEGER*4 lout
        INTEGER*4 i,let
        INTEGER*4 index,len
        CHARACTER*26 alower,aupper
        DATA alower /'abcdefghijklmnopqrstuvwxyz'/
        DATA aupper/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
!       [specification_statement]...
         
        strout = strin
        lout = len( strout )
        DO i = 1, lout
            let = index(aupper,strout(i:i))
            DO WHILE (let .gt. 0)
                IF (let .gt. 0) strout(i:i) = alower(let:let)
                let = index(aupper,strout(i:i))
            END DO
        END DO
!       [executable_statement]...
        RETURN
        END
