
        SUBROUTINE STRTERM(MSG,LINES,LUN)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) MSG(*) !Character string to be output.
        INTEGER LINES !Number of lines in MSG to output.
        INTEGER LUN !Logical unit on which to output (terminal).

*  Description:
*       Output a one-or-more-line message on a terminal with
*       machine-independent carriage-control on a terminal.

        INTEGER I

        DO I=1,LINES
          WRITE(LUN,100) MSG(I)
        END DO
100     FORMAT(A) !On SGI, this is how it's done.

        RETURN
        END
*
        SUBROUTINE STRTERM_NOCR(MSG,LUN)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) MSG !Character string to be output.
        INTEGER LUN !Logical unit on which to output (terminal).

*  Description:
*       Output a one-line message without a following carriage return
*       on a terminal.

        WRITE(LUN,100) MSG
100     FORMAT(A$) !On SGI, this is how it's done.

        RETURN
        END
*
        SUBROUTINE STRTERM_NOLF(MSG,LUN)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) MSG !Character string to be output.
        INTEGER LUN !Logical unit on which to output (terminal).

*  Description:
*       Output a one-line message without a preceding line feed
*       on a terminal.

        CHARACTER*1 VTUP(3)
        DATA VTUP/'1B'X,'[','A'/

100     FORMAT(A) !On SGI, this is how it's done.
101     FORMAT(3A$)

        WRITE(LUN,101) VTUP !Cursor up one line.
        WRITE(LUN,100) MSG

        RETURN
        END
*
        SUBROUTINE STRTERM_NOLFCR(MSG,LUN)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) MSG !Character string to be output.
        INTEGER LUN !Logical unit on which to output (terminal).

*  Description:
*       Output a one-line message without a preceding line feed
*       or a following carriage-return on a terminal.

        CHARACTER*1 VTUP(3)
        DATA VTUP/'1B'X,'[','A'/

100     FORMAT(A$) !On SGI, this is how it's done.
101     FORMAT(3A$)

        WRITE(LUN,101) VTUP !Cursor up one line.
        WRITE(LUN,100) MSG

        RETURN
        END
*
