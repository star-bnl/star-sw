
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
100     FORMAT(' 'A) !On VMS, this is how it's done.

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
100     FORMAT(' 'A$) !On VMS, this is how it's done.

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

        WRITE(LUN,100) MSG
100     FORMAT('+'A) !On VMS, this is how it's done.

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

        WRITE(LUN,100) MSG
100     FORMAT('+'A$) !On VMS, this is how it's done.

        RETURN
        END
*
        SUBROUTINE strterm_get_char(Long)

        IMPLICIT NONE

*  Output:
        INTEGER Long  !Integer-ASCII-equivalent of transmitted character,
                      !or zero, if nothing is ready.

*  Return value:  none

*  Description:
*       Get a single character from the terminal, w/o waiting, and without
*       waiting for a complete line (ie, no EOT character needed to make
*       characters available).
*       Return with the null byte as the character if no
*       character is ready at the terminal (ie, don't wait).

        INCLUDE 'strterm_vms_inc'

        BYTE BUFFER(8)
        INTEGER I

        INTEGER*2 CHBUF(4)

        INTEGER I_4
        INTEGER*2 I_2(2)
        EQUIVALENCE (I_4,I_2)

        LOGICAL FIRST
        SAVE FIRST

        EXTERNAL IO$_SENSEMODE,IO$M_TYPEAHDCNT
        EXTERNAL IO$_TTYREADALL,IO$M_NOECHO,IO$M_TIMED !$QIO mneumonics.
        EXTERNAL IO$M_PURGE

        DATA FIRST/.TRUE./


        IF (FIRST) THEN !Do this on first call only:
          FIRST=.FALSE.
          CALL SYS$ASSIGN('SYS$COMMAND:',str_term_channel,,)
        END IF


        CALL strterm_set_nowait !Set terminal I/O waiting off.
        CALL strterm_set_char   !Set terminal into single-char. mode.


*       Get the terminal byte-count (or zero):
        CALL SYS$QIOW(
     1     ,%VAL(str_term_channel)
     1     ,%VAL(%LOC(IO$_SENSEMODE)+%LOC(IO$M_TYPEAHDCNT)),,,
     1     ,CHBUF,,,,,)

        I_4=0 !Zero both half-words.
        I_2(1)=CHBUF(1) !Byte count is here.

        IF (I_4.LE.0) THEN !Nothing ready -- return zero:
          Long=0
          RETURN
        END IF


*       Have a positive byte count -- read one byte:
        BUFFER(1)=0

        CALL SYS$QIO( ,%VAL(str_term_channel)
     1    , %VAL( %LOC( IO$_TTYREADALL ) +
     1            %LOC( IO$M_TIMED ) ),,,
     1    , BUFFER     !Buffer.
     1    , %VAL(1)    !# of bytes.
     1    , %VAL(0)    !Timeout (immediate).
     1    , %VAL(0),,) !EOT character-set (n/a in TTYREADALL).

        I=BUFFER(1)

        Long=IAND(I,255) !Prevent a sign-extend.

        RETURN

        END
*
        SUBROUTINE strterm_get_char_from_line(Long)

        IMPLICIT NONE

*  Output:
        INTEGER Long  !Integer-ASCII-equivalent of transmitted character,
                      !or zero, if nothing is ready.

*  Return value:  none

*  Description:

*       VMS version is not operational -- just for compatability's sake,
*       this issues a call to strterm_get_char, which isn't quite the same.

*  What it should do (and does on Unix):
*       Get a character from the terminal, w/o waiting.
*       Wait for a complete line before transmitting.
*       Return with the null byte as the character if no
*       character is ready at the terminal (ie, don't wait).

        CALL strterm_get_char(Long)

        RETURN

        END
*
        SUBROUTINE strterm_set_char

        IMPLICIT NONE

*  No arguments

*  Return value:  none

*  Description:
*       Set the terminal into single-character ("punctual") I/O mode.
*       ie, make characters available immediately, without waiting for
*       an EOT (End Of Transmission, eg, a Return, Line Feed, etc.).
*       This is independent of whether terminal I/O is synchronous
*       (reads wait until something is really read) or asynchronous
*       (reads return a null byte if nothing is read).

        INCLUDE 'strterm_vms_inc'


*       Set the char-mode bit:

        str_term_mode = IOR(str_term_mode,str_term_mode_char_P)

        RETURN

        END
*
        SUBROUTINE strterm_set_line

        IMPLICIT NONE

*  No arguments

*  Return value:  none

*  Description:
*       Set the terminal into "line" I/O mode.  ie, characters are not
*       available until an EOT (End Of Transmission, eg, a Return, Line Feed,
*       etc.) is sent.
*       This is independent of whether terminal I/O is synchronous
*       (reads wait until something is really read) or asynchronous
*       (reads return a null byte if nothing is read).
*       Under VMS, this is done at the actual I/O call itself.  Here,
*       software flags are set, but no action is taken.

        INCLUDE 'strterm_vms_inc'


*       Clear the char-mode bit:

        str_term_mode = IAND(str_term_mode,NOT(str_term_mode_char_P))

        RETURN

        END
*
        SUBROUTINE strterm_set_normal

        IMPLICIT NONE

*  No arguments

*  Return value:  none

*  Description:
*       Set the terminal into "normal" I/O mode.  ie, wait at a read for
*       input to be typed, and do not make characters available until an
*       EOT (End Of Transmission, eg, a Return, Line Feed, etc.) is sent.
*       Under VMS, this is done at the actual I/O call itself.  Here,
*       software flags are set, but no action is taken.

        CALL strterm_set_wait !Set terminal I/O waiting on.
        CALL strterm_set_line !Set terminal into line-mode.

        RETURN
        END
*
        SUBROUTINE strterm_set_nowait

        IMPLICIT NONE

*  No arguments

*  Return value:  none

*  Description:
*       Set the terminal into "nowait", or asynchronous I/O mode.  ie, if
*       no characters are available at a read, a null byte is returned
*       and program flow continues, rather than waiting for something to be
*       transmitted.
*       Under VMS, this is done at the actual I/O call itself.  Here,
*       software flags are set, but no action is taken.


        INCLUDE 'strterm_vms_inc'

*       Set the nowait-mode bit:

        str_term_mode = IOR(str_term_mode,str_term_mode_nowait_P)

        RETURN

        END
*
        SUBROUTINE strterm_set_wait

        IMPLICIT NONE

*  No arguments

*  Return value:  none

*  Description:
*       Set the terminal into "wait", or synchronous I/O mode.  ie, if
*       no characters are available at a read,  program flow halts,
*       waiting for something to be transmitted, rather than returning
*       a null byte and continuing.  This is the "normal" mode.
*       Under VMS, this is done at the actual I/O call itself.  Here,
*       software flags are set, but no action is taken.


        INCLUDE 'strterm_vms_inc'

*       Clear the nowait-mode bit:

        str_term_mode = IAND(str_term_mode,NOT(str_term_mode_nowait_P))

        RETURN

        END
*
