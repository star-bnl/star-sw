*
        SUBROUTINE STRCAP(BPT,EPT,C)

        IMPLICIT NONE

*  Inputs:
        INTEGER BPT,EPT !Range in string C in which conversion is requested.

*  Input/output:
        CHARACTER*(*) C !String to be converted to all capitals.

*  Description:
*       Convert a specified range in a character string to all caps.
*       Provided for back compatibility.  Preferred usage is STRCAPS.

        CALL STRCAPS(C(BPT:EPT))

        RETURN
        END
*
        SUBROUTINE STRCAPS(C)

        IMPLICIT NONE

*  Input/output:
        CHARACTER*(*) C !String to be converted to all capitals.

*  Description:
*       Convert a character string to all caps.

        CHARACTER*1 NULL
        INTEGER I,L
        DATA NULL/'000'O/

        L=LEN(C)
        DO I=1,L
          IF (C(I:I).EQ.NULL) THEN !Keep from running past short ASCIZ strings.
            RETURN
          ELSE IF (C(I:I).GE.'a' .AND. C(I:I).LE.'z') THEN !Convert to capital.
            C(I:I)=CHAR(ICHAR(C(I:I))-ICHAR('a')+ICHAR('A'))
          END IF
        END DO
        RETURN
        END
*
        SUBROUTINE STRCLEAN(TEXT,LENGTH,CTEXT)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) TEXT !The character string to be cleaned-up.
        INTEGER LENGTH     !The length of TEXT to be cleaned-up.
*  Output:
        CHARACTER*(*) CTEXT !The cleaned-up text.

*  Description:
*       Copy all or as much as will fit of specified input text to an
*       output character string, and replace any non-printable characters in
*       with blanks (clean text).  Terminate the copying if input string has
*       a null, and pad the rest of the output string with blanks.

        CHARACTER*1 NULL
        INTEGER I,J,L,K
        DATA NULL/'000'O/

        L=LEN(TEXT) !The length of the specified argument.
        K=MIN(LENGTH,LEN(CTEXT)) !The output length.

        DO I=1,K
          IF (I.GT.L) THEN !Input exhausted - pad with blanks:
            DO J=I,K !Blank out the rest.
              CTEXT(J:J)=' '
            END DO
            RETURN
          ELSE IF (TEXT(I:I).EQ.NULL) THEN
*           Keep from running past short ASCIZ strings.
            DO J=I,K !Blank out the rest.
              CTEXT(J:J)=' '
            END DO
            RETURN
          ELSE IF (TEXT(I:I).LT.' ' .OR. TEXT(I:I).GT.'~') THEN !Non-printable:
            CTEXT(I:I)=' ' !Blank it out.
          ELSE !Printable:
            CTEXT(I:I)=TEXT(I:I)
          END IF
        END DO
        RETURN
        END
*
        SUBROUTINE STREND(S,EPT)

        IMPLICIT NONE

*  Input:
        CHARACTER*(*) S
*  Output:
        INTEGER EPT !Index to the last non-blank character in S.

*  Description:
*       Find the string-end -- set EPT to point at the last non-blank in S,
*       or 0 if all blanks.  Tabs are regarded as blanks.
*       No null is appended.

        INTEGER L,I

        L=LEN(S) !Length of string S.
*       Search for the end of the line:
        EPT=L !No trailing blanks confirmed yet (reset value).
        DO I=1,L
          IF ((S(I:I).NE.' ').AND.(S(I:I).NE.'  ')) THEN !Non-blank/tab.
            EPT=L !Reset to last character in S.
          ELSE IF (EPT.EQ.L) THEN !First blank/tab since last reset.
            EPT=I-1 !A trailing blank?  Point to previous character.
          END IF
        END DO

        RETURN
        END
*
        SUBROUTINE STRGETCHR(LONG,SEL,CHR)

        IMPLICIT NONE

*  Inputs:
        INTEGER LONG !Contains a long-word (4 bytes) -- the "source".
        INTEGER SEL  !A number, 1 to 4, indicating one of LONG's 4 bytes.
*  Output:
        CHARACTER*1 CHR !Contains the character-representation of the selected
*                        byte from LONG.

*  Description:
*       Get a character out of LONG, a 32 bit word.  The character
*       is the SELth byte of LONG, and goes into CHR.  The selection
*       is made in a machine-independent fashion.

        INTEGER I4
        BYTE I1(4)
        EQUIVALENCE (I1,I4)
        INTEGER J4
        BYTE J1(4)
        EQUIVALENCE (J1,J4)

        J4=LONG         !Gain byte-access.
        CALL STRDEC_ENDIAN_BYTE(J4) !Make J1(SEL) select bytes like a VAX.
        I4=0            !Start with all four bytes of I4 zeroed.
        I1(1) = J1(SEL) !Overwrite the selected byte (other 3 are zero).

        CALL STRDEC_ENDIAN_BYTE(I4) !Make it DEC-like (byte-ordering).
                                    !ie, have the CHAR function work on
                                    !what was put into I1(1).

        CHR=CHAR(I4)

        RETURN
        END
*
        SUBROUTINE STRGETCHRS( Buffer, Nchrs, Nwords, CHRS )

        IMPLICIT NONE

*  Inputs:
        INTEGER Buffer(*) !Longword-buffer from which CHRS is copied.
        INTEGER Nchrs !Number of characters to copy.

*  Outputs:
        INTEGER Nwords !Number of longwords copied from Buffer.
        CHARACTER*(*) CHRS !Character-string to be copied from Buffer.

*  Description:
*       Copy Nchrs characters from Buffer into CHRS.
*       Return the number of longwords accessed in Buffer; should
*       be (Nchrs+3)/4 .

        INTEGER I, J
        INTEGER SEL

        I=1
        J=1
        Nwords=0
        SEL=4 !Set this in case of funny Nchrs or Buffer_size.
        CHRS=' ' !Start out with all blanks.

        DO WHILE ( I.LE.Nchrs )
          SEL=MOD(I-1,4)+1 !Cycles: 1, 2, 3, 4, 1, ...
          CALL STRGETCHR( Buffer(J), SEL, CHRS(I:I) ) !Machine-independent order.
          IF      (SEL.EQ.1) THEN !Broke ground on a new word.
            Nwords=Nwords+1
          ELSE IF (SEL.GE.4) THEN !Will break ground on a new word on the next char.
            J=J+1
          END IF
          I=I+1 !Next character.
        END DO !WHILE ( I.LE.Nchrs )


        RETURN
        END
*
        SUBROUTINE STRINSERT(SOURCE,START,END,DEST,DPT)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) SOURCE !Source string.
        INTEGER START,END    !Index-range in SOURCE to be copied.
*  Outputs:
        CHARACTER*(*) DEST   !Destination string.
        INTEGER DPT          !Starting index in DEST -- SOURCE is copied to
*                             to DEST starting at DEST(DPT:DPT), and continues
*                             until the SOURCE range is exhausted or until
*                             DEST runs out of space.  DPT is returned to be
*                             the index of the first character in DEST
*                             following the characters copied over from SOURCE,
*                             unless DEST ran out of room, in which case
*                             DPT is set to zero.

*  Description:
*       Insert characters in SOURCE (from START to END)
*       to DEST (from DPT to .LE. LEN(DEST). Set DPT to point after
*       the last character inserted.
*       No null is appended.

        INTEGER SPT,DMAX

        DMAX=LEN(DEST)

        IF (DPT.LE.0) RETURN !Error, from some previous manipulation.
*       Move:
        DO SPT=START,END
          IF (DPT.GT.DMAX) GO TO 5000 !Fatal error.
          DEST(DPT:DPT)=SOURCE(SPT:SPT)
          DPT=DPT+1
        END DO
        RETURN !Done (success).

*       Error; DPT is too high (Fatal. DEST is zeroed):
5000    DPT=0
        RETURN
        END
*
        SUBROUTINE STRIP(S,EPT)

        IMPLICIT NONE

*  Input/output:
        CHARACTER*(*) S  !Character string containing possible trailing
*                         blanks and comments (comments indicated by "!").
*                         S is returned with trailing comments blanked out.
*  Output:
        INTEGER EPT      !Index to the last non-blank character in S, after
*                         comments have been removed.

*  Description:
*       Routine to strip off comments (and trailing blanks) at the end
*       of "command" lines, where comments are denoted by the usual
*       exclamation mark (!).  Leave EPT pointing at last non-blank.
*       No null is appended.

        INTEGER L,I

        L=LEN(S)
        I=INDEX(S,'!')
        IF (I.LE.0) THEN !No comment.
        ELSE IF (I.LE.L) THEN !Strip off the comment.
          S(I:L)=' '
        END IF
        CALL STREND(S,EPT) !Set EPT to string-end (last non-blank/tab).
        RETURN
        END
*
        SUBROUTINE STRPUTCHR(CHR,SEL,LONG)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*1 CHR !Contains the character-representation of the selected
*                        byte to be inserted into LONG.
        INTEGER SEL  !A number, 1 to 4, indicating one of LONG's 4 bytes.

*  Output:
        INTEGER LONG !Contains a long-word (4 bytes) -- the "sink".

*  Description:
*       Put a character into LONG, a 32 bit word.  The character becomes
*       is the SELth byte of LONG, and from CHR.  The selection
*       is made in a machine-independent fashion.

        INTEGER I4
        BYTE I1(4)
        EQUIVALENCE (I1,I4)
        INTEGER J4
        BYTE J1(4)
        EQUIVALENCE (J1,J4)

        I4=ICHAR(CHR) !Make it an integer.
        CALL STRDEC_ENDIAN_BYTE(I4) !Make it DEC-like (byte-ordering).
                                    !ie, want the desired byte to be in I1(1).

        J4=LONG         !Make a copy.
        CALL STRDEC_ENDIAN_BYTE(J4) !Make it DEC-like (byte-ordering).
        J1(SEL) = I1(1) !Overwrite the selected DEC-byte.
        CALL STRDEC_ENDIAN_BYTE(J4) !Put it back the way it was.
        LONG=J4         !Put the result back here.

        RETURN
        END
*
        SUBROUTINE STRPUTCHRS( CHRS, Nchrs, Buffer_size, Nwords, Buffer )

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) CHRS !Character-string to be copied to Buffer.
        INTEGER Nchrs !The number of characters from CHRS to copy.
        INTEGER Buffer_size !Size, in 32-bit words, of Buffer.

*  Outputs:
        INTEGER Nwords !Number of longwords in Buffer, from the copy.
        INTEGER Buffer(*) !Longword-buffer into which CHRS is copied.

*  Description:
*       Copy Nchrs characters from CHRS into Buffer;  pad 0, 1, 2 or 3
*       bytes in the last word in Buffer with spaces, if the Nchrs is
*       not a multiple of 4.

        INTEGER I, J
        INTEGER SEL

        I=1
        J=1
        Nwords=0
        SEL=4 !Set this in case of funny Nchrs or Buffer_size.

        DO WHILE ( (J.LE.Buffer_size) .AND. (I.LE.Nchrs) )
          SEL=MOD(I-1,4)+1 !Cycles: 1, 2, 3, 4, 1, ...
          CALL STRPUTCHR( CHRS(I:I), SEL, Buffer(J) ) !Machine-independent order.
          IF      (SEL.EQ.1) THEN !Broke ground on a new word.
            Nwords=Nwords+1
          ELSE IF (SEL.GE.4) THEN !Will break ground on a new word on the next char.
            J=J+1
          END IF
          I=I+1 !Next character.
        END DO !WHILE ( (J.LE.Buffer_size) .AND. (I.LE.Nchrs) )

*       Pad with 0, 1, 2 or 3 spaces, as needed:
*       (This loop is skipped if first loop is skipped.)
        SEL = SEL+1 !Next byte, of four in a longword, not yet used.
        DO WHILE (SEL.LE.4) !Done if SEL is now 5.
          CALL STRPUTCHR( ' ', SEL, Buffer(Nwords) )
          SEL=SEL+1
        END DO !WHILE (SEL.LE.4)


        RETURN
        END
*
        LOGICAL FUNCTION STRWID(BEG,WIDTH,END,S)

        IMPLICIT NONE

*  Inputs:
        INTEGER BEG   !Index to S -- make a gap in S starting here.
        INTEGER WIDTH !Width of gap to make in S.
        INTEGER END   !Effective end-of-string in S (can't use LEN(S)).

*  Input/Output:
        CHARACTER*(*) S !String to be "widenned".

*  Description:
*       Move everything in S, from and including BEG to END, up by WIDTH.
*       Effectively widen or create a gap in S. WIDTH is not negative.
*       BEG is left to point at the first position of the new gap.
*       Returns .true. for success, .false. for failure.

        INTEGER SPT,L

        L=LEN(S)
        IF ((END+WIDTH).GT.L) THEN !Not enough room in string.
          STRWID=.FALSE. !Failure.
          RETURN
        END IF
        DO SPT=END,BEG,-1 !Work backwards -- it's easier this way.
          S(SPT+WIDTH:SPT+WIDTH)=S(SPT:SPT)
        END DO
        STRWID=.TRUE. !Success.
        RETURN
        END
*
