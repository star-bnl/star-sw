	PROGRAM TESTBYTE

*	Test the strbyte routines.

	IMPLICIT NONE

	INTEGER Word,readword
	DATA Word/'3311dd00'x/


	CALL STRBYTE_OPENW('Testbyte01.dat')
	CALL STRBYTE_WRITE(4, Word)

	CALL STRBYTE_CLOSEW

	CALL STRBYTE_OPENR('Testbyte01.dat')
	CALL STRBYTE_READ(4, readword)

	CALL STRBYTE_CLOSER

	WRITE(6,101) Word, readword
101	FORMAT(' Written:',Z8.8'  Read back:',Z8.8)

	CALL EXIT
	END
