*  General Description of this package:
*
*	Filename: strbytef.f

*	This is a Fortran "substitute" for the c-routines in strbyte.c



*	This package contains calls to the strlib(VMS)/libstr(UNIX) and to the
*	msglib(VMS)/libmsg(UNIX) Fortran libraries.


*	This package has been incorporated into the general utility package strlib(VMS)
*	or libstr(UNIX) on December 11, 1992 (R. Hackenburg)


*
	SUBROUTINE strbyte_openw( Filename )

*  Input:
	CHARACTER*(*) Filename !ASCII name of file to be openned as a new file, to be written to.

*  Functional Description:

*	Open the new file for writing, named in <Filename>, in "byte-stream" mode.

	INTEGER STRBYTE_LUN_P
	PARAMETER (STRBYTE_LUN_P=27)
	LOGICAL STRBYTE_File_Opened
	COMMON/STRBYTE/ STRBYTE_File_Opened

*	This is from the calling-template for STROPENVER, stropenver.template .
*	Actual declarations needed:
	INTEGER Assigned_Version_Number
	CHARACTER*80 Filename_with_Version

	LOGICAL STROPENVER

*	The STROPENVER call takes these arguments:

*  Input arguments:
*	INTEGER LUN !Logical unit on which to open file.
*	CHARACTER*(<user>) FILENAME !Name of file.
*	CHARACTER*(<user>) COMMANDS !Command line, through which
*	                            !OPEN options are selected.
*	INTEGER VLIMIT !Caller-specified limit to the version number.
*	               !When VERSION exceeds this, STROPENVER fails.
*	CHARACTER*1 VCHAR !One-character version-field delimiter, caller
*	                  !specified.  eg, ";" or ".".
*  Output arguments:
*	INTEGER VERSION !The version number of a successfully opened file.
*	CHARACTER*(<user>) Filename_Version !Same as FILENAME, but with a version
*	                                    !number appended after an underscore.
*	                                    !Returns blank if an error occurs.
*	                                    !If VERSION is 0, the returned file name
*	                                    !is exactly the specified file name;
*	                                    !no version-field delimiter or digits
*	                                    !are appended.
*  Return values:
*	.TRUE. for a successful open on: FILENAME//VCHAR//VERSION.
*	.FALSE. for a failed open.
*  Functional description:
*	Combines the functions of STRVER and STROPEN, providing a simple
*	way to open version-appended files in a machine-independent fashion,
*	using the OPEN parameters specified in COMMANDS.  COMMANDS is a
*	character string containing all the "usual" OPEN parameters, except
*	for the UNIT=lun (FORTRAN Logical Unit) and FILE=filename, which are passed
*	separately, and without the usual single-quotes (apostrophes) normally
*	around literal arguments.


	IF ( .NOT. STROPENVER(
     1	      STRBYTE_LUN_P, Filename
     1	   ,  'STATUS=NEW'
     1	   // ',RECL=4, FORM=UNFORMATTED, ACCESS=DIRECT'
     1	  ,32767, '.', Assigned_Version_Number, Filename_with_Version
     1	   )) THEN
*	  File did not open:
#ifndef Linux
	  CALL MESSAGE(
     1	    'STRBYTE_OPENW-E1 (FORTRAN) Open failed on:'//Filename,1,-1)
	  strbyte_file_opened=.FALSE.
#endif
	ELSE
*	  File successfully openned:
#ifndef Linux
	  CALL MESSAGE(
     1	    'STRBYTE_OPENW-I1 (FORTRAN) Openned file:'//Filename,1,-1)
	  strbyte_file_opened=.TRUE.
#endif
	END IF

	RETURN
	END



*
	SUBROUTINE strbyte_openr( Filename )

*  Input:
	CHARACTER*(*) Filename !ASCII name of file to be openned as an old file, to be read from to.

*  Functional Description:

*	Open the old file for reading, named in <Filename>, in "byte-stream" mode.

	INTEGER STRBYTE_LUN_P
	PARAMETER (STRBYTE_LUN_P=27)
	LOGICAL STRBYTE_File_Opened
	COMMON/STRBYTE/ STRBYTE_File_Opened

*	This is from the calling-template for STROPEN, stropen.template .
*	Actual declaration needed:
	LOGICAL STROPEN

*	The STROPEN call takes these arguments:

*  Input arguments:
*	INTEGER LUN !Logical unit on which to open file.
*	CHARACTER*(<user>) FILENAME !Name of file.
*	CHARACTER*(<user>) COMMANDS !Command line, through which
*	                            !OPEN options are selected.
*  Return values:
*	.TRUE. for a successful open on: Filename .
*	.FALSE. for a failed open.
*  Functional description:
*	Attempts to open the specified file in a machine-independent fashion
*	using the OPEN parameters specified in COMMANDS.  COMMANDS is a
*	character string containing all the "usual" OPEN parameters, except
*	for the UNIT=lun (FORTRAN Logical Unit) and FILE=filename, which are passed
*	separately, and without the usual single-quotes (apostrophes) normally
*	around literal arguments.


	IF ( .NOT. STROPEN(
     1	      STRBYTE_LUN_P, Filename
     1	   ,  'STATUS=OLD'
     1	   // ',RECL=4, FORM=UNFORMATTED, ACCESS=DIRECT'
     1	   ) )  THEN
*	  File did not open:
#ifndef Linux
	  CALL MESSAGE(
     1	    'STRBYTE_OPENR-E1 (FORTRAN) Open failed on:'//Filename,1,-1)
	  strbyte_file_opened=.FALSE.
#endif
	ELSE
*	  File successfully openned:
#ifndef Linux
	  CALL MESSAGE(
     1	    'STRBYTE_OPENR-I1 (FORTRAN) Opened file:'//Filename,1,-1)
	  strbyte_file_opened=.TRUE.
#endif
	END IF

	RETURN
	END



*
	SUBROUTINE strbyte_write( Nbytes, buffer )

	IMPLICIT NONE

*  Input arguments:
	INTEGER Nbytes
	INTEGER buffer(*)

*  Functional Description:
*	Simulate a byte-stream write to a disk file openned by strbyte_openw.
*	Warning! Nbytes must be a multiple of four for this to work properly. 

	INTEGER STRBYTE_LUN_P
	PARAMETER (STRBYTE_LUN_P=27)
	LOGICAL STRBYTE_File_Opened
	COMMON/STRBYTE/ STRBYTE_File_Opened

	INTEGER Nbytes_to_file, Iword
	CHARACTER*132 M132

	IF (strbyte_file_opened) THEN
	  Nbytes_to_file=0
	  DO Iword=1,Nbytes/4
	    WRITE( STRBYTE_LUN_P, ERR=1 ) buffer(Iword)
	    Nbytes_to_file = Nbytes_to_file + 4
	  END DO !Iword=1,Nbytes/4

1	  CONTINUE !Come here on write error.

	  IF (Nbytes_to_file .NE. Nbytes) THEN
	    WRITE(M132,501) Nbytes_to_file, Nbytes
	    CALL MESSAGE( M132, 1, -1)
	    Nbytes = Nbytes_to_file

	  ELSE
	    WRITE(M132,101) Nbytes
	    CALL MESSAGE( M132, 1, -1)

	  END IF

	END IF

101	FORMAT('strbyte_write-I1 Wrote:'I11' bytes.')
501	FORMAT('strbyte_write-E1 Number of bytes written:'I11
     1	   ' is not the number requested:'I1)

	RETURN
	END



*
	SUBROUTINE strbyte_read( Nbytes, buffer )

	IMPLICIT NONE

*  Input arguments:
	INTEGER Nbytes
	INTEGER buffer(*)

*  Functional Description:
*	Simulate a byte-stream read from a disk file openned by strbyte_openr.
*	Warning! Nbytes must be a multiple of four for this to work properly. 

	INTEGER STRBYTE_LUN_P
	PARAMETER (STRBYTE_LUN_P=27)
	LOGICAL STRBYTE_File_Opened
	COMMON/STRBYTE/ STRBYTE_File_Opened

	INTEGER Nbytes_from_file, Iword
	CHARACTER*132 M132

	IF (strbyte_file_opened) THEN
	  Nbytes_from_file=0
	  DO Iword=1,Nbytes/4
	    READ( STRBYTE_LUN_P, ERR=1 ) buffer(Iword)
	    Nbytes_from_file = Nbytes_from_file + 4
	  END DO !Iword=1,Nbytes/4

1	  CONTINUE !Come here on read error.

	  IF (Nbytes_from_file .NE. Nbytes) THEN
	    READ(M132,501) Nbytes_from_file, Nbytes
	    CALL MESSAGE( M132, 1, -1)
	    Nbytes = Nbytes_from_file

	  ELSE
	    READ(M132,101) Nbytes
	    CALL MESSAGE( M132, 1, -1)

	  END IF

	END IF

101	FORMAT('strbyte_read-I1 Read:'I11' bytes.')
501	FORMAT('strbyte_read-E1 Number of bytes read:'I11
     1	   ' is not the number requested:'I1)

	RETURN
	END



*
	SUBROUTINE strbyte_closer

	IMPLICIT NONE

	INTEGER STRBYTE_LUN_P
	PARAMETER (STRBYTE_LUN_P=27)
	LOGICAL STRBYTE_File_Opened
	COMMON/STRBYTE/ STRBYTE_File_Opened

	IF (strbyte_file_opened) THEN
	  strbyte_file_opened=.FALSE.
	  CLOSE(UNIT=STRBYTE_LUN_P,ERR=1)
1	  CONTINUE !Come here on close errors -- ignore them.
	END IF
	RETURN
	END



*
	SUBROUTINE strbyte_closew

	IMPLICIT NONE

	INTEGER STRBYTE_LUN_P
	PARAMETER (STRBYTE_LUN_P=27)
	LOGICAL STRBYTE_File_Opened
	COMMON/STRBYTE/ STRBYTE_File_Opened

	IF (strbyte_file_opened) THEN
	  strbyte_file_opened=.FALSE.
	  CLOSE(UNIT=STRBYTE_LUN_P,ERR=1)
1	  CONTINUE !Come here on close errors -- ignore them.
	END IF
	RETURN
	END

