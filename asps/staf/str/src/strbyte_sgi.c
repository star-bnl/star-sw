/*  General Description of this package:

	Filename: strbyte_sgi.c   (completely self-contained, except for system header
	                    stdio.h)


	This package performs Fortran-callable byte-stream file i/o operations,
	not otherwise directly available from Fortran, be it VMS or UNIX.

	This file originated with D. Weygand, ca. 1989, for the purpose of
	reading MPS data-tapes, which were written from a VAX 750 during AGS
	experiment 818, into files of a form that could be transferred over to
	UNIX platforms and read in there by an analysis program.  A few small
	modifications were added in November, 1992, by R. Hackenburg, for
	essentially the same purpose, for AGS experiment 881.

	The procedures in this file work on both ends of the data-transfer process,
	ie, the file-writing routines run on VMS and write files from tapes, while
	the file-reading routines run on UNIX and read those same files, after having
	been shipped over the network.  All procedures in this file are both VMS and
	UNIX compatible.

	The byte order IS corrected in this package.

	This package contains calls to the strlib(VMS)/libstr(UNIX) and to the
	msglib(VMS)/libmsg(UNIX) Fortran libraries.


	This package has been incorporated into the general utility package strlib(VMS)
	or libstr(UNIX) on November 20, 1992 (R. Hackenburg)
*/

/*	Make "standard I/O" definitions:      */

#include <stdio.h>
#include <string.h>


	void	strend_( char *, int *, size_t );

/*	Define file-wide-global pointers fpr and fpw and function-pointer fopen,  type FILE, defined in stdio.h:     */

FILE *fpr, *fpw;

#define TRUE -1
#define FALSE 0




/*	Define procedures ("c-callable-only functions"):     */
void strfc(
/*  Inputs:   */

	 char *ftext        /*  Text-string, from a FORTRAN routine.          */
	,int ftext_length   /*  Text-string-length -- from a FORTRAN routine. */
    
/*  Outputs:  */

	,char *ctext        /*  Text-string, to a C routine.                  */
	,int ctext_length   /*  Text-string-length -- to a C routine.         */
	,int *ctext_lnb     /*  Last non-blank in resulting ctext.            */

	        )

/*  Description:

	Take a text-string, defined in a FORTRAN routine and passed into
a C routine, and copy it into a text-string defined in that C routine.
A null is appended to the string.

*/
{
	static int length;

/*	Check for nonsense:    */
	if ( ftext_length < 1 )
	{
	  printf("strfc-E1 FORTRAN text length: [%d] is nonsense.\n", ftext_length );
	  *ctext_lnb=0;
	  return;
	}

	if ( ctext_length < 1 )
	{
	  printf("strfc-E2 C text length: [%d] is nonsense.\n", ctext_length );
	  *ctext_lnb=0;
	  return;
	}

/*	Find the end of the specified text (this is a FORTRAN routine!!): */
	strend_( ftext, &length, ftext_length );

	if (length>(ctext_length-1))   /*  Make sure the given FORTRAN text isn't too long:  */
	{
	  printf("strfc-E3 FORTRAN text: [%s] is too long.\n", ftext);
	  *ctext_lnb=0;
	  return;
	}
/*	end if (length>(ctext_length-1))   Make sure the given FORTRAN text isn't too long:  */

	strncpy(ctext, ftext, length );
	ctext[length] = 0; /*  Append the c-required null.   */
	*ctext_lnb=length;

	return;
}



/*	Define procedures ("c- and f-callable functions"):     */





void strbyte_closer_()
{
	if (fpr)
	  fclose(fpr);
/*	end if (fpr)       */
}





void strbyte_closew_()
{
	if (fpw)
	  fclose(fpw);
/*	end if (fpw)       */
}




int strbyte_openr_(

/*

  FORTRAN callable:

	LOGICAL STRBYTE_OPENR, Success

	Success = STRBYTE_OPENR( Filename )

*  Input:
	CHARACTER*(*) Filename !ASCII name of file to be openned as an old file, to be read from.	
	                       !It is not necessary for the FORTRAN caller to worry about null-
	                       !terminators;  a null is appended to a local copy of "Filename"
	                       !before being passed to any "ANSI C Routines", or any other "C"
	                       !routines.  This is supposed to be a friendly interface to FORTRAN.


  Inputs:
*/


	 char *filename        /*  ASCII filename:  Fortran CHARACTER*(n).  */
	 /* (Fortran-hidden:) */
	,size_t filename_length  /*  Filename-length -- Fortran caller doesn't see this.  */

	        )

/*  Description:

	Open the old file for reading, named in <filename>, in byte-stream mode.    */

{

	static char local_filename[256];  /* fopen needs a local (non-pointer) filename. */
	int dummy;


	strfc( filename, filename_length, local_filename, 256, &dummy );


	if ( (fpr = fopen(local_filename,"r")))
	  {
	  printf("strbyte_openr-I1 Input file: [%s] opened on fpr: %x %p %p %x %x .\n",local_filename,fpr);
	  return TRUE;
	  }

	else
	  {
	  printf("strbyte_openr-E1 didn't open file (length: %d):\n[%s].\n",filename_length,local_filename);
	  return FALSE;
	  }

/*	end if ( (fpr = fopen(*filename,"r")))           */

}





int strbyte_openw_(

/*

  FORTRAN callable:

	LOGICAL STRBYTE_OPENW, Success

	Success = STRBYTE_OPENW( Filename )

*  Input:
	CHARACTER*(*) Filename !ASCII name of file to be openned as a new file, to be written to.
	                       !It is not necessary for the FORTRAN caller to worry about null-
	                       !terminators;  a null is appended to a local copy of "Filename"
	                       !before being passed to any "ANSI C Routines", or any other "C"
	                       !routines.  This is supposed to be a friendly interface to FORTRAN.


  Inputs:
*/

	 char *filename        /*  ASCII filename:  Fortran CHARACTER*(n).  */
	 /* (Fortran-hidden:) */
	,size_t filename_length  /*  Filename-length -- Fortran caller doesn't see this.  */

	       )

/*  Description:

	Open the new file for writing, named in <filename>, in byte-stream mode.    */
{

	static char local_filename[256];  /* fopen needs a local (non-pointer) filename. */
	int dummy;

	strfc( filename, filename_length, local_filename, 256, &dummy );


	if ( (fpw = fopen(local_filename,"w"))) 
	  {
	  printf("strbyte_openw-I1 Output file: [%s] opened.\n",local_filename);
	  return TRUE;
	  }

	else
	  {
	  printf("strbyte_openw-E1 unable to open output file.\n");
	  return FALSE;
	  }

/*	end if ( (fpw = fopen(*filename,"w")))           */

}





void strbyte_read_( int *Nbytes, void *buffer )
{
	int Nbytes_from_file;

	if (fpr)
	{
	  Nbytes_from_file = *Nbytes *
	  fread( buffer ,  (size_t) *Nbytes ,  (size_t) 1 ,  fpr );
/*
	  printf( "strbyte_read-I1  Bytes read: %d .\n" , Nbytes_from_file );
	  if (Nbytes_from_file != *Nbytes)
	  {
	    printf("strbyte_read-E1 Number of bytes read: %d is not the number expected: %d .\n"
	          , Nbytes_from_file, *Nbytes );

	  }
*/
/*	  end if (Nbytes_from_file != *Nbytes)    */

	  *Nbytes=Nbytes_from_file;

	}
	else
	  *Nbytes = 0;
/*	end if (fpr)       */
}





void strbyte_write_( int *Nbytes, void *buffer )
{
	int Nbytes_to_file;

	if (fpw)
	{
	  Nbytes_to_file = *Nbytes *
	  fwrite( buffer ,  (size_t) *Nbytes ,  (size_t) 1 ,  fpw );
/*
	  printf( "strbyte_write-I1  Bytes written: %d .\n", Nbytes_to_file);
	  if (Nbytes_to_file != *Nbytes)
	  {
	    printf("strbyte_write-E1 Number of bytes written: %d is not the number requested: %d .\n"
                  , Nbytes_to_file, *Nbytes );
	    *Nbytes=Nbytes_to_file;

	  }
*/
/*	  end if (Nbytes_to_file != *Nbytes)    */

	}
/*	end if (fpw)       */
}
