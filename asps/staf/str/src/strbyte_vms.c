/*  General Description of this package:

        Filename: strbyte_vms.c
           (completely self-contained, except for system header stdio.h)


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

        The byte order is not corrected in this package.

        This package contains calls to the strlib(VMS)/libstr(UNIX) and to the
        msglib(VMS)/libmsg(UNIX) Fortran libraries.


        This package has been incorporated into the general utility package strlib(VMS)
        or libstr(UNIX) on November 20, 1992 (R. Hackenburg)
*/

/*      Make "standard I/O" definitions:      */

#include <stdio.h>
#include descrip


/*      Define file-wide-global pointers fpr and fpw and function-pointer fopen,  type FILE, defined in stdio.h:     */

FILE *fpr, *fpw, *fopen();

#define TRUE -1
#define FALSE 0


/*      Define procedures ("C-callable-only functions"):     */





void strcf(
/*  Inputs:   */

         char *ctext        /*  Text-string, from a C routine.                  */
        ,int ctext_length   /*  Text-string-length -- from a C routine.         */
        ,int dummy          /* This is the length on Unix -- unused on VMS      */

/*  Outputs:  */
        ,struct dsc$descriptor_s *ftext_descriptor   /*  ASCII ftext descriptor;  Fortran CHARACTER*(n).  */

/*        This consists of:             */
/*        unsigned short        dsc$w_length;   */ /* length of data item in bytes,
                                             or if dsc$b_dtype is DSC$K_DTYPE_V, bits,
                                             or if dsc$b_dtype is DSC$K_DTYPE_P, digits (4 bits each) */
/*        unsigned char dsc$b_dtype;    */ /* data type code */
/*        unsigned char dsc$b_class;    */ /* descriptor class code = DSC$K_CLASS_D */
/*        char          *dsc$a_pointer; */ /* address of first byte of data storage */

        ,int *ftext_lnb     /*  Last non-blank in ftext.                    */

                )

/*  Description:

        Take a text-string, defined in a C routine and to be passed into
a FORTRAN routine, and copy it into a text-string defined in that C routine.
All characters in the text for the FORTRAN routine from the null to the
end of ftext are replaced with blanks.

*/
{
        static int i;
        static int length;
        static int do_blanks;

/*      Check for nonsense:    */
        if ( ctext_length < 1 )
        {
          printf("strcf-E2 C text length: [%d] is nonsense.\n", ctext_length );
          *ftext_lnb=0;
          return;
        }

        if ( ftext_descriptor->dsc$w_length < 1 )
        {
          printf("strcf-E1 FORTRAN text length: [%d] is nonsense.\n", ftext_descriptor->dsc$w_length );
          *ftext_lnb=0;
          return;
        }

/*      Find the null, then replace it and everything afterwards with blanks. */
        do_blanks = FALSE;
        length    = 0;
        for ( i = 0; i < ctext_length; ++i )
        {
          if     ( i > ftext_descriptor->dsc$w_length )
          { /* FORTRAN string isn't big enough */
            printf("strcf-E3 C text: [%s] is too long.\n", ctext);
            *ftext_lnb=0;
            return;
          }
          else if ( do_blanks    ) { ftext_descriptor->dsc$a_pointer[i] = ' '; }
          else if ( ctext[i] = 0 ) { ftext_descriptor->dsc$a_pointer[i] = ' '; do_blanks = TRUE; }
          else                     { ftext_descriptor->dsc$a_pointer[i] = ctext[i]; length = i+1; }
        }

/*      Find the end of the specified text (this is a FORTRAN routine!!): */
        strend( ftext_descriptor, &length );

        *ftext_lnb=length;

        return;
}



void strfc(

/*  Inputs:   */

        struct dsc$descriptor_s *ftext_descriptor   /*  ASCII ftext descriptor;  Fortran CHARACTER*(n).  */

/*        This consists of:             */
/*        unsigned short        dsc$w_length;   */ /* length of data item in bytes,
                                             or if dsc$b_dtype is DSC$K_DTYPE_V, bits,
                                             or if dsc$b_dtype is DSC$K_DTYPE_P, digits (4 bits each) */
/*        unsigned char dsc$b_dtype;    */ /* data type code */
/*        unsigned char dsc$b_class;    */ /* descriptor class code = DSC$K_CLASS_D */
/*        char          *dsc$a_pointer; */ /* address of first byte of data storage */

        ,int dummy                         /* This is the length on Unix -- unused on VMS */

/*  Outputs:  */

        ,char *ctext                       /* Text string (destination) -- defined in a C routine */
        ,int ctext_length                  /* Text string (destination) length.  */
        ,int *ctext_lnb                    /* Index to last non-blank in ctext.  */

               )


/*  Description:

        Copy the FORTRAN-defined text-string, ftext, into the C-defined text-string, ctext.
*/

{
        static int length;

/*      printf("strfc-i1  length:%d  class:%d  dtype:%d\n  ftext[%s]\n"
               , ftext_descriptor->dsc$w_length
               , ftext_descriptor->dsc$b_dtype
               , ftext_descriptor->dsc$b_class
               , ftext_descriptor->dsc$a_pointer );
*/

/*      Check for nonsense:  */

        if ( ftext_descriptor->dsc$w_length < 1 )
        {
          printf("strfc-E1 FORTRAN text length: [%d] is nonsense.\n", ftext_descriptor->dsc$w_length );
          *ftext_lnb=0;
          return;
        }

        if ( ctext_length < 1 )
        {
          printf("strfc-E2 C text length: [%d] is nonsense.\n", ctext_length );
          *ctext_lnb=0;
          return;
        }

/*      Find the end of the specified FORTRAN text.
        This is another Fortran routine, so just pass the descriptor given to this routine:
*/
        strend( ftext_descriptor, &length );

/*      printf("length: %d .\n",length);
*/
        if (length>(ctext_length-1))   /*  Make sure the given FORTRAN text isn't too long:  */
        {
          printf("strfc-E3 FORTRAN text: [%s] is too long.\n", ftext_descriptor->dsc$a_pointer);
          *ctext_lnb=0;
          return;
        }
/*      end if (length>(ctext_length-1))   Make sure the given FORTRAN text isn't too long:  */

        strncpy( ctext, ftext_descriptor->dsc$a_pointer, length);

        ctext[length] = 0; /*  Append the c-required null.   */
        *ctext_lnb=length;


/*
        printf("Local ftext is [%s].\n",ftext_descriptor->dsc$a_pointer );
        printf("Local length is [%d].\n",length);
*/

        return;
}





/*      Define procedures ("FORTRAN and C-callable functions"):     */





strbyte_closer()
{
        if (fpr)
          fclose(fpr);
/*      end if (fpr)       */
}





strbyte_closew()
{
        if (fpw)
          fclose(fpw);
/*      end if (fpw)       */
}





int strbyte_openr(

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
  Input:
*/
        struct dsc$descriptor_s *filename_descriptor   /*  ASCII filename descriptor;  Fortran CHARACTER*(n).  */

               )


/*  Description:

        Open the old file for reading, named in <filename>, in byte-stream mode.    */
{


        static char local_filename[256];  /* fopen needs a local (non-pointer) filename. */
        int dummy;
        void strfc( struct dsc$descriptor_s *ftext, int ftext_length, char *ctext, int ctext_length, int *ctext_lnb );


        strfc( filename_descriptor, dummy, local_filename, 256, &dummy );

        if ( (fpr = fopen(local_filename,"r")))
          {
          printf("strbyte_openr-I1 Input file: [%s] opened.\n",local_filename);
          return TRUE;
          }

        else
          {
          printf("strbyte_openr-E1 didn't open file:\n[%s].\n",local_filename);
          return FALSE;
          }

/*      end if ( (fpr = fopen(local_filename,"r")))           */

}




int strbyte_openw(

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
  Input:
*/
        struct dsc$descriptor_s *filename_descriptor   /*  ASCII filename descriptor;  Fortran CHARACTER*(n).  */

               )


/*  Description:

        Open the new file for writing, named in <filename>, in byte-stream mode.    */
{


        static char local_filename[256];  /* fopen needs a local (non-pointer) filename. */
        int dummy;
        void strfc( struct dsc$descriptor_s *ftext, int ftext_length, char *ctext, int ctext_length, int *ctext_lnb );


        strfc( filename_descriptor, dummy, local_filename, 256, &dummy );

        if ( (fpw = fopen(local_filename,"w")))
          {
          printf("strbyte_openw-I1 Output file: [%s] opened.\n",local_filename);
          return TRUE;
          }

        else
          {
          printf("strbyte_openw-E1 didn't open file:\n[%s].\n",local_filename);
          return FALSE;
          }

/*      end if ( (fpw = fopen(local_filename,"w")))           */

}





strbyte_read( int *Nbytes, void *buffer )
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
/*        end if (Nbytes_from_file != *Nbytes)    */

          *Nbytes=Nbytes_from_file;

        }
        else
          *Nbytes = 0;
/*      end if (fpr)       */
}





strbyte_write( int *Nbytes, void *buffer )
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
/*        end if (Nbytes_to_file != *Nbytes)    */

        }
/*      end if (fpw)       */
}
