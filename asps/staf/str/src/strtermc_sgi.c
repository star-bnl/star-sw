/*

                 This file is:  strtermc_sgi.c

                      Version:  1.00

        This code provides some common special-purpose terminal application routines,
such as synchronous terminal I/O (I/O waiting or blocking), asynchronous terminal I/O
(no I/O waiting or nonblocking I/O), record terminal I/O (line I/O or nonpunctual I/O)
and character terminal I/O (byte I/O or puntual I/O).

        The code in this file is platform dependent, written with the SGI c compiler,
invoked with the cc command, but should work on all Unix platforms.  The equivalent
VMS code is done in FORTRAN, in STRTERM_VMS.FOR.

        This code, in its original form, is a melange of both original and borrowed
code;  the borrowed code (not easily recognizable) is from "Advanced C Programming",
by ____,chapter 4, "Terminal I/O".

        Some procedures, or subroutines, defined in this file are intended for local
use only, by other c-routines in this same file, and are not a part of the general
STR package, and are not FORTRAN-callable.

        The FORTRAN-callable routines defined here are equivalent to those defined in
STRTERM_VMS.FOR on VMS platforms.

        Note that the order of function definitions in this file is important on
some platforms, such as the DECstation 5000 running ULTRIX;  alphabetical order
doesn't work.

        FORTRAN-callable routines ("void" ==> no argument or no return value)
*/

        void strterm_get_char_(long int *);
        void strterm_get_char_from_line_(long int *);
        void strterm_set_char_(void);
        void strterm_set_line_(void);
        void strterm_set_normal_(void);
        void strterm_set_nowait_(void);
        void strterm_set_wait_(void);

/*
        Non-FORTRAN-callable (internal-usage) routines:
*/

        char strterm_get_next_char(void);






/*
        File-wide "global" definitions:
*/

#include <fcntl.h>
#include <stdio.h>
#include <termio.h>
#include <unistd.h>

        int     ioctl( int, int, ... );
        int     msg_enabled_trace_( char *, int *, size_t );
        void    message_( char *, int *, int *, size_t );

static struct termio str_buf_save;
       struct termio str_buf;
static char          str_inbuf[10];
static int           str_inbuf_total=0, str_inbuf_next=0;

static int str_term_mode_nowait_P=1; /* nowait mode bit mask    */
static int str_term_mode_char_P  =2; /* character mode bit mask */

static int str_term_mode=0;






/*      The first group of routines are for local use only, and are not
        FORTRAN-callable.
*/


char strterm_get_next_char(void)
{
/*
  Not FORTRAN callable.

  No arguments

  Return value:
        char  -- contains the next byte read from the terminal, or a null if
        nothing is ready, or -1 if some kind of low-level read error occured.

  Description:
        Get a single character from the terminal buffer, w/o waiting (asynchronous),
        and without waiting for a complete line (ie, no EOT character needed to make
        characters available).  Return with the null byte as the character if no
        character is ready at the terminal or in the terminal buffer (ie, don't wait).
        The terminal should be in "char" mode (ie, no EOT required to make characters
        available).

  Warning:
        A Unix ambiguity fails to distinguish between EOF, sometimes
        indicated with a ^D from an input stream, but otherwise being
        a specific signal from the input device indicating that there
        is no (more) data available, and a "no character avaiable"
        condition.

        This will result in an unpleasant surprise if a program is exitted
        while in nowait-character mode:  The Unix shell begins receiving
        null bytes from the terminal, instead of waiting for real characters.
        This being indistiguishable under Unix from an EOF, or ^D, the session
        will be immediately ended, and the user logged off!!!
*/


        if (str_inbuf_next >= str_inbuf_total)
        {

/*        Need to read some more -- the small buffer is exhausted. */

          switch (str_inbuf_total = read(0, str_inbuf, sizeof(str_inbuf)))
          {

          case -1:    /* Indicate a low-level read-error of some kind.   */
            (void)printf("strterm_get_next_char-E1 read error\n");
            return(-1);

          case 0:     /* No characters available -- return a null byte.  */
            return(0);

          default:    /* Successful read of str_inbuf_total bytes -- initialize pointer.  */
            str_inbuf_next = 0;

          }
/*        end switch (str_inbuf_total = read(0, str_inbuf, sizeof(str_inbuf)))  */

        }

/*      A useful debug/trace statement:                                            */
/*      (void)printf("strterm_get_next_char-D1 c:%c\n",str_inbuf[str_inbuf_next]); */

        return(str_inbuf[str_inbuf_next++]);  /* Return the next byte and increment pointer. */

} /* char strterm_get_next_char(void) */








/*      The remaining routines are FORTRAN-callable.     */



void strterm_get_char_( long int *i1 )
{
/*
  FORTRAN callable:

        CALL STRTERM_GET_CHAR( Ichar )
*  Input:
        INTEGER Ichar !Integer-value of character read from the terminal
                      !in either "wait" or "nowait" mode, as set by the
                      !routines STRTERM_SET_WAIT, STRTERM_SET_NOWAIT,
                      !STRTERM_SET_NORMAL STRTERM_SET_LINE, and STRTERM_SET_CHAR.
                      !If in "nowait" mode, and if nothing is in the input
                      !buffer when this routine is called, then Ichar
                      !returns set to zero.  Unfortunately, a Unix bug
                      !renders the same effect on EOF (^D).

  Output:
        integer i1 -- passed by reference (pointer or address).

  Return value:  none ("void")

  Description:
        Get a single character from the terminal, w/o waiting, and without
        waiting for a complete line (ie, no EOT character needed to make
        characters available).
        Return with the null byte as the character if no
        character is ready at the terminal (ie, don't wait).

  Warning:
        A Unix ambiguity fails to distinguish between EOF, sometimes
        indicated with a ^D from an input stream, but otherwise being
        a specific signal from the input device indicating that there
        is no (more) data available, and a "no character avaiable"
        condition.

        This will result in an unpleasant surprise if a program is exitted
        while in nowait mode:       The Unix shell begins receiving
        null bytes from the terminal, instead of waiting for real characters.
        This being indistiguishable under Unix from an EOF, or ^D, the session
        will be immediately ended, and the user logged off!!!
*/

        char c;
        int n;

        if (str_term_mode != (str_term_mode_char_P | str_term_mode_nowait_P) )  /*Need to set the correct mode*/
        {
          (void)strterm_set_nowait_();    /* Set terminal I/O waiting off.*/
          (void)strterm_set_char_();      /* Set terminal into single-char. mode*/
        }


        switch(c=strterm_get_next_char())    /* Try to read a character (or get next one in buffer) */
        {
        case -1:                                 /* Some kind of low-level read-error  */
          (void)printf("strterm_get_char-E1 read error\n");
          (void)strterm_set_normal_();
          *i1 = -1;      /* Return an error indicator  */
        case 0:
          *i1 = 0;       /* Return a null byte */
        default:
          *i1 = (int) c & 255; /* Prevent sign extension */
        }
/*      end switch(c=strterm_get_next_char())  */

        return;

} /* void strterm_get_char_(long int *i1) */





void strterm_get_char_from_line_(long int *i1)
{
/*
  FORTRAN callable.

  Output:
        integer i1 -- passed by reference (pointer or address).

  Return value:  none ("void")

  Description:
        Get a character from the terminal, w/o waiting.
        Wait for a complete line before transmitting.
        Return with the null byte as the character if no
        character is ready at the terminal (ie, don't wait).

  Warning:
        A Unix ambiguity fails to distinguish between EOF, sometimes
        indicated with a ^D from an input stream, but otherwise being
        a specific signal from the input device indicating that there
        is no (more) data available, and a "no character avaiable"
        condition.

        This will result in an unpleasant surprise if a program is exitted
        while in nowait mode:       The Unix shell begins receiving
        null bytes from the terminal, instead of waiting for real characters.
        This being indistiguishable under Unix from an EOF, or ^D, the session
        will be immediately ended, and the user logged off!!!
*/

        static char c;
        static int n;

        (void)strterm_set_nowait_();      /* Set terminal I/O waiting off.*/

        n=read(0, &c, 1);           /*Try to read a character "c" from a line*/

        if (n < 0)        /* This is some kind of low-level read error.  */
        {
          (void)printf("strterm_get_char_from_line-E1 read error\n");
          *i1 = -1;       /*  Send back an error indication  */
        }
        else if (n == 0)  /*  This is either no-character, or EOF (^D).  */
        {
          *i1 = 0;        /*  Send back a null byte */
        }
        else              /*  A character is available -- return it.     */
        {
          *i1 = (int) c & 255;  /* Prevent sign extension */
        }
/*      end if (n < 0)    */

        (void)strterm_set_wait_();   /* Set terminal I/O waiting on. */

        return;

} /* void strterm_get_char_from_line_(long int *i1) */





void strterm_set_char_(void)
{
/*
  FORTRAN callable:

        CALL STRTERM_SET_CHAR

  No arguments

  Return value:  none

  Description:
        Set the terminal into single-character ("punctual") I/O mode.
        ie, make characters available immediately, without waiting for
        an EOT (End Of Transmission, eg, a Return, Line Feed, etc.).
        This is independent of whether terminal I/O is synchronous
        (reads wait until something is really read) or asynchronous
        (reads return a null byte if nothing is read).
*/

        int static id = 0;
        int nlines = 1;

        if ( (msg_enabled_trace_( "strterm_set_char-t1", &id , (sizeof("strterm_set_char-t1")-1) ) ) != 0 )
        {
          message_( "strterm_set_char-t1  (single) character mode.", &nlines, &id
                  , (sizeof("strterm_set_char-t1  (single) character mode.")-1) );

        }  /* end if msg_enabled_trace_( "strterm_set_char-t1 ...  */

        if ( (str_term_mode & str_term_mode_char_P) != str_term_mode_char_P )
        {
/*        It's not in the right mode -- set it.  */

          if (ioctl(0, TCGETA, &str_buf) == -1)
          {
            (void)printf("strterm_set_char-E3 ioctl error\n");
            return;  /* Failure */
          }

          str_buf_save = str_buf;
          str_buf.c_lflag &= ~ICANON;
          str_buf.c_cc[4] = sizeof(str_inbuf);  /* MIN  (size of buffer)              */
          str_buf.c_cc[5] = 2;                  /* TIME (delay-time in tenth-seconds) */
        }

        if (ioctl(0, TCSETAF, &str_buf) == -1)
        {
          (void)printf("strterm_set_char-E4 ioctl2 error\n");
          return;  /* Failure */
        }

        str_term_mode = str_term_mode | str_term_mode_char_P;  /* Set the character-mode bit */

        return; /* Successful return */
} /* void strterm_set_char_(void) */





void strterm_set_line_(void)  /* Set terminal to wait-for-line mode  */
{
/*
  FORTRAN callable:

        CALL STRTERM_SET_LINE

  No arguments

  Return value:  none ("void")

  Description:
        Set the terminal into "line" I/O mode.  ie, characters are not
        available until an EOT (End Of Transmission, eg, a Return, Line Feed,
        etc.) is sent.
        This is independent of whether terminal I/O is synchronous
        (reads wait until something is really read) or asynchronous
        (reads return a null byte if nothing is read).
*/
        int static id = 0;
        int nlines = 1;

        if ( (msg_enabled_trace_( "strterm_set_line-t1", &id, (sizeof("strterm_set_line-t1")-1) ) ) != 0 )
        {
          message_( "strterm_set_line-t1  Wait-line mode.", &nlines
                  , &id, (sizeof("strterm_set_line-t1  Wait-line mode.")-1) );

        }  /* end if msg_enabled_trace_( "strterm_set_line-t1 ...  */

        if ( (str_term_mode & str_term_mode_char_P) != str_term_mode_char_P )
        {
          return;  /* It's already in the desired mode. */
        }


/*      Set the mode.                                   */
        if (ioctl(0, TCSETAF, & str_buf_save) == -1)
        {
          (void)printf("strterm_set_line-E1 ioctl3 error\n");
        }


        str_term_mode = str_term_mode & ~str_term_mode_char_P;  /* Clear the character-mode bit */

        return;
} /* void strterm_set_line_(void)  */





void strterm_set_normal_(void)
{
/*
  FORTRAN callable:

        CALL STRTERM_SET_NORMAL

  No arguments

  Return value:  none ("void")

  Description:
        Set the terminal into "normal" I/O mode.  ie, wait at a read for
        input to be typed, and do not make characters available until an
        EOT (End Of Transmission, eg, a Return, Line Feed, etc.) is sent.
*/

        (void)strterm_set_wait_();        /* Set terminal I/O waiting on.*/
        (void)strterm_set_line_();       /* Set terminal into line-mode*/

        return;
} /* void strterm_set_normal_(void) */





void strterm_set_nowait_(void) /* Turn terminal I/O waiting off */
{
/*
  FORTRAN callable:

        CALL STRTERM_SET_NOWAIT

  No arguments

  Return value:  none ("void")

  Description:
        Set the terminal into "nowait", or asynchronous I/O mode.  ie, if
        no characters are available at a read, a null byte is returned
        and program flow continues, rather than waiting for something to be
        transmitted.
*/
        static int nowaitf, first=1;
        static int flags;

        int static id = 0;
        int nlines = 1;

        if ( (msg_enabled_trace_( "strterm_set_nowait-t1", &id, (sizeof("strterm_set_nowait-t1")-1) )) != 0 )
        {
          message_( "strterm_set_nowait-t1  No-wait mode.", &nlines
                  , &id, (sizeof("strterm_set_nowait-t1  No-wait mode.")-1) );

        }  /* end if msg_enabled_trace_( "strterm_set_nowait-t1 ...  */


/*      There's no need to do this more than once -- do it on first call only: */
        if (first)
        {
          first = 0;  /* Prevent this from being repeated on each call.  */

/*        Setup the necessary flag-bits for use in setting the mode:     */
          if ((flags = fcntl(0, F_GETFL, 0)) == -1)
          {
            (void)printf("strterm_set_nowait-E1 fcntl error\n");
            return;
          }

          nowaitf = flags |  O_NDELAY;  /* Make sure O_NDELAY is set   */

        }
/*      end if (first)  */


/*      Now that nowaitf is set up, use it to set the mode:  */
        if (fcntl(0,F_SETFL, nowaitf) == -1)
        {
          (void)printf("strterm_set_nowait-E2 fcntl error\n");
          return;
        }


        str_term_mode = str_term_mode | str_term_mode_nowait_P;  /* Set the nowait mode bit */

        return;
} /* void strterm_set_nowait_(void) */





void strterm_set_wait_(void) /* Turn terminal I/O waiting on */
{
/*
  FORTRAN callable:

        CALL STRTERM_SET_WAIT

  No arguments

  Return value:  none ("void")

  Description:
        Set the terminal into "wait", or synchronous I/O mode.  ie, if
        no characters are available at a read,  program flow halts,
        waiting for something to be transmitted, rather than returning
        a null byte and continuing.  This is the "normal" mode.
*/
        static int waitf, first=1;
        static int flags;

        int static id = 0;
        int nlines = 1;

        if ( (msg_enabled_trace_( "strterm_set_wait-t1", &id, (sizeof("strterm_set_wait-t1")-1) ) ) != 0 )
        {
          message_( "strterm_set_wait-t1  Wait mode.", &nlines
                  , &id, (sizeof("strterm_set_wait-t1  Wait mode.")-1) );

        }  /* end if msg_enabled_trace_( "strterm_set_wait-t1 ...  */

/*      There's no need to do this more than once -- do it on first call only: */
        if (first)
        {
          first = 0;  /* Prevent this from being repeated on each call.  */

/*        Setup the necessary flag-bits for use in setting the mode:     */
          if ((flags = fcntl(0, F_GETFL, 0)) == -1)
          {
            (void)printf("strterm_set_wait-E1 fcntl error\n");
            return;
          }

          waitf   = flags & ~O_NDELAY;  /* Make sure O_NDELAY is clear */
        }
/*      end if (first)  */


/*      Now that waitf is set up, use it to set the mode:  */
        if (fcntl(0,F_SETFL, waitf) == -1)
        {
          (void)printf("strterm_set_wait-E2 fcntl error\n");
          return;
        }

        str_term_mode = str_term_mode & ~str_term_mode_nowait_P;  /* Clear the nowait mode bit */

        return;
} /* void strterm_set_wait_(void) */






