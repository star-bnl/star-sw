        PROGRAM STRNORM

*       This program, when run, sets the terminal
*       into "normal" mode, as established by calling
*       strterm_set_normal, from strtermc_sgi.c, or
*       strterm_vms.for .

        IMPLICIT NONE

        CALL STRTERM_SET_NORMAL

        CALL EXIT

        END

