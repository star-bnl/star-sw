* $Id: tpe_close.F,v 1.1 1998/01/27 00:49:13 fisyak Exp $
* $Log: tpe_close.F,v $
* Revision 1.1  1998/01/27 00:49:13  fisyak
* Split sources
*
        SUBROUTINE TPE_CLOSE

        Implicit None

C----------------------------------------------------------------------

        call hcdir('//NTUP',' ')
        call hrend('NTUP')
        close(23)

        return
        end
