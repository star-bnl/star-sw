* $Id: etime.F,v 1.2 2009/09/03 15:16:37 jeromel Exp $
* $Log: etime.F,v $
* Revision 1.2  2009/09/03 15:16:37  jeromel
* define etime if GFORTRAN
*
* Revision 1.1.1.1  2004/01/12 23:49:38  potekhin
*
*
* Revision 1.2  2001/11/18 20:52:17  nevski
* get rid of stupid warning at link time, add pthread
*
* Revision 1.1  2001/02/27 10:14:50  nevski
*  first working release
*
#if !defined(CERNLIB_LINUX) || defined(GFORTRAN)
        function ETIME(XX)
        real  ETIME,XX(2),tt
        call timex(tt)
        etime=tt
        XX(1)=tt
        XX(2)=tt
        end
#endif

