* $Id: rndpol.F,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
* $Log: rndpol.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:41  potekhin
*
* Revision 1.1  2001/02/27 10:15:50  nevski
*  first working release
*
* Revision 1.1  2000/01/06 22:09:20  nevski
* unlimited track/vertex version
*
*
       REAL FUNCTION RNDPOL(THETA)
*-----------------------------------------------------------------------
*   RNDPOL - Generates random numbers according to a Polya distribution
*            with parameter THETA. Since this is simply a scaled Gamma
*            distribution with parameter 1+THETA, RNGAMA (V135) is used.
*            (Last changed on  6/ 7/95.)
*-----------------------------------------------------------------------
       REAL RNGAMA,THETA
       EXTERNAL RNGAMA
*** Verify the parameter.
       IF(THETA.GT.-1)THEN
            RNDPOL=RNGAMA(1+THETA)/(1+THETA)
       ELSE
            RNDPOL=0
       ENDIF
       END

