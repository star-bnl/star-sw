* $Id: rzversion.inc,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
* $Name:  $
* $Log: rzversion.inc,v $
* Revision 1.1.1.1  2004/01/12 23:49:41  potekhin
*
* Revision 1.1  2001/12/15 00:27:29  nevski
* upgrade atlsim commons
*
*  additional parameters to steer uncontrolable things in RZ/FZ etc
*  Irz96 - not used anymore, recognized automatically
*  Nrecp - If non zero, overwrites default NHrecp in AGI version of RZMAKE.
*          Otherwise it is 32k in standard cernlib and 1M in AGI.
*          In principal it may be passed in IQUEST(10) by calling HROPEN
*          with Copt='Q', but HRFILE anyway resets it to 100<NQUOTA<65000.
*          This is not needed for the new RZ format.
        integer            NHrecp
        common /rzNHRECP/  NHrecp
*  FZmaxL - replace the default FZ file length limit - 0.5 Gw (2GB/4BpW)
        integer            FzMaxL
        common /FzMaxLen/  FzMaxL
*
