* $Id: gstar.g,v 1.9 2003/12/19 18:48:41 potekhin Exp $
*
* $Log: gstar.g,v $
* Revision 1.9  2003/12/19 18:48:41  potekhin
* a) added the CVS tag
* b) removed readtab and readxfd references
*
   subroutine    GSTAR 
   entry   gstar_start
   print *,' **********************************************************'
   print *,' *                                                        *'
   print *,' *             GSTAR specific parts are:                  *'
   print *,' *                                                        *'
   print *,' *  gstar_part    - additional particles (like laserino)  *'
           call gstar_part
   print *,' *  gstar_input   - utility for USER/UNPUT U ... command: *'
   print *,' *  gstar_micky   - pseudo-physics generator (M-input)    *'
           call gstar_micky
   print *,' *                                                        *'
   print *,' **********************************************************'
           call HEPEVNT
   end

