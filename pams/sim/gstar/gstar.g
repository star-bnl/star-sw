
   subroutine    GSTAR 
   print *,' **********************************************************'
   print *,' *                                                        *'
   print *,' *             GSTAR specific parts are:                  *'
   print *,' *                                                        *'
   print *,' *  gstar_part    - additional particles (like laserino)  *'
           call gstar_part
   print *,' *  gstar_input   - utility for USER/UNPUT U ... command  *'
   print *,' *  gstar_readxdf - xdr file input (X input mode          *'
   print *,' *  gstar_readtab - staf table input (S-input mode)       *'
   print *,' *  gstar_micky   - pseudo-physics generator (M-input)    *'
   print *,' *                                                        *'
   print *,' **********************************************************'
   end

