* $Id: g2t_field.g,v 1.1 2000/01/11 17:29:57 nevski Exp $
* $Log: g2t_field.g,v $
* Revision 1.1  2000/01/11 17:29:57  nevski
* a separate g2t_field function provided to extract nominal field
*
*
  Function   G2T_FIELD(x)
* returns a nominal field value from input data 
*
  Implicit   NONE
  Real       G2T_field,x
  Structure  MFLG  {version, BField }
  Integer    imag/0/
  logical    First/.true./
 
  if (first) then
     Call RbPUSHD
     USE /DETM/MFLD/MFLG  stat=imag
     Call RbPOPD
     first = .false.
  endif
  G2T_field = mflg_Bfield
     
  end
