* $Id: dts_ifid_decode.F,v 1.1 1998/03/24 00:42:00 fisyak Exp $
* $Log: dts_ifid_decode.F,v $
* Revision 1.1  1998/03/24 00:42:00  fisyak
* Clean
*
	SUBROUTINE DTS_IFID_DECODE(ifid,fmt,ver)
 
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    Decodes the ifid parameter from a dts_get_item call to get the
c	proper format (fmt) and version (ver) numbers of the item.
C 
C FORMAL PARAMETERS:
C  
C     ifid = ifid parameter returned by dts_get_item	      (input)
c	fmt = format number encoded in ifid			(output)
c	ver = version number encoded in ifid			(output)
C  
C 
C-
	IMPLICIT NONE
	INTEGER*4 ifid, fmt, ver, iand
	INTEGER*4 dts_fmt_mask
	PARAMETER (dts_fmt_mask = '0000ffff'x)
!	[specification_statement]...
 
      
	fmt = iand (ifid, dts_fmt_mask)
	ver = ishft(ifid,-16)
!	[executable_statement]...
	RETURN
	END
