* $Id: dts_ifid_encode.F,v 1.1 1998/03/24 00:42:01 fisyak Exp $
* $Log: dts_ifid_encode.F,v $
* Revision 1.1  1998/03/24 00:42:01  fisyak
* Clean
*
	SUBROUTINE DTS_IFID_ENCODE(ifid, fmt, ver)
 
C+
C 
C FUNCTIONAL DESCRIPTION:	
C 
C    Encodes the format (fmt) and version (ver) numbers into an
c	ifid parameter for a dts_put_item.
C 
C FORMAL PARAMETERS:
C  
C     ifid = ifid parameter for a dts_put_item		  (output)
c	fmt = format number				    (input)
c	ver = version number				    (input)
C  
C 
C-
	IMPLICIT NONE
	INTEGER*4 ifid, fmt, ver, iand
	INTEGER*4 dts_fmt_mask
	PARAMETER (dts_fmt_mask = '0000ffff'x)
!	[specification_statement]...
 
                   
	ifid = iand(fmt,dts_fmt_mask) + ishft(ver,16)
!	[executable_statement]...
	RETURN
	END
