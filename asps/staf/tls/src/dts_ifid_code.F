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
	INTEGER*4 ifid, fmt, ver
	INTEGER*4 dts_fmt_mask
	PARAMETER (dts_fmt_mask = '0000ffff'x)
!	[specification_statement]...
 
      
	fmt = ifid .and. dts_fmt_mask
	ver = ishft(ifid,-16)
!	[executable_statement]...
	RETURN
	END
 
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
	INTEGER*4 ifid, fmt, ver
	INTEGER*4 dts_fmt_mask
	PARAMETER (dts_fmt_mask = '0000ffff'x)
!	[specification_statement]...
 
                   
	ifid = (fmt .and. dts_fmt_mask) + ishft(ver,16)
!	[executable_statement]...
	RETURN
	END
