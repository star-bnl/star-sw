	INTEGER*2 FUNCTION i2_array_val(buf,index)
	
C+
C 
C FUNCTIONAL DESCRIPTION:
C 
C    Returns the value of buf(index).
C 
C FORMAL PARAMETERS:
C  
C     buf = INT*2 array
c	index = location in buf array to return
C  
C [common blocks]
C 
C FUNCTION VALUE:
C 
C    buf(index)
C 
C [design]
C-
	IMPLICIT NONE
	INTEGER*2 buf(*)
	INTEGER*4 index
 
              
              
 
	i2_array_val = buf(index)
	return
	END
