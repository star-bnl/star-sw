* $Id: scv_state.F,v 1.2 1998/03/24 00:42:05 fisyak Exp $
* $Log: scv_state.F,v $
* Revision 1.2  1998/03/24 00:42:05  fisyak
* Clean
*
      LOGICAL FUNCTION SCV_STATE( ivalue, flag )
      IMPLICIT NONE
C   Input arguments (One to a line with definition after ! ) 
      INTEGER ivalue		! condition value to be checked
C   Output arguments : 
      CHARACTER*(1) flag 	! indicates level of severity being checked
				! S, I, W, E
C   Returned value  : TRUE if severity of ivalue is .LE. flag
C			FALSE if severify of ivalue is .GT. flag
C       If flag is not a valid SCV value (SIWE)
C       then TRUE is returned for odd values of ivalue
C       and FALSE is returned for even values of ivalue (VMS style),
C       and a bug-message is issued through the STAR message facility.
C
C   Functional Description : SCV_STATE checks the condition value (ivalue)
C	for the level of severity.  The level of severity (increasing)
C	are S, I, W, E for success, informational success, warning and error
C	respectively.
C
C   Created  2-NOV-1991   Doug Olson, Bob Hackenburg
C   Error conditions :   invalid values of ivalue or flag
C----------------------------------------------------------------------
      INTEGER mask, severity, iflag, max_severity, iand
      CHARACTER*80 bug_message
      INTEGER mid1, mid2, mid3
C----------------------------------------------------------------------
      DATA mask/255/	! severity codes are the low order 8 bits
      DATA max_severity/4/	!  maximum value for severity codes
      DATA mid1, mid2, mid3 /0,0,0/
C----------------------------------------------------------------------
      severity = iand(ivalue,mask)
      iflag = index('SIWE',flag)
      IF(iflag .eq. 0) iflag = index('siwe',flag)
      IF( iflag .le. 0) THEN
         WRITE(bug_message,1) flag
1        FORMAT('scv_state-B1  Unrecognized flag [',A1,
     1          '].  Must be one of:  S, I, W or E.')
         CALL MESSAGE(bug_message,1,mid1) !Issue the message.
        SCV_STATE = .FALSE.
      END IF
      IF( severity .gt. max_severity) THEN
         WRITE(bug_message,2) severity, max_severity, ivalue
2        FORMAT('scv_state-B2  Bad sev. code [',I4,
     1          '] is > [',i3,'], ivalue = ',i9)
c         CALL MESSAGE(bug_message,1,mid2) !Issue the message.
C        SCV_STATE = .FALSE.
C  treat this as flag = 'I' 
         iflag = 2
         severity = 2 + mod(severity,2)
      END IF
      IF( iflag .eq. 2) THEN                      ! flag = 'I'
         IF( mod(severity,2).eq.1 .and. severity.le.3) THEN
            SCV_STATE = .TRUE.
         ELSE
            SCV_STATE = .FALSE.
         ENDIF
      ELSEIF(iflag.eq.1) THEN                     ! flag = 'S'
         IF( severity.eq.1 ) THEN
            SCV_STATE = .TRUE.
         ELSE
            SCV_STATE = .FALSE.
         ENDIF
      ELSEIF(iflag.eq.3) THEN                     ! flag = 'W'
         IF( severity.le.3 ) THEN
           SCV_STATE = .TRUE.
         ELSE
            SCV_STATE = .FALSE.
         ENDIF
      ELSEIF(iflag.eq.4) THEN                     ! flag = 'E'
         IF( severity.le.4 ) THEN
            SCV_STATE = .TRUE.
         ELSE
            SCV_STATE = .FALSE.
         ENDIF
      ELSE
C        To get here should be impossible, and that's
C        a bug!  Let them know about their flawed ways:
         WRITE(bug_message,3) ivalue, flag
3        FORMAT('scv_state-B3  Impossible condition, ivalue=[',I9,
     1          '], flag=[',A1,']')
         CALL MESSAGE(bug_message,1,mid3) !Issue the message.
        SCV_STATE = .FALSE.
      ENDIF
      RETURN
      END
