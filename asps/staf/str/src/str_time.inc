*	File: str_time.inc

*	Some items to facilitate time zone usage:
*	This is intended for str internal-usage-only.

	INTEGER     Initialized_1_P
	PARAMETER ( Initialized_1_P =  12345678 )
	INTEGER     Initialized_2_P
	PARAMETER ( Initialized_2_P = -87654321 )
	INTEGER           Current_Zone, Initialized_1, Initialized_2
	COMMON /STR_TIME/ Current_Zone, Initialized_1, Initialized_2
	SAVE   /STR_TIME/ 

	CHARACTER*4        Current_Zone_Name
	COMMON /STR_TIMEC/ Current_Zone_Name
	SAVE   /STR_TIMEC/ 
