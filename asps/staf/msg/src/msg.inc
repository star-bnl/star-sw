*	This is the basic msg controls and data-structure include-file:  msg.inc

*	Maximum number of unique message-prefixes:
	INTEGER MSG_Nprefixes_max_P
	PARAMETER (MSG_Nprefixes_max_P=1000)

*	Maximum prefix length:
	INTEGER MSG_prefix_length_P
	PARAMETER (MSG_prefix_length_P=48)

*	Maximum "last-message" sample length:
	INTEGER MSG_sample_length_P
	PARAMETER (MSG_sample_length_P=64)

*	Maximum Node-name length:
	INTEGER MSG_Node_name_length_P
	PARAMETER (MSG_Node_name_length_P=48)


*	msg LUNs -- Journal Lun and Terminal Lun:
	INTEGER MSG_JL,MSG_TL
	COMMON /MSG_I/ MSG_JL, MSG_TL

*	Journal-enabled control-flag:
	LOGICAL MSG_Journal_Enable
	COMMON /MSG_I/ MSG_Journal_Enable

*	msg Summary output page length:
	INTEGER MSG_Summary_Page_Length
	COMMON /MSG_I/ MSG_Summary_Page_Length

*	msg new-definition defaults:
	LOGICAL MSG_All_Disable     !If true, newly defined messages are disabled.
	LOGICAL MSG_All_Nocount     !If true, newly defined messages won't count.
	INTEGER MSG_All_Count_Limit !Newly defined messages get this count-limit.
	COMMON /MSG_I/ MSG_All_Disable
	COMMON /MSG_I/ MSG_All_Nocount
	COMMON /MSG_I/ MSG_All_Count_Limit

*	msg count of total slow prefix-lookups (ie, character-searches):
	INTEGER MSG_Total_Lookups
	COMMON /MSG_I/ MSG_Total_Lookups


*	The Prefixes' data structure:
	INTEGER MSG_Nprefixes   !Number of prefixes now defined.
	INTEGER MSG_Length      !Length of each defined prefix.
	INTEGER MSG_Counts	!Occurance-counts for each defined prefix.
	INTEGER MSG_Lookups     !Slow-lookup-counts for each defined prefix.
	INTEGER MSG_Count_limit !Occurance-count at which each defined prefix is disabled.
	INTEGER MSG_Abort_limit !Occurance-count at which each defined prefix causes an abort.
	LOGICAL MSG_Active      !Whether each prefix is active (enabled -- produces a message).
	LOGICAL MSG_Counting    !Whether each is counting.
	COMMON /MSG_I/ MSG_Nprefixes
	COMMON /MSG_I/ MSG_Length(MSG_Nprefixes_max_P)
	COMMON /MSG_I/ MSG_Counts(MSG_Nprefixes_max_P)
	COMMON /MSG_I/ MSG_Lookups(MSG_Nprefixes_max_P)
	COMMON /MSG_I/ MSG_Count_limit(MSG_Nprefixes_max_P)
	COMMON /MSG_I/ MSG_Abort_limit(MSG_Nprefixes_max_P)
	COMMON /MSG_I/ MSG_Active(MSG_Nprefixes_max_P)
	COMMON /MSG_I/ MSG_Counting(MSG_Nprefixes_max_P)


*	Note:  Use a different named common here because not all machines
*	mix character & integer data the same way:

	CHARACTER*(MSG_Node_name_length_P) MSG_Node_name !Application-specified node-name.
	COMMON /MSG_C/ MSG_Node_name                     !(See subroutine MSG_Name_Node)

	CHARACTER*(MSG_prefix_length_P) MSG_Prefix       !Each defined prefix.
	CHARACTER*(MSG_sample_length_P) MSG_Sample       !Sample from last-occurance of a message for each prefix.
	COMMON /MSG_C/ MSG_Prefix(MSG_Nprefixes_max_P)
	COMMON /MSG_C/ MSG_Sample(MSG_Nprefixes_max_P)
