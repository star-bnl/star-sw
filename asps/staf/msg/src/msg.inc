*	This is the basic msg controls and data-structure include-file:  msg.inc

*	Maximum number of unique message-prefixes:
	INTEGER MSG_Nprefixes_max_P
	PARAMETER (MSG_Nprefixes_max_P=1000)

*	Maximum prefix length:
	INTEGER MSG_prefix_length_P
	PARAMETER (MSG_prefix_length_P=48)

*	Maximum number of unique message-classes:
	INTEGER MSG_Nclasses_max_P
	PARAMETER (MSG_Nclasses_max_P=1000)

*	Maximum class length:
	INTEGER MSG_class_length_P
	PARAMETER (MSG_class_length_P=1)

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

*	Time-stamp mode-select:
	LOGICAL MSG_TimeStamp_CPU        !If true, time-stamps occur on changed CPU times.
	COMMON /MSG_I/ MSG_TimeStamp_CPU !(Otherwise, only stamp a changed real-time or node name.)

*	Whether ID list is sorted:
	LOGICAL MSG_Sorted
	COMMON /MSG_I/ MSG_Sorted

*	msg Summary output features:
	INTEGER MSG_Summary_Page_Length
	LOGICAL MSG_Summary_Mode_Active   !Whether Active messages are listed (default = T).
	LOGICAL MSG_Summary_Mode_Counting !Whether Counting (but not displaying) messages are listed (default = T).
	LOGICAL MSG_Summary_Mode_Inactive !Whether Inactive messages are listed (default = F).
	LOGICAL MSG_Summary_Mode_Aborted  !Whether the Aborted message is listed (default = T).
	COMMON / MSG_I / MSG_Summary_Page_Length
	COMMON / MSG_I / MSG_Summary_Mode_Active
	COMMON / MSG_I / MSG_Summary_Mode_Counting
	COMMON / MSG_I / MSG_Summary_Mode_Inactive
	COMMON / MSG_I / MSG_Summary_Mode_Aborted

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
	INTEGER MSG_Nclasses    !Number of classes now defined.
	INTEGER MSG_SID         !Sorted ID list.
	INTEGER MSG_Iclass      !Class ID of each defined prefix.
	INTEGER MSG_Length      !Length of each defined prefix.
	INTEGER MSG_Counts	!Occurance-counts for each defined prefix.
	INTEGER MSG_Lookups     !Slow-lookup-counts for each defined prefix.
	INTEGER MSG_CPU_Mark    !CPU usage at Mark -- see MSG_Mark in msglib.f .
	INTEGER MSG_CPU_Total   !"Total" CPU usage -- see MSG_Mark in msglib.f .
	INTEGER MSG_Count_limit !Occurance-count at which each defined prefix is disabled.
	INTEGER MSG_Abort_limit !Occurance-count at which each defined prefix causes an abort.
	LOGICAL MSG_Active      !Whether each prefix is active (enabled -- produces a message).
	LOGICAL MSG_Counting    !Whether each is counting.
	COMMON /MSG_I/ MSG_Nprefixes
	COMMON /MSG_I/ MSG_Nclasses
	COMMON /MSG_I/ MSG_SID(         MSG_Nprefixes_max_P )
	COMMON /MSG_I/ MSG_Iclass(      MSG_Nprefixes_max_P )
	COMMON /MSG_I/ MSG_Length(      MSG_Nprefixes_max_P )
	COMMON /MSG_I/ MSG_Counts(      MSG_Nprefixes_max_P )
	COMMON /MSG_I/ MSG_Lookups(     MSG_Nprefixes_max_P )
	COMMON /MSG_I/ MSG_CPU_Mark(    MSG_Nprefixes_max_P )
	COMMON /MSG_I/ MSG_CPU_Total(   MSG_Nprefixes_max_P )
	COMMON /MSG_I/ MSG_Count_limit( MSG_Nprefixes_max_P )
	COMMON /MSG_I/ MSG_Abort_limit( MSG_Nprefixes_max_P )
	COMMON /MSG_I/ MSG_Active(      MSG_Nprefixes_max_P )
	COMMON /MSG_I/ MSG_Counting(    MSG_Nprefixes_max_P )


	LOGICAL MSG_Class_Default_Active      !Default active-state for messages of each class.
	LOGICAL MSG_Class_Default_Counting    !Default counting-state for messages of each class.
	INTEGER MSG_Class_Default_Count_Limit !Default count limit for messages of each class.
	INTEGER MSG_Class_Default_Abort_Limit !Default abort limit for messages of each class.
	COMMON /MSG_I/ MSG_Class_Default_Active(      0:MSG_Nclasses_max_P  )
	COMMON /MSG_I/ MSG_Class_Default_Counting(    0:MSG_Nclasses_max_P  )
	COMMON /MSG_I/ MSG_Class_Default_Count_Limit( 0:MSG_Nclasses_max_P  )
	COMMON /MSG_I/ MSG_Class_Default_Abort_Limit( 0:MSG_Nclasses_max_P  )


*	Note:  Use a different named common here because not all machines
*	mix character & integer data the same way:

	CHARACTER*( MSG_Node_name_length_P ) MSG_Node_name !Application-specified node-name.
	COMMON /MSG_C/ MSG_Node_name                       !(See subroutine MSG_Name_Node)

	CHARACTER*( MSG_prefix_length_P ) MSG_Prefix       !Each defined prefix.
	CHARACTER*( MSG_sample_length_P ) MSG_Sample       !Sample from last-occurance of a message for each prefix.
	COMMON /MSG_C/                    MSG_Prefix( MSG_Nprefixes_max_P )
	COMMON /MSG_C/                    MSG_Sample( MSG_Nprefixes_max_P )

	CHARACTER*( MSG_Class_length_P  ) MSG_Class        !Each defined class.
	COMMON /MSG_C/                    MSG_Class(  MSG_Nclasses_max_P  )
