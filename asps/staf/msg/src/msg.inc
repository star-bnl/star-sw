
	INTEGER MSG_JL,MSG_TL
	LOGICAL MSG_JOURNAL_ENABLE
	COMMON/MSG_I/MSG_JL,MSG_TL,MSG_JOURNAL_ENABLE

	LOGICAL MSG_ALL_DISABLE,MSG_ALL_NOCOUNT
	COMMON/MSG_I/MSG_ALL_DISABLE,MSG_ALL_NOCOUNT

	INTEGER MSG_ALL_COUNT_LIMIT
	COMMON/MSG_I/MSG_ALL_COUNT_LIMIT

	INTEGER MSG_TOTAL_LOOKUPS
	COMMON/MSG_I/MSG_TOTAL_LOOKUPS


	INTEGER MSG_Nprefixes_max_P
	PARAMETER (MSG_Nprefixes_max_P=1000)
	INTEGER MSG_prefix_length_P
	PARAMETER (MSG_prefix_length_P=48)
	INTEGER MSG_sample_length_P
	PARAMETER (MSG_sample_length_P=64)
	INTEGER MSG_Node_name_length_P
	PARAMETER (MSG_Node_name_length_P=48)

	INTEGER MSG_Nprefixes
	INTEGER MSG_Length
	INTEGER MSG_Counts
	INTEGER MSG_Lookups
	INTEGER MSG_Count_limit
	LOGICAL MSG_Active
	LOGICAL MSG_Counting
	COMMON/MSG_I/MSG_Nprefixes
	COMMON/MSG_I/MSG_Length(MSG_Nprefixes_max_P)
	COMMON/MSG_I/MSG_Counts(MSG_Nprefixes_max_P)
	COMMON/MSG_I/MSG_Lookups(MSG_Nprefixes_max_P)
	COMMON/MSG_I/MSG_Count_limit(MSG_Nprefixes_max_P)
	COMMON/MSG_I/MSG_Active(MSG_Nprefixes_max_P)
	COMMON/MSG_I/MSG_Counting(MSG_Nprefixes_max_P)


*	Note:  Use a different named common here because not all machines
*	mix character & integer data the same way:

	CHARACTER*(MSG_Node_name_length_P) MSG_Node_name
	COMMON/MSG_C/MSG_Node_name

	CHARACTER*(MSG_prefix_length_P) MSG_prefix
	CHARACTER*(MSG_sample_length_P) MSG_sample
	COMMON/MSG_C/MSG_prefix(MSG_Nprefixes_max_P)
	COMMON/MSG_C/MSG_sample(MSG_Nprefixes_max_P)
