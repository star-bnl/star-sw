
	INTEGER str_term_mode_nowait_P !nowait mode bit mask
	PARAMETER (str_term_mode_nowait_P=1)
	INTEGER str_term_mode_char_P   !character mode bit mask
	PARAMETER (str_term_mode_char_P  =2)

	INTEGER str_term_mode
	COMMON/STRTERM/str_term_mode

	INTEGER str_term_channel
	COMMON/STRTERM/str_term_channel
