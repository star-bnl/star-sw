#ifdef WIN32
#include "fortranc.h"

static const char sccsid_msgfH[] = "@(#)"__FILE__"\t\t1.55\tCreated 3/8/98 02:07:38, \tcompiled "__DATE__" "__TIME__;

#define message_                  F77_NAME(message,MESSAGE)
#define message_out_              F77_NAME(message_out,MESSAGE_OUT)
#define msg_class_                F77_NAME(msg_class,MSG_CLASS)
#define msg_count_                F77_NAME(msg_count,MSG_COUNT)
#define msg_disable_              F77_NAME(msg_disable,MSG_DISABLE)
#define msg_display_              F77_NAME(msg_display,MSG_DISPLAY)
#define msg_display_out_          F77_NAME(msg_display_out,MSG_DISPLAY_OUT)
#define msg_display_and_echo_     F77_NAME(msg_display_and_echo,MSG_DISPLAY_AND_ECHO)
#define msg_display_and_echo_out_ F77_NAME(msg_display_and_echo_out,MSG_DISPLAY_AND_ECHO_OUT)
#define msg_enable_               F77_NAME(msg_enable,MSG_ENABLE)
#define msg_enabled_              F77_NAME(msg_enabled,MSG_ENABLED)
#define msg_get_lun_              F77_NAME(msg_get_lun,MSG_GET_LUN)
#define msg_ini_                  F77_NAME(msg_ini,MSG_INI)
#define msg_journal_close_        F77_NAME(msg_journal_close,MSG_JOURNAL_CLOSE)
#define msg_journal_off_          F77_NAME(msg_journal_off,MSG_JOURNAL_OFF)
#define msg_journal_on_           F77_NAME(msg_journal_on,MSG_JOURNAL_ON)
#define msg_journal_open_         F77_NAME(msg_journal_open,MSG_JOURNAL_OPEN)
#define msg_journal_page_         F77_NAME(msg_journal_page,MSG_JOURNAL_PAGE)
#define msg_lun_page_             F77_NAME(msg_lun_page,MSG_LUN_PAGE)
#define msg_parse_                F77_NAME(msg_parse,MSG_PARSE)
#define msg_mark_                 F77_NAME(msg_mark,MSG_MARK)
#define msg_name_node_            F77_NAME(msg_name_node,MSG_NAME_NODE)
#define msg_nocount_              F77_NAME(msg_nocount,MSG_NOCOUNT)
#define msg_set_abort_limit_      F77_NAME(msg_set_abort_limit,MSG_SET_ABORT_LIMIT)
#define msg_set_by_command_       F77_NAME(msg_set_by_command,MSG_SET_BY_COMMAND)
#define msg_set_from_file_        F77_NAME(msg_set_from_file,MSG_SET_FROM_FILE)
#define msg_set_limit_            F77_NAME(msg_set_limit,MSG_SET_LIMIT)
#define msg_set_lun_              F77_NAME(msg_set_lun,MSG_SET_LUN)
#define msg_set_summary_mode_aborted_  F77_NAME(msg_set_summary_mode_aborted,MSG_SET_SUMMARY_MODE_ABORTED)
#define msg_set_summary_mode_active_   F77_NAME(msg_set_summary_mode_active,MSG_SET_SUMMARY_MODE_ACTIVE)
#define msg_set_summary_mode_counting_ F77_NAME(msg_set_summary_mode_counting,MSG_SET_SUMMARY_MODE_COUNTING)
#define msg_set_summary_mode_inactive_ F77_NAME(msg_set_summary_mode_inactive,MSG_SET_SUMMARY_MODE_INACTIVE)
#define msg_set_summary_page_length_   F77_NAME(msg_set_summary_page_length,MSG_SET_SUMMARY_PAGE_LENGTH)
#define msg_set_timestamp_cpu_    F77_NAME(msg_set_timestamp_cpu,MSG_SET_TIMESTAMP_CPU)
#define msg_share_                F77_NAME(msg_share,MSG_SHARE)
#define msg_sort_                 F77_NAME(msg_sort,MSG_SORT)
#define msg_state_load_           F77_NAME(msg_state_load,MSG_STATE_LOAD)
#define msg_state_store_          F77_NAME(msg_state_store,MSG_STATE_STORE)
#define msg_state_zero_           F77_NAME(msg_state_zero,MSG_STATE_ZERO)
#define msg_summary_              F77_NAME(msg_summary,MSG_SUMMARY)
#define msg_summary_cpu_          F77_NAME(msg_summary_cpu,MSG_SUMMARY_CPU)
#define msg_summary_event_        F77_NAME(msg_summary_event,MSG_SUMMARY_EVENT)
#define msg_time_stamp_           F77_NAME(msg_time_stamp,MSG_TIME_STAMP)
#define msg_time_stamp_out_       F77_NAME(msg_time_stamp_out,MSG_TIME_STAMP_OUT)
#define msg_to_journal_           F77_NAME(msg_to_journal,MSG_TO_JOURNAL)
#define msg_to_journal_out_       F77_NAME(msg_to_journal_out,MSG_TO_JOURNAL_OUT)
#define msg_to_lun_               F77_NAME(msg_to_lun,MSG_TO_LUN)
#define msg_to_lun_out_           F77_NAME(msg_to_lun_out,MSG_TO_LUN_OUT)


#define message_                  F77_NAME(message,MESSAGE)
#define message_out_              F77_NAME(message_out,MESSAGE_OUT)
#define msg_class_define_         F77_NAME(msg_class_define,MSG_CLASS_DEFINE)
#define msg_count_                F77_NAME(msg_count,MSG_COUNT)
#define msg_disable_              F77_NAME(msg_disable,MSG_DISABLE)
#define msg_display_              F77_NAME(msg_display,MSG_DISPLAY)
#define msg_display_out_          F77_NAME(msg_display_out,MSG_DISPLAY_OUT)
#define msg_display_and_echo_     F77_NAME(msg_display_and_echo,MSG_DISPLAY_AND_ECHO)
#define msg_display_and_echo_out_ F77_NAME(msg_display_and_echo_out,MSG_DISPLAY_AND_ECHO_OUT)
#define msg_enable_               F77_NAME(msg_enable,MSG_ENABLE)
#define msg_enabled_              F77_NAME(msg_enabled,MSG_ENABLED)
#define msg_get_lun_              F77_NAME(msg_get_lun,MSG_GET_LUN)
#define msg_ini_                  F77_NAME(msg_ini,MSG_INI)
#define msg_journal_close_        F77_NAME(msg_journal_close,MSG_JOURNAL_CLOSE)
#define msg_journal_off_          F77_NAME(msg_journal_off,MSG_JOURNAL_OFF)
#define msg_journal_on_           F77_NAME(msg_journal_on,MSG_JOURNAL_ON)
#define msg_journal_open_         F77_NAME(msg_journal_open,MSG_JOURNAL_OPEN)
#define msg_journal_page_         F77_NAME(msg_journal_page,MSG_JOURNAL_PAGE)
#define msg_lun_page_             F77_NAME(msg_lun_page,MSG_LUN_PAGE)
#define msg_parse_                F77_NAME(msg_parse,MSG_PARSE)
#define msg_mark_                 F77_NAME(msg_mark,MSG_MARK)
#define msg_name_node_            F77_NAME(msg_name_node,MSG_NAME_NODE)
#define msg_nocount_              F77_NAME(msg_nocount,MSG_NOCOUNT)
#define msg_set_abort_limit_      F77_NAME(msg_set_abort_limit,MSG_SET_ABORT_LIMIT)
#define msg_set_by_command_       F77_NAME(msg_set_by_command,MSG_SET_BY_COMMAND)
#define msg_set_from_file_        F77_NAME(msg_set_from_file,MSG_SET_FROM_FILE)
#define msg_set_limit_            F77_NAME(msg_set_limit,MSG_SET_LIMIT)
#define msg_set_lun_              F77_NAME(msg_set_lun,MSG_SET_LUN)
#define msg_set_summary_mode_aborted_  F77_NAME(msg_set_summary_mode_aborted,MSG_SET_SUMMARY_MODE_ABORTED)
#define msg_set_summary_mode_active_   F77_NAME(msg_set_summary_mode_active,MSG_SET_SUMMARY_MODE_ACTIVE)
#define msg_set_summary_mode_counting_ F77_NAME(msg_set_summary_mode_counting,MSG_SET_SUMMARY_MODE_COUNTING)
#define msg_set_summary_mode_inactive_ F77_NAME(msg_set_summary_mode_inactive,MSG_SET_SUMMARY_MODE_INACTIVE)
#define msg_set_summary_page_length_   F77_NAME(msg_set_summary_page_length,MSG_SET_SUMMARY_PAGE_LENGTH)
#define msg_set_timestamp_cpu_    F77_NAME(msg_set_timestamp_cpu,MSG_SET_TIMESTAMP_CPU)
#define msg_share_                F77_NAME(msg_share,MSG_SHARE)
#define msg_sort_                 F77_NAME(msg_sort,MSG_SORT)
#define msg_state_load_           F77_NAME(msg_state_load,MSG_STATE_LOAD)
#define msg_state_store_          F77_NAME(msg_state_store,MSG_STATE_STORE)
#define msg_state_zero_           F77_NAME(msg_state_zero,MSG_STATE_ZERO)
#define msg_summary_              F77_NAME(msg_summary,MSG_SUMMARY)
#define msg_summary_cpu_          F77_NAME(msg_summary_cpu,MSG_SUMMARY_CPU)
#define msg_summary_event_        F77_NAME(msg_summary_event,MSG_SUMMARY_EVENT)
#define msg_time_stamp_           F77_NAME(msg_time_stamp,MSG_TIME_STAMP)
#define msg_time_stamp_out_       F77_NAME(msg_time_stamp_out,mSG_TIME_STAMP_OUT)
#define msg_to_journal_           F77_NAME(msg_to_journal,MSG_TO_JOURNAL)
#define msg_to_journal_out_       F77_NAME(msg_to_journal_out,MSG_TO_JOURNAL_OUT)
#define msg_to_lun_               F77_NAME(msg_to_lun,MSG_TO_LUN)
#define msg_to_lun_out_           F77_NAME(msg_to_lun_out,MSG_TO_LUN_OUT)

	void  type_of_call message_( const char *msg, int len , int *one, int *ID);
	void  type_of_call message_out_( const char *msg, int len , int *one);
	void  type_of_call msg_class_define_( const char *Class, int Clen, const char *State, int Slen, int *CountLimit, int *AbortLimit );
	void  type_of_call msg_count_( const char *Prefix, int len );
	void  type_of_call msg_disable_( const char *Prefix, int len );
	void  type_of_call msg_display_( const char *msg, int len , int *one, int *ID);
	void  type_of_call msg_display_out_( const char *msg, int len, int *one );
	void  type_of_call msg_display_and_echo_( const char *msg, int len , int *one, int *LUN, int *ID);
	void  type_of_call msg_display_and_echo_out_( const char *msg, int len, int *one, int *LUN );
	void  type_of_call msg_enable_( const char *Prefix, int len );
	int   type_of_call msg_enabled_( const char *Prefix, int len , int *ID);
	void  type_of_call msg_get_lun_( int *Terminal_LUN, int *Journal_LUN );
	void  type_of_call msg_ini_( int *Journal_LUN );
	int   type_of_call msg_journal_close_( void );
	void  type_of_call msg_journal_off_( void );
	void  type_of_call msg_journal_on_( void );
	int   type_of_call msg_journal_open_( const char *FileName, int len );
	void  type_of_call msg_journal_page_( void );
	void  type_of_call msg_lun_page_( int *LUN );
	void  type_of_call msg_parse_( const char *msg, int *isep, int len, int *nprefix, int *nmessage );
	void  type_of_call msg_mark_( const char *Prefix, int len , int *ID);
	void  type_of_call msg_name_node_( const char *NodeName, int len );
	void  type_of_call msg_nocount_( const char *Prefix, int len );
	void  type_of_call msg_set_abort_limit_( const char *Prefix, int len, int *Limit );
	int   type_of_call msg_set_by_command_( const char *Command, int len );
	int   type_of_call msg_set_from_file_( int *LUN );
	void  type_of_call msg_set_limit_( const char *Prefix, int len , int *Limit);
	void  type_of_call msg_set_lun_( int *TERMINAL_LUN, int *JOURNAL_LUN );
	void  type_of_call msg_set_summary_mode_aborted_( int *Mode );
	void  type_of_call msg_set_summary_mode_active_( int *Mode );
	void  type_of_call msg_set_summary_mode_counting_( int *Mode );
	void  type_of_call msg_set_summary_mode_inactive_( int *Mode );
	void  type_of_call msg_set_summary_page_length_( int *Page_Length );
	void  type_of_call msg_set_timestamp_cpu_( int *Mode );
	void  type_of_call msg_share_( char*, const len );
	void  type_of_call msg_sort_( void );
	void  type_of_call msg_state_load_( int *LUN, const char *FileName, const int len );
	void  type_of_call msg_state_store_( int *LUN, const char *FileName, const int len );
	void  type_of_call msg_state_zero_( void );
	void  type_of_call msg_summary_( int *LUN );
	void  type_of_call msg_summary_cpu_( int *LUN );
	void  type_of_call msg_summary_event_( int *LUN, int *EVENTS );
	void  type_of_call msg_time_stamp_( int *LUN );
	void  type_of_call msg_time_stamp_out_( int *LUN );
	void  type_of_call msg_to_journal_( const char *msg, int len, int *one, int *ID );
	void  type_of_call msg_to_journal_out_( const char *msg, int len, int *one );
	void  type_of_call msg_to_lun_( const char *msg, int len, int *one, int *LUN, int *ID );
	void  type_of_call msg_to_lun_out_( const char *msg, int len , int *one, int *LUN);
#endif
