/*
	This contains prototypes for the msg library, C-interface.

	Created  2-Dec-1994   Robert Hackenburg
*/

#ifndef TRUE
#define TRUE -1
#endif
#ifndef FALSE
#define FALSE 0
#endif

	void	Message( const char *msg, int *ID );
	void	MessageOut( const char *msg );
	void	MsgClassDefine( const char *Class, const char *State, int CountLimit, int AbortLimit );
	void	MsgCount( const char *Prefix );
	void	MsgDisable( const char *Prefix );
	void	MsgDisplay( const char *msg, int *ID );
	void	MsgDisplayOut( const char *msg );
	void	MsgDisplayAndEcho( const char *msg, int LUN, int *ID );
	void	MsgDisplayAndEchoOut( const char *msg, int LUN );
	void	MsgEnable( const char *Prefix );
	int	MsgEnabled( const char *Prefix, int *ID );
	void	MsgGetLUN( int *Terminal_LUN, int *Journal_LUN );
	void 	MsgIni( int Journal_LUN );
	int	MsgJournalClose( void );
	void	MsgJournalOff( void );
	void	MsgJournalOn( void );
	int	MsgJournalOpen( const char* FileName  );
	void	MsgJournalPage( void );
	void	MsgLUNPage( int LUN );
	void	MsgMark( const char *Prefix, int *ID  );
	void	MsgNameNode( const char *NodeName );
	void	MsgNoCount( const char *Prefix );
	void	MsgSetAbortLimit( char *Prefix, int Limit );
	void	MsgSetByCommand( const char *Command );
	void	MsgSetFromFile( int LUN );
	void	MsgSetLimit( const char *Prefix, int Limit );
	void	MsgSetLUN( int TERMINAL_LUN, int JOURNAL_LUN );
	void	MsgSetSummaryModeAborted( int Mode );
	void	MsgSetSummaryModeActive( int Mode );
	void	MsgSetSummaryModeCounting( int Mode );
	void	MsgSetSummaryModeInactive( int Mode );
	void	MsgSetSummaryPageLength( int Page_Length );
	void	MsgSetTimeStampCPU( int Mode );
	void	MsgSort( void );
	void	MsgSummary( int LUN );
	void	MsgSummaryCPU( int LUN );
	void	MsgSummaryEvent( int LUN, int EVENTS );
	void	MsgTimeStamp( int LUN );
	void	MsgTimeStampOut( int LUN );
	void	MsgToJournal( const char *msg, int *ID );
	void	MsgToJournalOut( const char *msg );
	void	MsgToLUN( const char *msg, int LUN, int *ID );
	void	MsgToLUNOut( const char *msg, int LUN );
