/*	Created:  22oct1996   R. Hackenburg                                          */

static const char sccsid_msgH[] = "@(#)"__FILE__"\t\t1.55\tCreated 3/8/98 02:07:33, \tcompiled "__DATE__" "__TIME__;


/*
	This contains prototypes for the msg library, C-interface.

	Modified 15-Oct-1996  Robert Hackenburg  (for version 1.00)
	Created  2-Dec-1994   Robert Hackenburg
*/

#define VERSION 1.0

#ifndef FILE
#include <stdio.h>
#endif

#ifndef	NULL
#include <stdlib.h>
#endif

#ifndef pid_t
#include <sys/types.h>
#endif

#ifndef TRUE
#define TRUE -1
#endif
#ifndef FALSE
#define FALSE 0
#endif

typedef	void	(*funcPoint)(const char*, const char*, const int*);


	void	Message( const char *msg, int *ID );
	void	MessageOut( const char *msg );
	void	MsgAbortCheck( const int ID );
	void	MsgAlarm( const char *msg, int  severity );
	void	MsgAlarmRegister( funcPoint AlarmRoutine );
	void	MsgAlarmRoutineSample( char* Prefix, char* sansPrefix, int *Level );
	void	MsgAppendReturn( void );
	void	MsgNoAppendReturn( void );
	char	*MsgCela(int ELA);
	void	MsgCheck( const char *msg, int *ID, int *Active, int *Alarming, int *Counting );
	void	MsgClassDefine( const char *Class, const char *State, int CountLimit, int AbortLimit );
	void	MsgClean( int  length, char string[] );
	void	MsgCount( const char *Prefix );
	char	*MsgCtime(void);
	char	*MsgCCPU(int CPU);
	long	MsgCPU(void);
	void	MsgDeleteID( const int ID );
	void	MsgDisable( const char *Prefix );
	void	MsgDisableAlarm( const char *Prefix );
	void	MsgDisplay( const char *msg, int *ID );
	void	MsgDisplayOut( const char *msg );
	void	MsgDisplayAndFile(    const char *msg, FILE *fid, int *ID );
	void	MsgDisplayAndFileOut( const char *msg, FILE *fid );
	void	MsgEnable( const char *Prefix );
	void	MsgEnableAlarm( const char *Prefix );
	int	MsgEnabled( const char *Prefix, int *ID );
	void	MsgEnter( const char *Prefix, int *ID );
	int	MsgEnterClass( const char *Class, int *ID );
	FILE	*MsgFileOpen( const char* FileName, const char* FileAccessMode  );
	int	MsgFind( const char *Prefix, int *ID, int *Active, int *Alarming, int *Counting );
	int	MsgFindClass( const char *Clas , int *ID );
	void	MsgGetClass( const char *Prefix, char *Class );
	void	MsgIncr( const int ID );
	void	MsgIni( int LUN );
	void    MsgFinish( const char *switches, int Nevents );
	void	MsgInit( const char *switches, ... );
	int	MsgJournalClose( void );
	int	MsgJournalEnabled( void );
	FILE	*MsgJournalGet( void );
	void	MsgJournalOff( void );
	void	MsgJournalOn( void );
	int	MsgJournalOpen( const char* FileName  );
	void	MsgJournalPage( void );
	int	MsgLNB( const char *string, int   n );
	void	MsgMark( const char *Prefix, int *ID  );
	void	MsgNoCount( const char *Prefix );
	void	MsgNodeNameGet( char *NodeName );
	void	MsgNodeNameSet( const char *NodeName );
	void	MsgParse( const char   *msg, char   *prefix, char   **message );
	void	MsgParsePrefix( const char *Prefix, char *PrefixStripped, int  *PrefixNumber );
	void	MsgPrefixGet( const char *Prefix, int *Counts, int *CountLimit, int *Level
	                    , int *AbortLimit, int *Active, int *Counting, int *Alarming, char State[9] );
	void	MsgPrefixSet( const char *Prefix, int CountLimit, int Level
	                    , int AbortLimit, int Active, int Counting, int Alarming );
	int     MsgRemoveSharedMemory( const pid_t ProcessID );
	void	MsgResetID( const int ID );
	void	MsgSetAbortLimit( const char *Prefix, int Limit );
	int	MsgSetByCommand( const char *Command );
	int	MsgSetFromFile( FILE *fid );
	void	MsgSetLimit( const char *Prefix, int Limit );
	void	MsgSetLevel( const char *Prefix, int Level );
	void	MsgSetSummaryModeAborted( int Mode );
	void	MsgSetSummaryModeActive( int Mode );
	void	MsgSetSummaryModeCounting( int Mode );
	void	MsgSetSummaryModeInactive( int Mode );
	void	MsgSetSummaryPageLength( int Page_Length );
	void	MsgSetTimeStampCPU( int Mode );
	int	MsgShare( void );
	int	MsgShareNoCreate( const pid_t ProcessID );
	void	MsgSort( void );
	void	MsgState( int Counts, int Limit, int Level, int AbortLimit, int Active, int Counting, int Alarming, char State[9] );
	int	MsgStateLoad(  const char *fileName );
	int	MsgStateStore( const char *fileName );
	void	MsgStateZero( void );
	void	MsgSummaryFile( FILE* fid );
	void	MsgSummaryCPUFile( FILE* fid );
	void	MsgSummaryEventFile( FILE* fid, int EVENTS );
	long	MsgTime( void );
	void	MsgTimeStampFile( FILE *fid );
	void	MsgTimeStampFileOut( FILE *fid );
	void	MsgToJournal( const char *msg, int *ID );
	void	MsgToJournalOut( const char *msg );
	void	MsgToFile( const char *msg, FILE *fid, int *ID );
	void	MsgToFileOut( const char *msg, FILE *fid );
	long    MsgTPS(void);
	void	MsgTruncate( char *string, int Size );

/*	Back compatibility prototypes:  */
	void	MsgDisplayAndEcho( const char *msg, int LUN, int *ID );
	void	MsgDisplayAndEchoOut( const char *msg, int LUN );
	void	MsgGetLUN( int *Terminal_LUN, int *Journal_LUN );
	void	MsgLUNPage( int LUN );
	void	MsgNameNode( const char *NodeName );
	void	MsgSetLUN( int TERMINAL_LUN, int JOURNAL_LUN );
	void	MsgSummary( int LUN );
	void	MsgSummaryCPU( int LUN );
	void	MsgSummaryEvent( int LUN, int EVENTS );
	void	MsgTimeStamp( int LUN );
	void	MsgTimeStampOut( int LUN );
	void	MsgToLUN( const char *msg, int LUN, int *ID );
	void	MsgToLUNOut( const char *msg, int LUN );
