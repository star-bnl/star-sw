#ifndef _DAQ_READERCLASS_HH_
#define _DAQ_READERCLASS_HH_

#include <ctype.h>
#include <sys/types.h>
#include "daqConfig.h"
// Define the old EVP_READER-based interface:

#define DAQ_LEGACY_DEF(xxx)         \
struct _NAME2_(xxx,_t) _NAME1_(xxx);\
                                    \
int _NAME2_(xxx,Reader)(char *m)  {  \
  if(!m) return -1;                 \
  daqReader *rrr = (daqReader *)m;  \
  daq_dta *dd= rrr->det( _QUOTE_(xxx))->get("legacy"); \
  int size = 0;                     \
  if (dd && (size = dd->iterate())) { \
  memcpy(&_NAME1_(xxx),dd->Void,dd->get_size_t());} \
  return (dd && size) ? dd->ncontent: 0; }

#define DAQ_LEGACY_DECL(xxx)                \
extern struct  _NAME2_(xxx,_t) _NAME1_(xxx);\
extern int   _NAME2_(xxx,Reader)(char *mem)
struct DATAP;
struct rccnf ;
struct gbPayload;
struct gbPayload_0x02;
struct gbPayload_0x01a;
struct gbPayload_0x01;

class sfs_index;
//class rts_reader;
class MemMap;
class daq_det ;

enum Input_Type { none, live, file, pointer, dir };

#ifdef RTS_PROJECT_PP
#define _EVP_HOSTNAME    "ppdaq1.pp2pp.bnl.gov"
#else
#define _EVP_HOSTNAME    "evp.starp.bnl.gov"
#endif

#define EVP_PORT        8020


#define EVP_TYPE_0      1
#define EVP_TYPE_PHYS   2
#define EVP_TYPE_SPEC   4
#define EVP_TYPE_ANY    (EVP_TYPE_PHYS|EVP_TYPE_SPEC|EVP_TYPE_0)
#define EVT_TYPE_MON    8


#define EVP_STAT_OK     0
#define EVP_STAT_EOR    (-1)
#define EVP_STAT_EVT    (-2)
#define EVP_STAT_CRIT   (-3)
#define EVP_STAT_LOG  (-4)

typedef unsigned int u_int;
typedef unsigned long long int u_longlong;
typedef unsigned long long int UINT64;

struct SummaryInfo {
  u_int token ;		// current token
  u_int evt_time ;	// time in UNIX seconds
  u_int detectors ;	// detectors present bit mask according to DAQ!
  UINT64 detectors64;
  u_int daqbits;
  u_int daqbits_l1;
  u_int daqbits_l2;
  u_int evpgroups;
  u_int evp_daqbits;
  u_int flags;

  u_int trgword;
  u_int trgcmd;	// current trigger command
  u_int daqcmd;	// current DAQ command
  
  u_int L1summary[2];
  u_int L2summary[2];
  u_int L3summary[4];

  u_int seq ;		// event sequence from EVB    ????
};

class daqReader {
 public:
  
  // Filename can be:
  //    NULL      --> read from event pool
  //    Directory --> read from each file "1, 2, 3, .... N" in the directory
  //    filename  --> .daq file
  //    space separated files  --> read from each file in turn
  //
  daqReader(char *buffer, int size);
  daqReader(char *fname) ;
  ~daqReader(void) ;

  void init();

  int trgIds[64];
  int trgIdsSet;
  int trgIdsNotPresent;

  static const int DAQ_READER_MAX_DETS = 48 ;
  daq_det *dets[DAQ_READER_MAX_DETS] ;
  daq_det *pseudo_dets[DAQ_READER_MAX_DETS] ;

  daq_det *det(const char *det) ;
  void insert(daq_det *which, int rts_id) ;
  void de_insert(int rts_id) ;
  void Make() ;
  char *get_sfs_name(char *snippet=0) ;	// returns the full name of the SFS
  char *get(int which, int type=EVP_TYPE_ANY) ;	
  char *skip_then_get(int numToSkip, int num, int type=EVP_TYPE_ANY);
  // The following are the descriptors and pointers defining the event

	
  // These variables describe the "input source"
  //
  // live    ->   file is:  file_name = evp_disk/base_dir/run#/evt#
  // file    ->   file is:  file_name
  // pointer ->   file is at:  *data_memory
  // dir     ->   file is at:  file_name = fname/evt#
  char *data_memory;     
  int data_size;

  char evp_disk[256]; 
  char fname[256] ;     // This is the name argument of the evpReader
  char file_name[256] ;	// fully qualified file name containing evt data
  long long int file_size ;	// size of the file in bytes
  int desc ;		// file descriptor

  int isevp;   // backward compatability...
  int IsEvp() { return (input_type == live); };
  daqReader *rts() const { return (daqReader *)this; } // to simplify the offline transition

  // Return the offlineId for a daqId.  If present in file...
  int getOfflineId(int daqId);

  // These variables describe the event storage
  //
  //    All events are eventually mapped into memory
  //    The event_memory points to the first LRHD/DATAP/FILE record
  //    whatever it might be...  
  char *event_memory;
  int event_size;   // size of the current event measured from beginning of memmap
  u_int bytes ;	    // size of the current event measured from beginning of datap

  int getDetectorSize(const char *str);   // actually gets the size of any arbitrary file/directory...

  long long int evt_offset_in_file;

  char *mem;            // a datap pointer if applicable...
  sfs_index *sfs;       // the sfs reader object... (if no sfs only contains "/");

  // These variables describe the characteristics of the event
  //
  
  // year:  pre-2006  - .daq format
  //        2007      - .daq format, tpx bank in sfs format
  //        2008      - .sfs format/compat .daq format/ + legacy bank in .daq format
  //        2009-     - .sfs format + legacy bank in .daq format  
  //
  
  //int lrhd_offset;
  //int datap_offset;

  int sfs_lastevt;

  //char *getSFSEventNumber();

  int setMmap(int flag) ;		// flag=1 - enable memory mapping
  int setOpen(int flag) ;		// flag=1 - enable file open
  int setLog(int flag) ;		// flag=1 - enable logging...
  char *setEvpDisk(char *fs) ;	// sets the local directory name where the evp.star disks
  // are mounted


  int writeCurrentEventToDisk(char *fname);
  int status ;		// 0 OK, all others should disregard the event!

  // all the variables below are valid ONLY if the status is 0!!!

  //  These parameters are used for the evp reads with event number non-zero
  //  readall_lastevt --
  u_int readall_rundone;
  u_int readall_lastevt;
  u_int readall_run;
  void readall_reset() { readall_rundone = 0; readall_lastevt = 0; readall_run=0; };


  u_int event_number ;	// current event in the evp or file 
  u_int total_events ;	// total number of events seen by this object so far


	


  u_int run ;		// current run number

  u_int evb_type ;	// event type	(only from ACTIVE run)
  u_int evb_cou ;		// total events in this run (only from ACTIVE run)
  u_int evb_type_cou ;	// total events of the above type in this run (only from ACTIVE run)

  u_int token ;		// current token
  u_int trgcmd ;		// current trigger command
  u_int daqcmd ;		// current DAQ command
  u_int trgword ;		// the Trigger Word
  u_int phyword ;		// the Physics Word
  u_int daqbits ;		// "offline" bits aka L3 summary...
  u_int daqbits_l1;       // triggers satisfying l1 
  u_int daqbits_l2;       // triggers satisfying l2

  u_longlong daqbits64;
  u_longlong daqbits64_l1;
  u_longlong daqbits64_l2;

  u_int evpgroups ;       // evp groups aka L3 summary[2]     
  u_int flags ;

  u_int evt_time ;	// time in UNIX seconds
  u_int seq ;		// event sequence from EVB
  u_int detectors ;	// detectors present bit mask according to DAQ!
  UINT64 detectors64;

  int streaming_seq;        // -1 if not streaming
  int streaming_evb;        // evb + seq is the key here, there are multiple seq per run
  char streaming_node[12];
  
  u_int detector_bugs ;	// for per-detector bug tracing i.e. FY12 UU future protection TPX bug
  UINT64 detector_bugs64;

  u_int detsinrun ;
  UINT64 detsinrun64;
  u_int evpgroupsinrun;

  // copies from the trigger data format...
  // the meanings potentially change year to year...
  u_int L1summary[2];
  u_int L2summary[2];
  u_int L3summary[4];

  int fixDatapSummary(DATAP *datap);
  char *getInputType();
  int getNextEventFilename(int num, int type);
  int getNextEventFilenameFromLive(int type);
  int getNextEventFilenameFromDir(int eventNum);
  int openEventFile();
  int addToEventSize(int sz);
  int getEventSize();

  int hackSummaryInfo();
  int copySummaryInfoIn(SummaryInfo *info);
  int fillSummaryInfo(SummaryInfo *info, DATAP *datap);
  int fillSummaryInfo(SummaryInfo *info, gbPayload *gbPayload);

  // history...
  int fillSummaryInfo_v03(SummaryInfo *info, gbPayload *gbPayload);
  int fillSummaryInfo_v02(SummaryInfo *info, gbPayload_0x02 *gbPayload);
  int fillSummaryInfo_v01a(SummaryInfo *info, gbPayload_0x01a *gbPayload);
  int fillSummaryInfo_v01(SummaryInfo *info, gbPayload_0x01 *gbPayload);

  MemMap *memmap;

  int readNextFutureSummaryInfo(SummaryInfo *info);

  char *EVP_HOSTNAME;

  void setCopyOnWriteMapping();



 private:	// one shouldn't care... 

  int map_prot;
  int map_flags;

  char _static_str_return_[256];
  int crit_cou;

  int reconnect(void) ;

  char _evp_basedir_[40];
  char _last_evp_dir_[40];
  int do_mmap ;
  int do_open ;
  int do_log ;


  int issued ;
  int last_issued ;	// time

  int task ;


  int evpDesc ;		// message queue desc.
  // file variables

  rccnf *runconfig;

  int getStatusBasedEventDelay();
  Input_Type input_type;

  int allnumeric(char *str)
  {
    int i=0;
    while(str[i] != '\0') {
      if(!isdigit(str[i])) return 0;
      i++;
    }
    return 1;
  }
    
} ;

class MemMap {
 public:
  MemMap();
  ~MemMap();

  char *mem;
  char *map(int fd, long long int _offset, int _size, int map_prot, int map_flags);
  void unmap();

  long long int offset;
  int size;

  int real_mem;  // map real mem?
  char *map_real_mem(char *buffer, int _size);
 private:
  int page_size;
  long long int actual_offset;
  char *actual_mem_start;
  int actual_size;
  int fd;
};
  


#endif
