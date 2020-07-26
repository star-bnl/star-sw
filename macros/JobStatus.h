 class JobStatus : public TObject {
  //                                        +--------------------------------+---------------+------+-----+---------------------+----------------+
  //                                        | Field                          | Type          | Null | Key | Default             | Extra          |   
public:					 // +--------------------------------+---------------+------+-----+---------------------+----------------+
  Char_t jobID[64]; 			 // | jobID                          | varchar(64)   | NO   |     | 0                   |                |
  Char_t LibLevel[20]; 			 // | LibLevel                       | varchar(20)   | NO   |     |                     |                |
  Char_t LibTag[20]; 			 // | LibTag                         | varchar(20)   | NO   | MUL |                     |                |
  Char_t rootLevel[20]; 		 // | rootLevel                      | varchar(20)   | NO   |     |                     |                |
  Char_t path[128];			 // | path                           | varchar(128)  | NO   | MUL |                     |                |
  Int_t prodyear;			 // | prodyear                       | smallint(6)   | NO   |     | 0                   |                |
  Char_t logFile[64];			 // | logFile                        | varchar(64)   | NO   |     |                     |                |
  Char_t createTime[120];		 // | createTime                     | datetime      | NO   | MUL | 0000-00-00 00:00:00 |                |
  Char_t chainOpt[190];			 // | chainOpt                       | varchar(248)  | NO   |     | NULL                |                |
  Char_t jobStatus[32];			 // | jobStatus                      | varchar(32)   | NO   | MUL |                     |                |
  Char_t crashedCode[32];		 // | crashedCode                    | varchar(32)   | NO   |     |                     |                |
  Char_t errMessage[128];		 // | errMessage                     | varchar(128)  | NO   |     |                     |                |
  Int_t  NoEventDone;			 // | NoEventDone                    | mediumint(9)  | NO   | MUL | 0                   |                |
  Float_t  memUsageF;			 // | memUsageF                      | float(8,2)    | NO   | MUL | 0.00                |                |
  Float_t  memUsageL;			 // | memUsageL                      | float(8,2)    | NO   | MUL | 0.00                |                |
  Float_t  CPU_per_evt_sec;		 // | CPU_per_evt_sec                | float(8,2)    | NO   | MUL | 0.00                |                |
  Float_t  RealTime_per_evt;		 // | RealTime_per_evt               | float(8,2)    | NO   | MUL | 0.00                |                |
  Int_t  percent_of_usable_evt;		 // | percent_of_usable_evt          | smallint(6)   | NO   | MUL | 0                   |                |
  Int_t  avg_no_tracks;			 // | avg_no_tracks                  | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avg_no_tracksnfit15;		 // | avg_no_tracksnfit15            | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  NoEventVtx;			 // | NoEventVtx                     | smallint(6)   | NO   | MUL | 0                   |                |
  Float_t  avgNoVtx_evt;		 // | avgNoVtx_evt                   | float(8,2)    | NO   | MUL | 0.00                |                |
  Int_t  avg_no_primaryT;		 // | avg_no_primaryT                | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avg_no_primaryT_1vtx;		 // | avg_no_primaryT_1vtx           | mediumint(9)  | NO   |     | 0                   |                |
  Int_t  avg_no_primaryTnfit15;		 // | avg_no_primaryTnfit15          | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avg_no_primaryTnfit15_1vtx;	 // | avg_no_primaryTnfit15_1vtx     | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avg_no_V0Vrt;			 // | avg_no_V0Vrt                   | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avg_no_XiVrt;			 // | avg_no_XiVrt                   | smallint(6)   | NO   | MUL | 0                   |                |
  Int_t  avg_no_KinkVrt;		 // | avg_no_KinkVrt                 | smallint(6)   | NO   | MUL | 0                   |                |
  Int_t  avgNoTrack_usbevt;		 // | avgNoTrack_usbevt              | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avgNoTrackNfit15_usbevt;	 // | avgNoTrackNfit15_usbevt        | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avgNoPrTrack_1vtx_usbevt;	 // | avgNoPrTrack_1vtx_usbevt       | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avgNoPrTrackNfit15_1vtx_usbevt; // | avgNoPrTrackNfit15_1vtx_usbevt | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avgNoV0_usbevt;		 // | avgNoV0_usbevt                 | smallint(6)   | NO   | MUL | 0                   |                |
  Int_t  avgNoXi_usbevt;		 // | avgNoXi_usbevt                 | smallint(6)   | NO   | MUL | 0                   |                |
  Int_t  avgNoKink_usbevt;		 // | avgNoKink_usbevt               | smallint(6)   | NO   | MUL | 0                   |                |
  Char_t nodeID[64];			 // | nodeID                         | varchar(64)   | NO   |     |                     |                |
  Bool_t avail;				 // | avail                          | enum('Y','N') | NO   | MUL | Y                   |                |
  Int_t  id;				 // | id                             | mediumint(9)  | NO   | PRI | NULL                | auto_increment |
  Int_t  NoEventSkip;			 // | NoEventSkip                    | mediumint(9)  | NO   |     | 0                   |                |
  ClassDef(JobStatus,1)			 // +--------------------------------+---------------+------+-----+---------------------+----------------+
};					 
