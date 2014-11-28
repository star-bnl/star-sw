// detector numbers
#define BEMCMODULES 120
#define MAXDETBARREL 4
#define BTOW 1
#define BPRS 2
#define BSMDE 3
#define BSMDP 4

#define STATUS_OK 1
#define CAP1 124
#define CAP2 125

// Number of channels
#define BTOWCH 4800
#define BSMDCH 18000

// number of crates
#define MAXCRATES 30
#define MAXSMDCRATES 8
#define MAXBPRSCRATES 4
#define NBEMCTRIGGERTOWER 300
#define NPATCHESPERCRATE 10
#define NTOWERSPERCRATE 160

// raw data position in StEvent
#define BTOWBANK 0
#define BTOWHEADER 120
#define BTOWSIZE 4800

#define BSMDOFFSET 1
#define BSMDHEADER 128
#define BSMDSIZE 4800

#define BPRSOFFSET 9
#define BPRSHEADER 128
#define BPRSSIZE 4800

// DAQ header specific information
#define BTOWTDCERROFFSET 30
#define BTOWCRATEOFFSET 90
#define BTOWBYTESUM 164
#define BTOWERRFLAG 0
#define BTOWNOTPRESENT 4095
#define SMDCAPACITOR 32
#define BTOWTOKENOFFSET 60
