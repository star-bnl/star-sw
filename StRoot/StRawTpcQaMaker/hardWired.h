#ifndef hardWired_h
#define hardWired_h

#define N__SECTORS 24
#define ROWS__PER__SECTOR 45
#define MAX__PAD 182

#define __SMALL__

#ifdef __SMALL__
#define nTpcSequenceKeys 1
enum tpcSequenceKey { sCount };
#define TIME__BUCKETS 512
#else
#define nTpcSequenceKeys 6
enum tpcSequenceKey { sStart, sStop, sMax, sSize, sCount, sIntegral };
#define TIME__BUCKETS 512
#endif

#define nTpcValueKeys 2
enum tpcValueKey { vT0, vT0Shift};

static int nPadsPerRow[45]={
  88,96,104,112,118,126,134,142,150,158,166,174,182,
  98,100,102,104,106,106,108,110,112,112,114,116,118,120,122,122,
  124,126,128,128,130,132,134,136,138,138,140,142,144,144,144,144
};

#endif
