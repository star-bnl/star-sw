// ****************************************************************
// * l3TrgReader: Interface to TRG data. Should hide internals of
// *              trgStructures.h. Two seperate files 
// *              l3TrgReaderV12.cxx and l3TrgReaderV12.cxx
// *              implement the version dependent part of the 
// *              reader.
// *              The data are accessed with public member vars.
// ****************************************************************

#ifndef L3_TRG_READER_H
#define L3_TRG_READER_H


class L3_P;

class l3TrgReader
{
public:
    // Interface functions
    //virtual int read(void *buffer);
    int read(void *buffer);
    int readL3P(L3_P *l3p);
    void reset();
    

 protected:
    // Version dependant readers
    int readV12(void * buffer);
    int readV20(void * buffer);
    int readV21(void * buffer);

public:
    unsigned short token;
    unsigned int   bunchXing_hi, bunchXing_lo;

    unsigned short physicsWord;
    unsigned short triggerWord;

    unsigned char  ZDC[16];
    unsigned char  CTB[256]; 

    unsigned int   l2Result;
};


#endif
