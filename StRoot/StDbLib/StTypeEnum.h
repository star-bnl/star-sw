#ifndef STTYPEENUM_HH
#define STTYPEENUM_HH

enum StTypeE {Stchar,Stuchar,Stshort,Stushort,Stint,Stuint,Stlong,Stulong,Stfloat,Stdouble};

const int StTypeSize[]={sizeof(char),sizeof(unsigned char),sizeof(short),sizeof(unsigned short), sizeof(int),sizeof(unsigned int),sizeof(long),sizeof(unsigned long), sizeof(float),sizeof(double)};


#endif
