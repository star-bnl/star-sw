/***************************************************************************
 *
 * $Id: StTypeEnum.h,v 1.2 1999/09/30 02:06:13 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:   name-enum association needed to read buffers
 *
 ***************************************************************************
 *
 * $Log: StTypeEnum.h,v $
 * Revision 1.2  1999/09/30 02:06:13  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef STTYPEENUM_HH
#define STTYPEENUM_HH

enum StTypeE {Stchar,Stuchar,Stshort,Stushort,Stint,Stuint,Stlong,Stulong,Stfloat,Stdouble};

const int StTypeSize[]={sizeof(char),sizeof(unsigned char),sizeof(short),sizeof(unsigned short), sizeof(int),sizeof(unsigned int),sizeof(long),sizeof(unsigned long), sizeof(float),sizeof(double)};


#endif
