/***************************************************************************
 *
 * $Id: Name.h,v 1.1 2002/04/02 20:05:17 jklay Exp $
 *
 * Author: Bum Choi, UT Austin, Apr 2002
 *
 ***************************************************************************
 *
 * Description:  Useful utility methods for name manipulation in highpt
 *		 analysis
 *
 *
 ***************************************************************************
 *
 * $Log: Name.h,v $
 * Revision 1.1  2002/04/02 20:05:17  jklay
 * Bums analysis tools for highpt uDSTs
 *
 *
 **************************************************************************/
#ifndef Name_H
#define Name_H

// declarations

// bin,weight,charge?
void setName(char* buf,const char* base,int bin,int weight,const char* pm=0);
//
void setNameWeight(char* buf,const char* base, int bin, const char* pm=0);
// bin, charge?
void setName(char* buf,const char* base,int bin,const char* pm=0);
// ew, charge
void setName(char* buf,const char* base,const char* ew,const char* pm);
// charge
void setName(char* buf,const char* base,const char* pm);




#endif
