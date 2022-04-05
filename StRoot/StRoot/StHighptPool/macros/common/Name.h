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
