#ifndef STSTRINGAUX_H
#define STSTRINGAUX_H
#include <string>

class StString : public std::string {
public:
          StString(){fPrec=0;}   

StString& operator<<(const char *txt){ if(txt) { (*this)+=txt; }; return *this;}   
StString& operator<<(int I);   
StString& operator<<(unsigned int I);   
StString& operator<<(short I);   
StString& operator<<(unsigned short I);   
StString& operator<<(long I);   
StString& operator<<(unsigned long I);   
StString& operator<<(long long I);   
StString& operator<<(unsigned long long I);   
StString& operator<<(float F);   
StString& operator<<(double D);   
void      precision(int prec){fPrec = prec;}

const std::string &str(){return *((const std::string*)this);}

private:
int fPrec;
		
		};
#define stendl ("\n")
#endif
