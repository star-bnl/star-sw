#ifndef StDBtestBuff
#define  StDBtestBuff



class StDbBufferI  { 


public: 
 
  virtual ~StDbBufferI(){};
  virtual void SetClientMode() = 0; 
  virtual void SetStorageMode() = 0;
  virtual bool IsClientMode() = 0;
  virtual bool IsStorageMode() = 0;
   
  virtual bool  ReadScalar(char   &c, const char *aName)  = 0;
  virtual bool  ReadScalar(unsigned char  &c, const char *)  = 0; 
  virtual bool  ReadScalar(short  &h, const char *)  = 0;
  virtual bool  ReadScalar(unsigned short &h, const char *)  = 0;
  virtual bool  ReadScalar(int    &i, const char *)  = 0;
  virtual bool  ReadScalar(unsigned int   &i, const char *)  = 0;
  virtual bool  ReadScalar(long   &l, const char *)  = 0;
  virtual bool  ReadScalar(unsigned long  &l, const char *)  = 0;
  virtual bool  ReadScalar(float  &f, const char *)  = 0;
  virtual bool  ReadScalar(double &d, const char *)  = 0;
  virtual bool  ReadScalar(char   *&c, const char *)  = 0; 
  
  virtual bool     WriteScalar(const char   c, const char *)  = 0;
  virtual bool     WriteScalar(const unsigned char  c, const char *)  = 0;
  virtual bool     WriteScalar(const short  h, const char *)  = 0;
  virtual bool     WriteScalar(const unsigned short h, const char *)  = 0;
  virtual bool     WriteScalar(const int    i, const char *)  = 0;
  virtual bool     WriteScalar(const unsigned int   i, const char *)  = 0;
  virtual bool     WriteScalar(const long   l, const char *)  = 0;
  virtual bool     WriteScalar(const unsigned long  l, const char *)  = 0;
  virtual bool     WriteScalar(const float  f, const char *)  = 0;
  virtual bool     WriteScalar(const double d, const char *)  = 0;
  virtual bool     WriteScalar(const char  *c, const char *) = 0;

  virtual bool     ReadArray(char    *&c, int &len, const char *) = 0;
  virtual bool     ReadArray(unsigned char  *&c, int &len, const char *) = 0;
  virtual bool     ReadArray( short  *&c, int &len, const char *) = 0;
  virtual bool     ReadArray( unsigned short  *&c, int &len, const char *) = 0;
  virtual bool     ReadArray(int  *&c, int &len, const char *) = 0;
  virtual bool     ReadArray(unsigned int   *&c, int &len, const char *) = 0;
  virtual bool     ReadArray(long  *&c, int &len, const char *) = 0;
  virtual bool     ReadArray(unsigned long  *&c, int &len, const char *) = 0;
  virtual bool     ReadArray(float   *&c, int &len, const char *) = 0;
  virtual bool     ReadArray(double  *&c, int &len, const char *) = 0;
  virtual bool     ReadArray(char  **&c, int &len, const char *) = 0;

  virtual bool     WriteArray(char    *c, int len, const char *) = 0;
  virtual bool     WriteArray(unsigned char   *c, int len, const char *) = 0;
  virtual bool     WriteArray(short   *c, int len, const char *) = 0;
  virtual bool     WriteArray(unsigned short  *c, int len, const char *) = 0;
  virtual bool     WriteArray(int     *c, int len, const char *) = 0;
  virtual bool     WriteArray(unsigned int    *c, int len, const char *) = 0;
  virtual bool     WriteArray(long    *c, int len, const char *) = 0;
  virtual bool     WriteArray(unsigned long   *c, int len, const char *) = 0;
  virtual bool     WriteArray(float   *c, int len, const char *) = 0;
  virtual bool     WriteArray(double  *c, int len, const char *) = 0;
  virtual bool     WriteArray(char **c, int len, const char *) = 0;


};



#endif






