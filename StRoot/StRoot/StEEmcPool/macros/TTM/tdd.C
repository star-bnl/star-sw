// a cheap implementation of real TDD
// this actually is very hard in C-scheiss-scheiss

class Assert {
 
public:
  static bool  IsEqual(double v, double a, double eps=0.001)
  {
    assert(fabs(v-a)<eps);
  }

  static bool IsEqual(int v, int  a)
  {
    assert(v==a);
  }


  static bool IsEqual(const char *v, const char *a, int len=0)
  {
    if(len<=0) len=strlen(v);
    assert(strncmp(v,a,len)==0);
  }

  static bool IsTrue(bool l)
  {
    assert(l==true);
  }


  static bool IsFalse(bool l) 
  {
    assert(l==false);
  }


private:
  Assert();
  ~Assert();

};

