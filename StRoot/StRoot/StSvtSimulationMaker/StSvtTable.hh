#ifndef STSVTTABLE_HH
#define STSVTTABLE_HH

class StSvtTable
{
 
public:
    StSvtTable();
    ~StSvtTable();

    double prob1(double num , double  sigma);
    double prob2(double num , double  sigma);

    void setFreq(int i);
    double getFreq(int j, int k);

private:
    double mFreq[1000][2000];

  
  //ClassDef(StSvtTable,1)

};

#endif
