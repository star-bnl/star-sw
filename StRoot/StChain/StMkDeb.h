#ifndef StMkDeb_h
#define StMkDeb_h
class TObject;
class TObjArray;
class StMaker; 
class StMkDeb 
{
public:
  StMkDeb(){};
 ~StMkDeb(){};
  static int  Register  (const TObject *mk);
  static int  Register  (StMaker       *mk);
  static int  SetCurrent(const TObject *mk);
  static int  SetCurrent(const StMaker *mk);
  static int  GetCurrent(){return fgCurr;} 
  static int  SetUser(TObject *us); 
  static const char *GetUser(const TObject *us); 
  static const char *GetName(int id);  
  static void Ready();

  static int        fgCurr;
  static TObjArray *fgArr;
};  

 #endif
