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
  static void Cancel(const TObject *mk);
  static void Cancel(StMaker *mk);
  static int  SetCurrent(const TObject *mk,int kind=0);
  static int  SetCurrent(const StMaker *mk,int kind=0);
  static int  GetCurrent();
  static int  SetCurrent(int curr); 
  static int  SetUser(TObject *us); 
  static const char *GetUser(const TObject *us); 
  static const char *GetName(int id);  
  static void Pause(const char *tit="");
private:
  static void Ready();

  static int        fgCurr;
  static TObjArray *fgArr;
};  

 #endif
