#ifndef ROOT_SHOWERMAIN
#define ROOT_SHOWERMAIN

class RootShower;
class ShowerMain {
protected:
  RootShower  *fThisShower;
  void main();
public:
   ShowerMain(){ main();}
   void show();
};
#endif
