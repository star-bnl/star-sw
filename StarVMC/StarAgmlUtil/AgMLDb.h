#ifndef __AgMLDbFunctor__
#define __AgMLDbFunctor__

class AgMLDbFunctor;
 
class AgMLDbFunctor 
{
public:
  virtual void operator()( const char* tablename, const int row, double tr[4][4] )
  {
    for ( int i=0;i<4;i++ )
    for ( int j=0;j<4;j++ ) 
      tr[i][j]=0;
    for ( int i=0;i<4;i++ )
      tr[i][i]=1;
  }

  static AgMLDbFunctor *instance()                  { 
    if ( 0==sInstance ) { sInstance = new AgMLDbFunctor(); }
    return sInstance;   
  };

  static void           Register( AgMLDbFunctor *f ){ if ( sEnable ) sInstance = f; }
  static void           unlock(){ sEnable = true; }
  static void           lock(){ sEnable = false; }

private:
protected:
  static AgMLDbFunctor *sInstance;
  static bool           sEnable;
};

#endif
