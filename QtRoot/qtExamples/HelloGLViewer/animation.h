#ifndef QGLViewer_Particle
#define QGLViewer_Particle

#include <QGLViewer/qglviewer.h>

class Particle
{
public :
  Particle();

  void init();
  void draw();
  void animate();
  float speed()    const { return speed_.z;}
  float position() const { return pos_.z;  }

private :
  qglviewer::Vec speed_, pos_;
  int age_, ageMax_;
};


class Viewer : public QGLViewer
{
Q_OBJECT
public:
   Viewer(QWidget *parent=0, Qt::WindowFlags flags=0): QGLViewer(parent,(const QGLWidget *)0,flags){}
protected :
  virtual void draw();
  virtual void init();
  virtual void animate();
  virtual void reset();
  virtual QString helpString() const;

private:
  int nbPart_;
  int nResetCounter_;
  Particle* particle_;
signals:
 void speed(float speed, float position);
 void resetNeeded();

};


#endif
