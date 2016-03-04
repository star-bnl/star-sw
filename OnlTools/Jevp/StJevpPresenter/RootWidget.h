#pragma once

#include <QWidget>
#include <QVBoxLayout>

#include <TCanvas.h>


// The root widget will be used to set the size
// it has a QVBoxLayout, so that the internal display widget can automatically resize
//
class RootWidget : public QWidget {
    Q_OBJECT

 private:
    QWidget *displayWidget;
    ULong_t wid;
    TCanvas *canvas;
    std::string *name;
    
 public:
    RootWidget(char*, QWidget *);
    TCanvas *getCanvas();
    TCanvas *GetCanvas() { return getCanvas(); }

    void resizeEvent(QResizeEvent *);
    void paintEvent(QPaintEvent *);
};
