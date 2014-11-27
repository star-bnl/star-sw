//author Stefano Carrazza 12/03/09

#include <QtGui/QApplication>
#include "mywidget.h"

int main(int argc, char *argv[]){

    QApplication a(argc,argv);
    MyWidget *b=new MyWidget;
    b->show();
    return a.exec();
}

