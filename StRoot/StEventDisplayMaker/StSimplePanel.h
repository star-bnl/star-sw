/****************************************************************************
** Form interface generated from reading ui file 'Panel.ui'
**
** Created: Wed Jan 8 14:32:05 2003
**      by: The User Interface Compiler ($Id: StSimplePanel.h,v 1.1 2003/01/17 01:36:17 fine Exp $)
**
** WARNING! All changes made in this file will be lost!
****************************************************************************/

#ifndef StSimplePanel_H
#define StSimplePanel_H
#ifdef R__QT

#include <qgroupbox.h>

class QVButtonGroup;
class QButton;
class QWidget;
class QGridLayout;

class StSimplePanel : public QGroupBox {
Q_OBJECT

public:
    StSimplePanel( QWidget* parent = 0, const char* name = 0,int nColumns=3);
    ~StSimplePanel();
    void Add(QWidget *q, int col, int row =-1
            , unsigned int aligment = Qt::AlignCenter);

protected:
   QGridLayout *fLayout;
};

#endif // R__QT
#endif // StSimplePanel_H
