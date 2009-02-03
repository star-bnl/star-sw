/****************************************************************************
** Form interface generated from reading ui file 'Panel.ui'
**
** Created: Wed Jan 8 14:32:05 2003
**      by: The User Interface Compiler ($Id: StSimplePanel.h,v 1.3 2009/02/03 23:07:48 fine Exp $)
**
** WARNING! All changes made in this file will be lost!
****************************************************************************/

#ifndef StSimplePanel_H
#define StSimplePanel_H
#ifdef R__QT4
//MOC_SKIP_BEGIN

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
    void AddMulti(QWidget *q,int fromCol, int toCol=-1, int row =-1
            , unsigned int aligment = Qt::AlignCenter);

protected:
   QGridLayout *fLayout;
};

//MOC_SKIP_END
#endif // R__QT
#endif // StSimplePanel_H
