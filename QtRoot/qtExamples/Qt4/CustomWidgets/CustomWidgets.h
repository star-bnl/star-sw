#ifndef CustomWidgets_H
#define CustomWidgets_H

#include <ui_CustomWidgets.h>

class CustomWidgets : public QFrame, public Ui::CustomWidgets
{
    Q_OBJECT

public:
    CustomWidgets(QWidget* parent = 0);
    virtual ~CustomWidgets();

public slots:
    virtual void init();
    virtual void destroy();
    virtual void widget_destroyed( QObject * );
    virtual void SliderValue(double v);

protected slots:
    virtual void languageChange();

};

#endif // CustomWidgets_H
