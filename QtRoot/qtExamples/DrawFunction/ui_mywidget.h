/********************************************************************************
** Form generated from reading ui file 'mywidget.ui'
**
** Created: Wed Aug 28 18:37:07 2013
**      by: Qt User Interface Compiler version 4.4.3
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_MYWIDGET_H
#define UI_MYWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QComboBox>
#include <QtGui/QDialogButtonBox>
#include <QtGui/QDoubleSpinBox>
#include <QtGui/QGridLayout>
#include <QtGui/QGroupBox>
#include <QtGui/QHBoxLayout>
#include <QtGui/QLabel>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_MyWidget
{
public:
    QHBoxLayout *horizontalLayout;
    QVBoxLayout *verticalLayout;
    QGroupBox *groupBox;
    QGridLayout *gridLayout;
    QLabel *label;
    QLabel *label_2;
    QDoubleSpinBox *doubleSpinBox;
    QDoubleSpinBox *doubleSpinBox_6;
    QLabel *label_3;
    QDoubleSpinBox *doubleSpinBox_2;
    QDoubleSpinBox *doubleSpinBox_5;
    QComboBox *comboBox;
    QDialogButtonBox *buttonBox;

    void setupUi(QWidget *MyWidget)
    {
    if (MyWidget->objectName().isEmpty())
        MyWidget->setObjectName(QString::fromUtf8("MyWidget"));
    MyWidget->resize(293, 160);
    MyWidget->setMaximumSize(QSize(350, 182));
    QIcon icon;
    icon.addPixmap(QPixmap(QString::fromUtf8(":/icons/snapshot.png")), QIcon::Normal, QIcon::Off);
    MyWidget->setWindowIcon(icon);
    horizontalLayout = new QHBoxLayout(MyWidget);
    horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
    verticalLayout = new QVBoxLayout();
    verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
    groupBox = new QGroupBox(MyWidget);
    groupBox->setObjectName(QString::fromUtf8("groupBox"));
    groupBox->setStyleSheet(QString::fromUtf8(""));
    gridLayout = new QGridLayout(groupBox);
    gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
    label = new QLabel(groupBox);
    label->setObjectName(QString::fromUtf8("label"));

    gridLayout->addWidget(label, 0, 0, 1, 1);

    label_2 = new QLabel(groupBox);
    label_2->setObjectName(QString::fromUtf8("label_2"));

    gridLayout->addWidget(label_2, 1, 0, 1, 2);

    doubleSpinBox = new QDoubleSpinBox(groupBox);
    doubleSpinBox->setObjectName(QString::fromUtf8("doubleSpinBox"));
    doubleSpinBox->setStyleSheet(QString::fromUtf8(""));
    doubleSpinBox->setMinimum(-100000);
    doubleSpinBox->setMaximum(100000);
    doubleSpinBox->setValue(-10);

    gridLayout->addWidget(doubleSpinBox, 1, 2, 1, 1);

    doubleSpinBox_6 = new QDoubleSpinBox(groupBox);
    doubleSpinBox_6->setObjectName(QString::fromUtf8("doubleSpinBox_6"));
    doubleSpinBox_6->setMinimum(-100000);
    doubleSpinBox_6->setMaximum(100000);
    doubleSpinBox_6->setValue(10);

    gridLayout->addWidget(doubleSpinBox_6, 1, 3, 1, 1);

    label_3 = new QLabel(groupBox);
    label_3->setObjectName(QString::fromUtf8("label_3"));
    label_3->setMinimumSize(QSize(85, 20));

    gridLayout->addWidget(label_3, 2, 0, 1, 2);

    doubleSpinBox_2 = new QDoubleSpinBox(groupBox);
    doubleSpinBox_2->setObjectName(QString::fromUtf8("doubleSpinBox_2"));
    doubleSpinBox_2->setMinimum(-100000);
    doubleSpinBox_2->setMaximum(100000);
    doubleSpinBox_2->setValue(-10);

    gridLayout->addWidget(doubleSpinBox_2, 2, 2, 1, 1);

    doubleSpinBox_5 = new QDoubleSpinBox(groupBox);
    doubleSpinBox_5->setObjectName(QString::fromUtf8("doubleSpinBox_5"));
    doubleSpinBox_5->setMinimum(-100000);
    doubleSpinBox_5->setMaximum(100000);
    doubleSpinBox_5->setValue(10);

    gridLayout->addWidget(doubleSpinBox_5, 2, 3, 1, 1);

    comboBox = new QComboBox(groupBox);
    comboBox->setObjectName(QString::fromUtf8("comboBox"));
    comboBox->setStyleSheet(QString::fromUtf8("QComboBox {\n"
"\n"
"border: 2px solid blue;\n"
"\n"
"border-radius: 10px;\n"
"\n"
"padding: 0 8px;\n"
"\n"
"background: white;\n"
"\n"
"}"));
    comboBox->setEditable(true);
    comboBox->setDuplicatesEnabled(false);

    gridLayout->addWidget(comboBox, 0, 2, 1, 2);


    verticalLayout->addWidget(groupBox);

    buttonBox = new QDialogButtonBox(MyWidget);
    buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
    buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok|QDialogButtonBox::Reset);

    verticalLayout->addWidget(buttonBox);


    horizontalLayout->addLayout(verticalLayout);

    QWidget::setTabOrder(doubleSpinBox, doubleSpinBox_6);
    QWidget::setTabOrder(doubleSpinBox_6, doubleSpinBox_2);
    QWidget::setTabOrder(doubleSpinBox_2, doubleSpinBox_5);
    QWidget::setTabOrder(doubleSpinBox_5, buttonBox);

    retranslateUi(MyWidget);

    QMetaObject::connectSlotsByName(MyWidget);
    } // setupUi

    void retranslateUi(QWidget *MyWidget)
    {
    MyWidget->setWindowTitle(QApplication::translate("MyWidget", "Draw Function", 0, QApplication::UnicodeUTF8));
    groupBox->setTitle(QApplication::translate("MyWidget", "Set Function Parameters (TF2)", 0, QApplication::UnicodeUTF8));
    label->setText(QApplication::translate("MyWidget", "Function:", 0, QApplication::UnicodeUTF8));
    label_2->setText(QApplication::translate("MyWidget", "Set X min-max :", 0, QApplication::UnicodeUTF8));
    label_3->setText(QApplication::translate("MyWidget", "Set Y min-max :", 0, QApplication::UnicodeUTF8));
    Q_UNUSED(MyWidget);
    } // retranslateUi

};

namespace Ui {
    class MyWidget: public Ui_MyWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_MYWIDGET_H
