#ifndef _EDIT_DIALOG_H
#define _EDIT_DIALOG_H

#include <qdialog.h>
#include <QString>
#include <QTextEdit>

class EditDialog : public QDialog {
  Q_OBJECT

 public:
  EditDialog(QWidget *parent);
  QString text();

  ~EditDialog() {
    //printf("Deleting..\n");
  }

  static QString *run();

  public slots:
   void accept();

 private:
  QTextEdit *_edit;
};



#endif
