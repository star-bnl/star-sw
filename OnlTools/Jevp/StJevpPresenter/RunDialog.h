#ifndef _RUNDIALOG_H_
#define _RUNDIALOG_H_

#include <QDialog>

class QLineEdit;
class QDir;
class QLabel;
class QPushButton;
class QTableWidget;
class QTableWidgetItem;

class RunDialog : public QDialog
{
  Q_OBJECT;
    
 public:

  RunDialog(QWidget *parent = 0);
  char *getResult();

 private slots:

  void ok();
  void cancel();
   void selectrow(QTableWidgetItem *curr, QTableWidgetItem *prev);
  // void selectrow(int row, int col);//

 private:

  void fill();
  void showFiles(const QDir &directory, const QStringList &files);

  QPushButton *createButton(const QString &text, const char *member);
  QLineEdit *createLineEdit(const QString &text = QString());

  void createRunTable();

  QLineEdit *runLineEdit;
  QLabel *runLabel;

  QTableWidget *runTable;
  QPushButton *okButton;
  QPushButton *cancelButton;
 

};

#endif
