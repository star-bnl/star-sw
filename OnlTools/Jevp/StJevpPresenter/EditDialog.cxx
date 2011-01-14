#include "EditDialog.h"

#include <QVBoxLayout>
#include <QKeyEvent>
#include <QTextEdit>
#include <QLabel>
#include <QDialogButtonBox>

EditDialog::EditDialog(QWidget *parent) : QDialog(parent)
{
  QVBoxLayout *layout = new QVBoxLayout(this);

  QLabel *label = new QLabel("Enter a comment for this reference histogram: ");
  _edit = new QTextEdit();
  _edit->setMinimumHeight(200);
  _edit->setMinimumWidth(400);
    
  layout->addWidget(label);
  layout->addWidget(_edit);


  QDialogButtonBox *buttonBox = new QDialogButtonBox(QDialogButtonBox::Ok);
  layout->add(buttonBox);
  connect(buttonBox, SIGNAL(accepted()), this, SLOT(accept()));
  
  setAttribute(Qt::WA_DeleteOnClose, false);

  setLayout(layout);
}

void EditDialog::accept()
{
  QDialog::accept();
}

QString EditDialog::text() {
  return _edit->toPlainText();
}


QString *EditDialog::run() {
  QString *ret;
  
  EditDialog *dialog = new EditDialog(NULL);
  int ans = dialog->exec();
  if(ans == QDialog::Accepted) {
    ret = new QString(dialog->_edit->toPlainText());
  }
  else {
    ret = NULL;
  }

  delete dialog;

  return ret;
}

