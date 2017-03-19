#ifndef PICKER_H
#define PICKER_H

#include <QObject>

class Picker : public QObject
{
    Q_OBJECT

    QString _name;
    QString _state;

public:
    explicit Picker (QString name, QObject *parent = 0);

    QString name () const { return _name; }
    Q_PROPERTY (QString name READ name CONSTANT);

    QString pickerState () const { return _state; }
    void setPickerState (QString const &state)
    { _state = state; emit pickerStateChanged (); }
    Q_PROPERTY (QString pickerState READ pickerState WRITE setPickerState
                NOTIFY pickerStateChanged);

signals:
    void pickerStateChanged ();

public slots:

};

#endif // PICKER_H
