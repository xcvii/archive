#include "picker.h"

Picker::Picker (QString name, QObject *parent)
    : QObject(parent)
    , _name (name)
    , _state ("walk")
{
}
