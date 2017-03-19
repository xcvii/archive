TEMPLATE += app

QT += gui declarative xml network

HEADERS += \
    messageserver.h \
    orchard.h \
    picker.h

SOURCES += \
    messageserver.cc \
    main.cc \
    orchard.cc \
    picker.cc

OTHER_FILES += \
    messages.txt \
    scene.qml \
    Apple.qml \
    Picker.qml \
    Tree.qml

RESOURCES += \
    orchard.qrc
